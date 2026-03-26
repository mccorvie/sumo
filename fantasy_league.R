##
## Track Fantasy Sumo Scores 
##


picks_by_name <- list(
  Curtis = c( "Onosato",	"Aonishiki",	"Oho",	"Kusano",	"Mitakeumi" ),
  Jenny  = c( "Onosato",	"Aonishiki",	"Ichiyamamoto",	"Ura",	"Sadanoumi" ),
  Colleen = c( "Onosato",	"Wakatakakage",	"Atamifuji",	"Kusano",	"Mitakeumi" ),
  Ryan  = c( "Onosato",	"Wakatakakage",	"Abi",	"Daieisho",	"Tobizaru" ),
  Sonia = c( "Hoshoryu",	"Aonishiki",	"Wakamotoharu",	"Daieisho",	"Asakoryu" )
)

division = "Makuuchi"
basho_id = "202509"
max_day <- 15

match_t = NULL

for( day in 1:max_day)
{
  matches    <- get_matches( basho_id, day, division )
  torikumi_t <- matches$torikumi |> tibble()
  
  east <- torikumi_t |> 
    mutate( win = winnerId == eastId, day = day ) |> 
    select( rikishiId = eastId, opponentId = westId, win, day )
  
  west <- torikumi_t |> 
    mutate( win = winnerId == westId, day = day ) |> 
    select( rikishiId = westId, opponentId = eastId, win, day )
  
  match_t <- bind_rows( match_t, east, west )
}

picks_by_id <- map( picks_by_name, \(names) map_dbl( names,sumo_id ))

fantasy_t <- match_t
for( person in names( picks_by_id))
  fantasy_t <- fantasy_t |> mutate( !!person := (rikishiId %in% picks_by_id[[person]])&win )


fantasy_t <- fantasy_t |> group_by( day ) |> 
  summarize( across( names(picks_by_id), sum )) |> 
  mutate( across( names( picks_by_id), cumsum)) |> 
  pivot_longer( names(picks_by_id), names_to = "name",values_to = "wins") |> 
  mutate( match_count = day*5, win_rate = wins/match_count)

ggplot( fantasy_t, aes( x=day, y=wins, color=name)) + geom_line() + geom_point()+
  labs( title="Fantasy Sumo Wins" ) +theme_minimal()

ggplot( fantasy_t, aes( x=day, y=win_rate, color=name)) + geom_line() + geom_point()+
  labs( title="Fantasy Sumo Win Rate" ) +theme_minimal()


map( picks_by_id, \(picks) filter( match_t, rikishiId %in% picks) |> summarize( wins = sum( win )) |> pull( wins ) )
