

##
## Sumo ELO calculations
## copyright Ryan McCorvie 2025
##

# online sources for sumo data
# https://sumodb.sumogames.de/Rikishi.aspx
# https://www.sumo-api.com/

library(tidyverse)
library(plotly)
library(httr)
library(jsonlite)
source( "sumo_api.R" )


elo_as_of <- \( basho_id, day )
{
  elo_history |> 
    arrange( bashoId, day ) |> 
    filter( bashoId < basho_id | bashoId == basho_id & day <= !!day ) |> 
    group_by( rikishiId ) |> 
    summarize( elo = last( new_elo ), total_matches = last(total_matches), total_wins = last(total_wins)) |> 
    ungroup()
}




ggplot( current_elo, aes( x=elo )) + geom_density()


rikishiId = 14 # Tamawashi
rikishiId = 3081 # Hakuho
rikishiId = 8850 # Onosato
rikishiId = 12 # Watatakakge

rikishiId = 8850 # Onosato     
rikishiId =   12 # Wakatakakage
rikishiId =    7 # Kirishima   
rikishiId =   44 # Takayasu    
rikishiId =   20 # Kotozakura  
rikishiId = 8854 # Aonishiki   
rikishiId =   22 # Abi         
rikishiId =   14 # Tamawashi   
rikishiId =   41 # Oho         
rikishiId =   13 # Wakamotoharu
rikishiId =   24 # Hiradoumi   
rikishiId =   56 # Gonoyama    
rikishiId =    3 # Hakuoho     
rikishiId =    8 # Kotoshoho   
rikishiId =   74 # Atamifuji   
rikishiId =   11 # Ichiyamamoto
rikishiId = 8853 # Onokatsu 
rikishiId =   19 # Hoshoryu

rikishiId = 3114 # lowest ever elo
rikishiId = 2 # asanoyama, got a covid suspension

rikishiId = 21

##
##  Rikishi Elo history
##

rikishi_elo <- elo_history |> filter( rikishiId == !!rikishiId) |> arrange( year, month, day) |> 
  mutate( date = ymd( sprintf( "%s%02d", bashoId, day)))  |> 
  filter( date >= ymd( "20250101"))
  
#ggplot( rikishi_elo, aes( date, new_elo )) + geom_line() +geom_point( aes(color=win),size=1) + labs( title =sumo_name( rikishiId) )
ggplot( rikishi_elo, aes( date, new_elo ))  +geom_point( aes(color=win),size=1) + labs( title =sumo_name( rikishiId) )


##
##  Accuracy statistics
##

# brier score

elo_history |> 
  mutate( 
    brier_score = (pwin-win)^2,
    log_loss = -1/log(2) * (win * log(pwin) + (1-win)*log(1-pwin)),
  ) |> 
  summarize( brier_score = mean( brier_score ), log_loss = mean( log_loss ))

elo_history |> 
  filter( abs(pwin-0.5)>0.2) |> 
  mutate( 
    brier_score = (pwin-win)^2,
    log_loss = -1/log(2) * (win * log(pwin) + (1-win)*log(1-pwin)),
  ) |> 
  summarize( brier_score = mean( brier_score ), log_loss = mean( log_loss ))

ggplot( elo_history, aes( pwin)) + geom_density()

# calibration plot

buckets = 20
calibration_plot <- elo_history |> 
  mutate( quantile = floor(pwin*buckets) ) |> 
  group_by( quantile ) |> 
  summarize( win=mean(win), cnt=n(), win_std = win*(1-win)/sqrt( cnt)) |> 
  mutate( quantile = (quantile +0.5)/ buckets)

ggplot( calibration_plot, aes( quantile, win)) + 
  geom_errorbar(aes( min=win-2*win_std, max = win+2*win_std), color="plum")+
  geom_point( size=1) +
  geom_line( aes(quantile, quantile), color="lightblue") +
  theme_minimal()


##
##  Elo to odds calculations
##

sqrt(sqrt(10))
elo_to_pwin( 400,0) # 10-to-1
elo_to_pwin( 200,0) # 3-to-1
elo_to_pwin( 100,0) # 2-to-1  (maybe 7-t0-4)
elo_to_pwin( 50,0)  # 4-to-3

##
##  High Elo rikishi
##

sumo_name_t <- all_rikishi() |> select( rikishiId = id, shikona = shikonaEn )
current_elo1 <- current_elo |> left_join( sumo_name_t, by="rikishiId")

filter( elo_history, new_elo == min( new_elo))

top <- filter( elo_history, bashoId == "202507", day==15, new_elo > 1800)  |> 
  left_join( sumo_name_t, by="rikishiId" ) |>   
  arrange(new_elo)

top |> arrange( -new_elo) |> print(n=20)


##
##  Elo vs official rank
##

basho_id <- "202509"

banzuke_rank <- basho_sumo_rank( basho_id ) |> filter( rank_name != "Jonokuchi", rank_name != "Jonidan", rank_name != "Sandanme")

banzuke_elo <- banzuke_rank |> left_join( elo_as_of( prior_basho( basho_id), 15 ), by="rikishiId" ) |> 
  mutate( elo = coalesce( elo, 1500), ordinal_rank = 1:n())  |> 
  left_join( sumo_name_t, by="rikishiId" )

ggplot( banzuke_elo, aes( ordinal_rank, elo, color=rank_name)) + geom_point(size=3) + theme_minimal() + scale_color_brewer( palette ="Set2") +
  labs( title = "Elo vs. Sumo Association Rank")


filter( banzuke_elo ) |> arrange( -elo) |> head( 20)
filter( banzuke_elo, rank_name == "Juryo") |> arrange( -elo)


##
## tournament movement
##

basho_id <- "202509"
day <- 3

banzuke_rank <- basho_sumo_rank( basho_id ) |> 
  filter( rank_name != "Jonokuchi", rank_name != "Jonidan", rank_name != "Sandanme", rank_name != "Makushita", rank_name != "Juryo" )



banzuke_elo <- banzuke_rank |> left_join( elo_as_of( prior_basho( basho_id), 15 ), by="rikishiId" ) |> 
  mutate( elo = coalesce( elo, 1500), ordinal_rank = 1:n())  |> 
  rename( pre_tournament_elo = elo ) |> 
  left_join( elo_as_of( basho_id, day ), by="rikishiId" ) |> 
  left_join( sumo_name_t, by="rikishiId" )


pp<- ggplot( banzuke_elo, aes( ordinal_rank, elo, color=rank_name, text=shikona)) + 
  geom_segment( aes( x=ordinal_rank, y=pre_tournament_elo, xend=ordinal_rank, yend = elo), arrow=arrow(length=unit(0.1, "inches"))) +
  geom_point(size=1) + 
  theme_minimal() + scale_color_brewer( palette ="Set2") +
  labs( title = paste0("Elo movement during ", month, "/", year))

ggplotly(pp, tooltip = "text")


sumo_id( "Tobizaru")

##
## Match sheet
##

basho_id <- "202509"
day <- 3
division <- "Makuuchi"

pre_basho <- elo_as_of( prior_basho(basho_id), 15 ) |> 
  rename( 
    pre_basho_elo = elo,
    pre_basho_total_matches = total_matches,
    pre_basho_total_wins    = total_wins
  )

match_elo <- elo_as_of( basho_id, day) |> 
  left_join( pre_basho, by="rikishiId" ) |> 
  mutate( wins = total_wins - pre_basho_total_wins, matches = total_matches - pre_basho_total_matches, losses = matches - wins ) |> 
  select( rikishiId, elo, wins, losses )

matches <- get_matches( basho_id, day, division )
matches <- matches$torikumi |> tibble()

matches0 <- matches |> 
  left_join( rename( match_elo, elo_east = elo, wins_east = wins, losses_east = losses ), by = join_by( eastId == rikishiId )) |> 
  left_join( rename( match_elo, elo_west = elo, wins_west = wins, losses_west = losses ), by = join_by( westId == rikishiId )) |> 
  mutate(
    pwin_east = elo_to_pwin( elo_east, elo_west ),
    pwin_west = 1-pwin_east,
    odds_east = exp( (elo_east - elo_west) * elo_factor ), 
    odds_west = 1/odds_east,
    surprisal_east =  -log(pwin_east)/log(2),
    surprisal_west = -log(pwin_west)/log(2),
    elo_mismatch = abs( elo_east-elo_west ),
    resolved = winnerId != 0,
    winner_east = ifelse( resolved,  winnerId == eastId, NA ),
    surprisal = ifelse( resolved, ifelse( winner_east, surprisal_east, surprisal_west ), NA ),
  )


# prediction sheet
matches0 |> 
  mutate( 
    record_east = paste0( wins_east, "-", losses_east),
    record_west = paste0( wins_west, "-", losses_west)
  ) |> 
  select( eastId, eastShikona, elo_east, pwin_east, odds_east, surprisal_east, westId, westShikona, elo_west, pwin_west, odds_west, surprisal_west, elo_mismatch ) |> 
  print( n=35)


select( -id, -bashoId ) |> print(n=35)

matches0 |> arrange( -elo_mismatch ) |> 
  select( eastId, eastShikona, elo_east, pwin_east, westId, westShikona, elo_west, pwin_west, winnerEn, surprisal)

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
  

basho_id = "202509"
max_day <- 3

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
  pivot_longer( names(picks_by_id), names_to = "name",values_to = "wins")

ggplot( fantasy_t, aes( x=day, y=wins, color=name)) + geom_line() + geom_point()+
  labs( title="Fantasy Sumo Wins" ) +theme_minimal()


map( picks_by_id, \(picks) filter( match_table, rikishiId %in% picks) |> summarize( wins = sum( win )) |> pull( wins ) )



##
##  Reset cache for some day (after results are in, e.g, )
##

# reset some day
basho_id = "202509"
day = 4
map( divisions, \(div) get_matches( basho_id, day, div, T ))
