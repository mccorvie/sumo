
##
## Sumo ELO calculations
## copyright Ryan McCorvie 2025
##

# online sources for sumo data
# https://sumodb.sumogames.de/Rikishi.aspx
# https://www.sumo-api.com/


rikishi_names <- list()
sumo_name <- \( rikishi_id )
{
  cache <- rikishi_names[[ as.character( rikishi_id)]]
  if( !is.null( cache ))
    return( cache )
  
  qq    <- GET(paste0(base_url, "/api/rikishi/", rikishi_id))
  stats <- fromJSON(rawToChar(qq$content))
  rikishi_names[[ as.character( rikishi_id)]] <- stats$shikonaEn
  stats$shikonaEn
}

rikishi_ranks <- list()
sumo_rank <- \(rikishi_id)
{
  cache <- rikishi_ranks[[ as.character( rikishi_id)]]
  if( !is.null( cache ))
    return( cache )
  
  qq    <- GET(paste0(base_url, "/api/rikishi/", rikishi_id))
  stats <- fromJSON(rawToChar(qq$content))
  rikishi_names[[ as.character( rikishi_id)]] <- stats$currentRank
  stats$currentRank
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

rikishiId = 3114 # lowest ever elo
rikishiId = 2 # asanoyama, got a covid suspension

rikishi_elo <- elo_history |> filter( rikishiId == !!rikishiId) |> arrange( year, month, day) |> 
  mutate( date = ymd( sprintf( "%d%02d%02d", year, month, day))) 

ggplot( rikishi_elo, aes( date, new_elo, color=win )) + geom_point( size=0.75) + labs( title =sumo_name( rikishiId) )

elo_history |> mutate( brier_score = (pwin-win)^2) |> summarize( brier_score = mean( brier_score ))


ggplot( elo_history, aes( pwin)) + geom_density()

buckets = 20
calibration_plot <- elo_history |> 
  mutate( quantile = floor(pwin*buckets) ) |> 
  group_by( quantile ) |> 
  summarize( win=mean(win), cnt=n()) |> 
  mutate( quantile = (quantile +0.5)/ buckets)

ggplot( calibration_plot, aes( quantile, win)) + geom_point() +geom_line( aes(quantile, quantile), color="lightblue") 


#anti_join( current_elo, rename( rikishis_t, rikishiId = id ), by = "rikishiId")


sqrt(sqrt(10))
elo_to_pwin( 400,0) # 10-to-1
elo_to_pwin( 200,0) # 3-to-1
elo_to_pwin( 100,0) # 2-to-1  (maybe 7-t0-4)
elo_to_pwin( 50,0)  # 4-to-3


filter( current_elo, elo>2000) |> rowwise() |> mutate( shikona = sumo_name( rikishiId))
                                                       
                                                       
filter( elo_history, new_elo == min( new_elo))

top <- filter( elo_history, year==2025, month==7, day==15, new_elo > 1900)  |>  rowwise() |> mutate( shikona = sumo_name( rikishiId)) |> 
  arrange(new_elo)

top |> arrange( -new_elo)

rikishi_id <- 8850


last_tournament <- filter( elo_history, year==2025, month==7, day==1 )  |>  rowwise() |>
  mutate( shikona = sumo_name( rikishiId), rank = sumo_rank( rikishiId )) |> 
  group_by( .drop = T ) |> 
  arrange(new_elo) 
  


direction_factor <- factor( c("West", "East"))
rank_factor      <- factor( c( "Makushita", "Juryo", "Maegashira", "Komusubi", "Sekiwake", "Ozeki", "Yokozuna" ))

rank_table <- str_split( last_tournament$rank, " ", simplify = T)
colnames( rank_table ) <- c( "rank_name", "rank_number", "direction")
rank_table <- as_tibble( rank_table ) |> mutate( rank_number = as.numeric( rank_number))
rank_table$rank_name <- factor( rank_table$rank_name, rank_factor )
rank_table$direction <- factor( rank_table$direction, direction_factor )

last_tournament0 <- last_tournament |> bind_cols(  rank_table ) |> 
  arrange( rank_name, -rank_number, direction ) |> 
  mutate( rank=1:n())

ggplot( last_tournament0, aes( rank, old_elo, color=rank_name)) + geom_point(size=3) + theme_minimal() + scale_color_brewer( palette ="Set2") +
  labs( title = "ELO vs. Sumo Association Rank")

filter( last_tournament0, new_elo > 2000)
