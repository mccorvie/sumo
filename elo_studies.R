
##
## Sumo ELO calculations
## copyright Ryan McCorvie 2025
##

# online sources for sumo data
# https://sumodb.sumogames.de/Rikishi.aspx
# https://www.sumo-api.com/


ggplot( current_elo, aes( x=elo )) + geom_density()


rikishiId = 14 # Tamawashi
rikishiId = 3081 # Hakuho
rikishiId = 8850 # Onosato
rikishiId = 12 # Watatakakge

rikishi_elo <- elo_history |> filter( rikishiId == !!rikishiId) |> arrange( year, month, day) |> 
  mutate( date = ymd( sprintf( "%d%02d%02d", year, month, day)))


ggplot( rikishi_elo, aes( date, old_elo, color=win )) + geom_point( size=0.75)

elo_history |> mutate( brier_score = (pwin-win)^2) |> summarize( brier_score = mean( brier_score ))


ggplot( elo_history, aes( pwin)) + geom_density()

buckets = 20
calibration_plot <- elo_history |> 
  mutate( quantile = floor(pwin*buckets) ) |> 
  group_by( quantile ) |> 
  summarize( win=mean(win), cnt=n()) |> 
  mutate( quantile = (quantile +0.5)/ buckets)

ggplot( calibration_plot, aes( quantile, win)) + geom_point() +geom_line( aes(quantile, quantile), color="lightblue") 



anti_join( current_elo, rename( rikishis_t, rikishiId = id ), by = "rikishiId")
