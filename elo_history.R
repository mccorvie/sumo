
##
## Compute all the elo scores by time
## copyright Ryan McCorvie 2025
##


library( tidyverse )
library(httr)
library(jsonlite)
source( "sumo_api.R" )
source( "elo_functions.R" )


elo_history <- tibble()  

current_elo <- tibble( 
  rikishiId = as.numeric(NULL), 
  elo=as.numeric(NULL), 
  old_elo=as.numeric( NULL), 
  total_matches=as.numeric(NULL),
  total_wins=as.numeric(NULL)
)

learning_rate = 10
new_lr_boost  = 0
new_time      = 10 # number of matches
momentum_rate = 0.30


max = list( year = 2025, month=9, day=5)


for( year in 2000:2025 )
  for( month in months )
  {
    cat( "\n", get_basho_id( year, month ), " " )
    for( day in days )
    {
      if( year > max$year ) next
      if( year == max$year && month > max$month ) next
      if( year == max$year && month == max$month && day > max$day ) next
      
      cat( "*")
      for( division in divisions )
      {

        torikumi   <- get_matches( get_basho_id( year, month ), day, division ) 
        if( is.null( torikumi$torikumi ))
        {
          cat( "!! no results for ", get_basho_id( year, month ), " ", day, " ", division, "\n" )
          next
        }

        torikumi_t <- torikumi$torikumi |> tibble()
        
        tt <- torikumi_t |> pull( matchNo ) |> table() 
        if( any( tt>1 )) 
        {
          cat( "!!", division, "duplicates ")
          # torikumi_t <- torikumi_t |> group_by( matchNo) |>
          #   mutate( cnt = 1:n()) |>
          #   filter( cnt== max(cnt)) |>
          #   select( -cnt) |>
          #   ungroup()
          next
        }
        
        east <- torikumi_t |> 
          mutate( win = winnerId == eastId ) |> 
          select( rikishiId = eastId, opponentId = westId, win, bashoId, division, day, matchNo )
        
        west <- torikumi_t |> 
          mutate( win = winnerId == westId ) |> 
          select( rikishiId = westId, opponentId = eastId, win, bashoId, division, day, matchNo )
        
        
        elo_update <- bind_rows( east, west ) |> 
          left_join( current_elo, by = "rikishiId" ) |> 
          left_join( select( current_elo, rikishiId, opponent_elo = elo ), by = join_by( opponentId == rikishiId )) |> 
          mutate( 
            total_matches = coalesce( total_matches, 0 ) + 1,
            total_wins = coalesce( total_wins, 0 ) + win,
            old_elo = coalesce( old_elo, 1500 ),
            elo = coalesce( elo, 1500 ), 
            opponent_elo = coalesce( opponent_elo, 1500 ),
            pwin = elo_to_pwin( elo, opponent_elo ),
            lr  = learning_rate + new_lr_boost * pmax(0,(new_time-total_matches)/new_time),
            new_elo = elo + (1-momentum_rate) * lr * (win-pwin) + momentum_rate * (elo-old_elo)
          )
        
      
        elo_history <- bind_rows( elo_history, elo_update )

        current_elo <- select( elo_update, rikishiId, old_elo = elo, elo=new_elo, total_matches, total_wins ) |> 
          bind_rows( anti_join( current_elo, elo_update, by = "rikishiId"))
        
        tt <- current_elo |> pull( rikishiId) |> table() 
        if( any( tt > 1)) cat( paste( division, "-> dup in current_elo\n"))
      }
    }
  }


saveRDS(matches_cache, "matches_cache.Rdata")
saveRDS( elo_history,  "elo_history.Rdata")

saveRDS( sumo_name_t,  "sumo_name_t.Rdata")


matches_cache <- readRDS( "matches_cache.Rdata")
#saveRDS( elo_history, "elo_history.Rdata")


