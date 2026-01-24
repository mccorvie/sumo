##
## Compute all the elo scores by time
## copyright Ryan McCorvie 2025
##

library( tidyverse )
library( progress )
source( "sumo_api.R" )
source( "elo_reports.R")

options(nwarnings = 1000)  

max_day = day_number(today(), current_basho())
years <- 1990:2026

if( Sys.getenv("SUMO_CACHE") == "1" )
{
  cat( "loading sumo match cache...\n")
  matches_cache <- readRDS( "matches_cache.Rdata")
  sumo_name_t   <- readRDS( "sumo_name_t.Rdata")
}


past_bashos <- map( years, \(year) map( months, \(month) get_basho_id( year, month) )) |> 
  unlist() |> keep( \(basho) basho < current_basho() & basho != "202005" & basho != "201103" ) 

#  202005 canceled because of covid,  201103 canceled because of Fukushima 

# refresh cache for current basho
# this is to pick up data updates, such as match results from yesterday
div_matches <- map( 1:max_day, \(day) map( divisions, \(div) get_matches( current_basho(), day, div, T )))
saveRDS( matches_cache, "matches_cache.Rdata")

makuuchi_matches <- map( 1:max_day, \(day) get_matches( current_basho(), day, "makuuchi" ))

rikishi <- map( makuuchi_matches, \( match ) c( match$torikumi$eastShikona, match$torikumi$westShikona)) |> 
  unlist() |> unique()

headshot_exists <- file.exists( paste0( "sumo_headshots/", rikishi, ".jpg"))

if( !all( headshot_exists ))
  cat( "!!! Headshot missing for: ", paste( rikishi[!headshot_exists], collapse = " "), "\n")


faceoff_list  <- c( list_flatten( map( past_bashos, basho_faceoff, .progress=T )), basho_faceoff( current_basho(), max_day ))
warnings()
faceoff_list <- keep( faceoff_list, \(x) !is.null( x ))
saveRDS( faceoff_list, "faceoff_list.Rdata")

#faceoff_list <- readRDS( "faceoff_list.Rdata" )


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
momentum_rate = 0.3

elo_history <- vector( "list", length(faceoff_list))

pb <- progress_bar$new(
  format = "  calculating [:bar] eta: :eta",
  total = length(faceoff_list), clear = FALSE, width = 60
)


for( idx in seq_along( faceoff_list ))
{
  pb$tick()
  
  faceoff_t <- faceoff_list[[ idx ]]

  elo_update <- faceoff_t |> 
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
      new_elo = elo + lr * (win-pwin) + momentum_rate * (elo-old_elo)
    )
  
  elo_history[[ idx ]] <- elo_update
  
  current_elo <- select( elo_update, rikishiId, old_elo = elo, elo=new_elo, total_matches, total_wins ) |> 
    bind_rows( anti_join( current_elo, elo_update, by = "rikishiId"))
  
}

elo_history <- bind_rows( elo_history )
saveRDS( elo_history,  "elo_history.Rdata")



# to do ideas
#  1) elo of each banzuke rank
#  2) higher momentum + higher learning rate?
#

