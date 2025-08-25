
##
## Sumo ELO calculations
## copyright Ryan McCorvie 2025
##

# online sources for sumo data
# https://sumodb.sumogames.de/Rikishi.aspx
# https://www.sumo-api.com/

library( tidyverse )
library(httr)
library(jsonlite)


divisions <- c( "juryo", "makuuchi", "makushita" )
months <- seq(1,11, 2)
days   <- 1:15

get_basho_id <- \( year, month )
{
  sprintf( "%04d%02d",year, month)
}

base_url <- "https://www.sumo-api.com"

matches_cache <<- list()
get_matches <- \( basho_id, day, division )
{
  key <- paste0( basho_id, "-", sprintf( "%02d", day), "-", str_to_lower( division ))
  
  torikumi <- matches_cache[[key]]
  if( is.null( torikumi ))
  {
    qq       <- GET(paste0(base_url, "/api/basho/", basho_id, "/torikumi/", division, "/", day))
    torikumi <- fromJSON(rawToChar(qq$content))
    matches_cache[[key]] <<- torikumi
  }
  
  torikumi  
}


elo_factor <<- log(10)/400
elo_to_pwin <- \( elo1, elo2 )
{
  1/(1+exp( elo_factor * (elo2-elo1)))
}

elo_history <- tibble()  
current_elo <- tibble( rikishiId = as.numeric(NULL), elo=as.numeric(NULL))
learning_rate = 20

# year = 2024
# month = 11
# day = 2
# divisions = c( "juryu", "makuuchi" )


for( year in 2000:2025 )
  for( month in months )
  {
    cat( "\n", get_basho_id( year, month ), " " )
    for( day in days )
    {
      cat( "*")
      for( division in divisions )
      {
        torikumi   <- get_matches( get_basho_id( year, month ), day, division ) 
        if( is.null( torikumi$torikumi ))
        {
          cat( "\n!! no results for ", get_basho_id( year, month ), " ", day, " ", division, "\n")
          next
        }

        torikumi_t <- torikumi$torikumi |> tibble()
        
        east <- torikumi_t |> 
          mutate( win = winnerId == eastId ) |> 
          select( rikishiId = eastId, opponentId = westId, win  )
        
        west <- torikumi_t |> 
          mutate( win = winnerId == westId ) |> 
          select( rikishiId = westId, opponentId = eastId, win  )
        
        
        elo_update <- bind_rows( east, west ) |> 
          left_join( current_elo, by = "rikishiId" ) |> 
          left_join( select( current_elo, rikishiId, opponent_elo = elo ), by = c( opponentId = "rikishiId" )) |> 
          mutate( 
            elo = coalesce( elo, 1500 ), 
            opponent_elo =coalesce( opponent_elo, 1500 ),
            pwin = elo_to_pwin( elo, opponent_elo ),
            new_elo = elo + learning_rate * (win-pwin),
            year = year,
            month = month,
            day = day
          ) 
        
        
        elo_history <- bind_rows( elo_history, select( elo_update, rikishiId, win, pwin, old_elo= elo, new_elo, opponent_elo, year, month, day ))
        
        current_elo <- select( elo_update, rikishiId, elo=new_elo ) |> 
          bind_rows( anti_join( current_elo, elo_update, by = "rikishiId"))
        
      }
    }
  }

