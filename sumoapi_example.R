##
## Sumo API examples
## copyright Ryan McCorvie 2025
##


library( tidyverse )
library(httr)
library(jsonlite)

base_url <- "https://www.sumo-api.com"

# 1. List all rikishi
resp <- GET(paste0(base_url, "/api/rikishis"))
rikishis <- fromJSON(rawToChar(resp$content))
str(rikishis)

rikishis_t <- rikishis$records |> tibble()


# 2. Get stats for a specific rikishi (e.g., ID = 123)
rikishi_id <- 12
resp2 <- GET(paste0(base_url, "/api/rikishi/", rikishi_id, "/stats"))
stats <- fromJSON(rawToChar(resp2$content))
str(stats)

# 3. List tournaments (bashos)
# Suppose bashoId = 202501
basho_id <- 202501
resp3 <- GET(paste0(base_url, "/api/basho/", basho_id))
basho_info <- fromJSON(rawToChar(resp3$content))
str(basho_info)

# 4. Get match schedule for a division/day
basho_id <- 200001
division <- "makuuchi"
day <- 1
resp4 <- GET(paste0(base_url, "/api/basho/", basho_id, "/torikumi/", division, "/", day))


torikumi <- fromJSON(rawToChar(resp4$content))
torikumi_t <- torikumi$torikumi |> tibble()






library( tidyverse )
library(httr)
library(jsonlite)

base_url <- "https://www.sumo-api.com"


matches_cache <- list()
matches_cache$aaa <- 666

#is.null( )

get_basho_id <- \( year, month )
{
  sprintf( "%04d%02d",year, month)
}


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


divisions <- c( "juryo", "makuuchi" )
months <- seq(1,11, 2)
days   <- 1:15
division <- "makuuchi"

year = 2024

matches <- tibble()

for( month in months )
{
  for( day in days )
  {
    torikumi <- get_matches( get_basho_id( year, month ), day, division ) 
    torikumi_t <- torikumi$torikumi |> tibble()
    
    matches <- matches |> bind_rows( tibble( torikumi$torikumi ))
  }
}

year = 2024
month = 11
day = 1
division = "makuuchi"

torikumi <- get_matches( get_basho_id( year, month ), day, division ) 
torikumi_t <- torikumi$torikumi |> tibble()




east <- torikumi_t |> 
  mutate( win = winnerId == eastId ) |> 
  select( rikishiId = eastId, win  )
  

current_elo <- tibble( sumoid)




