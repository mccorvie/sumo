##
## Accessor functions for sumo API
## https://www.sumo-api.com/
##
## copyright Ryan McCorvie 2025
##

# online sources for sumo data
# https://sumodb.sumogames.de/Rikishi.aspx

library( tidyverse )
library(httr)
library(jsonlite)

divisions <- c( "makuuchi", "juryo", "makushita" )

months <- seq(1,11, 2)
days   <- 1:15

basho_name     <- c( "Hatsu", "Haru", "Natsu", "Nagoya", "Aki", "Kyushu" )
basho_location <- c( "Tokyo", "Osaka", "Tokyo", "Nagoya", "Tokyo", "Fukuoka")

compass_factor <- factor( c("West", "East"), ordered =T)
rank_factor    <- factor( c( "Jonokuchi", "Jonidan", "Sandanme", "Makushita", "Juryo", "Maegashira", "Komusubi", "Sekiwake", "Ozeki", "Yokozuna" ), ordered = T)


base_url <- "https://www.sumo-api.com"

get_matches <- \( basho_id, day, division, refresh = F )
{
  if( !exists( "matches_cache"))
    matches_cache <<-list()
  
  key <- paste0( basho_id, "-", sprintf( "%02d", day), "-", str_to_lower( division ))
  
  torikumi <- matches_cache[[key]]
  if( refresh || is.null( torikumi ))
  {
    qq       <- GET(paste0(base_url, "/api/basho/", basho_id, "/torikumi/", division, "/", day))
    torikumi <- fromJSON(rawToChar(qq$content))
    matches_cache[[key]] <<- torikumi
  }
  
  torikumi  
}


get_basho_id <- \( year, month )
{
  sprintf( "%04d%02d",year, month)
}

current_basho <- function()
{
  yy <- today() |> year()
  mm <- today() |> month()
  sprintf( "%d%02d", yy, rep( months, each=2)[mm])
}

prior_basho <- \( basho_id )
{
  year  = as.numeric( str_sub( basho_id, 1,4))
  month = as.numeric( str_sub( basho_id, 5,6))
  prev_year  = ifelse( month==1, year-1, year )
  prev_month = ifelse( month==1, 11, month-2 )
  sprintf( "%d%02d", prev_year, prev_month)  
}

basho_description <- \( basho_id )
{
  year  = as.numeric( str_sub( basho_id, 1,4))
  month = as.numeric( str_sub( basho_id, 5,6))
  paste( year, rep(basho_name,each=2)[month], "Basho in", rep(basho_location, each=2)[month])
}


active_rikishi <- \()
{
  if( exists( "active_rikishi_cache"))
    return( active_rikishi_cache )
  
  qq  <- GET(paste0(base_url, "/api/rikishis" ))
  rr  <- fromJSON(rawToChar(qq$content))
  active_rikishi_cache <<- tibble( rr$records )
  active_rikishi_cache
}

# all_rikishi

all_rikishi <- \()
{
  if( exists( "all_rikishi_cache"))
    return( all_rikishi_cache )
  
  qq  <- GET(paste0(base_url, "/api/rikishis" ))
  rr  <- fromJSON(rawToChar(qq$content))
  total <- rr$total
  skip  <- 0
  limit <- 1000
  all_rikishi_cache <<- NULL
  while( skip < total) 
  {
    qq  <- GET(paste0(base_url, "/api/rikishis?intai=true&skip=", skip, "&limit=", limit ))
    rr  <- fromJSON(rawToChar(qq$content))
    all_rikishi_cache <<- bind_rows( all_rikishi_cache, tibble( rr$records ))
    skip <- skip + limit
  }  
  
  all_rikishi_cache
}


sumo_name <- \( rikishi_id )
{
  filter( all_rikishi(), id == rikishi_id ) |> pull( shikonaEn )
}

sumo_id <- \( shikonaEn )
{
  filter( active_rikishi(), shikonaEn == !!shikonaEn ) |> pull( id )
}


basho_sumo_rank <- \(basho_id = current_basho())
{
  if( !exists( "basho_rikishi_ranks"))
    basho_rikishi_ranks <<- list()

  cache <- basho_rikishi_ranks[[ basho_id ]]
  if( !is.null( cache ))
    return( cache )
  
  qq  <- GET(paste0(base_url, paste0( "/api/ranks?bashoId=", basho_id )))
  banzuke_rank  <- fromJSON(rawToChar(qq$content))
  
  rank_table <- str_split( banzuke_rank$rank, " ", simplify = T)
  colnames( rank_table ) <- c( "rank_name", "rank_number", "compass")
  rank_table <- as_tibble( rank_table ) |> mutate( rank_number = as.numeric( rank_number))
  rank_table$rank_name <- factor( rank_table$rank_name, rank_factor )
  rank_table$compass   <- factor( rank_table$compass, compass_factor )
  
  banzuke_rank <- banzuke_rank |> bind_cols(  rank_table ) |> 
    arrange( rank_name, -rank_number, compass ) 
  
  basho_rikishi_ranks[[ basho_id]] <<- banzuke_rank
  banzuke_rank
}
