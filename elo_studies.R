

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


compass_factor <- factor( c("West", "East"), ordered =T)
rank_factor    <- factor( c( "Jonokuchi", "Jonidan", "Sandanme", "Makushita", "Juryo", "Maegashira", "Komusubi", "Sekiwake", "Ozeki", "Yokozuna" ), ordered = T)


active_rikishi <- \()
{
  if( exists( "active_rikishi_cache"))
    return( active_rikishi_cache )
  
  qq  <- GET(paste0(base_url, "/api/rikishis" ))
  rr  <- fromJSON(rawToChar(qq$content))
  active_rikishi_cache <<- tibble( rr$records )
  active_rikishi_cache
}

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
  filter( all_rikishi(), shikonaEn == !!shikonaEn ) |> pull( id )
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


basho_rikishi_ranks <- list()
basho_sumo_rank <- \(basho_id = current_basho())
{
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


elo_as_of <- \( basho_id, day )
{
  year  = as.numeric( str_sub( basho_id, 1,4))
  month = as.numeric( str_sub( basho_id, 5,6))
  
  elo_history |> 
    arrange( year, month, day ) |> 
    filter( year < !!year | 
              year == !!year & month < !!month | 
              year == !!year & month == !!month & day <= !!day ) |> 
    group_by( rikishiId ) |> 
    summarize( elo = last( new_elo )) |> 
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

rikishiId = 37

##
##  Rikishi Elo history
##

rikishi_elo <- elo_history |> filter( rikishiId == !!rikishiId) |> arrange( year, month, day) |> 
  mutate( date = ymd( sprintf( "%d%02d%02d", year, month, day)))  
  
ggplot( rikishi_elo, aes( date, new_elo, color=win )) + geom_point( size=0.75) + labs( title =sumo_name( rikishiId) )


##
##  Accuracy statistics
##

# brier score

elo_history |> mutate( brier_score = (pwin-win)^2) |> summarize( brier_score = mean( brier_score ))
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

top <- filter( elo_history, year==2025, month==7, day==15, new_elo > 1800)  |> 
  left_join( sumo_name_t, by="rikishiId" ) |>   
  arrange(new_elo)

top |> arrange( -new_elo)


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
day <- 1

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




##
## Match sheet
##

basho_id <- "202509"
day <- 2
division <- "Makuuchi"

match_elo <- elo_as_of( basho_id, day)


matches <- get_matches( basho_id, day, division )
matches <- matches$torikumi |> tibble()

matches0 <- matches |> 
  left_join( rename( match_elo, elo_east = elo ), by = join_by( eastId == rikishiId )) |> 
  left_join( rename( match_elo, elo_west = elo ), by = join_by( westId == rikishiId )) |> 
  mutate(
    pwin_east = elo_to_pwin( elo_east, elo_west ),
    pwin_west = 1-pwin_east,
    odds_east = exp( (elo_east - elo_west) * elo_factor ),
    odds_west = 1/odds_east,
    elo_mismatch = abs( elo_east-elo_west ),
    resolved = winnerId != 0,
    winner_east = ifelse( resolved,  winnerId == eastId, NA ),
    surprisal = ifelse( resolved, ifelse( winner_east, -log(pwin_east)/log(2), -log(pwin_west)/log(2) ), NA )
  )


matches0 |> arrange( -elo_mismatch )

##
## Track Scores 
##


picks

