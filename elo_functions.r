
##
## Library version of prediction sheet
## copyright Ryan McCorvie 2025
##

library(tidyverse)
source( "sumo_api.R" )

elo_factor  <- log(10)/400
elo_to_pwin <- \( elo1, elo2 ) 1/(1+exp( elo_factor * (elo2-elo1)))

elo_as_of <- \(basho_id, day)
{
  elo_history |> filter( bashoId < basho_id | bashoId == basho_id & day <= !!day) |> 
    arrange( bashoId, day ) |> 
    group_by( rikishiId ) |> 
    summarize( 
      elo = last(elo),
      total_matches = last(total_matches),
      total_wins = last( total_wins)
    ) |> 
    ungroup()
}


get_prediction_sheet <- \(basho_id, day, division="makuuchi")
{
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
  
  matches |> 
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
  
}

