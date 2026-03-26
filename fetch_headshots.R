##
## Scrape rikishi headshots from sumo.or.jp banzuke page
## copyright Ryan McCorvie 2025
##
## Division IDs: 1=Makuuchi, 2=Juryo, 3=Makushita, 4=Sandanme, 5=Jonidan, 6=Jonokuchi
##

library(httr)
library(jsonlite)
library(tidyverse)
source("sumo_api.R")

base_url     <- "https://www.sumo.or.jp"
headshot_dir <- "sumo_headshots"

# Extract English shikona, mirroring the JS lastShikona() function
clean_shikona <- \(s) s |> str_remove("\u3000.*") |> str_remove("\\(.*") |> str_trim()

fetch_banzuke_division <- \(basho_id, kakuzuke_id)
{
  url <- glue::glue("{base_url}/EnHonbashoBanzuke/indexAjax/{kakuzuke_id}/1/")

  resp <- POST(url, body = list(
    kakuzuke_id = as.character(kakuzuke_id),
    basho_id    = basho_id,
    page        = "1"
  ), encode = "form")

  if( resp$status_code != 200 ) {
    warning("HTTP ", resp$status_code, " for division ", kakuzuke_id)
    return(NULL)
  }

  data <- fromJSON(rawToChar(resp$content))

  if( data$Result == 0 ) {
    warning("No results for division ", kakuzuke_id)
    return(NULL)
  }

  tibble( data$BanzukeTable ) |>
    filter( banzuke_id != 0 ) |>
    transmute(
      shikona = clean_shikona(shikona),
      photo   = photo
    )
}

fetch_headshots <- \(basho_id = current_basho(), kakuzuke_ids = 1:2, overwrite = FALSE)
{
  rikishi <- map(kakuzuke_ids, \(div) fetch_banzuke_division(basho_id, div)) |>
    bind_rows() |>
    distinct(shikona, .keep_all = TRUE) |>
    filter(shikona != "", photo != "dummy.gif")

  cat("Found", nrow(rikishi), "rikishi across", length(kakuzuke_ids), "division(s)\n\n")

  for( i in seq_len(nrow(rikishi)) )
  {
    name  <- rikishi$shikona[[i]]
    photo <- rikishi$photo[[i]]
    dest  <- file.path(headshot_dir, paste0(name, ".jpg"))

    if( !overwrite && file.exists(dest) ) {
      cat("  skip:", name, "\n")
      next
    }

    img_url <- paste0(base_url, "/img/sumo_data/rikishi/60x60/", photo)
    result  <- tryCatch(
      GET(img_url, write_disk(dest, overwrite = TRUE)),
      error = \(e) { warning("Failed to download ", name, ": ", e$message); NULL }
    )

    if( !is.null(result) && result$status_code == 200 )
      cat("  saved:", name, "\n")
    else
      cat("  FAILED:", name, "(", photo, ")\n")
  }

  invisible(rikishi)
}

# Run for Makuuchi + Juryo by default
fetch_headshots()
