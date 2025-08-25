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
rikishi_id <- 3081
resp2 <- GET(paste0(base_url, "/api/rikishi/", rikishi_id))
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




# 5. Kimarite statistics
resp5 <- GET(paste0(base_url, "/api/kimarite?sortField=count" ))
kimarite <- fromJSON(rawToChar(resp5$content))
kimarite$records

