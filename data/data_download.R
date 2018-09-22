library(readr)

## can uncomment saveRDS calls if I re-download / update data
folder <- "~/Desktop/Personal R Work/world_football/data/fivethirtyeight/"

spi_matches <- read_csv(paste0(folder, "spi_matches.csv"))
saveRDS(spi_matches, paste0(folder, "spi_matches", lubridate::today(), ".RDS"))

intl_rankings <- read_csv(paste0(folder, "spi_global_rankings_intl.csv"))
saveRDS(intl_rankings, paste0(folder, "intl_rankings", lubridate::today(), ".RDS"))

club_rankings <- read_csv(paste0(folder, "spi_global_rankings.csv"))
saveRDS(club_rankings, paste0(folder, "club_rankings", lubridate::today(), ".RDS"))