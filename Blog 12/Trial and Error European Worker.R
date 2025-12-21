library(tidyverse)
library(googlesheets4)
library(mvtnorm)
set.seed(147) # For reproducibility

# Load csv file where the delimiter is the semi colon and the location is "G:\My Drive\rao@ualberta.ca 2022-12-08 10 58\shishir@tamu.edu\My Drive\Interesting papers\Survival Models\GitHub\Survival\Survival-Analysis\Blog 12\Data\European_Worker_Dataset\MW1.csv"

european_worker_1 <- read.csv2(
  "G:\\My Drive\\rao@ualberta.ca 2022-12-08 10 58\\shishir@tamu.edu\\My Drive\\Interesting papers\\Survival Models\\GitHub\\Survival\\Survival-Analysis\\Blog 12\\Data\\European_Worker_Dataset\\MW1.csv",
  sep = ";"
) %>%
  dplyr::select(c("Start", "Stop"))

european_worker_2 <- read.csv2(
  "G:\\My Drive\\rao@ualberta.ca 2022-12-08 10 58\\shishir@tamu.edu\\My Drive\\Interesting papers\\Survival Models\\GitHub\\Survival\\Survival-Analysis\\Blog 12\\Data\\European_Worker_Dataset\\MW2.csv",
  sep = ";"
) %>%
  dplyr::select(c("Start", "Stop"))

# The "Start" and "Stop" columns represent HH:MM:SS format, but are in character format as HH.MM.SS. Converting that to the proper format below.
convert_to_seconds <- function(time_str) {
  time_parts <- strsplit(time_str, "\\.")[[1]]
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  seconds <- as.numeric(time_parts[3])
  total_seconds <- hours * 3600 + minutes * 60 + seconds
  return(total_seconds)
}
european_worker_1 <- european_worker_1 %>%
  mutate(
    Start = sapply(Start, convert_to_seconds),
    Stop = sapply(Stop, convert_to_seconds)
  )
