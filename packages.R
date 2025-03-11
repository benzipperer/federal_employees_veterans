library(targets)
library(tarchetypes)
library(crew)

options(usethis.quiet = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(censusapi)
  library(fips)
  library(epidatatools)
  library(ipumsr)
  library(janitor)
  library(autumn)
  library(assertr)
  library(scales)
  library(arrow)
})