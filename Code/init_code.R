#Packages
library(data.table)
library(ggplot2)
library(leaflet)


#SourceFiles
runoff_info <- readRDS('Sources/runoff_eu_info.rds')
runoff_day <- readRDS('Sources/runoff_eu_day.rds')
runoff_year <- readRDS('Sources/runoff_eu_year.rds')
runoff_stats <- readRDS('Sources/runoff_stats.rds')
runoff_est <- readRDS('Sources/runoff_estimated.rds')
runoff_change <- readRDS('Sources/runoff_change.rds')
runoff_day_est <- readRDS('Sources/runoff_day.rds')
#Helpers
stn_pos <- readRDS('Sources/positions.rds')