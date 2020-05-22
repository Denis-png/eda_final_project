#Part 2 

source('./code/init_code.r')

#Creating a stats table
runoff_stats <- runoff_day[, .(mean = round(mean(value), 0), sd = round(sd(value), 0), min = min(value), max = max(value)), by = id]
#Adding a coeficient variation
runoff_stats$cv <- runoff_stats[, sd] / runoff_stats[, mean]
#Data table for stations names by id
runoff_snames <- runoff_info[,.(id = ID, Station)]
#Just a helper for myself and for R
runoff_stats$sname <- 'name'
#Loop that assigns station's name by it's id(merge function died on this step)
for (i in 1:length(runoff_snames$id)) {
  for (j in 1:length(runoff_stats$id)){
    if (runoff_snames[i, 1] == runoff_stats[j, 1]){
      runoff_stats[j, 7] <- runoff_snames[i,2]
    }
  }
} 
# I'm doing this through a loop, because it's not working for me another easier way, not enough pc resources for this.

#Let's have a look at some stats and test it
ggplot(runoff_stats, aes(x = sname, y = mean)) +
  geom_point()

#Creating new estimation statistic values mean/high and mean/low runoffs
runoff_stats_est <- runoff_stats[, .(id = id,MH = (mean/max), ML = (mean/min)), by = sname]
#Plotting to test and see if it looks logical
ggplot(runoff_stats_est, aes(x = sname , y = MH)) +
  geom_point()

#Here the fun part begins. I decided to divide stations by their position on map, using lat and lon
#All border were calculated by myself, they might not be that accutrate, but for this classification it's enough
stn_pos <- runoff_info[,.(Station, Lat, Lon, Alt)]
stn_pos[(Lat >= 55)&((Lon >= -18 & Lon <= -12)|(Lon > 0 & Lon <= 30)), Pos := factor('North')]
stn_pos[(Lat > 43 & Lat <= 60)&(Lon >= -10 & Lon <= 5), Pos := factor('West')]
stn_pos[(Lat >= 45 & Lat < 55)&(Lon >= 5 & Lon < 23), Pos := factor('Central')]
stn_pos[(Lat > 35 & Lat <= 43)&(Lon >= -10 & Lon < 2), Pos := factor('SouthWest')]
stn_pos[((Lat > 35 & Lat < 45)&(Lon >= 7 & Lon < 18))|((Lat > 35 & Lat < 42)&(Lon > 19 & Lon < 28)), Pos := factor('South')]
stn_pos[(Lat > 42 & Lat < 47)&(Lon > 14 & Lon < 30), Pos := factor('SouthEast')]
stn_pos[((Lat >=47 & Lat < 60)&(Lon > 20 & Lon < 60))|((Lat >= 42 & Lat < 70)&(Lon >= 30 & Lon <=60)), Pos := factor('East')]
#Now let's charectarize it by Altitude.
stn_pos[Alt <= 200, LF := factor('Lowland')]
stn_pos[Alt > 200 & Alt <= 500, LF := factor('Hill')]
stn_pos[Alt > 500 & Alt < 1000, LF := factor('Highland')]
stn_pos[Alt >= 1000, LF := factor('LowMountain')]
stn_pos
#Assigning Position and landform values to my main dataset
runoff_stats$Pos <- factor('Pos')
runoff_stats$LF <- factor('Lowland')
for (i in 1:length(stn_pos$Station)) {
  for (j in 1:length(runoff_stats$sname)){
    if (stn_pos[i, 1] == runoff_stats[j, 7]){
      runoff_stats[j, 8] <- stn_pos[i,5]
      runoff_stats[j, 9] <- stn_pos[i,6]
    }
  }
} 
#Creating a new characteristic before/after 1980
runoff_day$year <- substring(runoff_day$date,1,4)
runoff_day[year < 1980, year_type := factor('before')]
runoff_day[year >= 1980, year_type := factor('after')]
#Estimating values mean/high, mean/low runoffs by before/after year 1980
runoff_before <- runoff_day[year < 1980, .(Bmean = mean(value), Bmax = max(value), Bmin = min(value)), by = id]
runoff_after <- runoff_day[year >= 1980, .(Amean = mean(value), Amax = max(value), Amin = min(value)), by = id]
runoff_before_est <- runoff_before[,.(MH_Before = Bmean / Bmax, ML_Before = Bmean / Bmin), by = id]
runoff_after_est <- runoff_after[,.(MH_After = Amean / Amax, ML_After = Amean / Amin), by = id]
runoff_ba_est <- merge(runoff_before_est, runoff_after_est, by = 'id')
runoff_estimated <- merge(runoff_stats_est, runoff_ba_est, by = 'id')
#Changes in mean/high by stations
ggplot(runoff_estimated, aes(x = sname, y = (MH_Before - MH_After)))+
  geom_point()
#Changes in mean/low by stations (Those point on top are trying to reach infinity value, that's happening because of lowest runoff value is equal to 0, and for almost all of them change is 0)
ggplot(runoff_estimated, aes(x = sname, y = (ML_Before - ML_After)))+
  geom_point()
#Characterizing our runoff values by seasons
runoff_day$month <- as.numeric(substring(runoff_day$date,6,7))
runoff_day$Season <- factor('Winter')
runoff_day[month >= 3 & month <= 5, Season := factor('Spring')]
runoff_day[month >= 6 & month <= 8, Season := factor('Summer')]
runoff_day[month >= 9 & month <= 11, Season := factor('Autmn')]
#Aggregating mean values by seasons & before/after 1980
runoff_summer_b <- runoff_day[Season == 'Summer' & year_type == 'before', .(value = mean(value)), by = .(id, year_type)]
runoff_summer_a <- runoff_day[Season == 'Summer' & year_type == 'after', .(value = mean(value)), by = .(id, year_type)]
runoff_winter_b <- runoff_day[Season == 'Winter' & year_type == 'before', .(value = mean(value)), by = .(id, year_type)]
runoff_winter_a <- runoff_day[Season == 'Winter' & year_type == 'after', .(value = mean(value)), by = .(id, year_type)]
