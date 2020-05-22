#Part 2 

source('./code/init_code.r')

runoff_year_better <- runoff_year[Year >= 1980]
runoff_year_better
for (i in 1:length(runoff_day$date)) {
  if (runoff_day[i,year(date)] >= 1980) {
    runoff_day_better$id <- runoff_day[i, 1]
    runoff_day_better$year <- runoff_day[i, year(date)]
    runoff_day_better$value <- runoff_day[i,3]
  }
}


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
      runoff_stats[j, 6] <- runoff_snames[i,2]
    }
  }
} # I'm doing this through a loop, because it's not working for me another easier way, not enough pc resources for this.

#Let's have a look at some stats and test it
ggplot(runoff_stats, aes(x = sname, y = mean)) +
  geom_point()

#Creating new estimation statistic values mean/high and mean/low runoffs
runoff_stats_est <- runoff_stats[, .(MH = (mean/max), ML = (mean/min)), by = sname]
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
