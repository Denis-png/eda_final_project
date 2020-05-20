source('./code/init_code.r')

str(runoff_info)

#Qstn_1
for (i in 1:length(runoff_info[, Continent])){
  if (runoff_info[i, Continent] != "EU") {
    str(runoff_info[i, Continent])
  }
}

#Qstn_2{
countries <- data.table(Country = unique(runoff_info[, Country]), Stations = 0)
for (i in 1:length(runoff_info[, Station])){
    for (j in 1:length(countries[, Country])){ 
      if (runoff_info[i, Country] == countries[j, Country]){
        countries[j, 2] <- countries[j, 2] + 1
    }
  }
}

countries

#Qstn_3
rivers <- data.table(River = unique(runoff_info[, River]), Stations = 0)
for (i in 1:length(runoff_info[, Station])){
  for (j in 1:length(rivers[, River])){ 
    if (runoff_info[i, River] == rivers[j, River]){
      rivers[j, 2] <- rivers[j, 2] + 1
    }
  }
}
rivers

#Qstn_4
stns_chars <- data.table(Stn = runoff_info[,Station], Lat = runoff_info[, Lat], Lon = runoff_info[, Lon], Alt = runoff_info[, Alt])

dist_Avg <- data.table(AvgLat = mean(stns_chars[,Lat]), AvgLon = mean(stns_chars[,Lon]), AvgAlt = mean(stns_chars[,Alt], na.rm = TRUE))

dist_Max <- data.table(MaxLat = max(stns_chars[,Lat]), MaxLon = max(stns_chars[,Lon]), MaxAlt = max(stns_chars[,Alt], na.rm = TRUE))

dist_Min <- data.table(MinLat = min(stns_chars[,Lat]), MinLon = min(stns_chars[,Lon]), MinAlt = min(stns_chars[,Alt], na.rm = TRUE))

dist_Avg
dist_Max
dist_Min

#Qstn_5
stns_record <- data.table(Stn = runoff_info[,Station], RecLen = runoff_info[, N.Years])
record_dist <- data.table(AvgRL = mean(stns_record[,RecLen]), MaxRL = max(stns_record[,RecLen]), MinRL = min(stns_record[,RecLen]))
record_dist

