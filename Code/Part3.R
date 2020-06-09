source('./code/init_code.r')
library(reshape2)
#Choosing the stations
stations <- c('REYKJAFOSS','POLMAK','CEATAL IZMAIL','KOELN','YAKSHA (YAKSHINSKAYA)')

stations <- runoff_stats[sname %in% factor(stations), .(id, sname)]

runoff_day_est <- merge(runoff_stats[,.(id,sname)], runoff_day_est, by = 'id')
#Adding a year column 
runoff_day_est$year <- as.numeric(substring(runoff_day_est$date,1,4))
#Creating a data table with runoff by months. I'm taking records only since 1980
runoff_seasonal <- runoff_day_est[sname %in% stations$sname & year >= 1980, .(value, month), by = 'sname']
#Plot seasonality
ggplot(runoff_seasonal, aes(x = month, y = value))+
  geom_point()+
  facet_wrap(~sname, scales = 'free')
# Preparing our correlation matrix to plot
dt <- runoff_day_est[sname %in% stations$sname & year >=1980, .(value, year), by = 'sname']
runoff_mat_year <- dcast(dt, year~sname, mean)
runoff_cor_year <- cor(runoff_mat_year[, -1], use = "pairwise.complete.obs")
to_plot <- melt(runoff_cor_year)
#Plot cor matx
ggplot(to_plot, aes(x = Var1 , y = Var2, fill = value))+
  geom_tile(col = 'black')+
  geom_text(aes(label = round(value, 1)))+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab(label = "") +
  ylab(label = "")
#Creating a dt to plot a regression slopes
to_plot_sl <- runoff_day_est[sname %in% stations$sname, .(value, Season, year), by = 'sname']
to_plot_sl <- to_plot_sl[year >= 1980, .(mean = round(mean(value),0)), by = .(Season, year, sname)]
to_plot_summer <- to_plot_sl[Season == 'Summer'] 
to_plot_winter <- to_plot_sl[Season == 'Winter'] 
#A plot of regression slope for summer
ggplot(to_plot_summer, aes(x = year, y = mean)) +
  geom_point(aes(col = sname)) +
  geom_smooth(method = 'loess', formula = y ~ x, se = 0) +
  facet_wrap(~sname, scales = 'free')+
  xlab(label = "YEAR") +
  ylab(label = "Runoff(m3/s)") +
  theme_bw()
#A plot of regression slope for winter
ggplot(to_plot_winter, aes(x = year, y = mean)) +
  geom_point(aes(col = sname)) +
  geom_smooth(method = 'loess', formula = y ~ x, se = 0) +
  facet_wrap(~sname, scales = 'free')+
  xlab(label = "YEAR") +
  ylab(label = "Runoff(m3/s)") +
  theme_bw()
