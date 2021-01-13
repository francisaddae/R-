library(data.table)
library(lubridate)
library(ggplot2)


#### Data Analysis Using Data Table and Visualization using GGplot2 #### 
dt <- fread('da_js_coding_challenge.csv')
dt
str(dt)

## change date to proper formart in 
dt$stats_date <- ymd(dt$stats_date)
dt$channel_view_count <- as.integer(dt$channel_view_count)
dt$channel_subscriber_count <- as.integer(dt$channel_subscriber_count)
dt$channel_id   <- as.factor(dt$channel_id)


#Checking the unique years, months and days 
unYear <- unique(year(dt$stats_date))
unYear
unMonth <- sort(unique(month(dt$stats_date)))
unMonth
unDay <- unique(day(dt$stats_date))
unDay

#Number of records per year and month 
dt[,.N, by = .(year(stats_date), month(stats_date))][order(year,month)]

##### Data Cleaning/Transformation #### 

#Missing data (precentage wise)
nulldt <- function(x) {sum(is.na(x))/length(x) * 100}
apply(dt, 2, nulldt)

#There are null values in the susbscriber count per channel. This shows data is missing 
sum(is.na(dt))
dt[which(is.na(dt))]
dt <- na.omit(dt) 
dt

# Add monthlyViews {V100K, V500K, V1M, V5M , V10M, S10M+} per channel monthly views
dt[,monthlyViews := fcase(
  channel_view_count <= 100000, 'V100K', 
  channel_view_count > 100000 & channel_view_count <= 500000, 'V500K',
  channel_view_count > 500000 & channel_view_count <= 1000000, 'V1M',
  channel_view_count > 1000000 & channel_view_count <= 5000000, 'V5M',
  channel_view_count > 5000000 & channel_view_count <= 10000000, 'V10M', 
  channel_view_count >= 10000000, 'V10M+'), by = .(channel_id,stats_date)]
dt

#Add monhtly Subscriber counts per channel based on the cumlative number of subscribers per month on every channel
#Classify the Subscribers based on { S100K, S250K, S500K, S1M, S10M, S10M+ }
dt[, monthlySubscribers := fcase(
  channel_subscriber_count <= 100000, ' S100K',
  channel_subscriber_count > 100000 & channel_subscriber_count <= 250000, 'S250K',
  channel_subscriber_count > 250000 & channel_subscriber_count <= 500000, 'S500K',
  channel_subscriber_count > 500000 & channel_subscriber_count <= 1000000, 'S1M',
  channel_subscriber_count > 1000000 & channel_subscriber_count <= 10000000, 'S10M',
  channel_subscriber_count >= 10000000, 'S10M+'), by = .(channel_id,stats_date)]
dt

#factoring the added metrics 
dt$monthlySubscribers <- as.factor(dt$monthlySubscribers)
dt$monthlyViews <- as.factor(dt$monthlyViews)

dt
##### Exploratory Data Analysis #### 

#What is the monthly distristribution  based on Viwership?
a <- dt[,list(numRecs = .N), by = .(monthlyViews, month(stats_date))]
ggplot(a, aes(x = month, y = numRecs, fill = monthlyViews)) + geom_bar(stat = 'identity') + facet_wrap(~monthlyViews) +
  labs(title = 'Monthly distribution based on various Viwership ', x= 'Months', 
       y = 'Number of Channels', fill = 'Viwership')

#What is the monthly distribution based on Subscribers ?
b <- dt[, list(numRecs = .N), by = .(monthlySubscribers, month(stats_date))]
ggplot(b, aes(x = month, y = numRecs, color = monthlySubscribers)) + geom_bar(stat = 'identity') + 
  facet_wrap(~monthlySubscribers) + labs(title = 'Montly distibution based on various Subscribers', x = 'Months',
                                         y = 'Number of Channels', fill = 'Subscribers')

#What is the distribution of video posted?
c <- dt[, list(totalVideo = .N), by = .(year(stats_date),month(stats_date))]
ggplot(c, aes(month, totalVideo)) + geom_point() + geom_line() + facet_wrap(~year) + 
  labs(title = 'Number of video posted per months', x = 'Months', y = 'Number of Videos') 

#Precentage of those of user that have more subscribers than viewers based on video post
d <- dt[channel_subscriber_count > channel_view_count & channel_view_count > 0
          ,list(Records = .N), by = .(monthlyViews, monthlySubscribers) ]
ggplot(d, aes(x = monthlyViews, y = Records, fill = monthlySubscribers)) + geom_bar(stat = 'identity')  + 
  facet_wrap(~monthlySubscribers) + coord_polar(theta = 'y') + 
  labs(title = 'Precentage of user {Subscribers > Viewers} Video posted > 0', x = 'monthlyViews',
       y= 'Users',fill = 'Subscribers')

e <- dt[channel_subscriber_count > channel_view_count & channel_view_count <= 0
        ,list(Records = .N), by = .(monthlyViews, monthlySubscribers) ] 
ggplot(e, aes(x = monthlyViews, y = Records, fill = monthlySubscribers)) + geom_bar(stat = 'identity') + 
  coord_polar(theta = 'y') + labs(title = 'Precentage of user {Subscribers > Viewers} Video posted <= 0',
                                  x = 'monthlyViews', y= 'Users', fill = 'Subscribers')

#Rolling Metrics -> 3 Months rolling  mean for both subscribers and viewers per channel based on year

#rolling_3M_Avg_View for each channel. 
dt[, rolling_3M_Avg_View := frollmean(channel_view_count, 3, fill = -1), by= .(channel_id,year(stats_date))]
dt

#rolling_3M_Avg_Sub for each channel
dt[, rolling_3M_Avg_Sub := frollmean(channel_subscriber_count, 3, fill = -1), by =.(channel_id, year(stats_date))]
dt

#rolling_6M_Publication 
dt[, rolling_6M_Pub := frollsum(channel_video_count,6 ,fill = -1), by = .(channel_id, year(stats_date))]
dt 

dt$rolling_3M_Avg_Sub <- as.integer(dt$rolling_3M_Avg_Sub)
dt$rolling_3M_Avg_View <- as.integer(dt$rolling_3M_Avg_View)
dt$rolling_6M_Pub <- as.integer(dt$rolling_6M_Pub)



#Setting key for fast results 
setkey(dt,channel_id,stats_date)
str(dt)


#Distribtion of median year view per channel 
f <- dt[channel_view_count > 0, list(yearAvgView = log(mean(channel_view_count))), by = .(channel_id, 
                                                                                          year(stats_date),
                                                                                          month(stats_date))]
ggplot( f, aes(yearAvgView)) + geom_histogram(binwidth = 0.7, col = 'black', fill = 'white') + 
  geom_vline( aes(xintercept = mean(yearAvgView), col = 'red', linetype = 'dashed')) + facet_wrap(~year) + 
  labs(title = 'Yearly Distribution of rolling 3Months Average View ', x= 'Yearly Average View', col = 'Year Mean')

#Plot the median monthly views of the channels that have more than 1M subscribers 
#that generated less than 10M views on average in the last 3 months 

g <- dt[channel_subscriber_count > 1000000 & rolling_3M_Avg_View < 10000000, 
   list(channelMedian = median(channel_view_count)), by = .(channel_id, month(stats_date))][
     month >=  ( month(Sys.Date()) - (month(Sys.Date())/ 3) + 1), .(month, channelMedian)
   ]

ggplot(g, aes(x = month, y = channelMedian, fill = month)) + stat_summary(fun = sum, 
                                                          geom = 'bar') + 
  labs(title = 'Median monthly Views in the last 3Months', x = 'Months', y= 'Sum of channel Median')

#Plot the statistics in terms of video publications for the last 6 months (min, max,median 1st and 3rd quantile) 
#for every subscriber group for channels that generated more than 1M views in the last 3 months
h <- dt[, rolling_6M_Pub, by = .(monthlySubscribers, month(stats_date))][
  month >= ( month(Sys.Date()) - (month(Sys.Date())/ 3) + 1) & rolling_6M_Pub != -1, 
  .(month,rolling_6M_Pub,monthlySubscribers)
]

ggplot(h, aes(x = as.factor(month), y=rolling_6M_Pub, fill = monthlySubscribers)) + geom_boxplot() + scale_y_log10() +
  labs(title = 'Video Publication fpr the last 6M based on Subscriber Type', 
       x = 'Months', y = 'Rolling 6M Video Publication', fill = 'Monthly Subscriber Type')

#Plot the average of the monthly views of the last month as a function of the number of total subscribers
lastMonth = dt[.N, unique(stats_date)]
i <- dt[stats_date== lastMonth, list(last_month_mean = mean(channel_view_count), totalChannel = .N), 
   by = monthlySubscribers][, list(monthlySubscribers, totalChannel,
                                 last_month_mean)]
i 
ggplot(i, aes(x = "", y = last_month_mean, fill= as.factor(monthlySubscribers) )) + 
  geom_bar(stat = 'identity', width = 10) + geom_text(aes(label = 
                                                paste( round( last_month_mean / sum(last_month_mean) * 100, 1), '%')),
                                          position = position_stack(vjust = 0.5)) +
  coord_polar('y') + labs(title = 'Average Of Monthly Views as of last Month', 
                          x = NULL, y = NULL, fill = 'Subscriber Type' ) + theme_classic() + theme(
                            plot.title = element_text(hjust=0.5),axis.line = element_blank(),
                            axis.text = element_blank(),axis.ticks = element_blank())

