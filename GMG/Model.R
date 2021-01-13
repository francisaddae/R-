#Libraries imported for Model Analysis 
library(data.table)
library(ggplot2)
library(lubridate)
library(gganimate)
library(caret)


#Opening file 
PageViews <-fread("case_study_pageviews.csv")
Subscribers <- fread("case_study_subscribers.csv")

summary(PageViews)
summary(Subscribers)

##### Data Cleaning #### 

#Changing variables into correct types for PageViews
str(PageViews)
PageViews$user_id <- as.factor(PageViews$user_id)
PageViews$timestamp <- mdy_hm(PageViews$timestamp)
PageViews$article_publication_date <- mdy(PageViews$article_publication_date)
PageViews$article_format <- as.factor(PageViews$article_format)
PageViews$campaign <- as.factor(PageViews$campaign)
PageViews[,uniqueN(page_title)]
summary(PageViews)

#Changing varaibles into correct types for Subscribers
str(Subscribers)

#check the number of Unique user_id for both PageViewes and Subscribers 
PageViews[,uniqueN(page_path)]
Subscribers[,uniqueN(user_id)]

Subscribers$user_id <- as.factor(Subscribers$user_id)
Subscribers$subscription_date <- ymd_hms(Subscribers$subscription_date)


##### Data Transfomation#### 

#merge data together
setkey(PageViews, user_id)
setkey(Subscribers, user_id)
dt <- PageViews[Subscribers]

#drop columns that wont be USEFUL!
dt[,page_path := NULL] 
dt[,page_title := NULL] 
dt[,page_referrer := NULL]
str(dt)

#drop nulls in the data set 
dt <- na.omit(dt)
dt

setkey(dt,user_id)
str(dt)

#fixing campaign names 
unique(dt$campaign)
dt[campaign == "", campaign := 'other_sources' ]

##### Exploratory Data Analysis #### 

#What is the best time to send each campaign?
a <- dt[,list(hour = hour(timestamp)), by = .(campaign)][,list(demand = .N), by = .(campaign,hour)][order(hour)]
a
ggplot(a, aes(x = hour, y = demand, col = factor(campaign))) + geom_line() + geom_point() + 
  xlim(0,24) + scale_y_log10() + labs(title = 'Time plot based on campaign',
                                      x = 'Time(Hours)', y= 'Number of subscribers', col = 'Campaign') 
#from a, Check the difference in demand between email and other sources 
b <- data.table(seq(0,23,1), 
                a[campaign == 'email',demand] - a[campaign == 'other_sources', demand])
ggplot(b, aes( x= V1, y = V2)) + geom_point() + geom_line() + 
  labs(title = 'Difference between Emails and Other Sources', x = 'Time(Hours)', y = 'Number of Subcribers')


#What is the artcile is viewed the most by email users?
c <- dt[campaign == 'email',list(users = .N), by = article_format]
c
ggplot(c, aes(x = factor(article_format), y = users, fill = article_format)) + geom_bar(stat = 'identity') + 
  scale_y_log10() + labs(title =  ' Articles used by email campaign', 
                         x = NULL, y = 'Number of Users', fill = 'Articles')

#What's the best time to send an article?
d <- dt[,list(users = .N), by = .(article_format,hour(timestamp))]
d
ggplot(d, aes(x = hour, y = users, col = factor(article_format))) + geom_line() + scale_y_log10() + 
  labs( title = 'Timely Viewership for every article', x = 'Time(Hour)', 
        y = 'Number of Subscribers', col = 'Atricles')

ggplot(d[,.SD[which.max(users)], by = hour][order(hour)], aes(x =hour, y = users, col = article_format)) + 
  geom_point(aes(shape = article_format, size = users)) +  scale_y_log10() + labs(
    title = 'Best time to send an artcile', x = 'Time(hour)', y = 'Number of Subscribers', 
    col = 'Articles')

#Viwership of by subscribers on the daily basis and through whcih channel 
e <- dt[, list(numSubs =.N), by = .(wday(timestamp, label = TRUE, abbr = FALSE), 
                                    hour(timestamp), campaign, article_format)] 
e
pnts <- sort(unique(e$wday))
for (i in seq_along(pnts)){
  plot <- ggplot(subset(e,e$wday == pnts[i]), aes( x = factor(hour), y= numSubs, fill = campaign,size = numSubs)) + 
    geom_bar(stat = 'identity') + scale_y_log10()  + facet_wrap(.~article_format) +
    labs(title = paste0(pnts[i], "'s Viwership of Artciles Through Various Channels"),
        x = 'Time(Hours)', y= 'Number of People', fill = 'Content Channel')
  print(plot)
  Sys.sleep(2)
}


#How often are our users visiting in a week?  
f <- dt[,list(numUser = .N), by = .(wday(timestamp, label = TRUE, abbr = FALSE), hour(timestamp))]
f
ggplot(f, aes(x = hour, y = numUser)) + geom_line()  + scale_y_log10() + facet_grid(wday~.) +
  labs(title = 'User visits everyday in an entire week', x= 'Time', y="" )


#Precentage of users that clicked on a specific artcile based on the overall number of users 
g <- dt[, list(overallSize = .N/ dt[,uniqueN(user_id)] * 100), by = 
          .(article_format,wday(timestamp, label = TRUE, abbr = FALSE))][order(wday)]
g
ggplot(g, aes( x =" ", y = overallSize, fill = article_format)) + geom_bar(stat = 'identity', col = 'white') + 
  coord_polar("y", start = 0) + facet_wrap(~wday) + theme_void() +
  labs(title = 'Content Velocity based on the Overall Precentage Of Users', y = '', fill = 'Article') 

#Precentage of user and how they get thier contect by sources
h <- dt[,list(users = uniqueN(user_id)), by = .(campaign,wday(timestamp, label = TRUE, abbr = FALSE))]
h
ggplot(h, aes(x = "", y = users, fill = campaign)) + geom_bar(stat = 'identity') + 
  scale_y_log10() + coord_polar('y', start = 0) + facet_wrap(~wday) + theme_void() + 
  labs(title = 'Content Sources per day by Users', y= '', fill = 'Sources')

#based on subscriber data, Which type of media does each subscriber enjoy the most based on pageviews
i <- dt[,list(mediaType = fcase(
  recipe_section_pageviews > beauty_section_pageviews & 
    recipe_section_pageviews > news_section_pageviews &
    recipe_section_pageviews > food_section_pageviews &
    recipe_section_pageviews > fashion_section_pageviews, "Recipe Content Consumer",
  beauty_section_pageviews > recipe_section_pageviews& 
    beauty_section_pageviews > news_section_pageviews &
    beauty_section_pageviews > food_section_pageviews &
    beauty_section_pageviews > fashion_section_pageviews, "Beauty Content Consumer",
  food_section_pageviews > recipe_section_pageviews &
    food_section_pageviews > news_section_pageviews &
    food_section_pageviews > fashion_section_pageviews &
    food_section_pageviews > beauty_section_pageviews, "Food Content Consumer",
  fashion_section_pageviews > recipe_section_pageviews &
    fashion_section_pageviews > news_section_pageviews &
    fashion_section_pageviews > food_section_pageviews &
    fashion_section_pageviews & beauty_section_pageviews, "Fashion Content Consumer",
  news_section_pageviews > recipe_section_pageviews &
    news_section_pageviews > fashion_section_pageviews &
    news_section_pageviews > beauty_section_pageviews &
    news_section_pageviews > food_section_pageviews, "News Content Consumer",
  default = 'All Content Consumer'
),timestamp)][
  ,list(mediaType, totPop = .N), by = wday(timestamp,label = TRUE, abbr = FALSE)][
    ,list(numUsers = .N,totPop), by = .(mediaType,wday)][
      , list(perPop = numUsers/totPop * 100), by = .(mediaType, wday)]
i
ggplot(i[,unique(perPop), by = .(wday,mediaType)][order(wday)], 
       aes(x = "", y = V1, fill = mediaType)) + geom_bar(stat = 'identity', width = 1) + coord_polar('y', start = 0) +
  facet_wrap(~wday) + theme_void() + labs( title = "Media Consumption By Day", fill = "Media")

#rolling 2 hour averages everday based on time at which an article was clicked  
j <- dt[, list(users = .N), by = .(article_format,hour(timestamp),wday(timestamp, abbr = FALSE, label = TRUE))]

artfot <- j[,unique(article_format)]
for ( art in artfot){
  for (day in pnts){
    j[article_format == art & wday == day, rollMean := round(frollmean(users, 3, fill = 0))]
  }
  lower <- j[article_format == art, .SD[which.min(rollMean)]][,rollMean]
  upper <- j[article_format == art, .SD[which.max(rollMean)]][,rollMean]
  if (upper > lower){
    plt <- ggplot(subset(j[article_format == art]), aes( x= hour, y = rollMean, fill = factor(wday))) + 
      geom_histogram(stat = 'identity') +  scale_color_viridis_d() + ylim( lower, upper) + 
      labs(title = paste('3hours Rolling Average Viwership of ', art, 'Contents'), 
           x = 'Time', y = 'Rolling 3hour Averages', fill = 'Day')
    print(plt)
    
  } else{
    plt <- ggplot(subset(j[article_format == art]), aes( x= hour, y = users, fill = factor(wday))) + 
      geom_bar(stat = 'identity', position = 'dodge') +  scale_color_viridis_d() + labs( 
        title = paste('3hours Rolling Average Viwership of ', art, 'Contents'), 
        x = 'Time', y = 'Rolling 3hour Averages', fill = 'Day')
    print(plt)
  }
  Sys.sleep(2)
} 

### Number of articles published based on the day a subcriber
k <- dt[, list(numUsers = uniqueN(user_id)), 
        by = .(year(article_publication_date), month(article_publication_date,label = TRUE))]
ggplot(k, aes(x = factor(month), y = numUsers, col = factor(year))) + geom_point()+  
  scale_y_log10() + labs(title = 'Monthly Number Of Subscriber Added Based on Article Year',
                         x =  'Month', y = 'Number of Subscribers', col = 'Year')

### What type of user viewers certain articles on specific time frames 
#{0- 2 --> 'Early AM',3- 6: --> 'Morning, 7-9 --> 'Rush Hour, 10- 12 -->'Almost Lunch' }--> Monring Observers 
#{13-15 -->'MidDay' , 16-18 --> 'Dinner' , 19 - 21 -->'Leisure Hour' , 22-24 --> 'Late Night} --> Afternoon Observers 
l <- dt[,list(numUsers = uniqueN(user_id)), by = .(hour(timestamp),article_format)][order(hour)][
  ,list(numUsers,hour, article_format,viewIndicator =factor( fcase(
    between(hour, 0, 2), 'Early Morning',
    between(hour, 3, 6), 'Moorning',
    between(hour, 7, 9), 'Rush Hour',
    between(hour, 10, 12), 'Almost Lunch',
    between(hour, 13, 15), 'Mid-Day',
    between(hour, 16, 18), 'Dinner',
    between(hour, 19, 21), 'Leisure Hour',
    default = 'Late Night'
  )))
][,list(pepView = sum(numUsers)), by = .(viewIndicator, article_format)]
l
ggplot(l, aes(x = factor(viewIndicator), y = pepView, fill = article_format)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5, alpha = 0.8) + 
  scale_y_log10() +  scale_color_brewer('Dark2') + labs( 
    title = 'Specified Time Zones Needed For Media Consumption', x = "", y = 'Number of Subscriber', fill = 'Article')


