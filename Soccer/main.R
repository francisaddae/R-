library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats) 

#Data Reading 
soc <- read.csv('Soccer_Data.csv')

head(soc)
glimpse(soc)

#Data Cleaning
sum(is.na(soc))

#Data formatting 

#Date --> year-month-day formart 
#Away & Home team are in factors(categorical variable)
#Away & Home scores in integers 
#City & Country is the location where a game was played
#Neutral --> if a game was played on a home or away team.
#Result --> Which team won during the match? homeWin, awayWin or draw 

soc$date <-  ymd(soc$date)
soc$home_team <- as.factor(soc$home_team)
soc$away_team <- as.factor(soc$away_team)
soc$home_score <- as.integer(soc$home_score)
soc$away_score <- as.integer(soc$away_score)
soc$tournament <- as.factor(soc$tournament)
soc$city       <- as.factor(soc$city)
soc$country    <- as.factor(soc$country)
soc$neutral    <- as.factor(soc$neutral)
soc$result     <- as.factor(soc$result) 

#Summary of Data 
summary(soc) 


####  Exploratory Data Analysis #### 
#How many games were played each year?
ans <- soc %>% group_by(year(date)) %>% 
  summarize(ct = n())
ggplot(ans, aes(`year(date)`, ct)) + geom_point() + geom_smooth(method = 'lm') + 
  labs(x = 'Year', y= 'gamesPlayed', title = 'Number of games played each year')

#What are the top 10 home teams in soccer?
ans <- soc %>% select(home_team) %>% group_by(home_team) %>% summarize(ct = n()) %>% 
  arrange(desc(ct)) %>% top_n(10) 
ggplot(ans, aes(home_team, ct, size = ct)) + geom_point() + 
  labs(x = 'home team', y = '# of games',title = 'Top 10 hometeams', size = 'Games Played')

#What are the top 10 away teams in soccer? 
ans <- soc %>% group_by(away_team) %>% summarize(ct = n()) %>% 
  arrange(desc(ct)) %>% top_n(10) 
ggplot(ans, aes(away_team, ct, size = ct)) + geom_point() + 
  labs(x = 'away team', y = '# of games',title = 'Top 10 awayteams')

#Distribution of home scores?
breaks <- c(0, min(soc$home_score) + 1, mean(soc$home_score) + 5,max(soc$home_score))
ggplot(soc, aes(home_score)) + geom_histogram(breaks = breaks) + 
  labs(x = 'home_scores', y = 'scores count', title = 'Distribition of home scores')

#Distribution of away scores?
breaks <- c(0, min(soc$away_score) + 1, mean(soc$away_score) + 5,max(soc$away_score))
ggplot(soc, aes(away_score)) + geom_histogram(breaks = breaks) + 
  labs(x = 'away_scores', y = 'scores count', title = 'Distribition of away scores')

#What's the total goals scored each year through out the dataset? 
ans <- soc %>% group_by(year(date)) %>% summarise(hmGoals = sum(home_score), ayGoals = sum(away_score)) %>% 
  select(yr = `year(date)`, hmGoals, ayGoals)
ggplot(ans, aes(x = yr)) + geom_line(aes(y=hmGoals), col = 'darkred') + 
  geom_text(aes(x = 1990, y = 1550, label = 'home goal')) + geom_line(aes(y=ayGoals),col = 'darkblue') + 
  geom_text(aes(x = 2005, y = 700, label = 'away goal')) + 
  labs(x = 'year', y = 'Total Goals Scored',title = 'Summaztion of Goals throughout each year')

#What are the top 10 tournaments in played? 
ans <- soc %>% group_by(tournament) %>% summarise(ct = n()) %>% 
  arrange(desc(ct)) %>% top_n(25)
ggplot(ans, aes(reorder(tournament, -ct), ct)) + geom_col() + coord_flip() + 
  labs(x = 'tournament', y = '# of games', title = 'Top 25 tournaments')

#create a geolocation and see what are the top 25 locations where a game was hosted? 
ans <- soc %>% mutate(location = paste0(city,',',country)) %>% group_by(location) %>% 
  summarize(numUse = n()) %>% arrange(desc(numUse)) %>% top_n(25) 
ggplot(ans,aes(location,numUse)) + geom_segment(
  aes(x =location, xend = location, y = 100, yend=numUse), color = 'skyblue') + 
  geom_point(color = 'blue', size = 4, alpha = 0.6) + coord_flip() +
  labs(x = 'venue', y = '# of games', title = 'Top 25 locations where games were hosted')

#Distribution of Neutral zones through each year 
ans <- soc %>% group_by(neutral, year(date)) %>% summarize(ct = n()) %>% 
  select(neutral, yr = `year(date)`, ct)
ggplot(ans, aes(yr, ct, color = neutral)) + geom_line() + 
  labs(x = 'Date(year)', y = '# of neutral', title = 'Games played at home/away feilds vs neutral fields')

#Results throughout each year? 
ans <- soc %>% group_by(result, year(date)) %>% summarize(ct = n()) %>% 
  select(result, yr = `year(date)`, ct)
ggplot(ans, aes(yr, ct, color = result)) + geom_line() + 
  labs(x = 'Date(year)', y = '# of neutral', title = 'Game results during each year') 

#Objective: Do home_teams that perform better in the late 1990's perform the same as the early 2000's?  
#Looking records from Jan,1990 to December, 1999 
soc19 <-soc %>% filter(date >= '1990-01-01' & date <= '1999-12-31') %>%
  select(date, home_team, home_score, tournament, city, country, result) 

#What's the overall result for each each year?
ans <- soc19 %>% group_by(year(date),result) %>% tally(sort = TRUE) %>% 
  select(yr = `year(date)`, result, pnt = n)
ggplot(ans, aes(yr, pnt, col = result)) + geom_line() + 
  labs(x = 'Year', y = 'Results Count', title = 'Result from matches from 1990-1999')

#What's the summary description of both home goals throughout each year?
ggplot(soc19, aes(factor(year(date)), home_score)) + geom_boxplot() + 
  labs(x = 'Year', y = 'Scores', title = 'Descriptive Summary description of Home gaols from 1990-1999')

#What's the best location for a team to play in each year? 
ans <- soc19 %>% mutate(location = paste(city,',',country)) %>% group_by(year(date), location) %>% 
  filter(result == 'homeWin') %>% tally(sort = TRUE) %>% select(location, yr = `year(date)`, pnt = n) %>% 
  group_by(yr) %>% top_n(10, location)
ggplot(ans, aes(reorder(location,pnt), pnt, fill = factor(yr))) + geom_col() + coord_flip() +
  labs(x = 'Location: city,country', y = '# of homewins', fill = 'Year',
       title = 'Top 10 locations for home teams to win each year')

#What's the win percentage each year? 
#We need the number of games played each year  and divide that by the number of homewins each year
#Count of records each year 
x <- soc19 %>% group_by(year(date)) %>% tally() %>% select(yr = `year(date)`, pnt = n)

y <- soc19 %>% group_by(result, year(date)) %>% tally() %>% 
  filter(result == 'homeWin') %>% select(yr = `year(date)`, homeWin = n())

ans<- x %>% inner_join(y, by = 'yr') %>% 
  mutate(winPec = round(homeWin/pnt * 100), gamesRatio = round(pnt/sum(pnt) * 100)) %>% 
  select(yr, homeWin, winPec, gamesRatio, pnt)
ans
ggplot(ans, aes(yr)) + geom_bar(aes(y = homeWin), stat = 'identity') + 
  geom_text(aes(y = homeWin+20, label = paste0(winPec,'%'))) + geom_line(aes(y = pnt), color = 'darkred') + 
  geom_text(aes(y = pnt+10, label = paste0(gamesRatio,'%'))) + 
  labs(title = 'Wins Percentage through 1990-1999', x = 'Year', y = 'gamesPlayed')



#Looking at records from Januaray,2000-December,2010
soc20 <- soc %>% filter(date >= '2000-01-01' & date <= '2010-12-31') %>%
  select(date, home_team, home_score, tournament, city, country, result) 
soc20

#What's the overall result for each each year?
ans <- soc20 %>% group_by(year(date),result) %>% tally(sort = TRUE) %>% 
  select(yr = `year(date)`, result, pnt = n)
ggplot(ans, aes(yr, pnt, col = result))+ geom_line() + 
  labs(x = 'Year', y = 'Result', title = 'Game results from 2000-2010') 

#What's the summary description 
ggplot(soc20, aes(factor(year(date)), home_score)) + geom_boxplot() + 
  labs(x = 'Year', y = 'Scores',title = 'Descriptive Summary description of Home gaols from 2000-2010')

#What's the best location for a team to play in each year? 
ans <- soc20 %>% mutate(location = paste(city,',',country)) %>% group_by(year(date), location) %>% 
  filter(result == 'homeWin') %>% tally(sort = TRUE) %>% select(location, yr = `year(date)`, pnt = n) %>% 
  group_by(yr) %>% top_n(10, location)
ggplot(ans, aes(reorder(location,pnt), pnt, fill = factor(yr))) + geom_col() + coord_flip() +
  labs(x = 'Location: city,country', y = '# of homewins', fill = 'Year',
       title = 'Top 10 locations for home teams to win each year')

#What's the win percentage each year? 
x <- soc20 %>% group_by(year(date)) %>% tally() %>% select(yr = `year(date)`, pnt = n)

y <- soc20 %>% group_by(result, year(date)) %>% tally() %>% 
  filter(result == 'homeWin') %>% select(yr = `year(date)`, homeWin = n)

ans<- x %>% inner_join(y, by = 'yr') %>% 
  mutate(winPec = round(homeWin/pnt * 100), gamesRatio = round(pnt/sum(pnt) * 100)) %>% 
  select(yr, homeWin, winPec, gamesRatio, pnt)
ans
ggplot(ans, aes(yr)) + geom_bar(aes(y = homeWin), stat = 'identity') + 
  geom_text(aes(y = homeWin+20, label = paste0(winPec,'%'))) + geom_line(aes(y = pnt), color = 'darkred') + 
  geom_text(aes(y = pnt+15, label = paste0(gamesRatio,'%'))) + 
  labs(title = 'Wins Percentage through 2000-2010', x = 'Year', y = 'gamesPlayed')

#Interesting Insight/Questions: 
#1.As the games home teams play, thier overall win percentage goes up. 
#Therefore, there is a small correlation
#2.Total home team wins rose during the middle of the 1990's but differs quite alot during the 2000's
#3.Is there a correlation between games played and goals scored? Can a difference in goals shed a light  
#4.What there specific years where homeWins excited awayWins. Which teams won the most?  

#Correlation between goals and result.{Hypothesis: More home goals leads to more home win?} 
#focusing on on the previous two decades { 1990-2010} 
dt <- soc19 %>% full_join(soc20) %>% mutate(location = paste(city,',',country)) %>% 
  select(date, team = home_team, goals = home_score, tournament, location, result)
dt$location <- as.factor(dt$location)
summary(dt)

#General case
ggplot(dt %>% group_by(year(date)) %>% summarize(hmGoal = sum(goals), games = n()) %>% 
  select (yr = `year(date)`, hmGoal, games), aes(yr)) + geom_line(aes(y=hmGoal), col = 'darkred') + 
  geom_col(aes(y=games), fill= 'darkblue')

#When home team won 
ggplot(dt %>% group_by(year(date)) %>% filter(result == 'homeWin') %>% 
  summarize(hmGoal = sum(goals), games = n()) %>% select (yr = `year(date)`, hmGoal, games)
       ,aes(yr)) + geom_line(aes(y=hmGoal), col = 'darkred') + geom_col(aes(y=games), fill= 'darkblue')

#Those doesnt really answer the question. Let's see the average( total goals/ total games) per year when the
ggplot(dt %>% group_by(year(date)) %>%  filter(result == 'homeWin') %>% 
  summarize(avgGoal = mean(goals)) %>% select(yr = `year(date)`, avgGoal), 
  aes(yr, avgGoal)) + geom_col() 

#It seems more goals doesn't grarantee more future success in wins for any home team.

#Check if continents can factor in wins for hometeams
continents <- read.csv('countries-continents.csv')
continents

#General case
ggplot(dt %>% left_join(continents, by = c('team' = 'Country')) %>% filter(Continent != 'NA') %>% 
  group_by(Continent) %>%  summarize(pnt = sum(goals), gamesPlayed = n()), aes(Continent)) + 
  geom_col(aes(y = pnt), fill = 'red') + geom_col(aes(y=gamesPlayed), fill = 'blue') + 
  labs(caption = "Total Goals Scored in Red, GamesPlayed in Blue")

#Home team wining 
ggplot(dt %>% left_join(continents, by = c('team' = 'Country')) %>% 
         filter(Continent != 'NA' & result == 'homeWin') %>% group_by(Continent) %>%  
         summarize(pnt = sum(goals), gamesPlayed = n()), aes(Continent)) + 
  geom_col(aes(y = pnt), fill = 'red') + geom_col(aes(y=gamesPlayed), fill = 'blue') + 
  labs(caption = "Total Goals Scored in Red, GamesPlayed in Blue")

#It seems more Asia teams are more likely to score as much goals as European team at home. Conclusively,
#we can say more home goals some home teams benifits a,ot from scores more home goals;
#especially Asian and European teams 

#Classification of Games. 
#The most important thing is wether a teams wins coniceds with the field advantage. 
#Field advantage is when a home or away team gets to play on thier own turf
#Can a win be predicted based on filed neutral. Neutrality is when 
#True --> When a games is played on a Neutral field; FALSE when not 

#Let's add the continents to the data. This will be done by using the country at which the game was played.
#Reason why:Is the simplest way to explain aggragates and we dont need to factor in 
#which teams gets the label. 


#Merging the continents in the dataset
countries_and_continents <- continents %>% mutate(
  country = ifelse(nchar(Country) <= 2, case_when(Country == 'CZ'~ 'Czech Republic',
                                                   Country == 'US' ~ 'United States',
                                                   TRUE ~ as.character(Country)), Country)) %>% 
  rename(continent = Continent) %>%  select(country, continent)


dt <- soc %>% full_join(countries_and_continents, by = 'country')


#Lets do a little exploration with the data?

#What's the total games played on each?
ggplot(dt %>% group_by(continent) %>% tally(), aes(continent, n)) + geom_col() + 
  labs(x = 'Continents', y = '# of games', title = 'Number of games played on each continent')

#What's the total games played on each continent by neutrality?
ggplot(dt %>% group_by(continent,neutral) %>% tally(), aes(continent, n, fill = neutral)) + 
  geom_col(position = 'dodge') + labs(x = 'Continents', y = '# of games', 
                            title = 'Number of games played on each continent')

#What's the rolling average of games played each year by continent? 
ans <- dt %>% group_by(year(date), month(date),continent) %>% summarize(gamesPlayed = n()) %>% 
  select(year = `year(date)`, month = `month(date)`, gamesPlayed, continent) %>% arrange(year, month) %>% 
  mutate(rollingAvgGames = cumsum(gamesPlayed)/ row_number(),
         date = make_date(year = year, month = month)) %>% 
  select(date,gamesPlayed, rollingAvgGames, continent)
ans 
ggplot(ans, aes(date, gamesPlayed)) + geom_line() + facet_grid(continent~.) + 
  scale_x_date(date_minor_breaks = "10 years") + 
  labs(y ='# of games', title = 'Total games played in each continent') 

ggplot(ans, aes(date, rollingAvgGames)) + geom_line() + facet_grid(continent~.) + 
  scale_x_date(date_minor_breaks = "10 years") + 
  labs(y ='# of games', title = 'Moving averages of games played on each continent') 

#Rolling Averages of wins per year for each continent
ans <- dt %>% group_by(continent, year(date), result) %>% filter(result != 'draw') %>%  
  summarize(totalWins = n()) %>%  mutate(avgWins = cumsum(totalWins)/ row_number()) %>% 
  select(year = `year(date)`,continent,totalWins, avgWins,result) %>% ungroup()
ans
ggplot(ans, aes(year, totalWins, col = result, size = avgWins)) + geom_jitter() + facet_wrap(~continent) +
  labs(x = 'year', y = 'total Wins per year', col = 'Type of Wins', size = 'rolling Win Averages',
       title = "Can the continent in which a game was played affects it's win totals?") 

ggplot(ans, aes(year, totalWins, col = result, size = avgWins)) + geom_jitter() + facet_wrap(~continent) +
  labs(x = 'year', y = 'total Wins per year', col = 'Type of Wins', size = 'rolling Win Averages',
       title = "Can the continent in which a game was played affects it's win totals?",
       subtitle = "Smoothing curve: Linear Model") + geom_smooth(method = 'lm')

#Observation: 
#We previously know that Away teams won most games in each tournament.
#By using the rolling average of wins per each continent. we can see the trends within each facets. 
#1. Games played on European, African and North America soils tend to have the most away wins. 
#Meaning: Home teams tend to lost when thier play awayteam's on thier own turf.
#2. Oceaniaic and South America hosted games tend to have most home game wins but ther 
#win progression isnt as strongly
#correlated as European teams. 
#3. Asia hosted games have a bottleneck in terms in both home and away wins. 
#4. Oceaniaic and South America games have lower wining averages, there by reducing the win averages. 
 


#Are they any changes within the rolling wins each year?
ans %>% group_by(result) %>% mutate(winsChanges = lag(avgWins) - lead(avgWins)) %>% 
  arrange(desc(winsChanges))




       