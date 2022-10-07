rm(list=ls())

# Below is the appendix to my RStudio code. If you wish to run or...
# ...test out the code, then feel free to copy and paste as it is here.

install.packages("ggpubr")

library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggpubr)

# NBA Season from 1996 - 2020

# Main question: Has the advancement of technology and healthcare given the opportunity...
#... for people in athletics to become more athletic and stronger in general.
# Looking at a 20 year span of the NBA, we are able to see differences between players...
#... who played in 1996, all the way to 2021. 

# What is the mean average of ppg off of all players? 
# Players with the highest ppg? assists? rebounds?
# What is the average number of seasons a NBA player plays?
# What is current trend of the NBA? 
# Is there a trend in international players? 
# Any correlating / exciting things to know within this dataset?


dataset <- read.csv("all_seasons.csv")


head(dataset)
tail(dataset)
class(dataset)
is.data.frame(dataset)

apply(dataset, 2, function(i) any(is.na(i)), simplify=TRUE)
sum(is.na(dataset))
summary(dataset) # Interesting things to note in gathering our data
mean(dataset$player_height)   # Mean is 200.7285 for height in cm, (6.58) in feet
mean(dataset$player_weight)    # Mean is 100.5268 for weight in kg, pounds is (221.62365738)
  

# Summarization of data (cool stuff to know!)

oldest <- dataset %>% group_by(player_name) %>% filter(age=="44") %>% count() %>% arrange(desc(n))
oldest # Kevin Willis was the oldest at age 44 given from 1996-2020.

youngest <- dataset %>% group_by(player_name) %>% filter(age == "18") %>% count() %>% arrange(desc(n))
youngest # Andrew Bynum is the youngest at age 18

highest_ppg <- dataset %>% group_by(player_name) %>% filter(pts >= 25) %>% count() %>% arrange(desc(n))
# Only 54 players have averaged 25 or more pts per game between seasons 1996-2020.

highest_ppg_player <- dataset %>% group_by(player_name) %>% filter(pts >= 36) %>% count() %>% arrange(desc(n))

top_15 <- highest_ppg[c(1:15),]
top_15
class(top_15)

highest_ppg_player  # James Harden averaged 36 ppg (points per game) in 2018-2019 season 

# Correlation of some of our variables

# Between points and assists
ggscatter(dataset, x = "pts", y="ast",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pts", ylab = "ast")
# pts and assists are positively correlated, as players who are able to score...
#..., are able to do other things such as pass the ball or rebound. 
# R = 0.66, p < 2.2e^-6

# Between height and rebounds
ggscatter(dataset, x = "player_height", y="reb",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "player_height", ylab = "reb")

# Another positive correlation with player_height and rebounding. Of course this...
# ... is fairly obvious, as taller players are more likely to grab more rebounds.
# Shorter players will have less 

ggscatter(dataset, x = "gp", y="pts",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "gp", ylab = "pts")


# Between height and assist percentage

ggscatter(dataset, x = "player_height", y="ast_pct",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "player_height", ylab = "ast_pct")

# Negative correlation between player height and assist percentage. Shorter players...
# ... are usually playing the guard position, and therefore they are more likely to...
# ... pass out the ball, whereas taller players are more likely to get more rebounds.


# New draft variable where we compare NBA players drafted vs undrafted:

dataset$draft <- ifelse(dataset$draft_year == "Undrafted", "No", "Yes")

players_drafted <- dataset %>% group_by(season) %>% filter(draft=="Yes") %>% count()

players_not_drafted <- dataset %>% group_by(season) %>% filter(draft=="No") %>% count()

draft_stats <- data.frame(year = c(substr(players_drafted$season, 1,4)),
           drafted = c(players_drafted$n),
           undrafted = c(players_not_drafted$n))

draft_stats$year <- as.numeric(draft_stats$year)

head(draft_stats)
tail(draft_stats)

max(draft_stats$drafted)
min(draft_stats$drafted)

####
ggscatter(draft_stats, x = "year", y="drafted",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "year", ylab = "# drafted")
####

draft <- ggplot(draft_stats, aes(x=year, y=drafted)) + geom_line(color="blue",size=1.5) +
  scale_x_continuous(breaks=seq(1996,2022, by=4)) + 
  labs(title = "NBA Trends between Drafted and Undrafted Players")

undraft <- ggplot(draft_stats,aes(x=year, y=undrafted)) + geom_line(color="red", size=1.5) +
  scale_x_continuous(breaks=seq(1996,2022, by=4)) + 
  labs(title = "NBA Trends between Drafted and Undrafted Players")

draft
undraft


# Now let's look at trends within the NBA in regards to player height and weight.



physical_stats <- data.frame(dataset) %>% group_by(player_name) %>% summarise_at(vars(player_height, player_weight), list(mean))                                                                   
physical_stats <- physical_stats %>% mutate(BMI = player_weight / (player_height * player_height/100)*100)

physical_stats

ggplot(physical_stats, aes(x=player_height)) + geom_histogram(binwidth = 2, color="black", fill="white") +
  labs(title= "Count Distribution of Height")
  
ggscatter(physical_stats, x = "player_weight", y="player_height",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "player_weight", ylab = "player_height")

# Positive correlation and we can reject the null hypothesis. This further proves that the...
#... advancement of technology further makes athletes bigger and stronger. Note that a higher...
# ...weight does not mean a bad thing, it simply means that athletes are bigger and are able to...
# ... max out their potential of muscle growth and peak athleticism. 

top_75_physical_stats <- physical_stats %>% filter(rank(desc(BMI)) <= 75)
top_75_physical_stats$BMI <- round(top_75_physical_stats$BMI, digit=2)



# International players vs player of origin from USA


# # of players in various countries
country_total <- dataset %>% group_by(country) %>% count(player_name)  # country / player / # of years in league
country_total2 <- country_total %>% group_by(country) %>% count() %>% arrange(desc(n))

# # of USA Players vs International Players
USA <- country_total2 %>% filter(country == "USA")
international_players <- country_total2 %>% filter(country != "USA")

class(country_total2)
class(USA)

summary(country_total$n)

sum(country_total2$n)
1971 / 2341


country <- country_total2[c(1:79),]
class(country)


international <- dataset %>% filter(country!= "USA")
international <- international %>% group_by(season) %>% count()
international <- data.frame(year = c(substr(international$season, 1,4)),
                             total = c(international$n))

international$year <- as.numeric(international$year)

ggplot(international, aes(year, total)) + geom_line() + labs(title= "Total growth of International Players in NBA")

ggscatter(international, x = "year", y="total",
          add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "year", ylab = "total")

ggscatter(international, x="year", y="total",
          color = "black", shape = 19,
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ellipse = TRUE, mean.point = TRUE, conf.int = TRUE, cor.coef = TRUE,
          star.plot = TRUE)

# Very clear indication that this regression is highly correlated, with a value of 0.97,
#... and a p-value almost to 0, so therefore we reject null hypothesis. 

dataset.ts <- ts(dataset$age, start=c(1996), end=c(2022), frequency=12)
str(dataset.ts)
start(dataset.ts)
end(dataset.ts)
frequency(dataset.ts)

print(dataset.ts)

plot(dataset.ts, col="blue", lwd=3, ylab="Point average")
abline(reg=lm(dataset.ts~time(dataset.ts)), lwd=3) 

# Display yearly averages
plot(aggregate(dataset.ts, FUN=mean), lwd=3, lty=1)
summary(dataset$age)

# Boxplot across months will give us a sense 
boxplot(dataset.ts~cycle(dataset.ts))

summary(dataset.ts)                           

max(dataset$pts)



# College that has had NBA Players 

dataset$college
duration_in_NBA <- dataset %>% group_by(player_name) %>% filter() %>% count() %>% arrange(desc(n))





