#
#Project On MoneyBall

# setwd
setwd("~/Projects/R_MoneyBall")

# Reading CSV
batting <- read.csv('Batting.csv')
print(head(batting))



str(batting)

# Feature Engineering
"""
Batting Average
AVG =  H/ AB (HIt/At Bat)
"""

batting$BA <- batting$H / batting$AB

"""
On base percentage
OBA = H + BB + HBP/ AB +BB HBP +SF
"""
batting$OBP <- (batting$H + batting$BB + batting$HBP)/
  (batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles) AS not available in data
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Creating Slugging Average (SLG)

"""
SLG = ((1B) +(2*2B) + (3*3B) + (4*HR))/ AB
"""
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

summary(batting)

# Merging Salaries with batting

sal <- read.csv('Salaries.csv')

# Using subset() to reassign batting to only contain data from 1985 and onwards

batting <- subset(batting,yearID >= 1985)


# Merging

combo <- merge(batting,sal,by=c('playerID','yearID'))


summary(combo)

# Analyzing the lost players

"""
 The players lost were: first baseman 2000 AL MVP Jason Giambi (giambja01) to the New York Yankees, 
 outfielder Johnny Damon (damonjo01) to the Boston Red Sox and
 infielder Rainer Gustavo Olmedo ('saenzol01').
"""

lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )

lost_players


# Use subset again to only grab the rows where the yearID was 2001.

lost_players <- subset(lost_players,yearID == 2001)


lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB','salary')]


# Find Replacement Players for the key three players we lost
"""
Three constraints:

The total combined salary of the three players can not exceed 15 million dollars.
Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
Their mean OBP had to equal to or greater than the mean OBP of the lost players
"""

combo <- subset(combo, yearID == 2001)

"""
or you can use
library(dplyr)
combo <- filter(combo,yearID==2001)
"""
library(ggplot2)
ggplot(combo,aes(x=OBP,y=salary)) + geom_point()

#Looks like there is no point in paying above 8 million 

combo <- subset(combo,salary<8000000,OBP > 0)


# total AB of the lost players is 1469.
# cut off my combo at 1500/3= 500 AB.

combo <- subset(combo,AB >= 500)

library(dplyr)
arrange(combo,OBP)

head(arrange(combo,desc(OBP)),10)

options <- head(arrange(combo,desc(OBP)),10)

options <- options[,c('playerID','OBP','AB','salary')]


#Soln as 1st cannot be chosen
options[2:4,]

