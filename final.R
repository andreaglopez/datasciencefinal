##Final Project
##Andrea Gil-Lopez

data <- read.csv("voting-ineligibility-since-1980.csv")

nrow(data)

allowed <- subset(data, Total_Ineligible_Felon == 0)
range(allowed$Year)
statesallowed <- length(allowed)

for(i in 1:length(allowed)){
  statesallowed[i] <- allowed$State[i]
}
statesallowed ##states that have always allow felons to vote only 2 states and DC
#from 2000 to 2014 before 2000 included Mass.and Utah (1996)

year1980 <- subset(data, Year == 1980 )
year2014 <- subset(data, Year == 2014)
mean(year2014$Total_Ineligible_Felon) - mean(year1980$Total_Ineligible_Felon) 

mean(year1980$Percent.Ineligible.Because.of.Felony.Charges)
mean(year2014$Percent.Ineligible.Because.of.Felony.Charges)



library(ggplot2)
ggplot(data, aes(x = Year, y = Percent.Ineligible.Because.of.Felony.Charges, na.rn = T)) + geom_point() +
  ggtitle("Average Ineligible in the US ") +
  xlab("Year")+
  ylab("Average Percentage") +
  theme(legend.position="bottom")


year <- c(1980:2014)
avg <- length(year)
result <- rep(NA, length(year))

for(i in 1:length(year)){
  avg <- subset(data, Year == year[i])
  result[i] <- mean(avg$Voting.Eligible.Population)
}
range(result, na.rm = T)

plot( y = result, x = year, na.rm = T,
 main = "Average Eligible Voter in the US ",
  xlab = "Year",
  ylab = "Average Eligible Voter Population")
#################

avg2 <- length(year)
result2 <- rep(NA, length(year))

for(i in 1:length(year)){
  avg2 <- subset(data, Year == year[i])
  result2[i] <- mean(avg2$Total_Ineligible_Felon)
}
range(result2, na.rm = T)

plot(y = result2, x = year, na.rm = T,
      main = "Average Ineligible Voters in the US ",
      xlab = "Year",
      ylab = "Average Ineligible Voters")
##################

avg3 <- length(year)
result3 <- rep(NA, length(year))

for(i in 1:length(year)){
  avg3 <- subset(data, Year == year[i])
  result3[i] <- mean(avg3$Percent.Ineligible.Because.of.Felony.Charges)
}
range(result3, na.rm = T)
plot(y = result3, x = year, na.rm = T,
     main = "Average Ineligible Percentage in the US ",
     xlab = "Year",
     ylab = "Average Percentage")
##################
library(reshape2)
library(dbplyr)

total <- data.frame(year,result,result2)
#names(total)
names(total)[2] <- "Eligible"
names(total)[3] <- "Ineligible"
total <- na.omit(total)
total2<- melt(total, id.vars = 'year')

ggplot(total, aes(year)) +  
  geom_line(aes(y = result), color = "black") +
  geom_line(aes(y = result2), color = "red") +
  ggtitle("Average US Population for Voting Ineligible") +
  xlab("Year")+
  ylab("Amount of Population") +
  theme(legend.position="bottom")

ggplot(total2, aes(x=year, y=value, fill=variable)) + ggtitle("Average US Population Voters") +
  xlab("Year")+ ylab("Amount of Population") +
  geom_bar(stat='identity', position='dodge')
