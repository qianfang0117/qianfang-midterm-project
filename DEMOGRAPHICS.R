setwd("/Users/Qian/Desktop/chsi_dataset")
degraphi <- read.csv("DEMOGRAPHICS.csv")
# DATA CLEANING
#is.na(degraphi)
# there is no missing value in the dataset
head(degraphi)
class(degraphi)
summary(degraphi$Population_Size)
summary(degraphi$Poverty)
# The % of age group 0-85 should covers 90% or above 90% of whole population.
#degraphi$Age_19_Under+degraphi$Age_19_64+degraphi$Age_65_84+degraphi$Age_85_and_Over > 90
# Age 19 to 64 should have the largest proportion. 
#degraphi$Age_19_64 > degraphi$Age_19_Under
#degraphi$Age_19_64 > degraphi$Age_85_and_Over
#degraphi$Age_19_64 > degraphi$Age_65_84
# Age data is ok, no need to clean
# Next, check for race data
# the % of race, white, Black, Asian, Hispanic should be over 50% of whole population
#degraphi$White+degraphi$Black+degraphi$Asian+degraphi$Hispanic < 50
#degraphi$CHSI_County_Name[degraphi$White+degraphi$Black+degraphi$Asian+degraphi$Hispanic < 50]
#degraphi$CHSI_State_Name[degraphi$White+degraphi$Black+degraphi$Asian+degraphi$Hispanic < 50]
# Most of states are in the border, so it is reasonable to have many other races(like Latino) of people. 
#degraphi$CHSI_County_Name[degraphi$CHSI_State_Name=="Nebraska" & degraphi$White+degraphi$Black+degraphi$Asian+degraphi$Hispanic < 50]
# the race data has no need to clean
# Then we look at poverty data
summary(degraphi$Poverty)
#There is a extreme value of minimum poverty, -2222
#since this is a percentage data, it shouldn't be -2222. we clean it to NA. 
degraphi$Poverty[degraphi$Poverty==-2222.2] <- 0


#EDA 

library(ggplot2)
qplot(degraphi$CHSI_State_Abbr,degraphi$Population_Size,stat = "identity",geom = "bar",fill=factor(degraphi$CHSI_State_Abbr))
ggplot(data=degraphi, aes(degraphi$CHSI_State_Abbr,degraphi$Population_Size))+ geom_boxplot((aes(color=degraphi$CHSI_State_Abbr)))
qplot(degraphi$CHSI_State_Abbr,degraphi$Poverty,stat = "identity",geom = "bar",fill=factor(degraphi$CHSI_State_Abbr))
ggplot(data=degraphi,aes(x = degraphi$CHSI_State_Abbr,y=degraphi$Population_Size))+geom_point(aes(color=degraphi$CHSI_State_Abbr),stat = "identity")
ggplot(data=degraphi,aes(x = degraphi$CHSI_State_Abbr,y=degraphi$Poverty))+geom_point(aes(color=degraphi$CHSI_State_Abbr),stat = "identity")
ggplot(data=degraphi,aes(x = degraphi$CHSI_State_Abbr,degraphi$White))+ geom_point(aes(color=degraphi$CHSI_State_Abbr),stat = "identity")
ggplot(data=degraphi, aes(degraphi$CHSI_State_Abbr,degraphi$Black))+ geom_boxplot((aes(color=degraphi$CHSI_State_Abbr)))


# CLUSTER K MEANS
require(useful)
data2 <- cbind.data.frame(degraphi$Population_Size,degraphi$State_FIPS_Code)
class(data2)
set.seed(50)
data2k<- kmeans(data2, centers=3)
data2k
plot(data2k,data2)















