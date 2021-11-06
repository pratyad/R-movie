
setwd("~/Downloads")

# Set number of significant digits
options(digits = 3)
install.packages("tidyverse")
install.packages("caret")
install.packages("corrplot")
library(corrplot)
library(caret)
library(tidyverse)
winequality.white <- read.csv("~/Downloads/winequality-white.csv", sep=";")
Whitewine<-winequality.white
View(Whitewine)
str(Whitewine)
head(Whitewine)
# 11 attributes are given and 12(quality) is dependent entity
# correlation between all the attributes of the dataset Whitewine

c<-cor(Whitewine)
c
corrplot(c)

# find if there are any null value in the dataset
sum(is.na(Whitewine))
# there are not null values in the dataset. 

# sort by quality
Whitewine%>%count(quality)

#plot quality
Whitewine%>%ggplot(aes(quality)) + 
  geom_histogram(color="black", fill="white", bins = 20)

# we want to divide quality into three parts "Superior==3","Inferior==1" and "Fine==2"
# if quality is less than equal to 5 then wine is inferior, quality ==6 wine is fine
# if quality greater than equal to 7 then it is superior wine
# we want to add another column in Whitewine dataset with binary quality values

Wine<-Whitewine%>%mutate(grade= ifelse(quality<=5, 1, ifelse(quality<=6, 2, 3)))
head(Wine)
# Proportion of superior vs fine vs inferior wine
Wine%>%count(grade)

#We split the dataset in two parts, one for training and one for testing

# Test set will be 10% of the entire dataset
set.seed(2021, sample.kind = "Rounding")
test_index <- createDataPartition(y = Wine$grade, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)

# Train and test sets for Wine set
train_set <- Wine[-test_index,]
test_set  <- Wine[test_index,]

#Data Exploration

# display structure of training dataset

str(train_set)

# Statistical summary of training dataset

summary(train_set)

# grade distribution

train_set%>%ggplot(aes(grade))+
geom_histogram(color="black", fill="white", bins = 10)

# we can see from the histogram that fine quality wine are maximum in number and superior 
#quality is least in number

# plot histogram of various attributes to see how they are distributed
par(mfrow= c(3,4))
hist(train_set$fixed.acidity, main = "fixed_acidity" ,bins = 30)
hist(train_set$volatile.acidity, main = "volatile_acidity", bins = 30)
hist(train_set$citric.acid,main = "citric_acid", bins = 30)
hist(train_set$residual.sugar, main= "residual.sugar", bins = 30)
hist(train_set$chlorides,main = "chlorides", bins = 30)
hist(train_set$free.sulfur.dioxide, main = "free_sulfur_dioxide", bins = 30)
hist(train_set$total.sulfur.dioxide, main = "total_sulfur_dioxide", bins = 30)
hist(train_set$density, main ="density", bins = 30)
hist(train_set$pH,main = "pH", bins = 30)
hist(train_set$sulphates,main = "sulphates", bins = 30)
hist(train_set$grade, main = "grade", bins = 20)

#The histograms reveal that Total sulfur dioxide and pH are normally disributed 
#rest of variables are more or less right skewed (long-tailed). 
#The grade has a semi-normal discrete distribution.


# Plot a histogram of grade values
train_set%>%ggplot(aes(grade)) +
  geom_histogram(color = 'black', fill = '#099DD9', binwidth = 1) +
  # Used to show 0-10 range, even if there are no values close to 0 or 5
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
  xlab('Grade of White Wine') +
  ylab('Number of White Wines')



# which attribute contributes most in deciding quality of wine 
cor(train_set$quality, train_set[,-12])
# we can see it is alcohol quantity which changes quality of wine
# alcohol percent vs wine quality using boxplot
BP<-boxplot(alcohol~quality,data=train_set, main="Alcohol vs Quality", col=blues9,
        xlab="quality of wine", ylab="alcohol quantity")

# we will see effect of all attributes of quality of the wine using boxplot
# we have created another column called "grade" to reduce number of quality identifiers into 3
# we will use grade instead of quality to make these boxplots

# Independent attributes behavior to define quality of the wine

#Fixed Acidity

ggplot(data =train_set, aes(fixed.acidity)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'blue') +
  # Split data by category and display plots horizontally
  facet_wrap(~grade)

ggplot(data = train_set, aes(x = grade, y = fixed.acidity, color = grade, group = grade )) +
  geom_boxplot()

#The low and high means are not far from each other and their IQRs overlap
#The variability between low and high quality categories is moderate 

# Volatile Acidity

ggplot(data =train_set, aes(volatile.acidity)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'blue') +
  # Split data by category and display plots horizontally
  facet_wrap(~grade)

ggplot(data = train_set, aes(x = grade, y = volatile.acidity, color = grade, group = grade )) +
  geom_boxplot()

# There is a slight variability between the low and high mean but not substantial

#Citric Acid

ggplot(data=train_set, aes(citric.acid)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'blue') +
  # Split data by category and display plots horizontally
  facet_wrap(~grade)

ggplot(data = train_set, aes(x = grade, y = citric.acid, color = grade, group = grade )) +
  geom_boxplot()

# there is almost no variability between low and high mean 

#Residual Sugar

ggplot(data=train_set, aes(residual.sugar)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'blue') +
  # Split data by category and display plots horizontally
  facet_wrap(~grade)
ggplot(data = train_set, aes(x = grade, y = residual.sugar, color = grade, group = grade )) +
  geom_boxplot()

# there is almost no variability between low and high mean and and their IQRs overlap

#Chlorides

ggplot(data = train_set, aes(chlorides)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~grade)

ggplot(data = train_set, aes(x = grade, y = chlorides, color = grade, group = grade )) +
  geom_boxplot()

#the low and high means are very close to each other and their IQRs overlap like residual sugar

#Free_Sulfur_Dioxide

ggplot(data = train_set, aes(free.sulfur.dioxide)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~grade)

ggplot(data = train_set, aes(x = grade, y = free.sulfur.dioxide, color = grade, group = grade )) +
  geom_boxplot()

#The variability between low and high quality categories is low comparing to other variables 

#Total_Sulfur_Dioxide

ggplot(data = train_set, aes(total.sulfur.dioxide)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~grade)

ggplot(data = train_set, aes(x = grade, y = total.sulfur.dioxide, color = grade, group = grade )) +
  geom_boxplot()

#the low and high means are very close to each other 

#Density

ggplot(data = train_set, aes(density)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~grade)
ggplot(data = train_set, aes(x = grade, y = density, color = grade, group = grade )) +
  geom_boxplot()

#There is a slight difference between low and high means 

#pH

ggplot(data = train_set, aes(pH)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~grade)
ggplot(data = train_set, aes(x = grade, y = pH, color = grade, group = grade )) +
  geom_boxplot()

#The low and high means are close to each other 

#sulphates

ggplot(data = train_set, aes(sulphates)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~grade)
ggplot(data = train_set, aes(x = grade, y = sulphates, color = grade, group = grade )) +
  geom_boxplot()

#The low and high means are close to each other 

#Alcohol

ggplot(data = train_set, aes(alcohol)) +
  geom_histogram(binwidth = 0.2, color= 'black', fill= 'blue') +
  facet_wrap(~grade)
ggplot(data = train_set, aes(x = grade, y = alcohol, color = grade, group = grade )) +
  geom_boxplot()

#The low and high means are very far from each other and their IQRs do overlap only slightly being very dispersed

#It seems that volatile.acidity, total.sulfur.dioxide, density and alcohol 
#show the biggest variability (difference in means and IQR ranges) and 
#these variables will be used in further analysis.

draw_boxplot<-function(dataframe, variable, ylab)
{
  plot<-ggplot(data = train_set, aes(x= grade, y = variable, color = grade, group = grade))+
    geom_boxplot() +
    xlab('grade') +
    ylab(ylab)
  return(plot)
}

# Build 4 boxplots summarizing distributions of 4 selected attributes

draw_summary<- function()
{
  grid.arrange(draw_boxplot(train_set, train_set$volatile.acidity, 'volatile.acidity'),
               draw_boxplot(train_set, train_set$density, 'density'),
               draw_boxplot(train_set, train_set$total.sulfur.dioxide, 'total_sulfur_dioxide'),
               draw_boxplot(train_set, train_set$alcohol, 'alcohol'),
               ncol=4)
}

draw_summary()

# we can see from above that alcohol content is a major deciding factor in determining the quality of wine




