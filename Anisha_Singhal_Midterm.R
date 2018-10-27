# Part 1
#Ques 1. Since 2 devices have exploded and total 2000 devices were tested, so total number of tests done is 2000
# and the  number of devices exploded are 2. Hence, the probability of a battery to not explode in this device
# will be 1-(Probability of a device to explode). The probability of a device to explode is 2/2000. Hence, the
#required probability of a battery to explode is 0.999
probability<-1-(2/2000)
probability

# Ques 2. Given that mean= 52 and standard deviation= 10.5 and streaming has normal distribution.
# a. For streaming below 54 minutes
str1<-pnorm(54, mean=52, sd=10.5)
# For streaming above 60 minutes
str2<-1-pnorm(60, mean=52, sd=10.5)
probab<-1-(str1+str2)
probab
# Hence, the probability of streaming between 54 to 60 is 0.2

# b. For streaming at least 45 minutes, we need to find percentile above 45 minutes
probab1<- 1-pnorm(45, mean=52, sd=10.5)
# Probability is 0.74

# c. For streaming less than 35 minutes
probab2<- pnorm(35, mean=52, sd=10.5)
# Probability is 0.05

# Part 2
View(cars)
# Ques 1. Since Highway.mpg, city.mpg and price are all numeric variables, so we can use describe() function for descriptive 
# statistics and histogram with overlayed rug and density plot for plots.
# Highway.mpg
describe(cars$highway.mpg)
hist(cars$highway.mpg, col=c("steelblue", "red"), freq=F) 
rug(jitter(cars$highway.mpg), col="darkgray")
lines(density(cars$highway.mpg), col="yellow", lwd=2)  
box()
boxplot(cars$highway.mpg) #gives 3 ouliers
# Highway.mpg is a normal distribution with few outliers towards the right which means they have very high highway.mpg.
#Also,the mean(30.79) and median(30) are almost same which further conforms to the normal distribution.

# City.mpg
describe(cars$city.mpg)
hist(cars$city.mpg, col=c("steelblue", "red"), freq=F) 
rug(jitter(cars$city.mpg), col="darkgray")
lines(density(cars$city.mpg), col="yellow", lwd=2) 
box()
boxplot(cars$city.mpg) # gives 2 outliers
#City.mpg also follows a normal distribution as shown in the plot with few minor peaks coming out. It also has 2 outliers
#towards the right which means that 2 cars have very high city.mpg. The mean and median are almost same. 

# Price
describe(cars$price)
hist(cars$price, col=c("steelblue", "red"), freq=F) 
rug(jitter(cars$price), col="darkgray")
lines(density(cars$price), col="yellow", lwd=2) 
box()
boxplot(cars$price) #large number of outliers
# Price distribution is right skewed with many outliers towards the right which means that there are many cars with high prices.
#The mean is greater than the median which confirms the right skewdness as well as skew is 1.73. Also, the standard deviation
# is more than half of the mean which means the observatons are widely spread.

# Ques 2.
# mpg variable is average of city.mpg and highway.mpg
cars$mpg<-((cars$city.mpg+cars$highway.mpg)/2)

# price_cat to store High and Low for greater than 10000 and less than 10000 respectively
cars$price_cat[(cars$price>10000)]<-"High"
cars$price_cat[(cars$price<=10000)]<-"Low"

# Since mpg is numeric variable, it can be analysed by histogram and describe()
describe(cars$mpg)
hist(cars$mpg, col=c("steelblue", "red"), freq=F) 
rug(jitter(cars$mpg), col="darkgray")
lines(density(cars$mpg), col="yellow", lwd=2) 
box()
boxplot(cars$mpg) # gives three outliers
# mpg shows an approximate normal distribution with a few outliers towards right having very high mpg. The mean and median are 
# very close.

# We need to convert price_cat to factor variable as it has only two values "High" and "Low"
cars$price_cat<- as.factor(cars$price_cat)
# Now we examine it with table and barplot
tab<-table(cars$price_cat)
ptab<-prop.table(tab)
barplot(ptab, main = "Bar Plot", xlab = "Price category", ylab = "Frequency", col = c("steelblue","orange"))
box()
# Cars having price above 10000 are 98 and those below it are 95. Hence, both categories have almost equal proportion of cars.

#Relation between mpg and price_cat: Since mpg is numeric and price_cat is factor, so we can use boxplot for visual and 
#describeBy for statistical interpretation
describeBy(cars$mpg,cars$price_cat)
boxplot(cars$mpg ~ cars$price_cat, data=cars, main="Miles per Gallon By Price Category", 
        xlab="Price category", ylab="Miles per gallon", col=c("orange", "lightblue4"))
# From the statistics, the range of mpg is higher for high cost cars and widely spread. Also, the mean and
#median mpg of low cost cars is higher than the mean and median mpg of high cost cars. Hence, the average mpg of
#Low category is better than that of High. Also,the maximum mpg of High category is only near to the third quartile of Low
#category. The upper 50 percent mpg of cars in High are mostly within the lower 50 percent mpg of Low.

# Ques 3.
mean(cars$mpg[cars$price_cat=='Low'])-mean(cars$mpg[cars$price_cat=='High'])
# It gives 9.26 which means average mpg of Low cost cars is 9.26 higher than the average mpg of high cost cars.Hence,
# low cost cars have better average mpg.
sd(cars$mpg[cars$price_cat=='Low'])-sd(cars$mpg[cars$price_cat=='High'])
# Standard deviation of low cost cars is 1.13 more than standard deviation of high cost cars which means the mpg of low 
#cost cars is more widely spread.

# Ques 4. 
# a.From the boxplot plotted above, we can find 2 outliers in High category and 3 outliers in Low category
identify(cars$mpg~cars$price_cat)
# b.The outliers are at index 181 and 58 in High category and at index 18,29 and 82 in Low category. 
# All the outliers are over the whiskers. The detail of cars for these outliers is given below:
cars[c(18,29,58,82,181),]

# Part 3
# Ques 3
View(tix)
# First, we need to remove commas from Gross variable and convert it to numeric variable
tix$Gross<-as.numeric(gsub(",", "", tix$Gross))
# Also, type needs to be converted to factor
tix$Type<-as.factor(tix$Type)
# To determine relation between Type of theater(factor variable) and Gross(numeric variable), we need to use
# boxplot for visual and describeBy for statistical examination
describeBy(tix$Gross,tix$Type)
boxplot(Gross ~ Type, data=tix, main="Gross by Type", 
        xlab="Type", ylab="Gross", col=c("orange", "lightblue4","purple"))
#From the statistics, average gross is highest for Musical type which can be seen from boxplots also and lowest for Play.
#The range of Gross is also highest for Musical and lowest for Play. Musical and Play have many outliers as compared to Special.
#Hence, we can conclude that the company's speculation is wrong because there is a significant difference in Gross among the 
#three Types. The Gross of Musical is quite high. It's 50% of Gross above the median is having high gross values as compared to
# other types. Also, the IQR of Musical is highest which means a significant high amount of Gross is there in Musical than
# other types. The first quartile of Special is very low as compared to Musical which means first 25% of Special has very low 
# Gross than others. Hence, Musical provides maximum Gross values than others.