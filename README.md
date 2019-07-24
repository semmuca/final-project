# FIFA-Ranking-project using R 
library(tidyverse)
#project on fifa ranking

fifa <- read.csv(file = "C:/Users/semmu/Desktop/FIFA_rank.csv",sep = ",",  header = TRUE)
fifa_r<- as_tibble(fifa)
fifa_r

# selects all rows which have ranks 1 or 2 or 3
fifa_1 <- filter(fifa_r, rank %in% c(1), total_points > "0")
fifa_1

#selecting columns which will be used in our analysis(this is the final prepared data for Fifa rank)

FifaData<- select(fifa_1, rank, country_full,total_points, previous_points, rank_change,confederation,
       rank_date)
FifaData



#analysing for a single categorical variables

# frequency of countries who achived #1 rank 
rank_1_country <- FifaData[c(FifaData$rank) == "1",]
Rank_1_country <- matrix(rank_1_country$country_full)
FifaWinner <- table(Rank_1_country)
FifaWinner


# plot for for number of times a country Rannking # 1 between 8/24/2011 to 5/17/2018

barplot(FifaWinner, col = "blue", ylim = c(0,40),
        xlab = "country", ylab = "frequency", main = "Ranking # 1 (2011 to 2018)")

# the proportion of each country becoming #1 in ranking
slice.labels <- names(FifaWinner)
slice.percents <- round((FifaWinner/sum(FifaWinner)*100),2)
slice.labels <- paste(slice.labels,slice.percents)
slice.labels <- paste(slice.labels, "%", sep = "")
pie(FifaWinner, labels = slice.labels,
    col = rainbow(8))

 
# analysis for a single numeric variable 


# analysing the total score point for ranking # 1
tot_point <- FifaData$total_points
tot_point

# The analysis of score point to rank # 1  
#mean
mean(tot_point)

#median
median(tot_point)

table(tot_point)

#mode
which(table(tot_point) == max(table(tot_point)))

#range
range(tot_point)



#standard divation
sd(tot_point)


fivenum(tot_point)

summary(tot_point)

quantile(tot_point,c(0,0.25,0.5,0.75,1))

IQR(tot_point)

# the Z-score of the total points to rank # 1 represent the number of standard deviation  
# ...the scores have from the mean of the countries total point which ranked #1
scale(tot_point)

# bar plot showing the all of the total points given 
barplot(tot_point, xlab = "",ylab = "total_points",ylim = c(0,2000),col = "cyan",las = 2)



# frequency for the total points given in interval of 50

hist(tot_point,col = "cyan", ylim = c(0,30), xlab = "Total point ")
###Interpretation### The above plot shows the most frequently scored total point to rank  the countries #1 is between 1600 and 1650.
### which is 25 times. the total point is scored above 1750 only 3 times and 6 times countries ranked #1 with a total point
### less than 1450.


x <- hist(tot_point)
names(x)
# The ranges for each interval for the histogram
x$breaks


# The number of values(total points) that lie with in each of these intervals
x$counts
#.........................................................................................................
# five number anlysis using box blot for total points
boxplot(tot_point,col =hcl(0),horizontal = TRUE, xaxt = "n",
        xlab = "Total points")
axis(side = 1, at = fivenum(tot_point),labels = TRUE, las = 2)
text(fivenum(tot_point), rep(1.2,5),srt = 90, adj = 0,
     labels = c("Min","Lower Hinge", "Median", "Upper Hinge","Max"))


#Bivariate categorical (using two variable)

Fifa.relation<- data.frame(FifaData$country_full,FifaData$confederation)
Fifa.relation <- droplevels(Fifa.relation)
table(Fifa.relation)
table(Fifa.relation)

 barplot(table(Fifa.relation),xlab = "Country",ylab = "ranking #1", beside = TRUE, legend.text = TRUE,
         main = "confoderation VS country relation",border = FALSE,
         args.legend = list(x = "center"), ylim = c(0,40),col = rainbow(6))


# Bivariate numeriacal relation
plot(FifaData$total_points,FifaData$rank_change,ylim = c(0,2))
### the scater plot shows that except for some exceptions the rank change was not affected by the increase 
###in the total points for countries ranked #1.
plot(FifaData$total_points,FifaData$previous_points, ylim = c(1000,2000))
summary(FifaData$total_points)
summary(FifaData$previous_points)
summary(FifaData$total_points/FifaData$previous_points,
        xlab = "Total points", ylab = "Previous points")

###the plot shows the data falls allong a straight line with few exceptions and the score points for both previous
###current are almost the same.



# Examining the total points variable distribution

hist(tot_point, xlim = c(1400,1800),
     ylim = c(0,30), main = " distribution of total point", col = "cyan" )

#Central limit Theorem

# 1000 random samples of the data sample sizes 10,20,30,40


# sample size = 10, mean = 1583.408, sd = 88.75048
par(mfrow = c(2,2))
samples <- 1000

xbar <- numeric(samples)
for (size in c(10, 20, 30, 40)){
  for (i in 1:samples) {
    xbar[i] <- mean(rnorm(size,mean = 1583.408, sd = 88.75048 ))
  }
  
  hist(xbar, prob = TRUE, 
       breaks = 15, xlim=c(1500,1700), ylim = c(0, 0.03),
       main = paste("Sample Size =", size), col = "cyan")
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")}


library(sampling)
# Sampling
# Part 4) Sampling
sample.size <- 20

# simple random sampling without replacement

s <- srswor(sample.size,nrow(FifaData))
s
sample.a <- FifaData[s != 0,]
sample.a
#  Frequencies 

table(sample.a$rank_change)
# Percentages with the whole data

table(sample.a$rank_change)/table(FifaData$rank_change)


# systematic sampling
n <- 20
N <- nrow (FifaData)
N

# in each group
k <- floor(N / n)
k

# first group
r <- sample(k,1)
r

# selecting every kth item
s <- seq(r, by = k, length = n)

sample.b <- FifaData[s,]
sample.b

# Frequencies of each confederation
table(sample.b$rank_change)


# inclusion probabilities for total points to rank a national team #1
pik <- inclusionprobabilities(FifaData$total_points,20)
pik

#  using systematic sampling

s <- UPsystematic(pik)
sample.c <- FifaData[s != 0,]
sample.c

#Frequencies of confederation after sample.c
table(sample.c$rank_change)



# Ordering the data using rank_change variable
order.index <- order(FifaData$rank_change)
data <- FifaData[order.index, ]
data

# A stratified sample 
freq <- table(FifaData$rank_change)
freq

# from each stratum 
sizes <- round(20 * freq / sum(freq))
sizes

# The Samples 
stra <- strata(data, stratanames = c("rank_change"), size = sizes, method = "srswor")
stra
sample.d <- getdata(data,stra)
sample.d

# The frequencies for each rannk change
table(sample.d$confed)

# The percentage with respect to the data
table(sample.d$rank_change)/table(FifaData$rank_change)



# Comparing the means

# Mean of Total point with the entire data
mean(FifaData$total_points)

# Mean of total points variable for sample.a
mean(sample.a$total_points)

# Mean of total points variable for sample.b
mean(sample.b$total_points)

# Mean of total points variable for sample.c
mean(sample.c$total_points)

# Mean of Total points variable for sample.d
mean(sample.d$total_points)



# Implimentation  extra or additional feature using a ploting over grid and image.
# Load `png` 
rank.change.freq <- table(FifaData$rank_change)
library(png)

# Read in the png file 
my_image <- readPNG("C:/Users/semmu/OneDrive/Pictures/Saved Pictures/cs544image.png")

# Set up a plot area with no plot
plot(1:2,xlim = c(0,2), ylim = c(0,72), type='n', main="", 
     xlab="rank change", ylab="frequency of change")

# Get the plot information
lim <- par()
rasterImage(my_image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

#Add your plot
lines(rank.change.freq, lwd = 5,col = "red")

