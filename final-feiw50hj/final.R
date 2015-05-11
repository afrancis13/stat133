#################################################################
# Statistics 133, Lecture 2, Spring 2015
# Final Exam, Monday May 11th, 11:30 am - 14:30 pm

# Please read all instructions carefully.
#################################################################
# IMPORTANT
# Enter your personal information here, between the quotation marks:
name <- "Alex Francis"
github.name <- "afrancis13"
email.address <- "afrancis@berkeley.edu"

#################################################################
# The exam has a total of 100 points and for each task 
# the point is given in square brackets.
# The exam has 6 parts and the total number of points by part is:
# 15 pts : Part I, general R commands
# 20 pts : Part II, plotting
# 15 pts : Part III, apply and by statements
# 15 pts : Part IV, functions 
# 15 pts : Part V, simulations
# 20 pts : Part VI, string manipulation and regular expressions
#100 pts in Total

# If you spend about 30 min or so reading instructions and the like
# this gives you 1.5 min per point -- the exam is LONG ON PURPOSE!
# Please just work your way through it the best you can, prioritizing
# things you can easily do and remember everyone else is also
# racing the clock and will be graded on a curve.

#################################################################
# DO NOT manually set the path anywhere in this script, instead:

# RIGHT NOW!!   In the RStudio menu select:
# Session > Set Working Directory > To Source File Location

# NOTE!
# All "load" statements are included in the file already,
# DO NOT change them in any way or add a path to them,
# it will prevent us from being able to run your script.

# NOTE!
# There are a few calls to the function set.seed()
# DO NOT change these in any way.

#################################################################

#### PART I : General R commands [15 pts]

# [1 pt]
# Create [x], a numeric vector of length 1200 with 
# entries: 3, 6, 9, 12, etc
x <- seq(from=3, by=3, length.out=1200)
  
# [2 pts]
# Create [y], a list of length 10 where list k
# stores the output of the k-times multiplication
# table, including values up to 1000 so
# y[[1]] is 1, 2, 3, ..., 1000
# y[[2]] is 2, 4, 6, ..., 1000
# y[[3]] is 3, 6, 9, ..., 999

y <- lapply(seq(1, 10, 1), FUN=function(k) { return(seq(k, 1000, by=k)) })

# [1 pt]
# Create [z], a logical vector of length 5000 with entries
# TRUE and FALSE, where each value has
#  a probability 0.3 of being TRUE and 0.7 of being FALSE
# Hint: you can e.g. use [sample()], you will need to set all 4 arguments
set.seed(42)
z <- sample(c(TRUE, FALSE), size=5000, replace=TRUE, prob=c(0.3, 0.7))
  
  
# [1 pt]
# Create [v], a numeric vector with the results of 100 rolls of a dice,
# i.e. each element is one of the numbers 1 through 6, with equal probability.
  set.seed(31415)
v <- sample(1:6, size=100, replace=TRUE)


# [1 pt]
# Create [w], a character vector of length 26 with entries:
# "aa", "ab", "ac", "ad", ..., "az"
# Hint: the variable [letters] is an inbuilt constant in R 
# and stores the alphabet (try typing letters)
set.seed(2718)
w <- paste("a", letters, sep='')

  
# [1 pt]
# Create [m], a matrix of size 10x10 with entries that are 
# Normal random variables (hint: rnorm) with mean 8
# (arrange the values by column, as per default)
set.seed(344)
m <- matrix(rnorm(n=100, mean=8), nrow=10, ncol=10, byrow=FALSE)
  
# For the next few tasks you will use the data frame ChickWeight
# This data frame comes with R so you do not need to load any data.
# To get the size of the data frame type
dim(ChickWeight)
# To see the first few lines type
head(ChickWeight)
# There are 4 variables in the dataframe:
# > names(ChickWeight)
# [1] "weight" "Time"   "Chick"  "Diet"  

# [1 pt]
# Create [cw1] a subset of ChickWeight with Chicks on 
# Diet 1 at time 0
cw1 <- subset(x=ChickWeight, Time==0 & Diet == 1)

  
# [1 pt]
# Create [cw2] a subset of ChickWeight with only Chicks that
# weighed more than 41 at Time 0
cw2 <- subset(x=ChickWeight, Time == 0 & weight > 41)

  
# [1 pt]
# Create [cw3] a single number, the average chick weight at time 0
cw3 <- mean(subset(ChickWeight, Time == 0)$weight)
  

# [1 pt]
# Create [cw4] a random subset of 200 ChickWeight observations (any 200)
cw4 <- subset(ChickWeight, sample(1:dim(ChickWeight)[1], size=200))

# for the next two tasks you will use the data frame infants (size 1236x15)
# LEAVE AS IS:
load("KaiserBabies.rda") 

# [1 pt]
# Create a table [t1] of the smoking history of the mothers using the variables
# [smoke] for the rows of the table and  [number] for the columns.
# 
t1 <- table(infants$smoke, infants$number)

# [1 pt] 
# Now create a table [t2] which is the same table as before except restricted
# to mothers with College education (use [ed])
# 
t2 <- table(infants$smoke[infants$ed == "College"], infants$number[infants$ed == "College"])


# [2 pts] 
# Calculate [bdiff], the difference in average birthweight [column bwt] between babies born to mothers
# that are 35 years or older and mothers that are under 35 years of age [use column age for mothers age]
#  
bdiff <- mean(infants$bwt[infants$age >=35], na.rm=TRUE) - mean(infants$bwt[infants$age < 35], na.rm=TRUE)

#################################################################
#### PART II : Plotting [20 pts]

##### Cars [8 pts total, 2+3+3]
# We will now use the dataset "mtcars" which is icluded in the R package.
# To look at the dataframe you can just type "mtcars" at the prompt
# It is a data frame of size 32x12

# [2 pts]
# Make a box plot of [mpg] by [cyl] (so 3 boxplots in one plot)
boxplot(mtcars$mpg ~ mtcars$cyl)


# [3 pts]
# Use the function [lm()] to fit a regression model of [qsec] (1/4 mile time) regressed on [hp] (horsepower)
# Make a scatterplot of [qsec] versus [hp] and add theregression line to it
# Hint: the regression coefficients are stored in [coef] in the lm object and you can use [abline()] to
# add a line to a plot
regressionObject <- lm(mtcars$qsec ~ mtcars$hp)
plot(x=mtcars$hp, y=mtcars$qsec)
abline(a = coef(regressionObject)[1], b = coef(regressionObject)[2])

# [3 pt]
# Make a scatterplot of [disp] (on y-axis) versus [wt] 
# where disp is the engine displacement in cu. in. and wt is the weight of the car in lb/1000.
# Color the plotting symbol by [am] (automatic or manual)(any 2 colors)
# Edit the labels on the x-axis and y-axis to state what the variables are.
# Hint: first do the plot without adding the colors.
plot(x=mtcars$wt, y=mtcars$disp, xlab="Weight of car in lb/1000", ylab="Engine Displacement in cu. in.", col=mtcars$am + 5)

##### We will now use the World Bank Data [12 pts]
load("WorldBank.RData")

# [4 pts]
# Make a scatterplot of life expectancy ["life.expectancy"] vs. fertility ["fertility.rate"]
# The plotting symbol should be a small dot (.)
# Please color the symbols by region (pick any color for each region)
# You do _NOT_ need to add a legend saying which color is which region.
# Put on custom made x-axis and y-axis labels that fully describe the variables
# Add a vertical line at fertility=2.1 (the rate needed to maintain constant population size)
plot(x=WorldBank$fertility.rate, y=WorldBank$life.expectancy, pch=".", 
     col=WorldBank$region, xlab="Fertility Rate", ylab="Life Expectancy")
abline(v=2.1)
# [4 pts]
# Do you see patterns in the plot?  You just plotted these variables for all years between 1960 and 2014.
# Please redo the plot, but this time put two plots side by side (hint: before plotting set par(mfrow=...) )
# The left plot should include only data from 1960, the right one only from 2014.
par(mfrow=c(1, 2))
subset1960 <- subset(WorldBank, year==1960)
plot(x=subset1960$fertility.rate, y=subset1960$life.expectancy, pch=".", 
     col=WorldBank$region, xlab="Fertility Rate", ylab="Life Expectancy", 
     main="1960")
subset2013 <- subset(WorldBank, year==2013)
plot(x=subset2013$fertility.rate, y=subset2013$life.expectancy, pch=".", 
     col=WorldBank$region, xlab="Fertility Rate", ylab="Life Expectancy",
     main="2013")


# [4 pts]
# Make a histogram of GDP only for observations where the lending rating is "IDA"
# The width of the bars should be approximately 250 (use breaks to set how many bars)
# Add an x-axis label and a title.
par(mfrow=c(1, 1))
WorldBank$GDP <- WorldBank$GDP.per.capita.Current.USD * WorldBank$population
gdpSubset <- subset(WorldBank, lending=="IDA")$GDP
hist(gdpSubset, xlab="GDP", main="Histogram of GDP's of World Countries")


#################################################################
##### PART III : apply statements [15 pts]

# For the next few tasks you will use the list rain
# (list of length 5, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("rainfallCO.rda")

# [2 pts]
# Create [max.rain], a vector where each entry is the _maximum_ element of the
# corresponding vector in the list rain

max.rain <- sapply(X=rain, FUN=function(x) { return(max(x, na.rm=TRUE)) })

    
# [5 pts]
# Create [max.diff.rain], a vector of length 5 where each entry is the 
# __maximum absolute difference__ in rainfall on two consecutive days
# Hint: you can use the function [diff()] to get the difference between entry i and i+1 in a vector 
# and the function [abs()] for absolute value

max.diff.rain <- sapply(X=rain, FUN=function(x) { return(max(abs(diff(x)))) })

# [5 pts]
# Create [prop.rain], a vector of length 5 where each entry is the 
# number of rain days (i.e. rain > 0) as a function of total days

prop.rain <- sapply(X=rain, FUN=function(x) { return(length(x[x > 0]) / length(x)) })
  

# [3 pts]
# Make a plot with 6 panels and plot the histogram of the rainfall for each weather station
# in a separate panel (there will be one empty panel)
# use an apply statment to the the plotting
par(mfrow=c(2,3))
sapply(X=rain, FUN=function(x) { hist(x) })

#################################################################
##### PART IV : functions [20 pts]

# [6 pts]
# Write a function, [GenNorm()] which generates a vector of random normal
# plots a histogram, if desired.
# Input :
#   mean (the mean of the normal), default 0
#   sd (the standard deviation), default 1
#   n (the number of observations), default 1000
#   plot.hist (a logical variable, should we plot a histogram), default TRUE
# Output: 
#   a vector of n normal random variables with mean=mean and sd=sd
#   an optional plot
par(mfrow=c(1,1))
GenNorm <- function(mu, sigma, num, plot.hist=TRUE){
  normals <- rnorm(n=num, mean=mu, sd=sigma)
  if (plot.hist == TRUE) {
    hist(normals)
  }
  return(normals)
}

# TEST
# GENNORM(0, 1, 1000)
# works

# [6 pts]
# Write a function [standardizeVar] with
# Input :
#     [m] : a numeric matrix
#     [cols] : TRUE or FALSE (default TRUE)
# Output :
#     a matrix of the same size as [m] where if cols==TRUE the columns have been standardized
#     and if cols==FALSE the rows have been standardized
#     we standardize by subtracting the mean of the column/row and dividing by the sd of the column/row


standardizeVar <- function(m, cols=TRUE){
  newM <- matrix(rep(1, times=ncol(m) * nrow(m)), ncol=ncol(m), nrow=nrow(m))
  if (cols == TRUE){
    for (c in ncol(m)) {
      for (r in nrow(m)) {
        newM[r, c] <- ((m[r, c] - mean(m[, c])) / sd(m[, c]))
      }
    }
  } else {
    for (r in nrow(m)) {
      for (c in ncol(m)) {
        newM[r, c] <- ((m[r, c] - mean(m[r, ])) / sd(m[r, ]))
      }
    }    
  }
  return(newM)
}

# TESTS!
cool <- matrix(c(1,2, 3, 4, 5, 6), nrow=2)
standardizeVar(cool, cols=FALSE)

# [8 pts]
# Write a function [PermDiff()] that takes
# Inputs
#    [cases] : a numeric vector 
#    [controls] : a numeric vector 
#    [k] : a scalar, the number of permutations, default=5000

#    Assume that [cases] was of length k and [controls] of length m
#    Now select at random k individuals from the whole sample (i.e. all k+m observations)
#    and assign them to be cases, and the remaining m individuals as controls
#    Calculate the value [mean(cases)-mean(controls)] for each of these new samples

# Output
#    A vector of length k where each element is the value 
#    mean(cases)-mean(controls) for one permutation

PermDiff <- function(cases, controls, k=5000){
  pool <- cases + controls
  sumCases <- 0
  sumControls <- 0
  idcs <- sample(1:length(pool), size=k)
  for (i in 1:length(pool)) {
    if (i %in% idcs) {
      sumCases <- sumCases + pool[i]
    } else {
      sumControls <- sumControls + pool[i]
    }
  }
  return(sumCases/k - sumControls/(length(pool) - k))
}



#################################################################
##### PART V : simulations [15 pts]

# leave this here:
set.seed(123456)

# Simulate a simplified lottery and return the number of people who share the
# jackpot.

# Setup:
# Each player can pick 3 numbers from the numbers 1 through 19 (no duplicates)
# There are [k] players that buy a ticket.
# All player with the winning numbers share the jackpot

# Write a function NumJackpot with:

# Input
#    k : number of players
#    B : number of lottery games

# Output:
#    n.jackpot : vector of length B where each entry is the number of people
#                who shared the jackpot in that game

# So in each of the B games you have to :
#    generate the winning numbers
#    set a counter for jackpot winners to 0
#    generate a ticket for each player in turn, if they had the winning numbers 
#    increase the counter by 1

NumJackpot <- function(k, B){
  n.jackpot <- c()
  for (b in 1:B) {
    winningNumbers <- sort(sample(1:19, size=3, replace=FALSE))
    count <- 0
    for (p in 1:k) {
      thisTicket <- sort(sample(1:19, size=3, replace=FALSE))
      if (winningNumbers == thisTicket) {
        count <- count + 1
      }
    }
    n.jackpot <- c(n.jackpot, count)
  }
  return(n.jackpot)
}

# For B = 5000 and each value of k = 10000, 50000, 100000, 500000
# Plot a histogram of the output from NumJackpot (i.e. four histograms)
NumJackpot(100, 100)
par(mfrow=c(2,2))
for (k in c(100, 500, 1000, 5000)) {
  thisJackpotSim <- NumJackpot(k, 50)
  hist(thisJackpotSim)
}


#################################################################
##### PART VI : string manipulation and regular expressions [20 pts]

phrases <- c("stone", "steel", "sled", "Star", "silly", "cat", "dog", "catcat", "why?", "really!", "how much?", "atatat", "bar", "car")

# [2 pts]
# Create a vector [text1] that lists the elements in phrases that start
# with "st" (case doesn't matter)
text1 <- sapply(X=grep("?st", phrases, ignore.case=TRUE), FUN=function(x) { return(phrases[x]) })


# [1 pt]
# Create a vector [text2] that lists the elements in phrases that have 
# a match to "ar", _at the end of the phrase_ 
text2 <- sapply(X=grep("ar$", phrases, ignore.case=TRUE), FUN=function(x) { return(phrases[x]) })


# [2 pts]
# Create a vector [text3] that lists the elements in phrases that have 
# a match to any multiple of "ta"
text3 <- sapply(X=grep("(ta)", phrases, ignore.case=TRUE), FUN=function(x) { return(phrases[x]) })


# [2 pts]
# Create a vector [text4] that has the first 3 characters of each element in phrases
text4 <- c()
for (p in phrases) {
  text4 <- c(text4, substr(p, start=1, stop=3))
}

# [2 pts]
# Create a vector [text5] that has all the elements in phrases that have a punctuation mark 
text5 <- sapply(X=grep("[[:punct:]]", phrases, ignore.case=TRUE), FUN=function(x) { return(phrases[x]) })

# [2 pts]
# Create a vector [phrases2] where you have replaced the first instance of the letter "a" in each word with "@"
phrases2 <- gsub(pattern="a", replacement="@", phrases)


# [2 pts]
dna <- c("AGGATGATT", "AGCCTTAGC", "AGAGAGCT", "AGTTTCGTA", "CGTGGTGC", "CTAAGTGAC", "GTGGGACC", "GGTAGAGAC", "TAGATTACC")
# Create a vector [match1] with the index for all matches to "A*T" or "G*T"
match1 <- unique(c(grep("A(*){1}T", dna), grep("G(*){1}T", dna)))

# [2 pts]
# Create a vector [dna2] where you have removed all entries whose length is not a multiple of 3
dna2 <- c()
for (d in dna) {
  if (nchar(d) %% 3 == 0) {
    dna2 <- c(dna2, d)
  }
}


#################################################################

