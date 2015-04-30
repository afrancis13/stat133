###########################################################
#### Stat 133 Midterm 4

# leave this here:
set.seed(123456)


#### Simulation
# Write a function, [dice_sum()], that takes as input:
# [k] : the number of dice rolled
# [B] : the number of rolls
# and returns:
# [dsum] : a vector of length B where each element is the sum of a roll of k dice

# So if k=1 pick a number between 1 and 6 at random B times and return them,
# if k=2 then in each roll you pick twice a number between 1 and 6 at random,
# calculate their sum, do this B times and return
# and so on.

# We've set the default inputs to k=2 and B=100

dice_sum <- function(k=2, B=100){
  sum_vec <- c()
  for (i in 1:B) {
    this_roll <- sample(x=1:6, size=k)
    sum_roll <- sum(this_roll)
    sum_vec <- c(sum_vec, sum_roll)
  }
  return(sum_vec)
}

#### String manipulation

phrases <- c("dog", "doggy", "den", "good boy", "Really?", "How much?", "Only $8", "dogdogdog", "Oh god")

# Create a vector [text1] that lists the elements in phrases 
# where the SECOND TO LAST character is "o" (lower case o).

idcs <- grep("o", phrases)
text1 <- c()
for (i in idcs) {
  secondToLast <- substr(x=phrases[i], start=nchar(phrases[i])-1, stop=nchar(phrases[i])-1)
  print(secondToLast)
  if (secondToLast == "o") {
    text1 <- c(text1, phrases[i])
  }
}

# > text1
# [1] "dog"       "good boy"  "dogdogdog" "Oh god"   

# Create a vector [text2] that lists the elements in phrases that
# START with the letter "d"
idcs <- grep("\\<d", phrases)
text2 <- c()
for (i in idcs) {
  text2 <- c(text2, phrases[i])
}

# > text2
# [1] "dog"       "doggy"     "den"       "dogdogdog"

# Create a variable [no.punct] that equals the number of phrases with a punctuation mark in it.
no.punct <- length(grep("[[:punct:]]", phrases))

# Create a vector [even] that is of length 1000 and has the entries
# "even2", "even4", ...
# with no separation between the word and the letter

even <- paste("even", seq(from=2, to=1000, by=2), sep='')

# Start with [hotelCal] which is a character string, create 
# a _vector_ (not list) [hotelCal.split] which 
# stores the words of [hotelCal] each as a separate element.
# Also, convert all upper case letters to lower case.
# Please remove all punctuation marks.

hotelCal <- "On a dark desert highway, cool wind in my hair. Warm smell of colitas, rising up through the air. Up ahead in the distance, I saw a shimmering light. My head grew heavy and my sight grew dim I had to stop for the night.  There she stood in the doorway; I heard the mission bell.  And I was thinking to myself: 'This could be heaven or this could be hell'. Then she lit up a candle and she showed me the way."

noPunctLower <- tolower(gsub(pattern="[[:punct:]]", replacement="", x=hotelCal))
hotelCal.split <- strsplit(noPunctLower, " ")

# Write a function called updateDate. Your function should take the following
# arguments
#   <dates>: a character vector of dates of the form "month, year" (e.g. "May, 2001")
#   <old.yr>: a string indicating the year for which elements will be updated
#     (e.g. "2002")
#
# and return the following
#   <updated.dates>: a character vector of dates where <old.yr> has been replaced by
#     '2015'. This vector should only contain the elements whose date has been
#     updated. For example updateDate(c('May, 2008', 'June, 2011'), '2008') should
#     return 'May, 2015'.
updateDate <- function(dates, old.yr) {
  old.yr.idcs <- grep(old.yr, dates)
  new.dates <- gsub(old.yr, '2015', dates[old.yr.idcs])
  return(new.dates)
}

# TEST
# updateDate(c('May, 2008', 'June, 2011'), '2008') 
# May, 2015

# Write a function called [abbreviate] that takes in a vector of strings and returns
# a vector of the same length with only the first [k] characters from the orignal vector entries.

abbreviate <- function(vector, k){
  abbv <- c()
  for (i in 1:length(vector)) {
    abbv <- c(abbv, substr(vector[i], start=0, stop=k))
  }
  return(abbv)
}

# TEST:
# > vector <- c("win", "coolest")
# > abbreviate(vector, 2)
# [1] "wi" "co"
