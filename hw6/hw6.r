# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p) {
  vector.doctors <- c()
  this.doctors <- initial.doctors
  for (i in 1:n.days) {
    this.sample <- sample(1:n.doctors, size=2)
    doctor1 <- this.doctors[this.sample[1]]
    doctor2 <- this.doctors[this.sample[2]]
    if ((doctor1 == 0 && doctor2 == 1) || (doctor1 == 1 && doctor2 == 0)) {
      random.number <- runif(1)
      if (random.number < p) {
        this.doctors[this.sample[1]] = 1
        this.doctors[this.sample[2]] = 1
      }      
    }
    vector.doctors <- c(vector.doctors, this.doctors)
  }
  mat <- matrix(vector.doctors, nrow=n.doctors)
  return(mat)

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output

}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
test.ndoctors <- 30
test.ndays <- 100
test.pvec <- c(0.05, 0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.90, 0.95)
test.initial <- sample(0:1, size=test.ndoctors, replace=TRUE, prob=c(0.9, 0.1))

colors = rainbow(9)
return.matrix <- sim.doctors(test.initial, test.ndoctors, test.ndays, test.pvec[1])
counts <- c()
for (i in 1:test.ndays) {
  tabulate <- table(return.matrix[, i])
  counts <- c(counts, c(tabulate[names(tabulate) == 1]))
}
plot(x=1:test.ndays, y=counts, col=colors[1], type="l",
     main="Drug Adaptation Simulation", xlab="Day", 
     ylab="Number of Doctors that Endorse the Drug",
     xlim = c(0, 120), ylim = c(0, 30))

for (i in 2:9) {
  return.matrix <- sim.doctors(test.initial, test.ndoctors, test.ndays, test.pvec[i])
  counts <- c()
  for (j in 1:test.ndays) {
    tabulate <- table(return.matrix[, j])
    counts <- c(counts, c(tabulate[names(tabulate) == 1]))
  }
  lines(x=1:test.ndays, y=counts, col=colors[i], type="l", 
        main="Drug Adaptation Simulation", xlab="Day",
        ylab="Number of Doctors that Endorse the Drug",
        xlim = c(0, 100), ylim = c(0, 30))
  legend("topright", fill = rainbow(9), legend = c("p = 0.05", "p = 0.10", "p = 0.25", "p = 0.4", "p = 0.5", "p = 0.6", "p = 0.75", "p = 0.90", "p = 0.95"))
}
