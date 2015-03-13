#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)
colors <- c("white", "blue", "red")

rotate <- function(x) t(apply(x, 2, rev))

bml.init <- function(r, c, p){
  random_ints <- sample(x = c(0:2), size = r * c, replace=TRUE, prob = c(1 - p, p/2, p/2))
  m <- matrix(random_ints, r, c)
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
r.step <- function(m) {
  r <- nrow(m)
  c <- ncol(m)
  i <- 1
  while (i <= r) {
    j <- 1
    while (j <= c) {
      if (j != c) {
        if (m[i, j] == 2 && (m[i, j + 1] == 0)) {
          m[i, j + 1] = m[i, j]
          m[i, j] = 0
          j <- j + 1
        } 
        j <- j + 1
      }
      else {
        if (m[i, j] == 2 && m[i, 1] == 0) {
          m[i, 1] = m[i, j]
          m[i, j] = 0
          j <- j + 1
        }        
        j <- j + 1
      }
    }
    i <- i + 1
  }
  return(m)
}

b.step <- function(m) {
  r <- nrow(m)
  c <- ncol(m)
  j <- c
  while (j > 0) {
    i <- r
    while (i > 0) {
      if (i == 1) {
        if (m[i, j] == 1 && m[r, j] == 0) {
          m[r, j] = m[i, j]
          m[i, j] = 0
        }  
        i <- i - 1
      }
      else {
        if (m[i, j] == 1 && (m[i - 1, j] == 0)) {
          m[i - 1, j] = m[i, j]
          m[i, j] = 0
          i <- i - 1
        } 
        i <- i - 1
      }
    }
    j <- j - 1
  }
  return(m)
}


bml.step <- function(m){
  # ON EVEN TIMESTEPS, MOVE BLUE NORTH. ON ODD, MOVE RED EAST
  # ASSUMES START WITH TIMESTEP = 0 (EVEN).
  m <- b.step(m)
  m <- r.step(m)
  return(m)
  #return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

# return number of steps taken before gridlock, or number of steps overall, if we don't hit a gridlock
# for example, let's say we iterate 1000 times. return 1000 if we haven't gridlocked at that point
# (LOL am i doing this right)

bml.sim <- function(r, c, p){
  m <- bml.init(r, c, p)
  count <- 0
  for (i in 1:1000) {
    new_m <- bml.step(m)
    if (check.m(m, new_m)) {
      count = count + 1
      break
    }
    m <- new_m
    count <- count + 1
  }
  return(count)
}

#### Function to check for matrix equality
# Assumes matrices have the same dimensions
## Input : two matrices
## Output : 1 if equal, 0 if not

check.m <- function(m1, m2) {
  r <- nrow(m1)
  c <- ncol(m1)
  is_empty_count <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      if (m1[i, j] != m2[i, j]) {
        return(FALSE)
      } else if (m1[i, j] == 0) {
        is_empty_count <- is_empty_count + 1
      }
    }
  }
  if (is_empty_count != r * c) {
    return(TRUE)
  }
  return(FALSE)
}

