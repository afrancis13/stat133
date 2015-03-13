#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

# We're going to find the percentage of grids that experience a gridlock within the first
# 1000 iterations, based on different criterion. 
#     1. We're going to adjust the horizontal and vertical coordinates
#             a. r = 3, c = 3
#             b. r = 5, c = 3
#             c. r = 3, c = 5
#             c. r = 5, c = 5
#             d. r = 10, c = 10
#             e. r = 30, c = 30
#     2. We're going to adjust the density of the grid for each of those r-c combinations, 
#        incrementing p by 0.05, from 0.05 to 0.95

# We're going to run 1000 trials on each of these combinations (1000 * 6 * 18 = 96000 simulations).
# The resulting GRIDLOCK PERCENTAGES of these simulations for each combination will be stored in a
# table of results that connects r-c combinations and density with percentages. We will work on 
# visualizing the results after running the simulations.

rows <- c(3, 5, 3, 5, 10, 30)
cols <- c(3, 3, 5, 5, 10, 30)
densities <- seq(0.05, 0.95, by=0.05)

rc_combination <- c()
for (i in 1:6) {
  r <- rows[i]
  c <- cols[i]
  for (p in densities) {
    count <- 0
    for (iter in 1:100) {
      if (bml.sim(r, c, p) != 1000) {
        count <- count + 1
      }
    } 
    rc_combination <- c(rc_combination, c(count / 100))
  }
}

# We're going to find the percentage of grids that experience a gridlock within the first
# 1000 iterations, based on different criterion. 
#     1. We're going to adjust the horizontal and vertical coordinates
#             a. r = 3, c = 3
#             b. r = 5, c = 3
#             c. r = 3, c = 5
#             c. r = 5, c = 5
#             d. r = 10, c = 10
#             e. r = 30, c = 30
#     2. We're going to adjust the density of the grid for each of those r-c combinations, 
#        incrementing p by 0.05, from 0.05 to 0.95

counts_rc_combination <- c()
for (i in 1:6) {
  r <- rows[i]
  c <- cols[i]
  for (p in densities) {
    non_gridlock_count <- 0
    steps_until_gridlock_count <- 0
    for (iter in 1:100) {
      this_sim <- bml.sim(r, c, p) 
      if (this_sim != 1000) {
        steps_until_gridlock_count <- steps_until_gridlock_count + this_sim
        non_gridlock_count <- non_gridlock_count + 1
      }
    } 
    counts_rc_combination <- c(counts_rc_combination, c(steps_until_gridlock_count / non_gridlock_count))
  }
}

pdf(file = "bml_figures.pdf")
mat <- matrix(rc_combination, ncol = 19, byrow=TRUE)
rownames(mat) <- c("3 x 3", "5 x 3", "3 x 5", "5 x 5", "10 x 10", "30 x 30")
colnames(mat) <- densities

box_percents <- boxplot(mat, cex.names = 0.5, las = 2,                      
                        main="BMI Simulation - % of Simulations that Experience Gridlock",
                        xlab="Density of Traffic(p)", 
                        ylab="Gridlock %")

final_table <- as.table(mat)
save(final_table, file="final_table")

ggplot(data.frame(final_table), aes(x=Var2, fill=Var1, y=Freq)) + 
geom_bar(stat = 'identity', position = 'dodge') + 
theme(legend.title=element_blank()) +
labs(title="BMI Simulation - % of Simulations that Experience Gridlock", x="Density of Traffic (p)", y="% Gridlock")

mat_counts <- matrix(counts_rc_combination, ncol = 19, byrow=TRUE)
rownames(mat_counts) <- c("3 x 3", "5 x 3", "3 x 5", "5 x 5", "10 x 10", "30 x 30")
colnames(mat_counts) <- densities

box_counts <- boxplot(mat_counts, cex.names = 0.5, las = 2,
                      main="BMI Simulation - Average Steps Before Gridlock",
                      xlab="Density of Traffic(p)", 
                      ylab="Avg. Steps")

final_table_counts <- as.table(mat_counts)
save(final_table_counts, file="final_table_counts")

ggplot(data.frame(final_table_counts), aes(x=Var2, fill=Var1, y=Freq)) + 
geom_bar(stat = 'identity', position = 'dodge') +
theme(legend.title=element_blank()) +
labs(title="BMI Simulation - Average Steps Before Gridlock", x="Density of Traffic (p)", y="Average Number of Steps Before Gridlock")

dev.off()

