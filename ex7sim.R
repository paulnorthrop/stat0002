# The function ex7sim()  
# 1. simulates nsim samples of size n from a U(0, theta) distribution
# 2. calculates the observed value of estimators T1 and T2
# 3. returns a 2-column matrix, where column 1 contains nsim observed values of
#    T1 and column 2 contains nsim observed values of T2
ex7sim <- function(n = 10, nsim = 10000, theta = 4) {
  # A function to perform the simulation once
  estfn <- function() {
    # Simulate a sample of size n from a U(0, theta) distribution
    simdata <- runif(n = n, max = theta)
    # Calculate the estimates
    t1 <- 2 * mean(simdata)
    t2 <- ((n + 1) / n) * max(simdata)
    return(c(t1, t2))
  }
  # Repeat the simulation nsim times 
  # Use the function t() to transpose the 2 by nsim matrix that is returned
  estimates <- t(replicate(nsim, estfn()))
  colnames(estimates) <- c("T1", "T2")
  return(estimates)
}

# Call the function ex7sim() provided in the to do the simulation
# Set a random number seed so that everyone simulated the same values
# The seed is the deadline for submitting Exercises 7: 1/12/2022
set.seed(1122022)
simvals <- ex7sim()

# Look at the first 5 rows of simvals
head(simvals)