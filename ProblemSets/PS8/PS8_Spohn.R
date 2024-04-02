library(nloptr)
library(modelsummary)

set.seed(100)

# X matrix

N <- 100000
K <- 10

# Generate normally distributed random numbers
X <- matrix(rnorm(N * (K - 1)), nrow = N)

# Add a column of 1's as the first column
X <- cbind(1, X)

# eps vector
# Define the standard deviation
sigma <- 0.5

# Generate the vector eps
eps <- rnorm(N, mean = 0, sd = sigma)

# beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Creating Y
Y <- X %*% beta + eps


# Computing Bols

# Compute X transpose
X_transpose <- t(X)

# Compute X transpose times X
X_transpose_X <- t(X) %*% X

# Compute the inverse of X transpose times X
X_transpose_X_inverse <- solve(X_transpose_X)

# Compute X transpose times Y
X_transpose_Y <- t(X) %*% Y

# Compute beta hat OLS
beta_hat_OLS <- X_transpose_X_inverse %*% X_transpose_Y

# Print the OLS estimates
print(beta_hat_OLS)


# Calculating gradient descent method

# Set up a step size
learning_rate <- 0.0000003

# Set up a number of iterations
max_iter <- 1000

# Initialize theta to zeros
theta <- rep(0, ncol(X))

# Gradient descent loop
for (iter in 1:max_iter) {
  # Compute predictions
  predictions <- X %*% theta
  
  # Compute MSE
  mse <- mean((Y - predictions)^2)
  
  # Compute gradient of MSE with respect to theta
  gradient <- -2/N * t(X) %*% (Y - predictions)
  
  # Update theta using gradient descent
  theta <- theta - learning_rate * gradient
  
  # Print MSE for monitoring convergence
  cat("Iteration:", iter, "MSE:", mse, "\n")
  
  # Check for convergence
  if (sum(abs(gradient)) < 1e-6) {
    cat("Converged after", iter, "iterations\n")
    break
  }
}

# Print the estimated theta
print(theta)


# Bmle using nloptrs L-BFGS algorithm

log_likelihood <- function(theta, Y, X) {
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  residuals <- Y - X %*% beta
  N <- length(Y)
  log_likelihood <- -0.5 * (N * log(2 * pi * sig^2) + sum(residuals^2) / sig^2)
  return(-log_likelihood)  # Negative sign for maximization
}

# Define the gradient function
gradient <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- length(Y) / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

# Initial parameter values
initial_theta <- c(rep(0, ncol(X)), 1)  # Assuming initial standard deviation = 1

# Define lower and upper bounds (if needed)
lower_bounds <- c(rep(-Inf, ncol(X)), 0)
upper_bounds <- rep(Inf, length(initial_theta))

# Perform optimization using nloptr
opt <- nloptr(x0 = initial_theta, eval_f = log_likelihood, eval_grad_f = gradient,
              lb = lower_bounds, ub = upper_bounds, Y = Y, X = X,
              opts = list(algorithm = "NLOPT_LN_BOBYQA"))

# Extract the estimated parameters
estimated_theta <- opt$solution


# Computing Bols the easy way

OLS = lm(Y ~ X -1)
modelsummary(OLS, stars = TRUE)