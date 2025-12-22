library(tidyverse)
library(googlesheets4)
library(mvtnorm)
set.seed(147) # For reproducibility

# Load google sheets
pipeline_coatings_data <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1F8WZimxhyr1TPYQCu-AkS6s5GIuZJBKlvpNGuaazpVs/edit?usp=sharing"
)

# Select only the depth columns i.e columns 2 and 4
pipeline_depth_only <- pipeline_coatings_data[, c(2, 4)]
colnames(pipeline_depth_only)[c(1, 2)] <- c("Coating_1", "Coating_2")
pipeline_depth_only_full <- pipeline_depth_only

# Randomly delete 30% of coating 2 data to simulate missing values

num_rows <- nrow(pipeline_depth_only)
num_missing <- floor(0.3 * num_rows)
missing_indices <- sample(1:num_rows, num_missing)
pipeline_depth_only$Coating_2[missing_indices] <- NA

# Randomly delete 30% of coating 1 data to simulate missing values
num_rows <- nrow(pipeline_depth_only)
num_missing <- floor(0.3 * num_rows)
missing_indices <- sample(1:num_rows, num_missing)
pipeline_depth_only$Coating_1[missing_indices] <- NA

# Remove rows where both Coating_1 and Coating_2 are missing
pipeline_depth_only <- pipeline_depth_only %>%
  filter(!(is.na(Coating_1) & is.na(Coating_2)))

# Impute missing values in Coating_2 using the mean of Coating_2
mean_coating_2 <- mean(pipeline_depth_only$Coating_2, na.rm = TRUE)
pipeline_depth_only_meanImputed <- pipeline_depth_only
pipeline_depth_only_meanImputed$Coating_2[is.na(
  pipeline_depth_only$Coating_2
)] <- mean_coating_2

# Impute missing values in Coating_1 using the mean of Coating_1
mean_coating_1 <- mean(pipeline_depth_only$Coating_1, na.rm = TRUE)
pipeline_depth_only_meanImputed <- pipeline_depth_only
pipeline_depth_only_meanImputed$Coating_1[is.na(
  pipeline_depth_only$Coating_1
)] <- mean_coating_1

# conduct pairwise t-test on the mean imputed data
t_test_meanImputed <- t.test(
  pipeline_depth_only_meanImputed$Coating_1,
  pipeline_depth_only_meanImputed$Coating_2,
  paired = TRUE
)
t_test_meanImputed

# Impute missing values in Coating_2 using regression imputation using only the complete cases in pipeline_depth_only.
complete_cases <- pipeline_depth_only %>% drop_na()
regression_model <- lm(Coating_2 ~ Coating_1, data = complete_cases)
predicted_values <- predict(
  regression_model,
  newdata = pipeline_depth_only[is.na(pipeline_depth_only$Coating_2), ]
)
pipeline_depth_only_regressionImputed <- pipeline_depth_only
pipeline_depth_only_regressionImputed$Coating_2[is.na(
  pipeline_depth_only$Coating_2
)] <- predicted_values

# Impute missing values in Coating_1 using regression imputation using only the complete cases in pipeline_depth_only.
regression_model_2 <- lm(Coating_1 ~ Coating_2, data = complete_cases)
predicted_values_2 <- predict(
  regression_model_2,
  newdata = pipeline_depth_only[is.na(pipeline_depth_only$Coating_1), ]
)
pipeline_depth_only_regressionImputed$Coating_1[is.na(
  pipeline_depth_only$Coating_1
)] <- predicted_values_2


# conduct pairwise t-test on the regression imputed data
t_test_regressionImputed <- t.test(
  pipeline_depth_only_regressionImputed$Coating_1,
  pipeline_depth_only_regressionImputed$Coating_2,
  paired = TRUE
)
t_test_regressionImputed

# Using a multivariate normal model to impute missing values and then test for difference in the means.

# part (c)

# I will first use a Jeffreys prior and then a unit information prior.

# Using standard Jeffrey's prior for multivariate normal, we get a multivariate normal inverse Wishart posterior with parameters as shown in the teaching assistant's solution to Exercise 7.1

Y <- as.matrix(pipeline_depth_only) # Using Y so I can use the textbook's code.
p <- dim(Y)[2]
n <- nrow(Y)

# Starting values
O <- 1 * (!is.na(Y))
Y.full <- Y

for (j in 1:p) {
  Y.full[is.na(Y.full[, j]), j] <- mean(Y.full[, j], na.rm = TRUE) # Initial imputation with column means
}

THETA.jeffreys <- SIGMA.jeffreys <- Y.MISS.jeffreys <- NULL

# Gibbs sampler

for (s in 1:10000) {
  ybar <- apply(Y.full, 2, mean)
  S.jeffreys <- (t(Y.full) - c(ybar)) %*% t(t(Y.full) - c(ybar))
  ###sample Sigma given the data
  Sigma <- solve(rWishart(1, n, solve(S.jeffreys))[,, 1])
  ###

  ###sample theta given sigma and the data
  theta <- rmvnorm(1, ybar, Sigma / n)

  ###update missing data
  for (i in 1:n) {
    # print(i)
    b <- (O[i, ] == 0)
    # Skip if no missing values in this row
    if (!any(b)) {
      next
    }
    a <- (O[i, ] == 1)
    iSa <- solve(Sigma[a, a])
    beta.j <- Sigma[b, a] %*% iSa
    Sigma.j <- Sigma[b, b] - Sigma[b, a] %*% iSa %*% Sigma[a, b]
    theta.j <- theta[b] + beta.j %*% (t(Y.full[i, a]) - theta[a])
    Y.full[i, b] <- rmvnorm(1, theta.j, Sigma.j)
  }

  ### save results
  THETA.jeffreys <- rbind(THETA.jeffreys, theta)
  SIGMA.jeffreys <- rbind(SIGMA.jeffreys, c(Sigma))
  Y.MISS.jeffreys <- rbind(Y.MISS.jeffreys, Y.full[O == 0])
  ###
  # cat(s,round(theta,2),round(c(Sigma),2),"\n")
}

# 95% credible interval for the mean difference
mean.differences.jeffreys <- THETA.jeffreys[, 1] - THETA.jeffreys[, 2]
(quantile(mean.differences.jeffreys, probs = c(0.025, 0.975)))

# posterior mean difference
(mean(mean.differences.jeffreys))

# Using unit information prior for multivariate normal, we get a multivariate normal inverse Wishart posterior with parameters as shown in my solution to Exercise 7.2

Y <- as.matrix(pipeline_depth_only) # Using Y so I can use the textbook's code.
p <- dim(Y)[2]
n <- nrow(Y)

# Starting values
O <- 1 * (!is.na(Y))
Y.full <- Y

for (j in 1:p) {
  Y.full[is.na(Y.full[, j]), j] <- mean(Y.full[, j], na.rm = TRUE) # Initial imputation with column means
}

THETA.unitInfo <- SIGMA.unitInfo <- Y.MISS.unitInfo <- NULL

# Gibbs sampler

for (s in 1:10000) {
  ybar <- apply(Y.full, 2, mean)
  S.unitInfo <- (1 / n) * (t(Y.full) - c(ybar)) %*% t(t(Y.full) - c(ybar))
  ###sample Sigma given the data
  Sigma <- solve(rWishart(1, n + p + 1, (1 / (n + 1)) * solve(S.unitInfo))[,,
    1
  ])
  ###

  ###sample theta given sigma and the data
  theta <- rmvnorm(1, ybar, Sigma / (n + 1))

  ###update missing data
  for (i in 1:n) {
    b <- (O[i, ] == 0)
    # Skip if no missing values in this row
    if (!any(b)) {
      next
    }
    a <- (O[i, ] == 1)
    iSa <- solve(Sigma[a, a])
    beta.j <- Sigma[b, a] %*% iSa
    Sigma.j <- Sigma[b, b] - Sigma[b, a] %*% iSa %*% Sigma[a, b]
    theta.j <- theta[b] + beta.j %*% (t(Y.full[i, a]) - theta[a])
    Y.full[i, b] <- rmvnorm(1, theta.j, Sigma.j)
  }

  ### save results
  THETA.unitInfo <- rbind(THETA.unitInfo, theta)
  SIGMA.unitInfo <- rbind(SIGMA.unitInfo, c(Sigma))
  Y.MISS.unitInfo <- rbind(Y.MISS.unitInfo, Y.full[O == 0])
  ###
  # cat(s,round(theta,2),round(c(Sigma),2),"\n")
}

# 95% credible interval for the mean difference
mean.differences.unitInfo <- THETA.unitInfo[, 1] - THETA.unitInfo[, 2]
(quantile(mean.differences.unitInfo, probs = c(0.025, 0.975)))

# posterior mean difference
(mean(mean.differences.unitInfo))
