library(tidyverse)
library(mvtnorm)
set.seed(147) # For reproducibility

# Load xls file where the delimiter is the semi colon and the location is "G:\My Drive\rao@ualberta.ca 2022-12-08 10 58\shishir@tamu.edu\My Drive\Interesting papers\Survival Models\GitHub\Survival\Survival-Analysis\Blog 12\Data\concrete+compressive+strength\Concrete_Data.xls"

concrete_data <- readxl::read_excel(
  "G:\\My Drive\\rao@ualberta.ca 2022-12-08 10 58\\shishir@tamu.edu\\My Drive\\Interesting papers\\Survival Models\\GitHub\\Survival\\Survival-Analysis\\Blog 12\\Data\\concrete+compressive+strength\\Concrete_Data.xls"
)
colnames(concrete_data)
concrete_data_subset <- concrete_data[
  71:139,
  c("Age (day)", "Concrete compressive strength(MPa, megapascals)")
]
colnames(concrete_data_subset) <- c("Age", "Strength")

# Construct a new dataset such that we have one column for every unique Age value in concrete_data_subset which gives us the strength of concrete at that age.

concrete_data_wide <- concrete_data_subset |>
  group_by(Age) |>
  mutate(row_id = row_number()) |>
  ungroup() |>
  pivot_wider(
    names_from = Age,
    values_from = Strength,
    names_prefix = "Age_"
  ) |>
  select(-row_id)

# Plot histogram of all three columns on the same plot.
concrete_data_long <- concrete_data_wide |>
  pivot_longer(
    cols = everything(),
    names_to = "Age",
    values_to = "Strength"
  )
ggplot(concrete_data_long, aes(x = Strength, fill = Age)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(
    title = "Histogram of Concrete Strength at Different Ages",
    x = "Concrete Strength (MPa)",
    y = "Frequency"
  ) +
  theme_minimal()


# Randomly delete 30% of data in each column to simulate missing values

concrete_data_missing <- concrete_data_wide
num_rows <- nrow(concrete_data_missing)
for (col in colnames(concrete_data_missing)) {
  num_missing <- floor(0.3 * num_rows)
  missing_indices <- sample(1:num_rows, num_missing)
  concrete_data_missing[missing_indices, col] <- NA
}

# Compare the means of Age 3 vs Age 7.

concrete_data_missing_3_7 <- concrete_data_missing |>
  select(Age_3, Age_7)

# Remove rows where both Age_3 and Age_7 are missing
concrete_data_missing_3_7 <- concrete_data_missing_3_7 |>
  filter(!(is.na(Age_3) & is.na(Age_7)))

# Impute missing values in Age_7 using regression imputation using only the complete cases in concrete_data_missing_3_7.
complete_cases_3_7 <- concrete_data_missing_3_7 |> drop_na()
regression_model_3_7 <- lm(Age_7 ~ Age_3, data = complete_cases_3_7)
predicted_values_3_7 <- predict(
  regression_model_3_7,
  newdata = concrete_data_missing_3_7[is.na(concrete_data_missing_3_7$Age_7), ]
)
concrete_data_regressionImputed_3_7 <- concrete_data_missing_3_7
concrete_data_regressionImputed_3_7$Age_7[is.na(
  concrete_data_regressionImputed_3_7$Age_7
)] <- predicted_values_3_7

# Impute missing values in Age_3 using regression imputation using only the complete cases in concrete_data_missing_3_7.
regression_model_2_3_7 <- lm(Age_3 ~ Age_7, data = complete_cases_3_7)
predicted_values_2_3_7 <- predict(
  regression_model_2_3_7,
  newdata = concrete_data_missing_3_7[is.na(concrete_data_missing_3_7$Age_3), ]
)
concrete_data_regressionImputed_3_7$Age_3[is.na(
  concrete_data_regressionImputed_3_7$Age_3
)] <- predicted_values_2_3_7

# conduct pairwise t-test on the regression imputed data
t_test_regressionImputed_3_7 <- t.test(
  concrete_data_regressionImputed_3_7$Age_7,
  concrete_data_regressionImputed_3_7$Age_3,
  paired = TRUE
)
t_test_regressionImputed_3_7

# Using a multivariate normal model to impute missing values and then test for difference in the means.

# part (c)

# I will first use a Jeffreys prior and then a unit information prior.

# Using standard Jeffrey's prior for multivariate normal, we get a multivariate normal inverse Wishart posterior with parameters as shown in the teaching assistant's solution to Exercise 7.1

Y <- as.matrix(concrete_data_missing_3_7) # Using Y so I can use the textbook's code.
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
mean.differences.jeffreys <- THETA.jeffreys[, 2] - THETA.jeffreys[, 1]
(quantile(mean.differences.jeffreys, probs = c(0.025, 0.975)))

# posterior mean difference
(mean(mean.differences.jeffreys))

# Using unit information prior for multivariate normal, we get a multivariate normal inverse Wishart posterior with parameters as shown in my solution to Exercise 7.2

Y <- as.matrix(concrete_data_missing_3_7) # Using Y so I can use the textbook's code.
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
mean.differences.unitInfo <- THETA.unitInfo[, 2] - THETA.unitInfo[, 1]
(quantile(mean.differences.unitInfo, probs = c(0.025, 0.975)))

# posterior mean difference
(mean(mean.differences.unitInfo))

# Writing a function that will take two ages as input (from the values 3,7,28) and return the t-test results, credible intervals and posterior mean differences for both priors, using the above code.

analyze_concrete_strength <- function(age1, age2) {
  col1 <- paste0("Age_", age1)
  col2 <- paste0("Age_", age2)

  concrete_data_missing_subset <- concrete_data_missing |>
    select(all_of(c(col1, col2)))

  # Remove rows where both columns are missing
  concrete_data_missing_subset <- concrete_data_missing_subset |>
    filter(!(is.na(.data[[col1]]) & is.na(.data[[col2]])))

  # Regression imputation
  complete_cases <- concrete_data_missing_subset |> drop_na()

  regression_model_1 <- lm(
    as.formula(paste(col2, "~", col1)),
    data = complete_cases
  )
  predicted_values_1 <- predict(
    regression_model_1,
    newdata = concrete_data_missing_subset[
      is.na(concrete_data_missing_subset[[col2]]),
    ]
  )

  regression_model_2 <- lm(
    as.formula(paste(col1, "~", col2)),
    data = complete_cases
  )
  predicted_values_2 <- predict(
    regression_model_2,
    newdata = concrete_data_missing_subset[
      is.na(concrete_data_missing_subset[[col1]]),
    ]
  )

  concrete_data_regressionImputed <- concrete_data_missing_subset
  concrete_data_regressionImputed[[col2]][is.na(
    concrete_data_regressionImputed[[col2]]
  )] <- predicted_values_1
  concrete_data_regressionImputed[[col1]][is.na(
    concrete_data_regressionImputed[[col1]]
  )] <- predicted_values_2

  # t-test
  t_test_result <- t.test(
    concrete_data_regressionImputed[[col2]],
    concrete_data_regressionImputed[[col1]],
    paired = TRUE
  )

  # Multivariate normal imputation with Jeffreys prior
  Y <- as.matrix(concrete_data_missing_subset)
  p <- dim(Y)[2]
  n <- nrow(Y)

  O <- 1 * (!is.na(Y))
  Y.full <- Y

  for (j in 1:p) {
    Y.full[is.na(Y.full[, j]), j] <- mean(Y.full[, j], na.rm = TRUE)
  }

  THETA.jeffreys <- NULL

  for (s in 1:10000) {
    ybar <- apply(Y.full, 2, mean)
    S.jeffreys <- (t(Y.full) - c(ybar)) %*% t(t(Y.full) - c(ybar))
    Sigma <- solve(rWishart(1, n, solve(S.jeffreys))[,, 1])
    theta <- rmvnorm(1, ybar, Sigma / n)
    for (i in 1:n) {
      b <- (O[i, ] == 0)
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
    THETA.jeffreys <- rbind(THETA.jeffreys, theta)
  }
  mean.differences.jeffreys <- THETA.jeffreys[, 2] - THETA.jeffreys[, 1]
  credible_interval_jeffreys <- quantile(
    mean.differences.jeffreys,
    probs = c(0.025, 0.975)
  )
  posterior_mean_jeffreys <- mean(mean.differences.jeffreys)

  # Multivariate normal imputation with Unit Information prior
  Y <- as.matrix(concrete_data_missing_subset)
  p <- dim(Y)[2]
  n <- nrow(Y)

  O <- 1 * (!is.na(Y))
  Y.full <- Y
  for (j in 1:p) {
    Y.full[is.na(Y.full[, j]), j] <- mean(Y.full[, j], na.rm = TRUE)
  }
  THETA.unitInfo <- NULL
  for (s in 1:10000) {
    ybar <- apply(Y.full, 2, mean)
    S.unitInfo <- (1 / n) * (t(Y.full) - c(ybar)) %*% t(t(Y.full) - c(ybar))
    Sigma <- solve(rWishart(1, n + p + 1, (1 / (n + 1)) * solve(S.unitInfo))[,,
      1
    ])
    theta <- rmvnorm(1, ybar, Sigma / (n + 1))
    for (i in 1:n) {
      b <- (O[i, ] == 0)
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
    THETA.unitInfo <- rbind(THETA.unitInfo, theta)
  }
  mean.differences.unitInfo <- THETA.unitInfo[, 2] - THETA.unitInfo[, 1]
  credible_interval_unitInfo <- quantile(
    mean.differences.unitInfo,
    probs = c(0.025, 0.975)
  )
  posterior_mean_unitInfo <- mean(mean.differences.unitInfo)

  # Probability of mean difference being greater than 0 for both priors
  prob_greater_0_jeffreys <- mean(mean.differences.jeffreys > 0)
  prob_greater_0_unitInfo <- mean(mean.differences.unitInfo > 0)

  # Plot three intervals on the same plot
  intervals_df <- data.frame(
    Method = c("t-test", "Jeffreys Prior", "Unit Info Prior"),
    Lower = c(
      t_test_result$conf.int[1],
      credible_interval_jeffreys[1],
      credible_interval_unitInfo[1]
    ),
    Upper = c(
      t_test_result$conf.int[2],
      credible_interval_jeffreys[2],
      credible_interval_unitInfo[2]
    )
  )

  # Set factor levels to control X-axis order
  intervals_df$Method <- factor(
    intervals_df$Method,
    levels = c("t-test", "Jeffreys Prior", "Unit Info Prior")
  )

  p <- ggplot(intervals_df, aes(x = Method)) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    geom_point(aes(y = (Lower + Upper) / 2), size = 3) +
    labs(
      title = paste(
        "95% Intervals for Mean Difference between Age",
        age2,
        "and Age",
        age1
      ),
      y = "Mean Difference",
      x = "Method"
    ) +
    theme_minimal()

  return(list(
    t_test = t_test_result,
    credible_interval_jeffreys = credible_interval_jeffreys,
    posterior_mean_jeffreys = posterior_mean_jeffreys,
    credible_interval_unitInfo = credible_interval_unitInfo,
    posterior_mean_unitInfo = posterior_mean_unitInfo,
    plot = p,
    prob_greater_0_jeffreys = prob_greater_0_jeffreys,
    prob_greater_0_unitInfo = prob_greater_0_unitInfo
  ))
}

# Example usage:
result_3_7 <- analyze_concrete_strength(3, 7)

# Get the t-test 95% confidence interval
result_3_7$t_test$conf.int

# Get the Jeffreys prior 95% credible interval
result_3_7$credible_interval_jeffreys
# Get the unit information prior 95% credible interval
result_3_7$credible_interval_unitInfo

# Display the plot
print(result_3_7$plot)

# Get the probability that the mean difference is greater than 0
result_3_7$prob_greater_0_jeffreys
result_3_7$prob_greater_0_unitInfo

result_7_28 <- analyze_concrete_strength(7, 28)
print(result_7_28$plot)

## Modify the function to include mare features. See below.

analyze_concrete_strength <- function(
  dataset,
  alternative = "two.sided",
  threshold = 0,
  alpha = 0.05
) {
  # Use first two columns
  col1 <- colnames(dataset)[1]
  col2 <- colnames(dataset)[2]

  concrete_data_missing_subset <- dataset |>
    select(all_of(c(col1, col2)))

  # Remove rows where both columns are missing
  concrete_data_missing_subset <- concrete_data_missing_subset |>
    filter(!(is.na(.data[[col1]]) & is.na(.data[[col2]])))

  # Regression imputation
  complete_cases <- concrete_data_missing_subset |> drop_na()

  regression_model_1 <- lm(
    as.formula(paste(col2, "~", col1)),
    data = complete_cases
  )
  predicted_values_1 <- predict(
    regression_model_1,
    newdata = concrete_data_missing_subset[
      is.na(concrete_data_missing_subset[[col2]]),
    ]
  )

  regression_model_2 <- lm(
    as.formula(paste(col1, "~", col2)),
    data = complete_cases
  )
  predicted_values_2 <- predict(
    regression_model_2,
    newdata = concrete_data_missing_subset[
      is.na(concrete_data_missing_subset[[col1]]),
    ]
  )

  concrete_data_regressionImputed <- concrete_data_missing_subset
  concrete_data_regressionImputed[[col2]][is.na(
    concrete_data_regressionImputed[[col2]]
  )] <- predicted_values_1
  concrete_data_regressionImputed[[col1]][is.na(
    concrete_data_regressionImputed[[col1]]
  )] <- predicted_values_2

  # t-test with specified confidence level
  conf_level <- 1 - alpha
  t_test_result <- t.test(
    concrete_data_regressionImputed[[col2]],
    concrete_data_regressionImputed[[col1]],
    paired = TRUE,
    alternative = alternative,
    mu = threshold,
    conf.level = conf_level
  )

  # Multivariate normal imputation with Jeffreys prior
  Y <- as.matrix(concrete_data_missing_subset)
  p <- dim(Y)[2]
  n <- nrow(Y)

  O <- 1 * (!is.na(Y))
  Y.full <- Y

  for (j in 1:p) {
    Y.full[is.na(Y.full[, j]), j] <- mean(Y.full[, j], na.rm = TRUE)
  }

  THETA.jeffreys <- SIGMA.jeffreys <- NULL

  for (s in 1:10000) {
    ybar <- apply(Y.full, 2, mean)
    S.jeffreys <- (t(Y.full) - c(ybar)) %*% t(t(Y.full) - c(ybar))
    Sigma <- solve(rWishart(1, n, solve(S.jeffreys))[,, 1])
    theta <- rmvnorm(1, ybar, Sigma / n)
    for (i in 1:n) {
      b <- (O[i, ] == 0)
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
    THETA.jeffreys <- rbind(THETA.jeffreys, theta)
    SIGMA.jeffreys <- rbind(SIGMA.jeffreys, c(Sigma))
  }

  mean.differences.jeffreys <- THETA.jeffreys[, 2] - THETA.jeffreys[, 1]

  # Calculate credible intervals based on alternative
  if (alternative == "greater") {
    credible_interval_jeffreys <- c(
      quantile(mean.differences.jeffreys, probs = alpha),
      Inf
    )
  } else if (alternative == "less") {
    credible_interval_jeffreys <- c(
      -Inf,
      quantile(mean.differences.jeffreys, probs = 1 - alpha)
    )
  } else {
    credible_interval_jeffreys <- quantile(
      mean.differences.jeffreys,
      probs = c(alpha / 2, 1 - alpha / 2)
    )
  }

  posterior_mean_jeffreys <- mean(mean.differences.jeffreys)
  posterior_median_jeffreys <- median(mean.differences.jeffreys)

  # Multivariate normal imputation with Unit Information prior
  Y <- as.matrix(concrete_data_missing_subset)
  p <- dim(Y)[2]
  n <- nrow(Y)

  O <- 1 * (!is.na(Y))
  Y.full <- Y
  for (j in 1:p) {
    Y.full[is.na(Y.full[, j]), j] <- mean(Y.full[, j], na.rm = TRUE)
  }
  THETA.unitInfo <- SIGMA.unitInfo <- NULL
  for (s in 1:10000) {
    ybar <- apply(Y.full, 2, mean)
    S.unitInfo <- (1 / n) * (t(Y.full) - c(ybar)) %*% t(t(Y.full) - c(ybar))
    Sigma <- solve(rWishart(1, n + p + 1, (1 / (n + 1)) * solve(S.unitInfo))[,,
      1
    ])
    theta <- rmvnorm(1, ybar, Sigma / (n + 1))
    for (i in 1:n) {
      b <- (O[i, ] == 0)
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
    THETA.unitInfo <- rbind(THETA.unitInfo, theta)
    SIGMA.unitInfo <- rbind(SIGMA.unitInfo, c(Sigma))
  }
  mean.differences.unitInfo <- THETA.unitInfo[, 2] - THETA.unitInfo[, 1]

  # Calculate credible intervals based on alternative
  if (alternative == "greater") {
    credible_interval_unitInfo <- c(
      quantile(mean.differences.unitInfo, probs = alpha),
      Inf
    )
  } else if (alternative == "less") {
    credible_interval_unitInfo <- c(
      -Inf,
      quantile(mean.differences.unitInfo, probs = 1 - alpha)
    )
  } else {
    credible_interval_unitInfo <- quantile(
      mean.differences.unitInfo,
      probs = c(alpha / 2, 1 - alpha / 2)
    )
  }

  posterior_mean_unitInfo <- mean(mean.differences.unitInfo)
  posterior_median_unitInfo <- median(mean.differences.unitInfo)

  # Calculate probabilities based on alternative hypothesis
  if (alternative == "greater") {
    prob_threshold_jeffreys <- mean(mean.differences.jeffreys > threshold)
    prob_threshold_unitInfo <- mean(mean.differences.unitInfo > threshold)
    prob_threshold_ttest <- 1 - t_test_result$p.value
  } else if (alternative == "less") {
    prob_threshold_jeffreys <- mean(mean.differences.jeffreys < threshold)
    prob_threshold_unitInfo <- mean(mean.differences.unitInfo < threshold)
    prob_threshold_ttest <- 1 - t_test_result$p.value
  } else {
    # For two-sided, calculate probability of being greater than threshold
    prob_threshold_jeffreys <- mean(mean.differences.jeffreys > threshold)
    prob_threshold_unitInfo <- mean(mean.differences.unitInfo > threshold)
    # For t-test two-sided, this doesn't have a direct probability interpretation
    prob_threshold_ttest <- NA
  }

  # Predictive probability for next observation
  # Generate predictive samples from posterior
  predictive_diff_jeffreys <- numeric(10000)
  predictive_diff_unitInfo <- numeric(10000)

  for (s in 1:10000) {
    # Jeffreys prior predictive
    Sigma_jeff <- matrix(SIGMA.jeffreys[s, ], nrow = 2, ncol = 2)
    new_obs_jeff <- rmvnorm(1, THETA.jeffreys[s, ], Sigma_jeff)
    predictive_diff_jeffreys[s] <- new_obs_jeff[2] - new_obs_jeff[1]

    # Unit Info prior predictive
    Sigma_unit <- matrix(SIGMA.unitInfo[s, ], nrow = 2, ncol = 2)
    new_obs_unit <- rmvnorm(1, THETA.unitInfo[s, ], Sigma_unit)
    predictive_diff_unitInfo[s] <- new_obs_unit[2] - new_obs_unit[1]
  }

  # Calculate predictive probabilities based on alternative
  if (alternative == "greater") {
    pred_prob_jeffreys <- mean(predictive_diff_jeffreys > threshold)
    pred_prob_unitInfo <- mean(predictive_diff_unitInfo > threshold)
  } else if (alternative == "less") {
    pred_prob_jeffreys <- mean(predictive_diff_jeffreys < threshold)
    pred_prob_unitInfo <- mean(predictive_diff_unitInfo < threshold)
  } else {
    # For two-sided, calculate probability of being greater than threshold
    pred_prob_jeffreys <- mean(predictive_diff_jeffreys > threshold)
    pred_prob_unitInfo <- mean(predictive_diff_unitInfo > threshold)
  }

  # Create plot based on alternative hypothesis
  if (alternative == "two.sided") {
    # Two-sided: show intervals with median as point
    intervals_df <- data.frame(
      Method = c("t-test", "Jeffreys Prior", "Unit Info Prior"),
      Lower = c(
        t_test_result$conf.int[1],
        credible_interval_jeffreys[1],
        credible_interval_unitInfo[1]
      ),
      Upper = c(
        t_test_result$conf.int[2],
        credible_interval_jeffreys[2],
        credible_interval_unitInfo[2]
      ),
      Point = c(
        mean(
          concrete_data_regressionImputed[[col2]] -
            concrete_data_regressionImputed[[col1]]
        ),
        posterior_median_jeffreys,
        posterior_median_unitInfo
      )
    )

    # Set factor levels to control X-axis order
    intervals_df$Method <- factor(
      intervals_df$Method,
      levels = c("t-test", "Jeffreys Prior", "Unit Info Prior")
    )

    p <- ggplot(intervals_df, aes(x = Method)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
      geom_point(aes(y = Point), size = 3) +
      geom_text(
        aes(y = Lower, label = round(Lower, 2)),
        vjust = 1.5,
        size = 3
      ) +
      geom_text(
        aes(y = Upper, label = round(Upper, 2)),
        vjust = -0.5,
        size = 3
      ) +
      geom_text(
        aes(y = Point, label = round(Point, 2)),
        hjust = -0.5,
        size = 3
      ) +
      geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
      labs(
        title = paste0(
          round((1 - alpha) * 100, 1),
          "% Intervals for Mean Difference between Group 2 and Group 1"
        ),
        y = "Mean Difference",
        x = "Method"
      ) +
      theme_minimal()
  } else {
    # One-sided: show bounds with triangles
    if (alternative == "greater") {
      bounds_df <- data.frame(
        Method = c("t-test", "Jeffreys Prior", "Unit Info Prior"),
        Point = c(
          mean(
            concrete_data_regressionImputed[[col2]] -
              concrete_data_regressionImputed[[col1]]
          ),
          posterior_mean_jeffreys,
          posterior_mean_unitInfo
        ),
        Bound = c(
          t_test_result$conf.int[1],
          credible_interval_jeffreys[1],
          credible_interval_unitInfo[1]
        )
      )
      bound_label <- "Lower Bound"
    } else {
      bounds_df <- data.frame(
        Method = c("t-test", "Jeffreys Prior", "Unit Info Prior"),
        Point = c(
          mean(
            concrete_data_regressionImputed[[col2]] -
              concrete_data_regressionImputed[[col1]]
          ),
          posterior_mean_jeffreys,
          posterior_mean_unitInfo
        ),
        Bound = c(
          t_test_result$conf.int[2],
          credible_interval_jeffreys[2],
          credible_interval_unitInfo[2]
        )
      )
      bound_label <- "Upper Bound"
    }

    # Set factor levels to control X-axis order
    bounds_df$Method <- factor(
      bounds_df$Method,
      levels = c("t-test", "Jeffreys Prior", "Unit Info Prior")
    )

    p <- ggplot(bounds_df, aes(x = Method)) +
      geom_point(aes(y = Bound), shape = 17, size = 4, color = "blue") +
      # geom_point(aes(y = Point), size = 3) +
      geom_text(
        aes(y = Bound, label = round(Bound, 2)),
        vjust = -1,
        size = 3,
        color = "blue"
      ) +
      # geom_text(aes(y = Point, label = round(Point, 2)),
      #           hjust = -0.5, size = 3) +
      geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
      labs(
        title = paste0(
          "Mean Difference between Group 2 and Group 1\n(",
          round((1 - alpha) * 100, 1),
          "% ",
          bound_label,
          " shown as triangle)"
        ),
        y = "Mean Difference",
        x = "Method"
      ) +
      theme_minimal()
  }

  return(list(
    t_test = t_test_result,
    credible_interval_jeffreys = credible_interval_jeffreys,
    posterior_mean_jeffreys = posterior_mean_jeffreys,
    posterior_median_jeffreys = posterior_median_jeffreys,
    credible_interval_unitInfo = credible_interval_unitInfo,
    posterior_mean_unitInfo = posterior_mean_unitInfo,
    posterior_median_unitInfo = posterior_median_unitInfo,
    plot = p,
    prob_threshold_jeffreys = prob_threshold_jeffreys,
    prob_threshold_unitInfo = prob_threshold_unitInfo,
    prob_threshold_ttest = prob_threshold_ttest,
    predictive_prob_jeffreys = pred_prob_jeffreys,
    predictive_prob_unitInfo = pred_prob_unitInfo,
    regressionImputed = concrete_data_regressionImputed
  ))
}

# Example usage:

result_3_7_greater <- analyze_concrete_strength(
  concrete_data_missing |>
    select(Age_3, Age_7),
  alternative = "greater",
  threshold = 10,
  alpha = 0.05
)

print(result_3_7_greater$plot)
result_3_7_greater$t_test

result_3_7_greater$credible_interval_jeffreys
result_3_7_greater$credible_interval_unitInfo

result_3_7_greater$prob_threshold_jeffreys
result_3_7_greater$prob_threshold_unitInfo

result_3_7_greater$predictive_prob_jeffreys
result_3_7_greater$predictive_prob_unitInfo
