library(bench)
library(dplyr)

set.seed(123)

nobs <- 1000000

x1 <- runif(nobs)
x2 <- runif(nobs)
x3 <- runif(nobs)
x4 <- runif(nobs)
x5 <- runif(nobs)
x6 <- runif(nobs)
x7 <- runif(nobs)
x8 <- runif(nobs)
x9 <- runif(nobs)
x10 <- runif(nobs)

true_beta <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)

X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
eta <- X %*% true_beta
p <- 1 / (1 + exp(-eta))

y <- rbinom(nobs, 1, p)

df <- data.frame(
  y, x1, x2, x3, x4, x5,
  x6, x7, x8, x9, x10
)

form <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10



test_result <- function(df, formula, k, fitfunction){
  
  result <- list()
  
  time_summary <- bench::mark(
    glm = {
      glm(formula, data = df, family = binomial())
    },
    
    author_drglm = {
      drglm(formula, family = "binomial", data = df, k = k, fitfunction = fitfunction)
    },
    
    new_drglm = {
      drglm_new(formula, family = "binomial", df = df, k = k, fitfunction = fitfunction, shuffle = F)
    },
    
    iterations= 10,
    
    check = FALSE 
    
  )
  
  result$performance <- time_summary
  

  
  fit_glm <- glm(formula, data = df, family = binomial())
  
  fit_author_drglm <- drglm(formula, family = "binomial", data = df, k = k, fitfunction = fitfunction)
  
  fit_new_drglm <- drglm_new(formula, family = "binomial", df = df, k = k, fitfunction = fitfunction , shuffle = F)
  
  comparison_table <- tibble(
    parameter = names(coef(fit_glm)),
    glm_estimates = as.numeric(coef(fit_glm)),
    author_drglm_estimate  = as.numeric(fit_author_drglm$Estimates[, "Estimate"]),
    new_drglm = as.numeric(fit_new_drglm$Estimates[, "Estimate"]),
    glm_se = sqrt(diag(vcov(fit_glm))),
    author_drglm_se = as.numeric(fit_author_drglm$Estimates[, "standard error"]),
    new_drglm_se = as.numeric(fit_new_drglm$Estimates[, "standard error"])
    
  )
  
  result$Estimates <- comparison_table
  
  
  return (result)
  
}

result <- test_result(df, form, 10, 'glm')

result$performance[, c('expression', 'median', 'total_time', 'mem_alloc')]

result$Estimates













