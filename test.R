#library(tictoc)
#library(pryr)
library(dplyr)
#library(bench)
library(peakRAM)




# DRGLM 100


set.seed(123)

mc_simulation <- function() {

  nobs <- 5000000

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

  b <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)

  X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  eta <- X %*% b

  inv.logit <- function(p) {
    exp(p) / (1 + exp(p))
  }

  y <- rbinom(nobs, 1, inv.logit(eta))

  data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

  ram_result <- peakRAM({
    model_drglm <- drglm(
      y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
      family = "binomial",
      data = data,
      k = 100,
      fitfunction = "glm"
    )
  })
  
  time <- ram_result$Elapsed_Time_sec
  memory <- ram_result$Peak_RAM_Used_MiB / 1024

  pr <- sum(drglm_residuals(model_drglm, type = "pearson")^2)
  prdisp <- pr / model_drglm$df.residual
  beta <- model_drglm$coefficients
  se <- model_drglm$Estimates[, "standard error"]

  list(
    beta = beta,
    se = se,
    prdisp = prdisp,
    time = time,
    memory = memory
  )
}

B2_logistic <- replicate(500, mc_simulation(), simplify = FALSE)
# save(B2_logistic, file = "B2_logistic.RData")
# 


# DRGLM 50


set.seed(123)

mc_simulation <- function() {

  nobs <- 5000000

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

  b <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)

  X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  eta <- X %*% b

  inv.logit <- function(p) {
    exp(p) / (1 + exp(p))
  }

  y <- rbinom(nobs, 1, inv.logit(eta))

  data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

  ram_result <- peakRAM({
    model_drglm <- drglm(
      y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
      family = "binomial",
      data = data,
      k = 50,
      fitfunction = "glm"
    )
  })
  
  time <- ram_result$Elapsed_Time_sec
  memory <- ram_result$Peak_RAM_Used_MiB / 1024

  pr <- sum(drglm_residuals(model_drglm, type = "pearson")^2)
  prdisp <- pr / model_drglm$df.residual
  beta <- model_drglm$coefficients
  se <- model_drglm$Estimates[, "standard error"]

  list(
    beta = beta,
    se = se,
    prdisp = prdisp,
    time = time,
    memory = memory
  )
}

B3_logistic <- replicate(500, mc_simulation(), simplify = FALSE)
#save(B3_logistic, file = "B3_logistic.RData")
# 


# DRGLM 25


set.seed(123)

mc_simulation <- function() {

  nobs <- 5000000

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

  b <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)

  X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  eta <- X %*% b

  inv.logit <- function(p) {
    exp(p) / (1 + exp(p))
  }

  y <- rbinom(nobs, 1, inv.logit(eta))

  data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

  ram_result <- peakRAM({
    model_drglm <- drglm(
      y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
      family = "binomial",
      data = data,
      k = 25,
      fitfunction = "glm"
    )
  })
  
  time <- ram_result$Elapsed_Time_sec
  memory <- ram_result$Peak_RAM_Used_MiB / 1024

  pr <- sum(drglm_residuals(model_drglm, type = "pearson")^2)
  prdisp <- pr / model_drglm$df.residual
  beta <- model_drglm$coefficients
  se <- model_drglm$Estimates[, "standard error"]

  list(
    beta = beta,
    se = se,
    prdisp = prdisp,
    time = time,
    memory = memory
  )
}

B4_logistic <- replicate(500, mc_simulation(), simplify = FALSE)
# save(B4_logistic, file = "B4_logistic.RData")
# 


# DRGLM_new 100


set.seed(123)

mc_simulation <- function() {
  
  nobs <- 5000000
  
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
  
  b <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)
  
  X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  eta <- X %*% b
  
  inv.logit <- function(p) {
    exp(p) / (1 + exp(p))
  }
  
  y <- rbinom(nobs, 1, inv.logit(eta))
  
  data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  
  ram_result <- peakRAM({
    model_drglm <- drglm_new(
      y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
      family = "binomial",
      df = data,
      k = 100,
      fitfunction = "glm",
      shuffle = FALSE
    )
  })
  
  time <- ram_result$Elapsed_Time_sec
  memory <- ram_result$Peak_RAM_Used_MiB / 1024
  
  
  pr <- sum(drglm_residuals(model_drglm, type = "pearson")^2)
  prdisp <- pr / model_drglm$df.residual
  beta <- model_drglm$coefficients
  se <- model_drglm$Estimates[, "standard error"]
  
  list(
    beta = beta,
    se = se,
    prdisp = prdisp,
    time = time,
    memory = memory
  )
}

B5_logistic <- replicate(500, mc_simulation(), simplify = FALSE)
#save(B5_logistic, file = "B5_logistic.RData")

 
# DRGLM_new 50


set.seed(123)

mc_simulation <- function() {
  
  nobs <- 5000000
  
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
  
  b <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)
  
  X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  eta <- X %*% b
  
  inv.logit <- function(p) {
    exp(p) / (1 + exp(p))
  }
  
  y <- rbinom(nobs, 1, inv.logit(eta))
  
  data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  
  ram_result <- peakRAM({
    model_drglm <- drglm_new(
      y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
      family = "binomial",
      df = data,
      k = 50,
      fitfunction = "glm",
      shuffle = FALSE
    )
  })
  
  time <- ram_result$Elapsed_Time_sec
  memory <- ram_result$Peak_RAM_Used_MiB / 1024
  
  pr <- sum(drglm_residuals(model_drglm, type = "pearson")^2)
  prdisp <- pr / model_drglm$df.residual
  beta <- model_drglm$coefficients
  se <- model_drglm$Estimates[, "standard error"]
  
  list(
    beta = beta,
    se = se,
    prdisp = prdisp,
    time = time,
    memory = memory
  )
}

B6_logistic <- replicate(500, mc_simulation(), simplify = FALSE)
#save(B6_logistic, file = "B6_logistic.RData")


# DRGLM_new 25


set.seed(123)

mc_simulation <- function() {
  
  nobs <- 5000000
  
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
  
  b <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)
  
  X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  eta <- X %*% b
  
  inv.logit <- function(p) {
    exp(p) / (1 + exp(p))
  }
  
  y <- rbinom(nobs, 1, inv.logit(eta))
  
  data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  
  ram_result <- peakRAM({
    model_drglm <- drglm_new(
      y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
      family = "binomial",
      df = data,
      k = 25,
      fitfunction = "glm",
      shuffle = FALSE
    )
  })
  
  time <- ram_result$Elapsed_Time_sec
  memory <- ram_result$Peak_RAM_Used_MiB / 1024
  
  pr <- sum(drglm_residuals(model_drglm, type = "pearson")^2)
  prdisp <- pr / model_drglm$df.residual
  beta <- model_drglm$coefficients
  se <- model_drglm$Estimates[, "standard error"]
  
  list(
    beta = beta,
    se = se,
    prdisp = prdisp,
    time = time,
    memory = memory
  )
}

B7_logistic <- replicate(500, mc_simulation(), simplify = FALSE)
#save(B7_logistic, file = "B7_logistic.RData")



# GLM


set.seed(123)

mc_simulation <- function() {
  
  nobs <- 5000000
  
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
  
  b <- c(2, .75, -1.25, .5, .6, 1.45, -.4, 1.95, .55, 1.10, -.80)
  
  X <- cbind(1, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  eta <- X %*% b
  
  inv.logit <- function(p) {
    exp(p) / (1 + exp(p))
  }
  
  y <- rbinom(nobs, 1, inv.logit(eta))
  
  ram_result <- peakRAM({
    model_glm <- glm(
      y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
      family = binomial
    )
  })
  
  time <- ram_result$Elapsed_Time_sec
  memory <- ram_result$Peak_RAM_Used_MiB / 1024
  
  pr <- sum(residuals(model_glm, type = "pearson")^2)
  prdisp <- pr / model_glm$df.residual
  beta <- model_glm$coefficients
  se <- sqrt(diag(vcov(model_glm)))
  
  list(
    beta = beta,
    se = se,
    prdisp = prdisp,
    time = time,
    memory = memory
  )
}


B1_logistic <- replicate(500, mc_simulation(), simplify = FALSE)
#save(B1_logistic, file = "B1_logistic.RData")
 

k_25_time <- mean(unlist(lapply(B7_logistic, function(df){df$time})))

k_25_memory <- mean(unlist(lapply(B7_logistic, function(df){df$memory})))

k_50_time <- mean(unlist(lapply(B6_logistic, function(df){df$time})))

k_50_memory <- mean(unlist(lapply(B6_logistic, function(df){df$memory})))

k_100_time <- mean(unlist(lapply(B5_logistic, function(df){df$time})))

k_100_memory <- mean(unlist(lapply(B5_logistic, function(df){df$memory})))

glm_time <- mean(unlist(lapply(B1_logistic, function(df){df$time})))
glm_memory <- mean(unlist(lapply(B1_logistic, function(df){df$memory})))

drglm_50 <- mean(unlist(lapply(B3_logistic, function(df){df$time})))
drglm_50_memory <- mean(unlist(lapply(B3_logistic, function(df){df$memory})))



memory <- tibble(Peak_memory = c(round(glm_memory,3),
                                 round(k_25_memory,3), 
                                 round(k_50_memory,3),
                                 round(drglm_50_memory,3),
                                 round(k_100_memory,3)))

time_result <- tibble(Method = c('glm', 'drglm_new_k_25', 'drglm_new_k_50', 'drglm_50', 'drglm_new_k_100'),
                      Time = c(glm_time, k_25_time, k_50_time,drglm_50, k_100_time),
                      Peak_memory = c(paste(round(glm_memory,3),'G' ),
                                      paste(round(k_25_memory,3),'G' ), 
                                      paste(round(k_50_memory,3),'G' ),
                                      paste(round(drglm_50_memory,3),'G' ),
                                      paste(round(k_100_memory,3),'G' )))
base_time <- as.numeric(time_result[1, 2])
base_memory <- as.numeric(memory[1, 1])

time_result %>% 
  mutate(Time_change = paste(round((Time- base_time)*100/base_time,3), '%'),
         Memory_change = paste(round((memory$Peak_memory- base_memory)*100/base_memory,3), '%')) 

as.numeric(time_result$Peak_memory)
