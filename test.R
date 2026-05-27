

library(bench)

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



result <- bench::mark(
  glm = {
    glm(form, data = df, family = binomial())
  },
  
  author_drglm = {
    drglm(form, family = "binomial", data = df, k = 10, fitfunction = 'glm')
  },
  
  new_drglm = {
    drglm_new(form, family = "binomial", df = df, k = 10, fitfunction = 'glm', shuffle = F)
  },
  
  iterations= 10,
  
  check = FALSE 
  
  
)

result[, c("expression", "median", "mem_alloc")]

