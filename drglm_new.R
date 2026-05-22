drglm_new <- function(formula,family,df,k,fitfunction, shuffle){
  
  result <- list()
  class(result) <- "drglm"
  result$call <- match.call()
  result$formula <- formula
  result$family <- family
  
  
  ### shuffle data
  if(shuffle){
    df <- df[sample(nrow(df)), ]
  }
  
  result$data <- df
  

  if(family == 'binomial' & fitfunction == "glm"){ 
    
    ### use sequential partition to split data into k subsets
    
    label <- cut(seq(nrow(df)), breaks = k, label = F)
    index <- split(seq(nrow(df)), label)
    
    ### fit logistics model for each subset
    
    model <- lapply(index, function(i) {
      glm(formula, data = df[i, ], family = binomial())
    })
    
    
    ### H = X^T W X, equivalent to sum_i p_i(1-p_i) x_i x_i^T
    
    get_H <- function(model){
      p <- fitted(model) 
      X <- model.matrix(model)
      return( crossprod(X, p*(1-p)*X ) )
    }
  
    H <- lapply(model, get_H)
    
    # Beta's of models
    
    b <- lapply(model,function(m){as.matrix(coef(m))})
    
    ### Find overall beta
    
    HE <- Reduce('+', H)
    IH <- solve(HE)
      
    HB <- lapply(1:k, function(i){H[[i]] %*% b[[i]] })
    
    # Sum the elements of the list
    HB <- Reduce('+', HB)
    
    B <-  (IH%*%HB)
    OR  <-  exp(B)
    se <- sqrt(diag(IH))
    
    z_value <- as.vector(B) / se
    p_value <- 2 * (1 - pnorm(abs(z_value)))
    
    alpha <- 0.05
    z <- qnorm(1 - alpha / 2)
    
    lower <- as.vector(B) - z * se
    upper <- as.vector(B) + z * se
    
    #creating a data frame with four columns
    
    table <- data.frame("Estimate"= as.vector(B),
                        "Odds Ratio"=as.vector(OR),
                        "standard error"=se,
                        "z value"= z_value,
                        "Pr(>|z|)"=p_value,
                        "95% CI" = paste("[", round(lower, 2), ",", round(upper, 2), "]"),
                        check.names = FALSE)
    
    result$coefficients <- B
    result$Estimates <- table
    result$models <- model
    result$df.residual <- nrow(df) - nrow(B)
    
  }
  
  
  else if (family == "binomial" & fitfunction == "speedglm") {
    
    ### use sequential partition to split data into k subsets
    label <- cut(seq(nrow(df)), breaks = k, label = F)
    index <- split(seq(nrow(df)), label)
    
    ### fit logistic model for each subset using speedglm
    model <- lapply(index, function(i) {
      speedglm::speedglm(formula, data = df[i, ], family = binomial())
    })
    
    ### Find H matrix, H = X^T W X, where W = p(1-p)
    get_H <- function(model) {
      p <- fitted(model)
      X <- model.matrix(model)
      return(crossprod(X, p * (1 - p) * X))
    }
    
    H <- lapply(model, get_H)
    
    ### Betas of models
    b <- lapply(model, function(m) {
      as.matrix(coef(m))
    })
    
    ### Find overall beta. Formula can be found in the paper. 
    HE <- Reduce("+", H)
    IH <- solve(HE)
    
    HB <- lapply(1:k, function(i) {
      H[[i]] %*% b[[i]]
    })
    
    HB <- Reduce("+", HB)
    
    B <- IH %*% HB
    OR <- exp(B)
    se <- sqrt(diag(IH))
    
    z_value <- as.vector(B) / se
    p_value <- 2 * (1 - pnorm(abs(z_value)))
    
    alpha <- 0.05
    z <- qnorm(1 - alpha / 2)
    
    lower <- as.vector(B) - z * se
    upper <- as.vector(B) + z * se
    
    table <- data.frame(
      "Estimate" = as.vector(B),
      "Odds Ratio" = as.vector(OR),
      "standard error" = se,
      "z value" = z_value,
      "Pr(>|z|)" = p_value,
      "95% CI" = paste("[", round(lower, 2), ",", round(upper, 2), "]"),
      check.names = FALSE
    )
    
    result$coefficients <- B
    result$Estimates <- table
    result$models <- model
    result$df.residual <- nrow(df) - nrow(B)
  }
  
  else {
    stop("Unsupported family or fitfunction")
  }
  
  
  class(result) <- "drglm"
  return(result)
  
}


### Rest of code are same.


print.drglm <- function(x, ...) {
  cat("Generalized Linear Model in Divide-and-Recombine Approach\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nFamily:", x$family, "\n")
  cat("\nCoefficients:\n")
  print(x$Estimates)
  invisible(x)
}


drglm_residuals <- function(model, type = "response") {
  B <- as.matrix(model$Estimates[, "Estimate"])
  mf <- model.frame(model$formula, data = model$data)
  y <- model.response(mf)
  X <- model.matrix(model$formula, data = model$data)
  eta <- as.vector(X %*% B)
  mu <- switch(model$family,
               "gaussian" = eta,
               "binomial" = 1 / (1 + exp(-eta)),
               "poisson" = exp(eta),
               stop("Unsupported family for residuals"))
  if (type == "response") {
    return(y - mu)
  } else if (type == "deviance") {
    res <- switch(model$family,
                  "gaussian" = y - mu,
                  "binomial" = {
                    eps <- .Machine$double.eps
                    y <- pmin(pmax(y, eps), 1 - eps)
                    mu <- pmin(pmax(mu, eps), 1 - eps)
                    sign(y - mu) * sqrt(2 * (y * log(y / mu) + (1 - y) * log((1 - y) / (1 - mu))))
                  },
                  "poisson" = {
                    eps <- .Machine$double.eps
                    y <- pmax(y, eps)
                    mu <- pmax(mu, eps)
                    sign(y - mu) * sqrt(2 * (y * log(y / mu) - (y - mu)))
                  },
                  stop("Unsupported family for deviance residuals"))
    return(res)
  } else if (type == "pearson") {
    var_fun <- switch(model$family,
                      "gaussian" = rep(1, length(mu)),
                      "binomial" = mu * (1 - mu),
                      "poisson" = mu,
                      stop("Unsupported family for Pearson residuals"))
    return((y - mu) / sqrt(var_fun))
  } else {
    stop("Unsupported residual type. Use 'response', 'deviance', or 'pearson'.")
  }
}



print.drglm <- function(x, ...) {
  cat("Generalized Linear Model in Divide-and-Recombine Approach\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nFamily:", x$family, "\n")
  cat("\nCoefficients:\n")
  print(x$Estimates)
  invisible(x)
}


drglm_residuals <- function(model, type = "response") {
  B <- as.matrix(model$Estimates[, "Estimate"])
  mf <- model.frame(model$formula, data = model$data)
  y <- model.response(mf)
  X <- model.matrix(model$formula, data = model$data)
  eta <- as.vector(X %*% B)
  mu <- switch(model$family,
               "gaussian" = eta,
               "binomial" = 1 / (1 + exp(-eta)),
               "poisson" = exp(eta),
               stop("Unsupported family for residuals"))
  if (type == "response") {
    return(y - mu)
  } else if (type == "deviance") {
    res <- switch(model$family,
                  "gaussian" = y - mu,
                  "binomial" = {
                    eps <- .Machine$double.eps
                    y <- pmin(pmax(y, eps), 1 - eps)
                    mu <- pmin(pmax(mu, eps), 1 - eps)
                    sign(y - mu) * sqrt(2 * (y * log(y / mu) + (1 - y) * log((1 - y) / (1 - mu))))
                  },
                  "poisson" = {
                    eps <- .Machine$double.eps
                    y <- pmax(y, eps)
                    mu <- pmax(mu, eps)
                    sign(y - mu) * sqrt(2 * (y * log(y / mu) - (y - mu)))
                  },
                  stop("Unsupported family for deviance residuals"))
    return(res)
  } else if (type == "pearson") {
    var_fun <- switch(model$family,
                      "gaussian" = rep(1, length(mu)),
                      "binomial" = mu * (1 - mu),
                      "poisson" = mu,
                      stop("Unsupported family for Pearson residuals"))
    return((y - mu) / sqrt(var_fun))
  } else {
    stop("Unsupported residual type. Use 'response', 'deviance', or 'pearson'.")
  }
}




