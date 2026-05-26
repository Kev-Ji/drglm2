drglm_new <- function(formula, family, df, k = 10,
                      fitfunction = "glm",
                      shuffle = TRUE) {
  
  result <- list()
  class(result) <- "drglm"
  result$call <- match.call()
  result$formula <- formula
  result$family <- family
  
  if (shuffle) {
    df <- df[sample.int(nrow(df)), , drop = FALSE]
  }
  
  if (family != "binomial") {
    stop("Currently only binomial family is supported.")
  }
  
  if (!(fitfunction %in% c("glm", "speedglm"))) {
    stop("fitfunction must be either 'glm' or 'speedglm'.")
  }
  
  n <- nrow(df)
  
  ### sequential dividing dataframe into indices
  
  label <- cut(seq_len(n), breaks = k, labels = FALSE)
  index <- split(seq_len(n), label)
  

  
  if (fitfunction == "glm") {
    
    mf <- model.frame(formula, data = df)
    y <- model.response(mf)
    X <- model.matrix(formula, data = mf)
    
    p <- ncol(X)
    par_names <- colnames(X)
    
    H <- vector("list", length(index))
    V <- vector("list", length(index))
    b <- vector("list", length(index))
    
    fam <- binomial()
    
    for (j in seq_along(index)) {
      
      idx <- index[[j]]
      
      fit_j <- glm.fit(
        x = X[idx, , drop = FALSE],
        y = y[idx],
        family = fam
      )
      
      b[[j]] <- as.matrix(fit_j$coefficients)
      rownames(b[[j]]) <- par_names
      
      R_j <- qr.R(fit_j$qr)
      
      H[[j]] <- crossprod(R_j)
      V[[j]] <- chol2inv(R_j) ## compute for covariance matrix
    }
  }
  

  
  if (fitfunction == "speedglm") {
    
    model <- lapply(index, function(i) {
      speedglm::speedglm(
        formula,
        data = df[i, , drop = FALSE],
        family = binomial()
      )
    })
    
    V <- lapply(model, vcov)
    
    H <- lapply(V, function(v) {
      solve(as.matrix(v))
    })
    
    b <- lapply(model, function(m) {
      as.matrix(coef(m))
    })
    
    par_names <- names(coef(model[[1]]))
    p <- length(par_names)
  }
  

  ### sum H_j
  HE <- Reduce("+", H)
  
  ## sum H_j * beta_j
  
  HB <- Reduce(
    "+",
    lapply(seq_along(H), function(i) {
      H[[i]] %*% b[[i]]
    })
  )
  
  B <- solve(HE, HB)
  rownames(B) <- par_names
  
  OR <- exp(B)
  

  
  v <- lapply(V, function(vcov_i) {
    as.matrix(diag(vcov_i)) / k
  })
  
  v_com <- Reduce("+", v) / k
  
  se <- as.vector(sqrt(v_com))
  

  
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
  
  rownames(table) <- par_names
  
  result$coefficients <- B
  result$Estimates <- table
  result$df.residual <- n - p
  
  class(result) <- "drglm"
  return(result)
}