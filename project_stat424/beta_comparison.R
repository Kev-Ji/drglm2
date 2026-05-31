library(dplyr)
library(gt)



mean_beta_glm <- function(result_list){
  
  beta_mat <- do.call(rbind, lapply(result_list, function(df){df$beta}))
  
  return(colMeans(beta_mat)) 
}

mean_beta <- function(result_list){
  
  beta_mat <- sapply(result_list, function(df){drop(df$beta)})
  return(rowMeans(beta_mat))
   
}









true_beta <- c(
  "(Intercept)" = 2,
  x1 = 0.75,
  x2 = -1.25,
  x3 = 0.50,
  x4 = 0.60,
  x5 = 1.45,
  x6 = -0.40,
  x7 = 1.95,
  x8 = 0.55,
  x9 = 1.10,
  x10 = -0.80
)

glm_beta <- mean_beta_glm(B1_logistic)

drglm_25_beta <- mean_beta(B4_logistic)
drglm_50_beta <- mean_beta(B3_logistic)
drglm_100_beta <- mean_beta(B2_logistic)

drglm_new_25_beta <- mean_beta(B7_logistic)
drglm_new_50_beta <- mean_beta(B6_logistic)
drglm_new_100_beta <- mean_beta(B5_logistic)

parameter_names <- c(
  beta_0 = "\u03b2\u2080 (Intercept)",
  beta_1 = "\u03b2\u2081",
  beta_2 = "\u03b2\u2082",
  beta_3 = "\u03b2\u2083",
  beta_4 = "\u03b2\u2084",
  beta_5 = "\u03b2\u2085",
  beta_6 = "\u03b2\u2086",
  beta_7 = "\u03b2\u2087",
  beta_8 = "\u03b2\u2088",
  beta_9 = "\u03b2\u2089",
  beta_10 = "\u03b2\u2081\u2080"
)


beta_table <- tibble(
  Parameter = parameter_names,
  `True Value` = true_beta,
  `CMLE: K = 1` = glm_beta,
  `D&R: K = 25` = drglm_25_beta,
  `D&R: K = 50` = drglm_50_beta,
  `D&R: K = 100` = drglm_100_beta,
  `D&R_new: K = 25` = drglm_new_25_beta,
  `D&R_new: K = 50` = drglm_new_50_beta,
  `D&R_new: K = 100` = drglm_new_100_beta
)

bias_row <- tibble(
  Parameter = "Mean Absolute Bias",
  `True Value` = NA_real_,
  `CMLE: K = 1` = mean(abs(glm_beta - true_beta)),
  `D&R: K = 25` = mean(abs(drglm_25_beta - true_beta)),
  `D&R: K = 50` = mean(abs(drglm_50_beta - true_beta)),
  `D&R: K = 100` = mean(abs(drglm_100_beta - true_beta)),
  `D&R_new: K = 25` = mean(abs(drglm_new_25_beta - true_beta)),
  `D&R_new: K = 50` = mean(abs(drglm_new_50_beta - true_beta)),
  `D&R_new: K = 100` = mean(abs(drglm_new_100_beta - true_beta))
)

final_beta_table <- bind_rows(beta_table, bias_row)

final_beta_table


final_beta_table %>%
  gt() %>%
  tab_header(
    title = md("**Comparison of true and estimated parameters using Monte Carlo simulation**"),
    subtitle = md("Logistic regression model with *n* = 5 million observations???averaged over 500 replications")
  ) %>%
  fmt_number(
    columns = -Parameter,
    decimals = 4
  ) %>%
  sub_missing(
    columns = `True Value`,
    missing_text = "-"
  ) %>%
  tab_spanner(
    label = "Mean Estimated Value",
    columns = c(
      `CMLE: K = 1`,
      `D&R: K = 25`,
      `D&R: K = 50`,
      `D&R: K = 100`,
      `D&R_new: K = 25`,
      `D&R_new: K = 50`,
      `D&R_new: K = 100`
    )
  ) %>%
  tab_spanner(
    label = "D&R",
    columns = c(`D&R: K = 25`, `D&R: K = 50`, `D&R: K = 100`)
  ) %>%
  tab_spanner(
    label = "D&R_new",
    columns = c(`D&R_new: K = 25`, `D&R_new: K = 50`, `D&R_new: K = 100`)
  ) %>%
  cols_label(
    `CMLE: K = 1` = md("*K* = 1"),
    `D&R: K = 25` = md("*K* = 25"),
    `D&R: K = 50` = md("*K* = 50"),
    `D&R: K = 100` = md("*K* = 100"),
    `D&R_new: K = 25` = md("*K* = 25"),
    `D&R_new: K = 50` = md("*K* = 50"),
    `D&R_new: K = 100` = md("*K* = 100")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Parameter == "Mean Absolute Bias"
    )
  )


