library(dplyr)
library(gt)

mean_time <- function(result_list) {
  mean(sapply(result_list, function(df) df$time))
}

# Mean computation times
glm_time <- mean_time(B1_logistic)

drglm_25_time <- mean_time(B4_logistic)
drglm_50_time <- mean_time(B3_logistic)
drglm_100_time <- mean_time(B2_logistic)

drglm_new_25_time <- mean_time(B7_logistic)
drglm_new_50_time <- mean_time(B6_logistic)
drglm_new_100_time <- mean_time(B5_logistic)

time_table <- tibble(
  Method = c(
    "CMLE",
    "D&R",
    "D&R_new",
    "Time Reduction (%)",
    "Time Reduction_new (%)",
    "Speedup Factor",
    "Speedup Factor_new"
  ),
  `K = 1` = c(
    glm_time,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  ),
  `K = 25` = c(
    NA,
    drglm_25_time,
    drglm_new_25_time,
    (glm_time - drglm_25_time) / glm_time * 100,
    (glm_time - drglm_new_25_time) / glm_time * 100,
    glm_time / drglm_25_time,
    glm_time / drglm_new_25_time
  ),
  `K = 50` = c(
    NA,
    drglm_50_time,
    drglm_new_50_time,
    (glm_time - drglm_50_time) / glm_time * 100,
    (glm_time - drglm_new_50_time) / glm_time * 100,
    glm_time / drglm_50_time,
    glm_time / drglm_new_50_time
  ),
  `K = 100` = c(
    NA,
    drglm_100_time,
    drglm_new_100_time,
    (glm_time - drglm_100_time) / glm_time * 100,
    (glm_time - drglm_new_100_time) / glm_time * 100,
    glm_time / drglm_100_time,
    glm_time / drglm_new_100_time
  )
)



time_table_display <- time_table %>%
  mutate(
    across(
      -Method,
      ~ case_when(
        Method %in% c("Speedup Factor", "Speedup Factor_new") & !is.na(.) ~ paste0(round(., 2), "\u00d7"),
        !is.na(.) ~ as.character(round(., 2)),
        TRUE ~ ""
      )
    )
  )


time_table_display %>%
  gt() %>%
  tab_header(
    title = md("**Comparison of mean computation time(seconds) between CMLE, D&R, and D&R_new approaches**"),
    subtitle = md("Logistic regression model with *n* = 5 million observations, averaged over 500 replications")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Method %in% c(
        "Time Reduction (%)",
        "Time Reduction_new (%)",
        "Speedup Factor",
        "Speedup Factor_new"
      )
    )
  )






mean_memory <- function(result_list) {
  mean(sapply(result_list, function(df) df$memory))
}

# Mean peak memory usage
glm_memory <- mean_memory(B1_logistic)

drglm_25_memory <- mean_memory(B4_logistic)
drglm_50_memory <- mean_memory(B3_logistic)
drglm_100_memory <- mean_memory(B2_logistic)

drglm_new_25_memory <- mean_memory(B7_logistic)
drglm_new_50_memory <- mean_memory(B6_logistic)
drglm_new_100_memory <- mean_memory(B5_logistic)



memory_table <- tibble(
  Method = c(
    "Peak Memory (GB)",
    "Peak Memory_new (GB)",
    "Reduction vs. CMLE (%)",
    "Reduction_new vs. CMLE (%)"
  ),
  `K = 1` = c(
    glm_memory,
    NA,
    NA,
    NA
  ),
  `K = 25` = c(
    drglm_25_memory,
    drglm_new_25_memory,
    (glm_memory - drglm_25_memory) / glm_memory * 100,
    (glm_memory - drglm_new_25_memory) / glm_memory * 100
  ),
  `K = 50` = c(
    drglm_50_memory,
    drglm_new_50_memory,
    (glm_memory - drglm_50_memory) / glm_memory * 100,
    (glm_memory - drglm_new_50_memory) / glm_memory * 100
  ),
  `K = 100` = c(
    drglm_100_memory,
    drglm_new_100_memory,
    (glm_memory - drglm_100_memory) / glm_memory * 100,
    (glm_memory - drglm_new_100_memory) / glm_memory * 100
  )
)

memory_table


memory_table_display <- memory_table %>%
  mutate(
    across(
      -Method,
      ~ ifelse(is.na(.), "-", as.character(round(., 2)))
    )
  )

memory_table_display %>%
  gt() %>%
  tab_header(
    title = md("**Peak memory usage comparison**"),
    subtitle = md("Logistic regression model with *n* = 5 million observations, averaged over 500 replications")
  ) %>%
  cols_label(
    Method = "Method",
    `K = 1` = md("*K* = 1"),
    `K = 25` = md("*K* = 25"),
    `K = 50` = md("*K* = 50"),
    `K = 100` = md("*K* = 100")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Method %in% c(
        "Reduction vs. CMLE (%)",
        "Reduction_new vs. CMLE (%)"
      )
    )
  )


