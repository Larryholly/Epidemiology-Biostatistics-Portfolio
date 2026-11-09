library(dplyr)
library(survey)

# Setup
chi_vars <- c("sleep_group", "age_group", "income_group", "alcohol_group",
              "height_group", "diabetes", "prediabetes", "race", "education",
              "marital_stat", "gender", "usually_works_35plus_hours",
              "feeling_depressed", "smoking_status")

df1_w <- df1 |>
  filter(!is.na(WTMEC2YR), WTMEC2YR > 0) |>
  mutate(
    diabetes     = replace(diabetes,     diabetes %in% 9, NA),
    education    = replace(education,    education %in% 9, NA),
    marital_stat = replace(marital_stat, marital_stat %in% c(77, 99), NA),
    prediabetes  = replace(prediabetes,  prediabetes %in% 9, NA),
    feeling_depressed          = replace(feeling_depressed, feeling_depressed %in% c(7, 9), NA),
    usually_works_35plus_hours = replace(usually_works_35plus_hours,
                                         usually_works_35plus_hours %in% c(7, 9), NA)
  )

table(df1_w$feeling_depressed, useNA = "ifany")
table(df1_w$prediabetes, useNA = "ifany")
table(df1_w$marital_stat, useNA = "ifany")

# Unweighted screening for small cells and sample sizes
run_chi <- function(var) {
  d    <- df1_w |> filter(!is.na(.data[[var]]), !is.na(obese))
  tab  <- table(d[[var]], d$obese)
  test <- suppressWarnings(chisq.test(tab))
  
  data.frame(
    variable     = var,
    n            = sum(tab),
    min_observed = min(tab),
    min_expected = round(min(test$expected), 1)
  )
}

chi_results <- bind_rows(lapply(chi_vars, run_chi))
chi_results
chi_results |> filter(min_observed < 10 | min_expected < 5)


table(df1_w$prediabetes, df1_w$obese, useNA = "ifany")
table(df1_w$usually_works_35plus_hours, df1_w$obese, useNA = "ifany")
table(df1_w$feeling_depressed, df1_w$obese, useNA = "ifany")



#dataset for weighted analysis
#

# Weighted results to report
des_all <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                     weights = ~WTMEC2YR, nest = TRUE, data = df1_w)

run_svychi <- function(var) {
  d    <- subset(des_all, !is.na(des_all$variables[[var]]) & !is.na(obese))
  test <- svychisq(as.formula(paste("~", var, "+ obese")), d)
  
  data.frame(
    variable = var,
    F_stat   = round(unname(test$statistic), 2),
    p_value  = test$p.value
  )
}

svychi_results <- bind_rows(lapply(chi_vars, run_svychi))
svychi_results |> 
  arrange(desc(p_value))

# Combined table with unweighted n and weighted p value
final_tbl <- chi_results |>
  select(variable, n) |>
  left_join(svychi_results, by = "variable") |>
  mutate(p_value = signif(p_value, 3))
final_tbl