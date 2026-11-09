
library(survey)

vars_models <- c("obese","sleep_group","age_group","gender","race","education",
                 "income_group","marital_stat","alcohol_group","smoking_status",
                 "feeling_depressed")

# Clean special codes and flag complete cases (no rows are dropped here)
df_full <- df1_w |>
  mutate(
    # numeric codes that are really categories
    gender            = factor(gender),
    race              = factor(race),
    education         = factor(education),
    marital_stat      = factor(marital_stat),
    smoking_status = factor(smoking_status),
    feeling_depressed = factor(feeling_depressed),
    
    # character variables, with the reference group listed first
    age_group = factor(age_group,
                       levels = c("20–29", "30–44", "45–59", "60–74", "75+")),
    income_group = factor(income_group,
                          levels = c("Above low-income", "Low income", "Very low income")),
    alcohol_group = factor(alcohol_group,
                           levels = c("None", "Less than 1 drink/day", "1 to <2 drinks/day",
                                      "2 to <3 drinks/day", "3–4 drinks/day", "5+ drinks/day"))
  ) |>
  mutate(cc = if_all(all_of(vars_models), ~ !is.na(.x)))

sum(df_full$cc)      # analytic sample size
nrow(df_full)        # all adults with an exam weight

# Design on the full data, then subset to complete cases
des_full <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest    = TRUE,
  data    = df_full
)

des <- subset(des_full, cc)

# Check
svymean(~obese, des)
degf(des)

# Unadjusted model
m_crude <- svyglm(obese ~ sleep_group, design = des, family = quasibinomial())
summary(m_crude)
exp(cbind(OR = coef(m_crude), confint(m_crude)))
regTermTest(m_crude, "sleep_group")

# Degrees of freedom check
degf(des)
length(coef(m_adj))

# Adjusted model
m_adj <- svyglm(
  obese ~ sleep_group + age_group + gender + race + education +
    income_group + marital_stat + alcohol_group + smoking_status +
    feeling_depressed,
  design = des,
  family = quasibinomial()
)
summary(m_adj)
exp(cbind(OR = coef(m_adj), confint(m_adj)))
regTermTest(m_adj, "sleep_group")

# Check: these should match each other
nobs(m_crude)
nobs(m_adj)

# Odds ratios with intervals for the sleep terms, same df for both models
df_design <- degf(des)    # 15 in your case

# Odds ratios with intervals for the sleep terms, same df for both models
get_or <- function(m, label, ddf) {
  est <- coef(m); ci <- confint(m, ddf = ddf)
  idx <- grep("sleep_group", names(est))
  data.frame(model = label, term = names(est)[idx],
             OR = round(exp(est[idx]), 2),
             lower = round(exp(ci[idx, 1]), 2),
             upper = round(exp(ci[idx, 2]), 2))
}

res <- rbind(get_or(m_crude, "Unadjusted", df_design),
             get_or(m_adj,   "Adjusted",   df_design))
rownames(res) <- NULL
res



# Degrees of freedom check
degf(des)
length(coef(n_adj))



#slim model

n_adj <- svyglm(
  obese ~ sleep_group + age_group + gender + race  ,
  design = des,
  family = quasibinomial()
)

summary(n_adj, df.resid = df_design)
exp(cbind(OR = coef(n_adj), confint(n_adj, ddf = df_design)))
regTermTest(n_adj, "sleep_group", df = df_design)

# Overall p value for sleep in each model
regTermTest(m_crude, "sleep_group", df = df_design)
regTermTest(m_adj,   "sleep_group", df = df_design)
regTermTest(n_adj,   "sleep_group", df = df_design)


#crude vs slim model
get_or <- function(m, label, ddf) {
  est <- coef(m)
  ci <- confint(m, ddf = ddf)
  
  idx <- grep("sleep_group", names(est))
  
  data.frame(
    model = label,
    term = names(est)[idx],
    OR = round(exp(est[idx]), 2),
    lower = round(exp(ci[idx, 1]), 2),
    upper = round(exp(ci[idx, 2]), 2)
  )
}

res <- rbind(
  get_or(m_crude, "Unadjusted", df_design),
  get_or(n_adj, "Adjusted (age, gender, race)", df_design)
)

rownames(res) <- NULL
res



