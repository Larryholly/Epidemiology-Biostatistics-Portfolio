#dataset: df1 
# adults above 18 years old

#categorical: sleep_group, age_group_ income_group_ alcohol_group height_group bmi_group 
# marital_stat diabetes, prediabetes,race,education gender
# num: age height bmi arm_circumference


numerical_summary <- function(data, variable) {
  data |>
    summarise(
      mean_x = mean({{ variable }}, na.rm = TRUE),
      median_x = median({{ variable }}, na.rm = TRUE),
      sd_x = sd({{ variable }}, na.rm = TRUE),
      min_x = min({{ variable }}, na.rm = TRUE),
      max_x = max({{ variable }}, na.rm = TRUE)
    )
}

numerical_summary(df1,age)
numerical_summary(df1,height)
numerical_summary(df1,bmi)


categorical_summary <- function(data, variable) {
  data |>
    count({{ variable }}) |>
    mutate(percent = round(n / sum(n) * 100, 1))
}


categorical_summary(df1, sleep_group)

categorical_summary(df1,obese)

categorical_summary(df1, age_group)

categorical_summary(df1, income_group)

categorical_summary(df1, alcohol_group)

categorical_summary(df1, height_group)

categorical_summary(df1, bmi_group)

categorical_summary(df1, marital_stat)

categorical_summary(df1, diabetes)

categorical_summary(df1, prediabetes)

categorical_summary(df1, race)

categorical_summary(df1, education)

categorical_summary(df1, gender) 

categorical_summary(df1, usually_works_35plus_hours) 

categorical_summary(df1, feeling_depressed) 

categorical_summary(df1, smoking_status) 

