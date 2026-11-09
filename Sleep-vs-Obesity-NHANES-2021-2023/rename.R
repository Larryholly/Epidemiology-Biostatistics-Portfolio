#Rename

#sleep variables
sleep1 <-  sleep |> 
  rename(
    Usual_sleep_time_on_weekdays_or_workdays = SLQ300,
    Usual_wake_time_on_weekdays_or_workday = SLQ310,
    sleep_hours = SLD012,
    usual_sleep_time_on_weekends = SLQ320,
    usual_wake_time_on_weekends = SLQ330,
    sleep_hours_weekends = SLD013
  )

#body measurement variables 
body_measurements1 <-  body_measurements |> 
  rename(
    Body_Measures_Component_Status_Code = BMDSTATS, 
    weight = BMXWT,
    weight_comment = BMIWT,
    recumbent_length = BMXRECUM,
    recumbent_length_comment = BMIRECUM,
    head_circumference = BMXHEAD,
    head_cirumference_comment = BMIHEAD,
    height =  BMXHT,
    height_comment = BMIHT,
    bmi = BMXBMI,
    bmi_category = BMDBMIC,
    upper_leg_length = BMXLEG,
    upper_leg_length_comment = BMILEG,
    upper_are_length = BMXARML,
    upper_are_length_commment = BMIARML,
    arm_circumference = BMXARMC,
    arm_circumference_comment = BMIARMC,
    waist_circumference = BMXWAIST,
    waist_circumference_comment = BMIWAIST,
    hip_circumference = BMXHIP ,
    hip_circumference_comment = BMIHIP,
    
  )

#income variables

income1 <- income |> 
  rename(
    income8 = INDFMMPI,
    monthly_category = INDFMMPC,
    savings_more_than_twenty_thousand = INQ300,
    Total_savings_cash_assets = IND310
    
  )

#diabetes variables
diab1 <- diab |> 
  rename(
    diabetes = DIQ010,
    age_developed_diabetes = DID040,
    prediabetes = DIQ160,
    blood_tested = DIQ180,
    use_insulin = DIQ050, 
    how_long_use_insulin = DID060,
    unit_of_measure = DIQ060U,
    take_diabetes_pill = DIQ070
  )
 
#alcohol use variable 
alcohol1 <- alcohol |>
  rename(
    ever_drink = ALQ111,
    how_often_drink = ALQ121,
    avg_drinks_per_day = ALQ130,
    days_4plus_drinks = ALQ142,
    times_4plus_drinks_2hours = ALQ270,
    times_8plus_drinks_1day = ALQ280,
    ever_4_5plus_drinks_every_day = ALQ151,
    times_4_5plus_drinks_past_month = ALQ170
  )

#demograpic variables
demo1 <- demo %>%
  rename(
    age = RIDAGEYR,
    age_in_months_at_screening_0_to_24_mos = RIDAGEMN,
    race = RIDRETH1,
    race_with_nh_asian = RIDRETH3,
    six_month_time_period = RIDEXMON,
    age_in_months_at_exam_0_to_19_years = RIDEXAGM,
    served_active_duty_in_us_armed_forces = DMQMILIZ,
    country_of_birth = DMDBORN4,
    length_of_time_in_us = DMDYRUSR,
    education = DMDEDUC2,
    marital_stat = DMDMARTZ,
    pregnancy_status_at_exam = RIDEXPRG,
    total_number_of_people_in_the_household = DMDHHSIZ,
    hh_ref_persons_gender = DMDHRGND,
    hh_ref_persons_age_in_years = DMDHRAGZ,
    hh_ref_persons_education_level = DMDHREDZ,
    hh_ref_persons_marital_status = DMDHRMAZ,
    hh_ref_persons_spouses_education_level = DMDHSEDZ,
    ratio_of_family_income_to_poverty = INDFMPIR,
    data_release_cycle = SDDSRVYR,
    interview_examination_status = RIDSTATR,
    gender = RIAGENDR
  )

#occupation/work variables 
occupation1 <- occupation |> 
  rename(
    employment_status = OCD150,
    hours_worked_last_week = OCQ180,
    usually_works_35plus_hours = OCQ210,
    usual_workdays_per_week = OCQ215,
    reason_not_working = OCQ383
  )
#mental health variables
mental1 <- mental |>
  rename(
    little_interest = DPQ010,
    feeling_depressed = DPQ020,
    sleep_problems = DPQ030,
    fatigue_low_energy = DPQ040,
    appetite_problems = DPQ050,
    negative_self_image = DPQ060,
    concentration_problems = DPQ070,
    psychomotor_changes = DPQ080,
    thoughts_of_death = DPQ090,
    difficulty_from_problems = DPQ100
  )

#tobacco use variables 
smoke1 <-  smoke |> 
  rename(
    smoked_at_least_100_cigs_in_life = SMQ020,
    do_you_smoke_cigs = SMQ040,
    days_smoked_cigs_past_30_days = SMD641,
    avg_cigs_per_Day_past_30_days =SMD650,
    cigs_menthol_indicator = SMD100MN,
    cigs_smoked_in_entire_life = SMQ621,
    age_first_cig = SMD630,
    questionnaire_mode_flag = SMAQUEX2
  )
#sub grouping required 


# BMXHT - Standing Height (cm)/ height

body_measurements1  <- body_measurements1  |>
  mutate(
    height_group = case_when(
      height < 155 ~ "Below average",
      height >= 155 & height <= 165 ~ "Average",
      height > 165 ~ "Above average",
      TRUE ~ NA_character_
    )
  )

body_measurements1$height_group

#BMXBMI - Body Mass Index (kg/m**2)/  bmi

body_measurements1 <- body_measurements1 |>
  mutate(
    bmi_group = case_when(
      bmi < 18.5             ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
      bmi >= 25 & bmi < 30   ~ "Overweight",
      bmi >= 30              ~ "Obesity",
      TRUE                   ~ NA_character_
    ),
    obese = case_when(
      is.na(bmi) ~ NA_integer_,
      bmi >= 30  ~ 1L,
      TRUE       ~ 0L
    )
  )

body_measurements1$bmi_group
body_measurements1$obese



#ALQ130 - Avg # alcoholic drinks/day/past 12 mos- Continuous / how_often_drink

alcohol1 <- alcohol1 |>
  mutate(
    alcohol_group = case_when(
      how_often_drink %in% c(77, 99) ~ NA_character_,
      how_often_drink == 0           ~ "None in past year",
      how_often_drink %in% 7:10      ~ "Infrequent (monthly or less)",
      how_often_drink %in% 5:6       ~ "Occasional (weekly to 2-3 times/month)",
      how_often_drink %in% 1:4       ~ "Frequent (2 or more times/week)",
      TRUE                           ~ NA_character_
    ),
    alcohol_group = factor(alcohol_group,
                           levels = c("None in past year",
                                      "Infrequent (monthly or less)",
                                      "Occasional (weekly to 2-3 times/month)",
                                      "Frequent (2 or more times/week)"))
  )

alcohol1$alcohol_group

 
# Income: / family_index
# INDFMMPI  - Family monthly poverty level index
# ≤ 1.30 → very low income (SNAP threshold)
# 1.30–1.85 → low income (WIC / school lunch threshold)
# > 1.85 → above low‑income 

income1 <- income1 |>
  mutate(
    income_group = case_when(
      income8 <= 1.30 ~ "Very low income",
      income8 > 1.30 & income8 <= 1.85 ~ "Low income",
      income8> 1.85 ~ "Above low-income",
      TRUE ~ NA_character_
    )
  )

income1$income_group

#age groups

demo1 <- demo1 |>
  mutate(
    age_group = case_when(
      age >= 18 & age < 30 ~ "18–29",
      age >= 30 & age < 45 ~ "30–44",
      age >= 45 & age < 60 ~ "45–59",
      age >= 60 & age < 75 ~ "60-75",
      age >= 75 ~ "75+",
      TRUE ~ NA_character_
    )
  )

demo1$age_group

#exposure groups
#recommended sleep= reference group

  sleep1 <- sleep1 |>
  mutate(
    sleep_hours = as.numeric(sleep_hours),
    
    sleep_group = case_when(
      sleep_hours < 7                     ~ "Short sleep (<7 hours)",
      sleep_hours >= 7 & sleep_hours <= 9 ~ "Recommended sleep (7–9 hours)",
      sleep_hours > 9                     ~ "Long sleep (>9 hours)",
      TRUE                                ~ NA_character_
    ),
    
    sleep_group = factor(sleep_group,
                         levels = c("Recommended sleep (7–9 hours)",
                                    "Short sleep (<7 hours)",
                                    "Long sleep (>9 hours)"))
  )

table(sleep1$sleep_group, useNA = "ifany")




class(sleep1$sleep_hours)     # "numeric"
class(sleep1$sleep_group)     # "factor"
levels(sleep1$sleep_group) 

#smoking group

smoke1 <- smoke1 |>
  mutate(
    smoking_status = case_when(
      smoked_at_least_100_cigs_in_life == 2 ~ "Never",
      smoked_at_least_100_cigs_in_life == 1 & do_you_smoke_cigs %in% c(1, 2) ~ "Current",
      smoked_at_least_100_cigs_in_life == 1 & do_you_smoke_cigs == 3 ~ "Former",
      TRUE ~ NA_character_
    ),
    smoking_status = factor(smoking_status,
                            levels = c("Never", "Former", "Current"))
  )
