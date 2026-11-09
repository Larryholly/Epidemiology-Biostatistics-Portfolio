# Among U.S. adults, is short sleep duration associated with obesity 
# after adjusting for demographic and socioeconomic factors? 

# Datasets: demo, alcohol, diab, sleep, body_measurements, income, smoke,mental :)

#Variables:  height, smoke_status,feeling_depressed usually_works_35plus_hours
# diabetes prediabetes, race, education,marital_stat, gender
#categories sleep_group bmi_group age_group income_group alcohol_group height_group

#cofounders:gender, age, race, education, alcohol group, do_you_smoke_cigs feeling_depressed usually_works_35plus_hours

#Exposure - sleep variable: SLD012:Sleep hours - weekdays or workdays  / sleep_hours/ sleep_group


#outcome -- BMXBMI - Body Mass Index (kg/m**2)/bmi / (bmi_group)

#Body measurements 
# BMXHT - Standing Height (cm) /height /  (height_group)
#height < 155 ~ "Below average",
#height >= 155 & height <= 165 ~ "Average",
#height > 165 ~ "Above average"
#BMXBMI - Body Mass Index (kg/m**2)

#Income--/income /  (income_group)
# Income:  INDFMMPI  - Family monthly poverty level index
# ≤ 1.30 → very low income (SNAP threshold)
# 1.30–1.85 → low income (WIC / school lunch threshold)
# > 1.85 → above low‑income 

#Diabetes-- (diabetes)
#DIQ010 - Doctor told you have diabetes
#1  yes
#2 no
#3 Bprderline
#7 Refused
#9 Don't know

#DIQ160 - Ever told you have prediabetes (prediabetes)
#1 Yes
#2 No 
#7 Refused
#9 Don't know 


#Alcohol consumption-- /  (alcohol_group)
#ALQ130 - Avg # alcoholic drinks/day/past 12 mos- Continuous 

#Demographics --
# RIDAGEYR - Age in years at screening (age) (age_group)
#RIAGENDR (gender)
# 1: male 2: female

# RIDRETH1 - Race/Hispanic origin (race)
# 1: Mexican American 2: Other Hispanic
# 3:white 4:black 5:other 

# DMDEDUC2-  Education level - Adults 20+/ education (education)
# 1: less than 9th grade, 2: 9-11th, 3: high school graduate/ GED equiv 
# 4: AA degree or Some college, 5 College graduate in above
# 7: refused 9: don't know

# DMDMARTZ - Marital status (marital_stat)
# 1: Married/Living with partner
# 2: Widowed/Divorced/Seperated
# 3: Nerver Married
#77: Refused 
#99 dont know 


#OCQ210 - Usually work 35 or more hours per week (usually_works_35plus_hours)
#1 yes
#2 no
#7 refused
#9 Don't know 

#DPQ020 - Feeling down, depressed, or hopeless (feeling_depressed)
#0 not at all
#1 several days
# 2 more than half the days
#3 nearly everyday
#7 refused 
#9 don't know

#SMQ040 - Do you now smoke cigarettes? (do_you_smoke_cigs)
#1 Everyday
#2 Some days
#3 Not at all
#7 Refused
#9 Don't know







#limitations

The design is cross-sectional, so obesity could be causing poor sleep as well as the reverse.
Sleep is self-reported.
Physical activity, diet, and sleep apnea aren’t controlled.
Power is limited by the few PSUs in one NHANES c
