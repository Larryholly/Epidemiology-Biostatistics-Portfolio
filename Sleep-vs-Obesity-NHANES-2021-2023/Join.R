# Data Preperation

##Load Packages

```{r}
library(tidyverse)
library(naniar)
library(survey)
library(haven)
library(ggplot2)
```

## Load Datasets


sleep <- read_xpt("SLQ_L_.xpt")
body_measurements <- read_xpt("BMX_L_.xpt")
demo <-  read_xpt("DEMO_L_.xpt")
diab <-  read_xpt("DIQ_L.xpt")
income <-  read_xpt("INQ_L.xpt")
alcohol <-  read_xpt("ALQ_L.xpt")
smoke <- read_xpt("SMQ_L.xpt")
mental <- read_xpt("DPQ_L.xpt")
occupation <- read_xpt("OCQ_L.xpt")

##inspect imported datasets 

glimpse(body_measurements)
glimpse(sleep)
glimpse(income)
glimpse(diab)
glimpse(mental)
glimpse(occupation)
glimpse(smoke)
glimpse(demo)
  
  
#datasets :

# sleep disorder. body measurements, demographics, diabetes, Income, alcohol
#merge into df 


df <- sleep1 |>
  left_join(body_measurements1, by = "SEQN") |>
  left_join(demo1, by = "SEQN") |>
  left_join(diab1, by = "SEQN") |>
  left_join(income1, by = "SEQN") |>
  left_join(alcohol1, by = "SEQN") |> 
  left_join(smoke1, by = "SEQN") |> 
  left_join(occupation1, by="SEQN") |> 
  left_join(mental1, by="SEQN")

df1 <- df |>
  select(
    height,
    arm_circumference,
    waist_circumference,
    income_group,
    diabetes,
    prediabetes,
    alcohol_group,
    age_group,
    race,
    education,
    marital_stat,
    sleep_group,
    bmi_group,
    age,
    height_group, 
    gender,
    smoking_status,
    feeling_depressed,
    usually_works_35plus_hours,
    SEQN,
    WTMEC2YR,
    SDMVPSU,
    SDMVSTRA,
    obese,
    bmi
  ) |>
  filter(age >= 20)

df1 <- df1 |>
  mutate(
    race = factor(race, levels = 1:5,
                  labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White",
                             "Non-Hispanic Black", "Other race, including multiracial")),
    gender = factor(gender, levels = 1:2, labels = c("Male", "Female")),
    education = factor(education, levels = 1:5,
                       labels = c("Less than 9th grade", "9th-11th grade",
                                  "High school graduate or GED",
                                  "Some college or associate degree",
                                  "College graduate or above")),
    marital_stat = factor(marital_stat, levels = 1:3,
                          labels = c("Married or living with partner",
                                     "Widowed, divorced, or separated", "Never married")),
    diabetes = factor(diabetes, levels = 1:3, labels = c("Yes", "No", "Borderline")),
    prediabetes = factor(prediabetes, levels = 1:2, labels = c("Yes", "No")),
    feeling_depressed = factor(feeling_depressed, levels = 0:3,
                               labels = c("Not at all", "Several days",
                                          "More than half the days", "Nearly every day")),
    usually_works_35plus_hours = factor(usually_works_35plus_hours, levels = 1:2,
                                        labels = c("Yes", "No"))
  )


