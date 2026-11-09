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


