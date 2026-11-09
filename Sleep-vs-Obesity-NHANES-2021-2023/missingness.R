
#missing variable summary 

#exposure variable:
#SLD012:Sleep hours - weekdays or workdays , missing(113), 
# SLD013 114: sleep hours on weekends , missing (114)


sleep1 |>
  summarise(across(everything(),
                   ~sum(is.na(.)))) |>
  t()


#outcome variable 
#bmi_group 389 missing

body_measurements1 |>
  summarise(across(everything(),
                   ~sum(is.na(bmi_group)))) |>
  t()
 

#obese
#missing ?
body_measurements1 |>
  summarise(across(everything(),
                   ~sum(is.na(obese)))) |>
  t()
  
#covariates 


#Demographics --

#  age- 0 missing
# gender  0 missing



demo1 |>
  summarise(across(everything(),
                   ~sum(is.na(RIAGENDR)))) |>
  t()

demo1 |>
  summarise(across(everything(),
                   ~sum(is.na(age)))) |>
  t()



# Race/Hispanic origin
# 0 missing 

demo1 |>
  summarise(across(everything(),
                   ~sum(is.na(race)))) |>
  t()

#   Education level - Adults 20+
#4139 missing
  
  demo1 |>
  summarise(across(everything(),
                   ~sum(is.na(education)))) |>
  t()

 # Marital status
 # 4141 missing 

 demo1 |>
   summarise(across(everything(),
                    ~sum(is.na(marital_stat)))) |>
   t()

# Body measurements-- 

#  height 361 missing 

body_measurements1 |>
  summarise(across(everything(),
                   ~sum(is.na(height)))) |>
  t()

#Income--

#  2944 missing
  
Income1 |>
  summarise(missing_income8 = sum(is.na(income8))) |> 
  t()
  

#Diabetes--
# have diabetes ? 4 missing
# Had blood tested past three years? 3440 missing 



diab1 |>
  summarise(across(everything(),
                   ~sum(is.na(.)))) |>
  t()

#Alcohol consumption--
#ALQ130 2268 missing 


alcohol1 |>
  summarise(across(everything(),
                   ~sum(is.na(how_often_drink)))) |>
  t()


occupation1 |>
  summarise(across(everything(),
                   ~sum(is.na(usually_works_35plus_hours)))) |>
  t()




mental1 |>
  summarise(across(everything(),
                   ~sum(is.na(feeling_depressed)))) |>
  t()



smokel1 |>
  summarise(across(everything(),
                   ~sum(is.na(do_you_smoke_cigs)))) |>
  t()







# missing values for each variable, ordered from highest to lowest

sleep1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))

sleep1 |> 
  gg_miss_var()


body_measurements1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing)) |> 
  print(n=Inf)


body_measurements1 |> 
  gg_miss_var()




income1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))


income1 |> 
  gg_miss_var()


diab1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))

diab1 |> 
  gg_miss_var()

alcohol1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))

alcohol1 |> 
  gg_miss_var()

demo1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing)) |> 
  print(n=Inf)

demo1 |> 
  gg_miss_var()


occupation1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))

occupation1 |> 
  gg_miss_var()

mental1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))

mental1 |> 
  gg_miss_var()

smoke1 |>
  summarise(across(
    everything(),
    ~ mean(is.na(.)) * 100
  )) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))

smoke1 |> 
  gg_miss_var()

#overall missingness plot
df1 |>
  gg_miss_var(show_pct = TRUE) +
  labs(title = "Missing data by variable",
       subtitle = paste("Adults 18+, n =", nrow(df1)),
       y = "% missing")
