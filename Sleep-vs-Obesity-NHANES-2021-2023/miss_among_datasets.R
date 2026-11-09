library(naniar)
#exposure (sleep)
sleep1 |> 
  select(sleep_hours,sleep_group) |> 
  vis_miss()

sleep1 |> 
  gg_miss_var()

#outcome(bmi)

body_measurements1 |> 
  gg_miss_var()

#bmi
body_measurements1 |> 
  select(bmi,height,height_group,bmi_group,obese) |> 
  vis_miss()

#Covariates
#height

#income

income1 |> 
  gg_miss_var()

# Family monthly poverty level index

income1 |> 
  select(income8,income_group) |> 
  vis_miss()

#diabetes
diab1 |> 
  gg_miss_var()

# has diabetes?, Had blood tested past three years (diabetes)
diab1 |> 
  select(diabetes,prediabetes) |> 
  vis_miss()

# - Avg # alcoholic drinks/day/past 12 mos- Continuous

alcohol |> 
  gg_miss_var()
         
 alcohol1 |> 
 select(alcohol_group,how_often_drink) |> 
  vis_miss()
 
 
 #demographics
 #age gender race education level
 
 demo1 |> 
   select(age, age_group, gender, race,education,marital_stat ) |> 
   vis_miss()
 

 demo1 |> 
   gg_miss_var()
 
 #occupation
 
 demo1 |> 
   select(usually_works_35plus_hours) |> 
   vis_miss()
 
 
 demo1 |> 
   gg_miss_var()
 
 #mental 
 
 mental1 |> 
   select(feeling_depressed) |> 
   vis_miss()
 
 
 mental1 |> 
   gg_miss_var()
 
 
 #smoker
 
 smoke1 |> 
   select(do_you_smoke_cigs ) |> 
   vis_miss()
 
 
 smoke1 |> 
   gg_miss_var()
 
 
 
 
 
 
 
 
 

 ### check after merge , filter >= 18
 #13.5% missing 
 df1 |>
   naniar::vis_miss()
 
 