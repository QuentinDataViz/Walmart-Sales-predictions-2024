############## Walmart Sales Prediction 2024 #################

#map_france <- rnaturalearth::ne_states(country = "France", returnclass = "sf")
rm(list = ls()) 
cat("\014")  #Effacer la console

package <- c("tidyverse", "randomForest","chron", "readxl", "lubridate", "sf","jsonlite", "scales", "httr", "janitor", "tidytext")
lapply(package, require, character.only = TRUE)
rm(package)

options( "digits"=2, "scipen"=100) 



Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

############################## CHARGEMENT ################################### 


attributes <- 
  read_xlsx("data/attributes_description.xlsx") %>% 
  clean_names() %>% 
  print()


events <- 
  read_xlsx("data/Events_HolidaysData.xlsx") %>% 
  clean_names() %>% 
  print()




submissions <- 
  read_csv("data/Kaggle_Submission_Format.csv") %>% 
  print()


econ <- 
  read_xlsx("data/macro_economic.xlsx") %>% 
  clean_names() %>% 
  print()


sub_cloths <- 
  read_csv("data/submission.csv") %>% 
  clean_names() %>% 
  print()


train <- 
  read_csv("data/train.csv") %>% 
  clean_names() %>% 
  print()


weather <- 
  read_xlsx("data/WeatherData.xlsx") %>% 
  clean_names() %>% 
  print()







############################## TRANSFORMATION ##################################


# Le dataset train et submission sont basés sur des données mensuelles. 

event_clean <- 
events %>% 
    mutate(.before = 'month_date', 
         jour = str_sub(month_date, start = 3, end = 4), 
         mois = str_sub(month_date, start = 6, end = 7)) %>% 
  select(-month_date) %>% 
  count(name = "number_event", year, mois, day_category) %>% 
  mutate(across(1:3, ~as_factor(.))) %>% 
  print()
  
  
  
econ_clean <- 
econ %>% 
  mutate(.before = 'year_month', 
         year = str_sub(year_month, start = 1, end = 4), 
         mois = str_remove(year_month, ".* - ")) %>% 
  mutate(mois = month(parse_date_time(mois, orders = "b"))) %>% 
  select(-year_month) %>% 
  mutate(advertising_expenses_in_thousand_dollars_facteur = as_factor(ifelse(advertising_expenses_in_thousand_dollars != "?", "oui", "non"))) %>% 
  mutate(across(1:2, ~as_factor(.))) %>% 
  print()
  




train_fusion <- 
train %>% 
  rename(mois = month) %>% 
  mutate(across(1:3, ~as_factor(.))) %>% 
  left_join(econ_clean, by = c("year", "mois")) %>% 
  filter(!is.na(sales_in_thousand_dollars )) %>% 
  select(-c( party_in_power, 
            mill_use_in_480_lb_netweright_in_million_bales, 
            production_in_480_lb_netweright_in_million_bales,
            yieldperharvested_acre,
            average_upland_harvested_million_acres,
            advertising_expenses_in_thousand_dollars)) %>% 
  print()






sub_fusion <- 
  sub %>% 
  rename(mois = month) %>% 
  mutate(across(1:3, ~as_factor(.))) %>% 
  left_join(econ_clean, by = c("year", "mois")) %>% 
  select(-c(x4,
            party_in_power, 
    mill_use_in_480_lb_netweright_in_million_bales, 
    production_in_480_lb_netweright_in_million_bales,
    yieldperharvested_acre,
    average_upland_harvested_million_acres,
    advertising_expenses_in_thousand_dollars)) %>% 
  print()


formule <- 
  as.formula(paste("sales_in_thousand_dollars ~ ",  paste(train_fusion %>% select(-"sales_in_thousand_dollars") %>% colnames(), collapse = "+"))) %>% 
  print()


rf <- 
randomForest(data = train_fusion, 
             formule, 
             mtry = 15, 
             nodesize = 15, 
             ntree = 800,
             seed = 149)
rf
plot(rf)
varImpPlot(rf)
importance(rf)


mean(rf$mse) %>%  sqrt()



str(sub_fusion)
str(train_fusion)




submissions %>% 
mutate("Sales(In ThousandDollars)" = predict(rf, newdata = sub_fusion)) %>% 
  write_csv("prod/test.csv")
