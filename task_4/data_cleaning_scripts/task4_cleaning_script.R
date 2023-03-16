library(tidyverse)
library(janitor)
library(readxl) # For reading in data from excel

boing_boing_candy_2015 <- read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
boing_boing_candy_2016 <- read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
boing_boing_candy_2017 <- read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))

first_clean_2015 <- clean_names(boing_boing_candy_2015) # Janitor function to clean names
first_clean_2016 <- clean_names(boing_boing_candy_2016)
first_clean_2017 <- clean_names(boing_boing_candy_2017)

names(first_clean_2015) # Use these to check that the names have been cleaned correctly
names(first_clean_2016)
names(first_clean_2017)

glimpse(first_clean_2015)

col_names_2015 <- colnames(first_clean_2015) # Assign column names then check them
col_names_2016 <- colnames(first_clean_2016)
col_names_2017 <- colnames(first_clean_2017)

col_names_2015
col_names_2016
col_names_2017

# df <- mydata[ -c(1,3:4) ]
# remove irrelevant data from each of the three years

second_clean_2015 <- first_clean_2015[ -c(16,18,23,26,33,38,41,45,57,69,71,82,85,88,93,94,95,97:114,116:124) ]
second_clean_2015

second_clean_2016 <- first_clean_2016[ -c(12,21,22,26,27,31,43,46,48,49,71,79,90,94,101,102,104,105,107:122) ]
second_clean_2016

second_clean_2017 <- first_clean_2017[ -c(1,12,21,22,26,27,31,43,46,48,49,68,69,72,81,92,96,104,105,107,108,112:120) ]
second_clean_2017

col_names_reduced_2015 <- colnames(second_clean_2015)
col_names_reduced_2016 <- colnames(second_clean_2016)
col_names_reduced_2017 <- colnames(second_clean_2017)

col_names_reduced_2015 # 80 columns remain
col_names_reduced_2016 # 89 columns remain
col_names_reduced_2017 # 90 columns remain

# Assumptions made in data set 1:

# We have decided to retain data on anything related with food / products but have omitted all non-food options.
# This includes some dubious data but it seems better to retain data than remove at this stage. 
# [Edit, we later removed bread, puddings etc]

# We have decided to keep m&ms as separate entities but strongly considered combining different colours into
# regular m&ms. (Keeping peanut separate)

# We will combine anonymous_brown_globs with mary_janes 

second_clean_2015$timestamp <- as.Date(second_clean_2015$timestamp)
second_clean_2015$year <- as.numeric(format(second_clean_2015$timestamp, "%Y"))


# rename: my_data %>% 
 # rename(
 #   sepal_length = Sepal.Length,
 #   sepal_width = Sepal.Width
 # )

third_clean_2015 <- second_clean_2015 %>% 
  rename(
    age = how_old_are_you,
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
    bonkers_the_candy = bonkers, 
    brach_product = brach_products_not_including_candy_corn,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    hersheys_kisses = hershey_s_kissables,  
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor, 
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    toblerone = tolberone_something_or_other, 
    peanut_m_ms = peanut_m_m_s,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    odd_marshmallow_peanut = those_odd_marshmallow_circus_peanut_things
  )

third_clean_2016 <- second_clean_2016 %>% 
  rename(
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    gender = your_gender,
    age = how_old_are_you,
    country = which_country_do_you_live_in,
    state_province_county_etc = which_state_province_county_do_you_live_in,
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
    box_o_raisins = boxo_raisins,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor,
    peanut_m_ms = peanut_m_m_s,
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    odd_marshmallow_peanuts = those_odd_marshmallow_circus_peanut_things,
    toblerone = tolberone_something_or_other
  )

# Find and replace for start of column names for 2017 data, for example:
# "q6_" -> ""

colnames(second_clean_2017) <- gsub("^.{3}", "", colnames(second_clean_2017))
names(second_clean_2017)

third_clean_2017 <- second_clean_2017 %>% 
  rename(
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
    box_o_raisins = boxo_raisins,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor,
    peanut_m_ms = peanut_m_m_s,
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    boo_berry_crunch = sandwich_sized_bags_filled_with_boo_berry_crunch,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    odd_marshmallow_peanuts = those_odd_marshmallow_circus_peanut_things,
    toblerone = tolberone_something_or_other
  )

# Error in `rename()`:
# ! Names must be unique.
# ✖ These names are duplicated:
#  * "mary_janes" at locations 8 and 49.

# convert age to from character to number

third_clean_2015$age <- as.double(third_clean_2015$age)
third_clean_2016$age <- as.double(third_clean_2016$age)
third_clean_2017$age <- as.double(third_clean_2017$age)

third_clean_2015
third_clean_2016
third_clean_2017

col_names_reduced_clean_2015 <- colnames(third_clean_2015)
col_names_reduced_clean_2016 <- colnames(third_clean_2016)
col_names_reduced_clean_2017 <- colnames(third_clean_2017)

# Need to tidy the countries, USA in particular is horrible. 

sort(unique(third_clean_2016$country))

# Can do a long if else, running through each country.

# if ( test_expression1) {
#  statement1
# } else if ( test_expression2) {
#  statement2
# } else if ( test_expression3) {
#  statement3
# } else {
#  statement4
# }



# if (third_clean_2016$country == "United Sates" | "United State" | "united states" | "United states" | "United States" | "UNited States" |
#    "united states of america" | "United States of America" | "United Stetes" | "Units States" | "us" | "Us" | 
#    "US" | "usa" | "uSA" | "Usa" | "USA" | "USA (I think but it's an election year so who can really tell)" | 
#    "USA USA USA" | "USA USA USA USA" | "USA!" | "USA! USA!" | "USA! USA! USA!" | "USA!!!!!!" | "USSA" | 
#    "United  States of America" | "Trumpistan" | "u.s." | "U.s." | "U.S." | "U.S.A." | "america" | "America" | "Murica" | 
#    "the best one - usa" | "The Yoo Ess of Aaayyyyyy") {
  third_clean_2016$country = "USA"
} 

# Error in third_clean_2016$country == "United Sates" | "United State" : 
#  operations are possible only for numeric, logical or complex types

#else if {
#  if ("Australia") {
#    "Australia"
#  } else {
#    if ("Austria") {
#      "Austria"
 #   }
 # }
    
 # )
  
#}


#(  
  


# Need to check data for duplicate information
  
# We are cleaning the data but also need to consider tidying the data too
  

  # changed numbers in country column to NA
  third_clean_2016$country <- gsub('[0-9.]', '', third_clean_2016$country) 
  
  third_clean_2016 <- third_clean_2016 %>% 
    mutate(country = na_if(country, ""))
  
  
  third_clean_2017$country <- gsub('[0-9.]', '', third_clean_2017$country) 
  
  third_clean_2017 <- third_clean_2017 %>% 
    mutate(country = na_if(country, ""))
  
  
# Removing other random answers from country - 2016, 
# there is a debate whether to omit answers like "Trumpistan" and "god's country",
# if they are joking around, the data is more likely to be incorrect and it might make sense to remove.
# Knowing where to draw the line is tricky, as we don't want to silence people or remove good data.
  
  
  na_test <- third_clean_2016 %>% 
    mutate(country = as.character(country)) %>% 
    mutate(country = case_when(
      country == "A tropical island south of the equator"  | 
        country == "Denial" |
        country == "god's country" |
        country == "Murica" |
        country == "Neverland" |
        country == "Not the USA or Canada" |
        country == "one of the best ones" |
        country == "See above" |
        country == "Somewhere" |
        country == "there isn't one for old men" |
        country == "this one" |
        country == "Trumpistan"  ~ NA_character_, 
      TRUE ~ country
    ))
  

# use dplyr package
library(dplyr)

country_2016 <- c(third_clean_2016$country)
  
  

third_clean_2016 <- third_clean_2016 %>% 
  mutate ( country = str_to_lower(third_clean_2016$country))

unique_names_country <- sort(unique(third_clean_2016$country))
unique_names_country

USA_names <-  c("the yoo ess of aaayyyyyy",                                                                         
                "trumpistan" ,                                                   
                "u.s.",  "merica",  "murica",
                "sub-canadian north america 'merica",
                "united  states of america",                                   
                "america",  "the best one - usa",                                           
                "united sates",                                                                                                     
                "united states" ,                                                                                      
                "united stetes",                                                                                                  
                "us", "usa usa usa",                                                                                                              
                "usa usa usa usa",                                                                                                          
                "usa! usa!",                                                                                                     
                "usa!!!!!!",                                                                                                              
                "u.s.a.", "usa! usa! usa!", 
                "united state",
                "united states of america", 
                "units states",
                "usa", "usa!",
                "ussa","usa (i think but it's an election year so who can really tell)" ) 

fourth_clean_2016 <- third_clean_2016 %>% 
  mutate (country = ifelse(third_clean_2016$country %in%  USA_names, 'USA', third_clean_2016$country))

fourth_clean_2016





third_clean_2017 <- third_clean_2017 %>% 
  mutate ( country = str_to_lower(third_clean_2017$country))

unique_names_country <- sort(unique(third_clean_2017$country))
unique_names_country

na_test <- third_clean_2017 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = case_when(
      country == "a"  | 
      country == "atlantis" |
      country == "canae" |
      country == "cascadia" |
      country == "earth" |
      country == "europe" |
      country == "fear and loathing" |
      country == "i don't know anymore" |
      country == "insanity lately" |
      country == "narnia" |
      country == "subscribe to dmuz on youtube" |
      country == "ud" | 
      country == "unhinged states" ~ NA_character_, 
    TRUE ~ as.character(country)
  ))

#NA_names <- c("a","atlantis","canae","cascadia","earth","europe","fear and loathing" |
#                country == "i don't know anymore" |
 #               country == "insanity lately" |
  #              country == "subscribe to dmuz on youtube" |
   #             country == "ud" | 
    #            country == "narnia" |
     #           country == "unhinged states")

Australia_names <- c("australia")

Canada_names <- c("can","canada","canada`","soviet canuckstan")

Finland_names <- c("finland")

France_names <- c("france")

Germany_names <- c("germany")

Hong_Kong_names <- c("hong kong")

Netherlands_names <- c("the netherlands","netherlands")

South_Africa_names <- c("south africa")

Spain_names <- c("spain")

UK_names <- c("uk","united kingdom","scotland","endland","england")

USA_names <-  c("alaska","'merica","ahemamerca","america","california",
                "i pretend to be from canada, but i am really from the united states",
                "murica","murrika","n america","new jersey","new york","north carolina",
                "pittsburgh","the united states","the united states of america","trumpistan",
                "u s","u s a","unied states","unite states","united sates","united staes",
                "united state","united statea","united stated","united states","united states of america",
                "united statss","united ststes","unites states","us","us of a",
                "usa","usa usa usa!!!!","usa! usa! usa!","usa? hard to tell anymore..",
                "usaa","usas","usausausa","ussa","u.s.a.")

# Now all the countries with one entry which are correct but need to be added due to the lower case

China_names <- c("china")
Costa_Rica_names <- c("costa rica")
Denmark_names <- c("denmark")
Greece_names <- c("greece")
Iceland_names <- c("iceland")
Indonesia_names <- c("indonesia")
Ireland_names <- c("ireland")
Japan_names <- c("japan")
Mexico_names <- c("mexico")
Singapore_names <- c("singapore")
South_Korea_names <- c("korea","south korea")
Sweden_names <- c("sweden")
Switzerland_names <- c("switzerland")
Taiwan_names <- c("taiwan")
UAE_names <- c("uae")

fourth_clean_2017 <- third_clean_2017 %>% 
  mutate(country = case_when(country %in% Australia_names ~ "Australia",
                             country %in% Canada_names ~ "Canada", 
                             country %in% Finland_names ~ "Finland",
                             country %in% France_names ~ "France", 
                             country %in% Germany_names ~ "Germany", 
                             country %in% Hong_Kong_names ~ "Hong Kong",
                             country %in% Netherlands_names ~ "Netherlands", 
                             country %in% South_Africa_names ~ "South Africa", 
                             country %in% Spain_names ~ "Spain",
                             country %in% UK_names ~ "UK", 
                             country %in% USA_names ~ "USA",
                             country %in% China_names ~ "china",
                             country %in% Costa_Rica_names ~ "costa rica",
                             country %in% Denmark_names ~ "denmark",
                             country %in% Greece_names ~ "greece",
                             country %in% Iceland_names ~ "iceland",
                             country %in% Indonesia_names ~ "indonesia",
                             country %in% Ireland_names ~ "ireland", 
                             country %in% Japan_names ~ "japan", 
                             country %in% Mexico_names ~ "mexico", 
                             country %in% Singapore_names ~ "singapore", 
                             country %in% South_Korea_names ~ "south korea", 
                             country %in% Sweden_names ~ "sweden", 
                             country %in% Switzerland_names ~ "switzerland", 
                             country %in% Taiwan_names ~ "taiwan", 
                             country %in% UAE_names ~ "uae", 
                             TRUE  ~ as.character(country)))

head(fourth_clean_2017$country,500)

# fifth_clean_2017 <- fourth_clean_2017 %>%
# mutate(country = case_when(
#    ...
#  )
#  )

unique_names_country_2017 <- sort(unique(fourth_clean_2017$country))
unique_names_country_2017 # Check that it has worked for all versions of USA



#fourth_clean_2017 <- third_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in% Australia_names, "Australia", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  Canada_names, "Canada", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  Finland_names, "Finland", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  France_names, "France", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  Germany_names, "Germany", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  Hong_Kong_names, "Hong Kong", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  Netherlands_names, "Netherlands", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  South_Africa_names, "South Africa", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  Spain_names, "Spain", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  UK_names, "UK", third_clean_2017$country))
#fourth_clean_2017 <- fourth_clean_2017 %>% 
#  mutate (country = ifelse(third_clean_2017$country %in%  USA_names, "USA", third_clean_2017$country))






# 

third_clean_2017 <- third_clean_2017 %>% 
  mutate ( country = str_to_lower(third_clean_2017$country))

# unique_names_country <- sort(unique(third_clean_2017$country))
# unique_names_country

USA_names <-  c("'merica","AhemAmerca","america","America","California",
                "I pretend to be from Canada, but I am really from the United States",
                "Murica","murrika","N America","New Jersey","New York","North Carolina",
                "Pittsburgh","The United States","The United States of America","Trumpistan",
                "U S","u s a","Unied States","unite states","United Sates","United staes",
                "United State","United Statea","United Stated","united states","united States",
                "United states","United States","united states of america","United States of America",
                "United Statss","united ststes","United ststes","Unites States","us","Us","US","US of A",
                "usa","Usa","USa","USA","USA USA USA!!!!","USA! USA! USA!","USA? Hard to tell anymore",
                "USAA","usas","USAUSAUSA","USSA") 

fourth_clean_2017 <- third_clean_2017 %>% 
  mutate (country = ifelse(third_clean_2017$country %in%  USA_names, 'USA', third_clean_2017$country))



fourth_clean_2017







# below text might need to be edted to fourth clean rather than third clean


  
# changed numbers in country column to NA
fourth_clean_2016$country <- gsub('[0-9.]', '', fourth_clean_2016$country) 

fourth_clean_2016 <- fourth_clean_2016 %>% 
  mutate(country = na_if(country, ""))


fourth_clean_2017$country <- gsub('[0-9.]', '', fourth_clean_2017$country) 

fourth_clean_2017 <- fourth_clean_2017 %>% 
  mutate(country = na_if(country, ""))


# removing other random answers from country - 2016

fifth_clean_2016 <- fourth_clean_2016 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = case_when(
    country == "A tropical island south of the equator"  | 
      country == "Denial" |
      country == "god's country" |
      country == "Neverland" |
      country == "Not the USA or Canada" |
      country == "one of the best ones" |
      country == "See above" |
      country == "Somewhere" |
      country == "there isn't one for old men" |
      country == "this one"  ~ NA_character_, 
    TRUE ~ country
  ))



# Katrina's code for 2016:

# changed numbers in country column to NA
third_clean_2016$country <- gsub('[0-9.]', '', third_clean_2016$country)
third_clean_2016 <- third_clean_2016 %>%
  mutate(country = na_if(country, ""))

# cleaning country column
third_clean_2016 <- third_clean_2016 %>%
  mutate ( country = str_to_lower(third_clean_2016$country))
unique_names_country <- sort(unique(third_clean_2016$country))
unique_names_country
fourth_clean_2016 <- third_clean_2016 %>%
  mutate(country = as.character(country)) %>%
  mutate(country = case_when(
    country == "a tropical island south of the equator"  |
      country == "cascadia" |
      country == "denial" |
      country == "god's country" |
      country == "there isn't one for old men" |
      country == "neverland" |
      country == "not the usa or canada" |
      country == "one of the best ones" |
      country == "see above" |
      country == "somewhere" |
      country == "the republic of cascadia" |
      country == "this one"
    ~ NA_character_,
    TRUE ~ country
  ))
Belgium_names <- c("belgium")
Canada_names <- c("canada")
Croatia_names <- c("croatia")
UK_names <- c("england", "uk", "united kindom", "united kingdom")
Spain_names <- c("españa")
France_names <- c("france")
Germany_names <- c("germany")
Hungary_names <- c("hungary")
Kenya_names <- c("kenya")
Netherlands_names <- c("the netherlands","netherlands")
Sweden_names <- c("sweden")
Australia_names <- c("australia")
Austria_names <- c("austria")
Brasil_names <- c("brasil")
China_names <- c("china")
Finland_names <- c("finland")
Japan_names <- c("japan")
Korea_names <- c("korea")
Mexico_names <- c("mexico")
New_Zealand_names <- c("new zealand")
Panama_names <- c("panama")
Philippines_names <- c("philippines")
Portugal_names <- c("portugal")
South_Korea_names <- c("south korea")
Switzerland_names <- c("switzerland")
USA_names <-  c("the yoo ess of aaayyyyyy", "eua", "sub-canadian north america 'merica",
                "trumpistan" ,
                "u.s.",  "merica",  "murica",
                "Sub-Canadian North America 'Merica",
                "united  states of america",
                "america",  "the best one - usa",
                "united sates",
                "united states" ,
                "united stetes",
                "us", "usa usa usa",
                "usa usa usa usa",
                "usa! usa!",
                "usa!!!!!!",
                "u.s.a.", "usa! usa! usa!",
                "united state",
                "united states of america",
                "units states",
                "usa", "usa!",
                "ussa","usa (i think but it's an election year so who can really tell)")
fifth_clean_2016_test <- fourth_clean_2016 %>% mutate(country = case_when(country %in% Belgium_names ~ "Belgium",
                                                                          country %in% Canada_names ~ "Canada",
                                                                          country %in% Croatia_names ~ "Croatia",
                                                                          country %in% UK_names ~ "UK",
                                                                          country %in% Spain_names ~ "Spain",
                                                                          country %in% France_names ~ "France",
                                                                          country %in% Germany_names ~ "Germany",
                                                                          country %in% Hungary_names ~ "Hungary",
                                                                          country %in% Kenya_names ~ "Kenya",
                                                                          country %in% Netherlands_names ~ "Netherlands",
                                                                          country %in% Sweden_names ~ "Sweden",
                                                                          country %in% Australia_names ~ "Australia",
                                                                          country %in% Austria_names ~ "Austria",
                                                                          country %in% Brasil_names ~ "Brasil",
                                                                          country %in% China_names ~ "China",
                                                                          country %in% Finland_names ~ "Finland",
                                                                          country %in% Japan_names ~ "Japan",
                                                                          country %in% Korea_names ~ "Korea",
                                                                          country %in% Mexico_names ~ "Mexico",
                                                                          country %in% New_Zealand_names ~ "New Zealand",
                                                                          country %in% Panama_names  ~ "Panama",
                                                                          country %in% Philippines_names ~ "Philippines",
                                                                          country %in% Portugal_names ~ "Portugal",
                                                                          country %in% South_Korea_names ~ "South Korea",
                                                                          country %in% Switzerland_names ~ "Switzerland",
                                                                          country %in% USA_names ~ "USA",
                                                                          TRUE  ~ as.character(country)))
fifth_clean_2016_test %>%
  count(is.na(country))
third_clean_2016 %>%
  count(is.na(country))
sort(unique(fifth_clean_2016_test$country))





# My code for 2017 as a fresh section:

# changed numbers in country column to NA
third_clean_2016$country <- gsub('[0-9.]', '', third_clean_2016$country)
third_clean_2016 <- third_clean_2016 %>%
  mutate(country = na_if(country, ""))

third_clean_2017 <- third_clean_2017 %>% 
  mutate ( country = str_to_lower(third_clean_2017$country))

unique_names_country <- sort(unique(third_clean_2017$country))
unique_names_country

fourth_clean_2017 <- third_clean_2017 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = case_when(
      country == "a"  | 
      country == "atlantis" |
      country == "canae" |
      country == "cascadia" |
      country == "earth" |
      country == "europe" |
      country == "fear and loathing" |
      country == "i don't know anymore" |
      country == "insanity lately" |
      country == "narnia" |
      country == "subscribe to dmuz on youtube" |
      country == "ud" | 
      country == "unhinged states" 
      ~ NA_character_, 
    TRUE ~ country
  ))

Australia_names <- c("australia")
Canada_names <- c("can","canada","canada`","soviet canuckstan")
Finland_names <- c("finland")
France_names <- c("france")
Germany_names <- c("germany")
Hong_Kong_names <- c("hong kong")
Netherlands_names <- c("the netherlands","netherlands")
South_Africa_names <- c("south africa")
Spain_names <- c("spain")
UK_names <- c("uk","united kingdom","scotland","endland","england")
USA_names <-  c("alaska","'merica","ahemamerca","america","california",
                "i pretend to be from canada, but i am really from the united states",
                "murica","murrika","n america","new jersey","new york","north carolina",
                "pittsburgh","the united states","the united states of america","trumpistan",
                "u s","u s a","unied states","unite states","united sates","united staes",
                "united state","united statea","united stated","united states","united states of america",
                "united statss","united ststes","unites states","us","us of a",
                "usa","usa usa usa!!!!","usa! usa! usa!","usa? hard to tell anymore..",
                "usaa","usas","usausausa","ussa","u.s.a.")
China_names <- c("china")
Costa_Rica_names <- c("costa rica")
Denmark_names <- c("denmark")
Greece_names <- c("greece")
Iceland_names <- c("iceland")
Indonesia_names <- c("indonesia")
Ireland_names <- c("ireland")
Japan_names <- c("japan")
Mexico_names <- c("mexico")
Singapore_names <- c("singapore")
South_Korea_names <- c("korea","south korea")
Sweden_names <- c("sweden")
Switzerland_names <- c("switzerland")
Taiwan_names <- c("taiwan")
UAE_names <- c("uae")

fifth_clean_2017_test <- fourth_clean_2017 %>% 
  mutate(country = case_when(country %in% Australia_names ~ "Australia",
                             country %in% Canada_names ~ "Canada", 
                             country %in% Finland_names ~ "Finland",
                             country %in% France_names ~ "France", 
                             country %in% Germany_names ~ "Germany", 
                             country %in% Hong_Kong_names ~ "Hong Kong",
                             country %in% Netherlands_names ~ "Netherlands", 
                             country %in% South_Africa_names ~ "South Africa", 
                             country %in% Spain_names ~ "Spain",
                             country %in% UK_names ~ "UK", 
                             country %in% USA_names ~ "USA",
                             country %in% China_names ~ "China",
                             country %in% Costa_Rica_names ~ "Costa Rica",
                             country %in% Denmark_names ~ "Denmark",
                             country %in% Greece_names ~ "Greece",
                             country %in% Iceland_names ~ "Iceland",
                             country %in% Indonesia_names ~ "Indonesia",
                             country %in% Ireland_names ~ "Ireland", 
                             country %in% Japan_names ~ "Japan", 
                             country %in% Mexico_names ~ "Mexico", 
                             country %in% Singapore_names ~ "Singapore", 
                             country %in% South_Korea_names ~ "South Korea", 
                             country %in% Sweden_names ~ "Sweden", 
                             country %in% Switzerland_names ~ "Switzerland", 
                             country %in% Taiwan_names ~ "Taiwan", 
                             country %in% UAE_names ~ "UAE", 
                             TRUE  ~ as.character(country)))

head(fourth_clean_2017$country,500)

fifth_clean_2017_test %>%
  count(is.na(country))

third_clean_2017 %>%
  count(is.na(country))

sort(unique(fifth_clean_2017_test$country))






## deleted irrelevant columns

second_clean_2017 <- first_clean_2017[ -c(1,12,21,22,26,27,31,43,46,48,49,68,69,72,81,92,96,104,105,107,108,112:120) ]
second_clean_2017


## standardising and simplifying column names

third_clean_2017 <- second_clean_2017 %>% 
  rename(
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
    box_o_raisins = boxo_raisins,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor,
    peanut_m_ms = peanut_m_m_s,
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    boo_berry_crunch = sandwich_sized_bags_filled_with_boo_berry_crunch,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    odd_marshmallow_peanuts = those_odd_marshmallow_circus_peanut_things,
    toblerone = tolberone_something_or_other
  )


#convert age to from character to number

third_clean_2017$age <- as.integer(second_clean_2017$age)


# remove the q1 etc from the 2017 column titles

colnames(second_clean_2017) <- gsub("^.{3}", "", colnames(second_clean_2017))




# changed numbers in country column to NA

third_clean_2017$country <- gsub('[0-9.]', '', third_clean_2017$country) 

third_clean_2017 <- third_clean_2017 %>% 
  mutate(country = na_if(country, ""))



# searching for unique entries in 2017 country column to clean

sort(unique(third_clean_2017$country))



# cleaning country column

third_clean_2017 <- third_clean_2017 %>% 
  mutate ( country = str_to_lower(third_clean_2017$country))

unique_names_country <- sort(unique(third_clean_2017$country))
unique_names_country

fourth_clean_2017 <- third_clean_2017 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = case_when(
    country == "a"  | 
      country == "atlantis" |
      country == "canae" |
      country == "cascadia" |
      country == "earth"|
      country == "europe"|
      country ==  "fear and loathing"|
      country ==  "i don't know anymore"|
      country ==  "insanity lately"|
      country ==  "narnia"|
      country ==  "subscribe to dmuz on youtube"|
      country ==  "ud"|
      country ==  "unhinged states" ~ NA_character_, 
    TRUE ~ country
  ))

sort(unique(fourth_clean_2017$country))

Australia_names <- c("australia")
Canada_names <- c("canada","canada`", "soviet canuckistan", "can")
Finland_names <- c("finland")
France_names <- c("france")
Germany_names <- c("germany")
Hong_Kong_names <- c("hong kong")
Netherlands_names <- c("the netherlands","netherlands")
South_Africa_names <- c("south africa")
Spain_names <- c("spain")
UK_names <- c("uk","united kingdom","scotland","endland","england")

USA_names <-  c("alaska","'merica","ahemamerca","america","california",
                "i pretend to be from canada, but i am really from the united states",
                "murica","murrika","n america","new jersey","new york","north carolina",
                "pittsburgh","the united states","the united states of america","trumpistan",
                "u s","u s a","unied states","unite states","united sates","united staes",
                "united state","united statea","united stated","united states","united states of america",
                "united statss","united ststes","unites states","us","us of a",
                "usa","usa usa usa!!!!","usa! usa! usa!","usa? hard to tell anymore",
                "usaa","usas","usausausa","ussa","u.s.a.")

# Now all the countries with one entry which are correct but need to be added due to the lower case

China_names <- c("china")
Costa_Rica_names <- c("costa rica")
Denmark_names <- c("denmark")
Greece_names <- c("greece")
Iceland_names <- c("iceland")
Indonesia_names <- c("indonesia")
Ireland_names <- c("ireland")
Japan_names <- c("japan")
Mexico_names <- c("mexico")
Singapore_names <- c("singapore")
South_Korea_names <- c("korea","south korea")
Sweden_names <- c("sweden")
Switzerland_names <- c("switzerland")
Taiwan_names <- c("taiwan")
UAE_names <- c("uae")

fifth_clean_2017 <- fourth_clean_2017 %>% 
  mutate(country = case_when(country %in% Australia_names ~ "Australia",
                             country %in% Canada_names ~ "Canada", 
                             country %in% Finland_names ~ "Finland",
                             country %in% France_names ~ "France", 
                             country %in% Germany_names ~ "Germany", 
                             country %in% Hong_Kong_names ~ "Hong Kong",
                             country %in% Netherlands_names ~ "Netherlands", 
                             country %in% South_Africa_names ~ "South Africa", 
                             country %in% Spain_names ~ "Spain",
                             country %in% UK_names ~ "UK", 
                             country %in% USA_names ~ "USA",
                             country %in% China_names ~ "China",
                             country %in% Costa_Rica_names ~ "Costa Rica",
                             country %in% Denmark_names ~ "Denmark",
                             country %in% Greece_names ~ "Greece",
                             country %in% Iceland_names ~ "Iceland",
                             country %in% Indonesia_names ~ "Indonesia",
                             country %in% Ireland_names ~ "Ireland", 
                             country %in% Japan_names ~ "Japan", 
                             country %in% Mexico_names ~ "Mexico", 
                             country %in% Singapore_names ~ "Singapore", 
                             country %in% South_Korea_names ~ "South Korea", 
                             country %in% Sweden_names ~ "Sweden", 
                             country %in% Switzerland_names ~ "Switzerland", 
                             country %in% Taiwan_names ~ "Taiwan", 
                             country %in% UAE_names ~ "UAE", 
                             TRUE  ~ as.character(country)))

# I discovered later that I could have just used this for the countries which are already written correctly:
# fifth_clean_2016 <- fifth_clean_2016 %>% 
#  mutate(country = case_when(country == "NA" ~ NA_character_, 
#                             TRUE ~ country))

sort(unique(fifth_clean_2017$country))


write.csv(clean_data_2015, "clean_data/clean_data_2015", row.names = FALSE)
clean_data_2015 

write.csv(clean_data_2016, "clean_data/clean_data_2016", row.names = FALSE)
clean_data_2016 

write.csv(clean_data_2017, "clean_data/clean_data_2017", row.names = FALSE)
clean_data_2017 











# Complete script as a separate chunk from here down:
# I would normally delete all of the above (or keep in a separate file) but keeping here so that people
# can see the work or thought processes. 

library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(dplyr)

# Importing our raw data

candy_2015 <- read_excel(here::here('raw_data/boing-boing-candy-2015.xlsx'))
candy_2016 <- read_excel(here::here('raw_data/boing-boing-candy-2016.xlsx'))
candy_2017 <- read_excel(here::here('raw_data/boing-boing-candy-2017.xlsx'))

# performing the first clean on the columns names

first_clean_2015 <- clean_names(candy_2015)
first_clean_2016 <- clean_names(candy_2016)
first_clean_2017 <- clean_names(candy_2017)

names(first_clean_2015)

col_names_2015 <- colnames(first_clean_2015)
col_names_2016 <- colnames(first_clean_2016)
col_names_2017 <- colnames(first_clean_2017)

# extracting all the clean columns names

col_names_2015
col_names_2016
col_names_2017

# performing the second cleam by removing information that are not related to candy. # df <- mydata[ -c(1,3:4) ]

second_clean_2015 <- first_clean_2015[ -c(16,18,23,26,33,38,41,45,57,69,71,82,85,88,93,94,95,97:114,116:124) ]
second_clean_2015

second_clean_2016 <- first_clean_2016[ -c(21,22,26,27,31,46,48,49,71,79,90,94,101,102,104,105,107:122) ]
second_clean_2016

second_clean_2017 <- first_clean_2017[ -c(12,21,22,26,27,31,43,46,48,49,68,69,72,81,92,96,104,105,107,108,112:120) ]
second_clean_2017

col_names_reduced_2015 <- colnames(second_clean_2015)
col_names_reduced_2016 <- colnames(second_clean_2016)
col_names_reduced_2017 <- colnames(second_clean_2017) 

col_names_2015 <- colnames(second_clean_2015)
col_names_2016 <- colnames(second_clean_2016)
col_names_2017 <- colnames(second_clean_2017)



col_names_reduced_2015 ## 80 columns remain
col_names_reduced_2016 ## 91 columns remain
col_names_reduced_2017 ## 91 columns remain

# converting the date column to yyyy/mm/dd only with time included.

second_clean_2015$timestamp <- as.Date(second_clean_2015$timestamp)
second_clean_2015$year <- as.numeric(format(second_clean_2015$timestamp, "%Y"))




# renaming some columns that were too long or inconsistent across the datasets

##2015 data
second_clean_2015 <- rename( second_clean_2015, age = how_old_are_you, going_out = are_you_going_actually_going_trick_or_treating_yourself,
                             mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
                             bonkers_the_candy = bonkers, brach_product = brach_products_not_including_candy_corn,
                             free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
                             hersheys_kisses = hershey_s_kissables,  hersheys_milk_chocolate = hershey_s_milk_chocolate,
                             jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor, reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
                             toblerone = tolberone_something_or_other, peanut_m_ms = peanut_m_m_s,
                             chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
                             odd_marshmallow_peanut = those_odd_marshmallow_circus_peanut_things)

third_clean_2015 <- second_clean_2015
names(third_clean_2015)

##2016 data

third_clean_2016 <- second_clean_2016 %>% 
  rename(
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    gender = your_gender,
    age = how_old_are_you,
    country = which_country_do_you_live_in,
    state_province_county_etc = which_state_province_county_do_you_live_in,
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
    box_o_raisins = boxo_raisins,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor,
    peanut_m_ms = peanut_m_m_s,
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    odd_marshmallow_peanuts = those_odd_marshmallow_circus_peanut_things,
    toblerone = tolberone_something_or_other
  )
names(third_clean_2016)

## 2017 data


colnames(second_clean_2017) <- gsub("^.{3}", "", colnames(second_clean_2017))

#the above code removes  the 3 first characters from 2017 dataset the columns names, commenting this it so i don't run it a second time.
second_clean_2017 <- rename( second_clean_2017, internal_id = ernal_id)

names(second_clean_2017)
third_clean_2017 <- second_clean_2017 %>%
  rename(
    ##  x100_grand_bar = 100_grand_bar,
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
    box_o_raisins = boxo_raisins,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor,
    peanut_m_ms = peanut_m_m_s,
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    boo_berry_crunch = sandwich_sized_bags_filled_with_boo_berry_crunch,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    odd_marshmallow_peanuts = those_odd_marshmallow_circus_peanut_things,
    toblerone = tolberone_something_or_other
  )
names(third_clean_2017)

## converting age column to integer

third_clean_2015$age <- as.integer(third_clean_2015$age)
third_clean_2016$age <- as.integer(third_clean_2016$age) # Some NAs have been introduced here but they look correct
third_clean_2017$age <- as.integer(third_clean_2017$age) # Some NAs have been intrduced here but they look correct

# Cleaning the country column in 2016 dataset

third_clean_2016 <- third_clean_2016 %>% 
  mutate ( country = str_to_lower(third_clean_2016$country))

unique_names_country <- sort(unique(third_clean_2016$country))
unique_names_country

## changing USA names to USA

USA_names <-  c("the yoo ess of aaayyyyyy",                                                                         
                "trumpistan" ,  "sub-canadian north america... 'merica",                                                 
                "u.s.",  "merica",  "murica",                                                                                                               
                "united  states of america",                                   
                "america",  "the best one - usa",                                           
                "united sates",                                                                                                     
                "united states" ,                                                                                      
                "united stetes",                                                                                                  
                "us", "usa usa usa",                                                                                                              
                "usa usa usa usa",                                                                                                          
                "usa! usa!", "eua",                                                                                                    
                "usa!!!!!!",                                                                                                              
                "u.s.a.", "usa! usa! usa!", 
                "united state",
                "united states of america", 
                "units states",
                "usa", "usa!",
                "ussa","usa (i think but it's an election year so who can really tell)" ) 

fourth_clean_2016 <- third_clean_2016 %>% 
  mutate (country = ifelse(third_clean_2016$country %in%  USA_names, 'USA', third_clean_2016$country))

unique_names_country_4 <- sort(unique(fourth_clean_2016$country))

##changing odd names to NA

odd_names <- c( "30.0", "47.0", "44.0", "51.0", "45.0","54.0", "a tropical island south of the equator","cascadia","denial",
                "see above", "the republic of cascadia", "not the usa or canada", "god's country", "there isn't one for old men",
                "one of the best ones", "neverland",  "somewhere", "this one")

fourth_clean_2016_test <- fourth_clean_2016 %>% 
  mutate (country = ifelse(fourth_clean_2016$country %in%  odd_names, 'NA', fourth_clean_2016$country))

unique_names_country_5 <- sort(unique(fourth_clean_2016_test$country))

## changing uk names to UK

uk_names <- c("united kindom","united kingdom", "england", "uk")

fourth_clean_2016_test_1 <- fourth_clean_2016_test %>% 
  mutate (country = ifelse(fourth_clean_2016_test$country %in%  uk_names, 'UK', fourth_clean_2016_test$country))

unique_names_country_5 <- sort(unique(fourth_clean_2016_test_1$country))

## moving the dataframe to our fith clean.

fifth_clean_2016 <- fourth_clean_2016_test_1

# Cleaning the Country column in 2017 dataset

third_clean_2017
third_clean_2017 <- third_clean_2017 %>% 
  mutate ( country = str_to_lower(third_clean_2017$country))

unique_names_country <- sort(unique(third_clean_2017$country))

unique_names_country

## country names for 2017

Australia_names <- c("australia")
Canada_names <- c("can","canada","canada`","soviet canuckistan")
Finland_names <- c("finland")
France_names <- c("france")
Germany_names <- c("germany")
Hong_Kong_names <- c("hong kong")
Netherlands_names <- c("the netherlands","netherlands")
South_Africa_names <- c("south africa")
Spain_names <- c("spain")
UK_names <- c("uk","united kingdom","scotland","endland","england","u.k.")
USA_names_17 <-  c("'merica","ahem....amerca","america","california",
                   "i pretend to be from canada, but i am really from the united states.",
                   "murica","murrika","n america","new jersey","new york","north carolina",
                   "pittsburgh","the united states","the united states of america","trumpistan",
                   "u s","u s a","unied states","unite states","united sates","united staes",
                   "united state","united statea","united stated","united states","united states of america","united states of america",
                   "united statss","united ststes","united ststes","unites states","us","us of a",
                   "usa","usa usa usa!!!!","usa! usa! usa!","usa? hard to tell anymore..",
                   "usaa","usas","usausausa","ussa","u.s.a.", "unhinged states", "u.s.", "uae",  "alaska", "n. america")

odd_names_17 <- c("1","32", "35", "45", "46", "a", "atlantis", "canae", "cascadia",
                  "narnia", "europe", "i don't know anymore", "earth", "insanity lately",
                  "subscribe to dm4uz3 on youtube", "ud", "fear and loathing" )


fifth_clean_2017<- third_clean_2017 %>% 
  mutate(country = case_when(country %in% Australia_names ~ "Australia", 
                             country %in% Canada_names ~ "Canada", 
                             country %in% Finland_names ~ "Finland",
                             country %in% France_names ~ "France", 
                             country %in% Germany_names ~ "Germany", 
                             country %in% Hong_Kong_names ~ "Hong Kong",
                             country %in% Netherlands_names ~ "Netherlands", 
                             country %in% South_Africa_names ~ "South Africa", 
                             country %in% Spain_names ~ "Spain",
                             country %in% UK_names ~ "UK", 
                             country %in% USA_names_17 ~ "USA",
                             country %in%  odd_names_17 ~ "NA",
                             TRUE  ~ as.character(country)))

unique_names_country_t <- sort(unique(fifth_clean_2017$country))

# more updates to 2016 columns
fifth_clean_2016
names(fifth_clean_2016)

fifth_clean_2016$timestamp <- as.Date(fifth_clean_2016$timestamp)
fifth_clean_2016$year <- as.numeric(format(fifth_clean_2016$timestamp, "%Y"))
head(fifth_clean_2016)

na_2017_coutry <-  fifth_clean_2016[is.na(fifth_clean_2016$country), ] # checking number of NA
na_2017_coutry <-  subset(fifth_clean_2016, country == "NA") # checking number of "NA"

fifth_clean_2016 <- fifth_clean_2016 %>% 
  mutate(country = case_when(country == "NA" ~ NA_character_, 
                             TRUE ~ country))

fifth_clean_2016 <- fifth_clean_2016 %>% 
  mutate (country =  str_to_title(country))

fifth_clean_2016 <- fifth_clean_2016 %>% 
  mutate(country = case_when(country == "Usa" ~ "USA",
                             country == "Uk" ~ "UK",
                             TRUE ~ country))

fifth_clean_2016

sixth_clean_2016 <-  select (fifth_clean_2016, - state_province_county_etc) 
# Not sure if the space matters here when removing a column, copied and pasted directly and it has removed



# more updates to 2017 columns


fifth_clean_2017 <-  clean_names(fifth_clean_2017)

fifth_clean_2017 <- fifth_clean_2017 %>% 
  mutate(year = 2017)
fifth_clean_2017

na_2017_coutry <-  fifth_clean_2017[is.na(fifth_clean_2017$country), ] # checking number of NA
na_2017_coutry <-  subset(fifth_clean_2017, country == "NA") # checking number of "NA"

fifth_clean_2017 <- fifth_clean_2017 %>% 
  mutate(country = case_when(country == "NA" ~ NA_character_, 
                             TRUE ~ country))

fifth_clean_2017 <- fifth_clean_2017 %>% 
  mutate (country =  str_to_title(country))

fifth_clean_2017 <- fifth_clean_2017 %>% 
  mutate(country = case_when(country == "Usa" ~ "USA",
                             country == "Uk" ~ "UK",
                             TRUE ~ country))

unique_names_country_t <- sort(unique(fifth_clean_2017$country))

## removing the last 2 columns because they are not consistent with the rest of columns

sixth_clean_2017 <-  select (fifth_clean_2017, -joy_other, -despair_other, -state_province_county_etc)

clean_data_2015 <- third_clean_2015
clean_data_2016 <- sixth_clean_2016
clean_data_2017 <- sixth_clean_2017

write.csv(clean_data_2015, "clean_data/clean_data_2015", row.names = FALSE)
clean_data_2015 

write.csv(clean_data_2016, "clean_data/clean_data_2016", row.names = FALSE)
clean_data_2016 

write.csv(clean_data_2017, "clean_data/clean_data_2017", row.names = FALSE)
clean_data_2017 

na_2015_age <-  third_clean_2015[is.na(third_clean_2015$age), ]
na_2016_age <-  fifth_clean_2016[is.na(fifth_clean_2016$age), ]
na_2017_age <-  third_clean_2017[is.na(third_clean_2017$age), ]

na_2015_age # 285 NAs for age in 2015 data
na_2016_age # 68 NAs for age in 2016 data
na_2017_age # 108 NAs for age in 2017 data










