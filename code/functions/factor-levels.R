library(glue)
# note missing 7, 13
intervention_approach_levels = 
  c("1" = "cooperative learning",
    "2" = "entertainment",
    "3" = "peer influence, discussion/dialogue",
    "4" = "interpersonal contact",
    "5" = "extended and imaginary contact",
    "6" = "value consistency and self-worth",
    "7" = "cross-cultural/intercultural training",
    "8" = "social categorization",
    "9" = "cognitive and emotional training",
    "10" = "diversity training",
    "11" = "multicultural, antibias, moral education",
    "12" = "sensitivity, cultural competence for health and law",
    "13" = "conflict resolution",
    "14" = "other")

intevention_type_levels = 
  c("1" = "lab",
    "2" = "field",
    "3" = "online") # "survey/online"

time_measurement_levels = 
  c(`-1` = "coder mistake",
    `1` = "same day",
    `2` = "day after or more")

prejudice_type_levels = 
  c("1" = "race/ethnicity",
    "2" = "nationality",
    "3" = "immigrants/asylum seekers/refugees",
    "4" = "income/SES",
    "5" = "age",
    "6" = "sexuality",
    "7" = "body size",
    "8" = "ability",
    "9" = "transgender / genderqueer",
    "10" = "minimal/artificial",
    "11" = "religion",
    "12" = "other")

light_touch_levels = c(`0` = "regular",
                       `1` = "light touch")



setting_levels = c("1" = "preschool / daycare / kindergarten",
                   "2" = "elementary (grades 1-5)",
                   "3" = "middle/high (grades 6-12)",
                   "4" = "college",
                   "5" = "work",
                   "6" = "community",
                   "7" = "faith-based",
                   "8" = "other",
                   "9" = "Online (mturk or other)")

outcome_type_levels = 
  c("1" = "behavior",
    "2" = "implicit attitudes",
    "3" = "explicit attitudes OR beliefs",
    "4" = "perceived norms",
    "6" = "emotion",
    "7" = "empathy",
    "8" =  "behavioral intentions",
    "9" = "MEDIATOR")


qi <- function(x){
  quantile(x = dat$n_treatment, probs =  c(x))
}


group_5_levels = c("< 20%",
                   ">= 20% - 39%",
                   ">= 40% - 59%",
                   ">= 60% - 79%",
                   ">= 80%")

group_6_levels = c(glue("< {round(1/6, 2)}%"),
                   glue(">= {round(1/6, 2)}% - {round(2/6, 2) - .01}%"),
                   glue(">= {round(2/6, 2)}% - {round(3/6, 2) - .01}%"),
                   glue(">= {round(3/6, 2)}% - {round(4/6, 2) - .01}%"),
                   glue(">= {round(4/6, 2)}% - {round(5/6, 2) - .01}%"),
                   glue(">= {round(5/6, 2)}%"))
