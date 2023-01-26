## Author:        Virginia Sanson


getwd()

install.packages("tidyverse")
library(tidyverse)


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)


## Build plan-level dataset

source("data-code/1_Plan_Data.R")
source("data-code/2_Plan_Characteristics.R")
source("data-code/3_Service_Areas.R")
source("data-code/4_Penetration_Files.R")



## Organize final data
full.ma.data <- read_rds("data/output/full_ma_data.rds")
contract.service.area <- read_rds("data/output/contract_service_area.rds")
ma.penetration.data <- read_rds("data/output/ma_penetration.rds")
plan.premiums <- read_rds("data/output/plan_premiums.rds")



#create objects

tot.obs <- as.numeric(count(full.ma.data %>% ungroup()))
#question1
tot.obs

plan.type.table <- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)
plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n) 
plan.type.year1 <- pivot_wider(plan.type.year1, names_from = "year", values_from = "n", names_prefix = "Count_")

#question2
nrow(plan.type.table)

#question3
view(plan.type.year1)

#question4
final.plans <- full.ma.data %>%
  filter(snp== 'No' & eghp == "No" &
           (planid < 800 | planid >= 900))

plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year,-n)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from = "year", values_from = "n", names_prefix = "Count_")
view(plan.type.year2)

#question 5
final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment)
         
#nrow(final.plans)
         
         
#question 6 
         fig.avg.enrollment <- final.data %>%
           group_by(fips, year) %>%
           select(fips, year, avg_enrollment) %>%
           summarize(all_enroll=sum(avg_enrollment))
         
         %>%
           ggplot(aes(x=as.factor(year), y=all_enroll)) +
           stat_summary(fun.y = "mean", geom="bar") +
           labs(
             x="Year",
             y="People", 
             title =""
           ) +scale_y_continuous(labels=comma) +
           theme_bw()
         
         
         fig.avg.enrollment
         
         
         
         save.image("Hwk1_workspace.Rdata")