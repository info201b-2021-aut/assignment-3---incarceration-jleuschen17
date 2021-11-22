data <- data
#Introduction: The United States prison system is a far from perfect institution. With problems ranging from racial disparity to overcrowded jails, the system as a whole has been a topic of debate in recent years. 
#One area of concern is the rates of incarceration across different races and ethnicities. The Vera Institute of Justice Prison Dataset is a useful tool in answering questions regarding incarceration rates across 
#different racial and ethnic backgrounds. Specifically data relating to prison population and ethnic makeup are especially useful. For this project I will be analyzing the **INSERT NAMES HERE** features
#of the Vera Institute data set
library(dplyr)
library(tidyr)
library(knitr)
summary_info <- list()

filtered_data <- filter(data, !is.na(black_jail_pop), !is.na(total_jail_pop), !is.na(male_jail_pop), !is.na(female_jail_pop), !is.na(male_juvenile_jail_pop), !is.na(female_juvenile_jail_pop), year == 2016)
summary_info$black_prop <- summarise(filtered_data, black_prop = sum(black_jail_pop) / sum(total_jail_pop))$black_prop[1]
summary_info$male_prop <- summarise(filtered_data, male_prop = sum(male_jail_pop) / sum(total_jail_pop))$male_prop[1]
summary_info$female_prop <- summarise(filtered_data, female_prop = sum(female_jail_pop) / sum(total_jail_pop))$female_prop[1]
summary_info$child_num <- summarise(filtered_data, child_num = sum(male_juvenile_jail_pop), + sum(female_juvenile_jail_pop))$child_num[1]
summary_info$num_overcrowded_jails <- nrow(filter(data, jail_rated_capacity < total_jail_pop))