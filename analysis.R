data <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

#Introduction: The United States prison system is a far from perfect institution. With problems ranging from racial disparity to overcrowded jails, the system as a whole has been a topic of debate in recent years. 
#One area of concern is the rates of incarceration across different races and ethnicities. The Vera Institute of Justice Prison Dataset is a useful tool in answering questions regarding incarceration rates across 
#different racial and ethnic backgrounds. Specifically data relating to prison population and ethnic makeup are especially useful. For this project I will be analyzing the **INSERT NAMES HERE** features
#of the Vera Institute data set
library(dplyr)
library(tidyr)
library(knitr)

filtered_data <- filter(data, !is.na(black_jail_pop), !is.na(total_jail_pop), !is.na(latinx_jail_pop), !is.na(white_jail_pop), year == 2016)
filtered_data_1985 <- filter(data, !is.na(black_jail_pop), !is.na(total_jail_pop), !is.na(latinx_jail_pop), !is.na(white_jail_pop), year == 1985)

black_prop_jail <- round(summarise(filtered_data, black_prop = sum(black_jail_pop) / sum(total_jail_pop))$black_prop[1], 2)
white_prop_jail <- round(summarise(filtered_data, white_prop = sum(white_jail_pop) / sum(total_jail_pop))$white_prop[1], 2)
latinx_prop_jail <- round(summarise(filtered_data, latinx_prop = sum(latinx_jail_pop) / sum(total_jail_pop))$latinx_prop[1], 2)
blackVwhite_prop <- round((black_prop_jail * 5.7) / white_prop_jail, 2)
difference_in_pop <- round((summarize(filtered_data, total=sum(total_jail_pop))$total[1] / 
  summarize(filtered_data_1985, total=sum(total_jail_pop))$total[1]), 2) * 100
num_overcrowded_jails <- nrow(filter(data, jail_rated_capacity < total_jail_pop))

#Summary: After some initial analysis of the dataset I found a few metrics which help to paint a picture of the US prison system. First the breakdown of gender in prisons icnluded in the Vera Institute dataset indicated a significantly higher
#number of males are incarcerated compared to females--82.7 percent male, 13.1 percent female. Next, one area of prison systems which has been a major topic of discussion in recent years is incarceration rates across african american populations. 
#The data indicates approximatley 34% of incarceration are african american. Aside from gender and race, one interesting metric relating to Incarceration is the number of jails which qualify as overcrowded--over 23,000. Finally, 
#the data places the number of minors in prison at 2932 although this number is only counting prisons included in the dataset. 
#PLOT
library(ggplot2)
library(reshape2)

plot_data <- filter(data, !is.na(black_jail_pop), !is.na(white_jail_pop), !is.na(aapi_jail_pop), !is.na(native_jail_pop), !is.na(latinx_jail_pop), !is.na(total_jail_pop))
plot_data <- filter(plot_data, year > 1985)
plot_data <- group_by(plot_data, year)

plot_race_by_year <- function(state_name, plot_data) {
  plot_data_state <- filter(plot_data, state == state_name)
  plot_data_line <- summarize(plot_data_state, Black = sum(black_jail_pop), 
                              White = sum(white_jail_pop), 
                              AAPI = sum(aapi_jail_pop), 
                              Native = sum(native_jail_pop), 
                              Latinx = sum(latinx_jail_pop))
  plot_data_line <- select(plot_data_line, year, Black, White, AAPI, Native, Latinx)
  plot_data_line <- melt(plot_data_line, id='year')
  plot_title <- paste('Incarcerations in', state_name, 'by Racial Group')
  plot <- ggplot(plot_data_line, aes(x=year, y=value, colour=variable)) + geom_line() + ggtitle(plot_title) + 
    xlab('Year') + ylab('Incarcerations') + guides(fill=guide_legend(title="Racial Group")) + 
    theme(plot.title=element_text(size=8), legend.key.height=unit(4, 'mm'), legend.key.width=unit(5, 'mm'), 
          legend.title=element_text(size=7), legend.text=element_text(size=6), 
          axis.text.x=element_text(size=5), axis.text.y=element_text(size=5, angle=90), 
          axis.title.x=element_text(size=6), axis.title.y=element_text(size=6)) + labs(color='Racial Group')
  return(plot)
}

plot_usAvg_race_by_year <- function(plot_data) {
  plot_data_line <- summarize(plot_data, Black = sum(black_jail_pop), 
                              White = sum(white_jail_pop), 
                              AAPI = sum(aapi_jail_pop), 
                              Native = sum(native_jail_pop), 
                              Latinx = sum(latinx_jail_pop))
  plot_data_line <- select(plot_data_line, year, Black, White, AAPI, Native, Latinx)
  plot_data_line <- melt(plot_data_line, id='year')
  plot_title <- 'US Incarcerations by Racial Group'
  plot <- ggplot(plot_data_line, aes(x=year, y=value, colour=variable)) + geom_line() + ggtitle(plot_title) + 
    xlab('Year') + ylab('Incarcerations') + guides(fill=guide_legend(title="Racial Group")) + 
    theme(plot.title=element_text(size=8), legend.key.height=unit(4, 'mm'), legend.key.width=unit(5, 'mm'), 
          legend.title=element_text(size=7), legend.text=element_text(size=6), 
          axis.text.x=element_text(size=5), axis.text.y=element_text(size=5, angle=90), 
          axis.title.x=element_text(size=6), axis.title.y=element_text(size=6)) + labs(color='Racial Group')
  return(plot)
}

tx_plot <- plot_race_by_year('TX', plot_data) + theme(legend.position="none")
ca_plot <- plot_race_by_year('CA', plot_data)
fl_plot <- plot_race_by_year('FL', plot_data) + theme(legend.position="none")
ga_plot <- plot_race_by_year('GA', plot_data)
oh_plot <- plot_race_by_year('OH', plot_data) + theme(legend.position="none")
pa_plot <- plot_race_by_year('PA', plot_data)
ny_plot <- plot_race_by_year('NY', plot_data) + theme(legend.position="none")
az_plot <- plot_race_by_year('AZ', plot_data)
il_plot <- plot_race_by_year('IL', plot_data) + theme(legend.position="none")
mi_plot <- plot_race_by_year('MI', plot_data)
usAvg_plot <- plot_usAvg_race_by_year(plot_data)
################################3
plot_data_2 <- filter(data, !is.na(black_jail_pop), !is.na(white_jail_pop), !is.na(total_jail_pop))
plot_data_2 <- filter(plot_data_2, year == 2016)
plot_data_2 <- group_by(plot_data_2, state)

plot_data_2_prop <- summarize(plot_data_2, total = sum(total_jail_pop), 
                              Black = sum(black_jail_pop) / sum(total_jail_pop), 
                              White = sum(white_jail_pop) / sum(total_jail_pop))

plot_data_2_pop <- summarize(plot_data_2, total = sum(total_jail_pop), 
                             Black = sum(black_jail_pop), 
                             White = sum(white_jail_pop))

plot_data_2_standerdized <- mutate(plot_data_2, standerdized_white_jail_pop = white_jail_pop / 5.7)

plot_data_2_prop_standerdized <- summarize(plot_data_2_standerdized, total = sum(total_jail_pop), 
                                           Black = sum(black_jail_pop) / sum(total_jail_pop), 
                                           White = sum(standerdized_white_jail_pop) / sum(total_jail_pop))

plot_data_2_pop_standerdized <- summarize(plot_data_2_standerdized, total = sum(total_jail_pop), 
                                          Black = sum(black_jail_pop), 
                                          White = sum(standerdized_white_jail_pop))

plot_data_2_prop_graph <- pivot_longer(plot_data_2_prop, cols=c('Black', 'White'), 
                                       names_to='variable', values_to='value')

plot_data_2_pop_graph <- pivot_longer(plot_data_2_pop, cols=c('Black', 'White'), 
                                      names_to='variable', values_to='value')


plot_data_2_prop_standerdized_graph <- pivot_longer(plot_data_2_prop_standerdized, cols=c('Black', 'White'), 
                                                    names_to='variable', values_to='value')

plot_data_2_pop_standerdized_graph <- pivot_longer(plot_data_2_pop_standerdized, cols=c('Black', 'White'), 
                                                   names_to='variable', values_to='value')


prop_graph_states <- ggplot(plot_data_2_prop_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Proportion') + scale_fill_discrete(name='Racial Group') + ggtitle('Incarcertaion Rates Across US States') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size=4))

pop_graph_states <- ggplot(plot_data_2_pop_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Population') + scale_fill_discrete(name='Racial Group') + ggtitle('Incarcertaion Totals Across US States') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size=4))

prop_standerdized_graph_states <- ggplot(plot_data_2_prop_standerdized_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Standerdized Proportion') + scale_fill_discrete(name='Racial Group') + ggtitle('Standerdized Incarcertaion Rates Across US States') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size=4))

pop_standerdized_graph_states <- ggplot(plot_data_2_pop_standerdized_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Standerdized Population') + scale_fill_discrete(name='Racial Group') + ggtitle('Standerdized Incarcertaion Totals Across US States') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size=4))

####################################
library(usmap)
library(dplyr)
library(ggplot2)
library(patchwork)
usmap_data <- filter(data, !is.na(black_jail_pop), !is.na(total_jail_pop), year==2016)
usmap_data <- mutate(usmap_data, black_jail_prop = black_jail_pop / total_jail_pop)
usmap_data <- filter(usmap_data, black_jail_prop < 1.00)
usmap_plot <- plot_usmap(data = usmap_data, values='black_jail_prop') + 
  scale_fill_continuous(name = 'Black Incarceration Proportion', label=scales::comma) + 
  theme(legend.position = 'right') + ggtitle('US Black Incarceration Rates')
