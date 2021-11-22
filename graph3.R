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


usmap_plot
