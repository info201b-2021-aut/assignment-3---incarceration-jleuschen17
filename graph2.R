plot_data_2 <- filter(data, !is.na(black_jail_pop), !is.na(white_jail_pop), !is.na(total_jail_pop))
plot_data_2 <- filter(plot_data_2, year == 2016)
plot_data_2 <- group_by(plot_data_2, state)

plot_data_2_prop <- summarize(plot_data_2, total = sum(total_jail_pop), 
                              Black = sum(black_jail_pop) / sum(total_jail_pop), 
                              White = sum(white_jail_pop) / sum(total_jail_pop))

plot_data_2_pop <- summarize(plot_data_2, total = sum(total_jail_pop), 
                             Black = sum(black_jail_pop), 
                             White = sum(white_jail_pop))

plot_data_2_standerdized <- mutate(plot_data_2, standerdized_white_jail_pop = white_jail_pop / 6)

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


prop_graph_states <- ggplot(plot_data_2_prop_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Proportion') + scale_fill_discrete(name='Racial Group')
pop_graph_states <- ggplot(plot_data_2_pop_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Population') + scale_fill_discrete(name='Racial Group')

prop_standerdized_graph_states <- ggplot(plot_data_2_prop_standerdized_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Standerdized Proportion') + scale_fill_discrete(name='Racial Group')

pop_standerdized_graph_states <- ggplot(plot_data_2_pop_standerdized_graph, aes(x=state, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + xlab('State') + ylab('Standerdized Population') + scale_fill_discrete(name='Racial Group')