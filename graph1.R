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
    xlab('Year') + ylab('Incarcerations') + guides(fill=guide_legend(title="Racial Group"))
  return(plot + labs(color='Racial Group'))
}
plot_race_by_year('TX', plot_data)
