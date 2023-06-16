plot_cases <- function(covid_cleaned){

    covid_data_2020 <- covid_cleaned %>%
        mutate(date = as.Date(date)) %>%
        filter(year(date) == 2020) %>%
        mutate(month = month(date, label = TRUE))

    monthly_cases <- covid_data_2020 %>%
        group_by(month, location) %>% mutate(total_cases = total_cases / 1000000) %>%
        summarize(total_cases = sum(total_cases))

    g <- monthly_cases %>% ggplot() +
        geom_line(aes(x = month, y = total_cases, color = location, group = location)) +
        labs(title = "Total Cases per Month and Location (2020)", x = "Month", y = "Total Cases") +
        scale_color_discrete(name = "Location")
    g

}