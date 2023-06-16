plot_deaths_LE <- function(covid_data){

    countrylist <- c("United States", "United Kingdom", "Australia")
    df <- covid_data %>% filter(location %in% countrylist) %>%
        mutate(total_deaths_per_million = ifelse(is.na(total_deaths_per_million), 0, total_deaths_per_million))

    g <- df %>% ggplot() +
        geom_bar(aes(x = location, y = total_deaths_per_million), stat = "identity", fill = "purple") +
        theme_bw() +
        scale_y_continuous(labels = scales::number_format()) +
        labs(title = "Total Deaths", x = "Country", y = "Total Deaths per Million")

    g

}