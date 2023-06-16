plot_admissions <- function(covid_cleaned){

    df <- covid_cleaned %>%
        mutate(date = as.Date(date)) %>%
        filter(year(date) == 2020) %>%
        mutate(month = month(date, label = TRUE))

    monthly_admissions <- df %>%
        group_by(month, location) %>%
        summarise_at( vars( c(weekly_icu_admissions, weekly_hosp_admissions)), ~mean(.)) %>%
        gather(Indicator, Value, weekly_icu_admissions, weekly_hosp_admissions) %>%
        mutate(Score = gsub("Explained by: ", "", Score))



    g <- monthly_admissions %>% ggplot() +
        geom_line(aes(x = month, y = indicator, color = location, group = location)) +
        labs(title = "Total Cases per Month and Location (2020)", x = "Month", y = "Total Deaths") +
        scale_color_discrete(name = "Location")
    g

}