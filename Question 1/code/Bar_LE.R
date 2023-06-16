scatter_smokers <- function(covid_data) {

    df <- covid_data %>% group_by(location) %>% mutate(new_deaths = ifelse(is.na(new_deaths), 0, new_deaths)) %>%
        summarise(new_deaths = sum(new_deaths))


    g <- df %>% ggplot() +
        geom_point(aes(x = df$gdp_per_capita, y = new_deaths)) +
        theme_bw() +
        labs(title = "Scatterplot: Male Smokers vs Total Deaths", x = "Male Smokers", y = "Total Deaths")

    g

}