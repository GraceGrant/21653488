plot_deaths <- function(covid_cleaned) {

    df <- covid_cleaned %>% group_by(location) %>% mutate(new_deaths = ifelse(is.na(new_deaths), 0, new_deaths)) %>%
        summarise(new_deaths = sum(new_deaths))

    bar_deaths <- df %>% ggplot() +
        geom_bar(aes(x = location, y = new_deaths), stat = "identity", fill = "turquoise") +
        theme_bw() +
        labs(title = "Total Deaths per Continent", x = "Continent", y = "Total Deaths")

    bar_deaths

}