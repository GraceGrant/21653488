plot_deaths <- function(covid_cleaned) {

    df <- covid_cleaned %>% group_by(location) %>% mutate(total_deaths = total_deaths / 1000000) %>%
        summarise(total_deaths = sum(total_deaths))

    bar_deaths <- df %>% ggplot() +
        geom_bar(aes(x = location, y = total_deaths), stat = "identity", fill = "turquoise") +
        theme_bw() +
        labs(title = "Total Deaths per Continent", x = "Continent", y = "Total Deaths in Millions")

    bar_deaths

}