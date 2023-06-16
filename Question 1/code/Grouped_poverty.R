grouped_poverty <- function(covid_data){

    factors <- covid_data %>%
        select(location, aged_70_older, extreme_poverty, diabetes_prevalence, total_deaths_per_million) %>%
        group_by(location) %>%
        filter(!is.na(extreme_poverty)) %>% filter(!is.na(aged_70_older)) %>% filter(!is.na(diabetes_prevalence)) %>%
        slice(n()) %>% arrange(desc(extreme_poverty))

    countries_group1 <- c("Laos", "Djibouti", "Zimbabwe", "India", "South Africa")
    countries_group2 <- c("Uruguay", "Ukraine", "Thailand", "Russia", "Malaysia")

    filtered_df <- factors %>%
        filter(location %in% c(countries_group1, countries_group2)) %>%
        mutate(group = ifelse(location %in% countries_group1, "Group 1", "Group 2")) %>%
        mutate(location = factor(location, levels = c(countries_group1, countries_group2)))

    g <- filtered_df %>% ggplot() +
        geom_bar(aes(x = group, y = total_deaths_per_million, fill = location),
                 stat = "identity", position = position_dodge()) +
        labs(title = "Total Deaths Comparison Based on Poverty",
             x = "Group",
             y = "Total Deaths per Million",
             fill = "Country") +
        theme_bw() +
        scale_fill_brewer(palette="Set3")





}