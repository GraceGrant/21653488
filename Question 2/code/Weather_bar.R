avg_london <- function(london_2000){

    df <- london_2000 %>% na.omit() %>% select(c(cloud_cover, sunshine)) %>%
        summarise_at(vars(c(cloud_cover, sunshine)), ~mean(.)) %>%
        mutate(sunshine = sunshine / 1.77) %>%
        gather(key = "variable", value = "mean_value")

    bar_colours <- c("grey", "yellow")

    g <- df %>% ggplot() +
        geom_bar(aes(x = variable, y = mean_value, fill = variable), stat = "identity") +
        theme_bw() +
        scale_fill_manual(values = bar_colours) +
        labs(title = "Average weather statistics", x = "Variable", y = "Mean value")
    g
}