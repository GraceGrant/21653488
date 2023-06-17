temp_bar <- function(UK_19080){

    df <- UK_1980 %>% na.omit() %>%
        mutate(Year = year(DATE)) %>%
        group_by(Year) %>%
        summarise(avg_temp = mean(TAVG))

    g <- df %>% ggplot() +
        geom_bar(aes(x = Year, y = avg_temp, fill = avg_temp), stat = "identity") +
        theme_bw() +
        labs(title = "Average Temperature per Year", x = "Year", y = "Avg Temp") +
        theme(legend.position = "none") +
        geom_hline(yintercept = 16, col = "red", lwd = 2)
    g
}