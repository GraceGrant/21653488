storm_line <- function(UK_19080){

    df <- UK_1980 %>% na.omit() %>%
        mutate(Year = year(DATE)) %>%
        group_by(Year) %>%
        summarise(Total_DT32 = sum(EMXT))

    g <- df %>% ggplot() +
        geom_line(aes(x = Year, y = Total_DT32)) +
        theme_bw() +
        labs(title = "Change in DYTS per Year", x = "Year", y = "Total DYTS")
    g
}