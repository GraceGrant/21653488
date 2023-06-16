london_rain <- function(london_weather){

    df <- london_weather %>% filter(date > "20201130")

    g <- df %>% ggplot() +
        geom_bar(aes(x = date, y = precipitation), stat = "identity", fill = "blue") +
        theme_bw() +
        labs(title = "December Rainfall", x = "Day", y = "Rain") +
        theme(axis.text.x = element_blank())
    g
}