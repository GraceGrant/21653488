london_rain <- function(london_weather){

    df <- london_weather %>% filter(date > "20201130")


    g <- df %>% ggplot(aes(x=date, y=precipitation)) +
        geom_segment( aes(x=date, xend=date, y=0, yend=precipitation), color = "blue", linetype = "dotdash") +
        geom_point( size=5, color="blue", fill=alpha("lightblue", 0.3), alpha=0.7, shape=21, stroke=2)  +
        theme_bw() +
        labs(title = "Precipitation per Day in December 2020", x = "Days", y = "Rain") +
        theme(axis.text.x = element_blank())
    g
}