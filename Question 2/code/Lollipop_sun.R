lollipop_sun <- function(london_2000){

    df <- london_weather %>% filter(date > "20201130")



    g <- df %>% ggplot(aes(x=date, y=sunshine)) +
        geom_segment( aes(x=date, xend=date, y=0, yend=sunshine), color = "orange", linetype = "dotdash") +
        geom_point( size=5, color="orange", fill=alpha("yellow", 0.3), alpha=0.7, shape=21, stroke=2)  +
        theme_bw() +
        labs(title = "Sunshine Hours per Day in December 2020", x = "Days", y = "Sunshine") +
        theme(axis.text.x = element_blank()) +
        ylim(0, 10)
    g




}