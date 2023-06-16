scatter_coldplay <- function(coldplay_data){



    g <- coldplay_data %>% ggplot() +
        geom_point(aes(x = popularity, y = danceability, color = popularity)) +
        labs(title = "Scatter Plot of Popularity and Danceability for Coldplay", x = "Popularity", y = "Danceability") +
        theme_bw() +
        scale_fill_brewer(palette = "Set3")

    g

}