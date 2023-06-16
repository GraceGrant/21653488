scatter_metallica <- function(metallica_data){



    g <- metallica_data %>% ggplot() +
        geom_point(aes(x = popularity, y = danceability, color = popularity)) +
        labs(title = "Scatter Plot of Popularity and Danceability for Metallica", x = "Popularity", y = "Danceability") +
        theme_bw() +
        scale_fill_brewer(palette = "Set3")

    g

}