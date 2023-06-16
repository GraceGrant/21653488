box_coldplay <- function(studio_coldplay){

    df <- studio_coldplay %>% filter(!grepl("Edition", album_name)) %>% group_by(album_name)

    g <- df %>% ggplot() +
        geom_boxplot(aes(x = album_name, y = popularity, color = album_name)) +
        labs(title = "Box and Whisker Plot",
         x = "Album",
         y = "Popularity") +
        theme(axis.text.x = element_blank())

    g


}