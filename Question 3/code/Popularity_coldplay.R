popularity_coldplay <- function(studio_coldplay){

    df <- studio_coldplay %>% filter(!grepl("Edition", album_name)) %>% group_by(album_name)

    g <- df %>% ggplot() +
        geom_bar(aes(x=album_name, y=popularity, fill = album_name), stat = "identity") +
        theme_bw() +
        scale_fill_brewer(palette="Set3") +
        labs(title = "Coldplay Albums by Popularity", x = "Popularity", y = "Albums") +
        theme(axis.text.x = element_blank())
    g
}