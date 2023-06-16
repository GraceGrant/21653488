popularity_metallica <- function(studio_metallica){

    list <- c("And Justice for All (Remastered)", "72 Seasons", "Death Magnetic", "Garage Inc", "HardwiredTo Self-Destruct", "Kill Em All (Remastered)", "Load", "Lulu", "Master of Puppets (Remastered)", "Metallica (Remastered)", "Reload", "Ride The Lightning (Remastered)", "Some Kind Of Monster", "St. Anger")

    df <- studio_metallica %>% filter(!grepl("Box Set|2021|Deluxe|,", album)) %>% filter(album %in% list) %>% group_by(album)

    g <- df %>% ggplot() +
        geom_bar(aes(x=album, y=popularity, fill = album), stat = "identity") +
        theme_bw() +
        labs(title = "Metallica Albums by Popularity", x = "Albums", y = "Popularity") +
        theme(axis.text.x = element_blank())
    g
}