box_metallica <- function(studio_metallica){

    list <- c("And Justice for All (Remastered)", "72 Seasons", "Death Magnetic", "Garage Inc", "HardwiredTo Self-Destruct", "Kill Em All (Remastered)", "Load", "Lulu", "Master of Puppets (Remastered)", "Metallica (Remastered)", "Reload", "Ride The Lightning (Remastered)", "Some Kind Of Monster", "St. Anger")

    df <- studio_metallica %>% filter(!grepl("Box Set|2021|Deluxe|,", album)) %>% filter(album %in% list) %>% group_by(album)

    g <- df %>% ggplot() +
        geom_boxplot(aes(x = album, y = popularity, color = album)) +
        labs(title = "Box and Whisker Plot for Metallica",
             x = "Album",
             y = "Popularity") +
        theme(axis.text.x = element_blank())

    g


}