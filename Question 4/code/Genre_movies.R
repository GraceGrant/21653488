genre_bar_movies <- function(titles_data){

    movies <- titles_data %>% filter(type == "MOVIE") %>% arrange(desc(imdb_score)) %>% slice(1:10)

    custom_text <- c("Comedy", "Animation", "Drama", "Doc", "Drama", "SciFi", "Doc", "Comedy", "Doc", "Comedy")


    g <- movies %>% ggplot() +
        geom_bar(aes(x = title, y = imdb_score, fill = title), stat = "identity") +
        theme_bw() +
        labs(title = "Movies by Genre and Popularity", x = "TV Show", y = "IMDB Score", fill = "Title") +
        scale_fill_brewer(palette = "Set3") +
        theme(axis.text.x = element_blank()) +
        geom_text(aes(x = title, y = imdb_score, label = custom_text), vjust = -0.5, color = "black", size = 3)


    g




}