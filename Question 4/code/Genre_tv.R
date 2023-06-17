genre_bar_tv <- function(titles_data){

    shows <- titles_data %>% filter(type == "SHOW") %>%
        arrange(desc(imdb_score)) %>% filter(title != '#ABtalks') %>% slice(1:10)

    custom_text <- c("Reality", "Drama", "Animation", "Doc", "Comedy", "Comedy", "Drama", "Romance", "Doc", "Animation")


    g <- shows %>% ggplot() +
        geom_bar(aes(x = title, y = imdb_score, fill = title), stat = "identity") +
        theme_bw() +
        labs(title = "TV Shows by Genre and Popularity", x = "TV Show", y = "IMDB Score", fill = "Title") +
        scale_fill_brewer(palette = "Set3") +
        theme(axis.text.x = element_blank()) +
        geom_text(aes(x = title, y = imdb_score, label = custom_text), vjust = -0.5, color = "black", size = 3)


        g




}