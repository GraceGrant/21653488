lollipop <- function(titles_data){

    shows <- titles_data %>% filter(type == "SHOW") %>%
         arrange(desc(imdb_score)) %>% filter(title != '#ABtalks') %>% slice(1:10)



    g <- shows %>% ggplot(aes(x=title, y=imdb_score)) +
        geom_segment( aes(x=title, xend=title, y=0, yend=imdb_score), linetype = "dotdash") +
        geom_point( size=5, color="purple", fill=alpha("lightblue", 0.3), alpha=0.7, shape=21, stroke=2)  +
        theme_bw() +
        labs(title = "Genres by Popularity", x = "Genre", y = "IMDB Score") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1.2))
    g




}