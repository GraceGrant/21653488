lollipop_dir <- function(merged_data){

    directors <- merged_data %>% filter(role == "DIRECTOR") %>% filter(release_year > 1980) %>%
        select (c("title", "type", "imdb_score", "name")) %>% arrange(desc(imdb_score)) %>% slice(1:20)



    g <- directors %>% ggplot(aes(x=name, y=imdb_score)) +
        geom_segment( aes(x=name, xend=name, y=0, yend=imdb_score), color = "grey", linetype = "dotdash") +
        geom_point( size=5, color="purple", fill=alpha("lightblue", 0.3), alpha=0.7, shape=21, stroke=2)  +
        theme_bw() +
        labs(title = "Top Directors by IMDB Score", x = "Director", y = "IMDB Score") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1))
    g




}