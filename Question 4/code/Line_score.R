line_score <- function(merged_data){

    highest_scores <- merged_data %>%
        group_by(release_year, title) %>%
        summarize(max_imdb_score = max(imdb_score)) %>% na.omit

    plot <- ggplot(highest_scores, aes(x = release_year, y = max_imdb_score)) +
        geom_line(alpha = 0.7, color = "grey") +
        geom_smooth(color = "purple") +
        labs(x = "Release Year", y = "IMDb Score", title = "IMDb Score over Release Year") +
        theme_bw()
    plot



}