bubble <- function(merged_google){

    filter_merged <- merged_google %>% filter(!grepl("varies|Varies", Size)) %>% na.omit() %>% filter(!grepl("NAN|Nan|NaN|nan", Sentiment))

    filter_merged$Installs <- as.numeric(gsub("[^0-9]", "", filter_merged$Installs))
    filter_merged$Size <- as.numeric(gsub("[^0-9]", "", filter_merged$Size))
    filter_merged$Installs <- filter_merged$Installs / 1000
    filter_merged <- filter_merged %>% filter(Size < 200)

    g <- ggplot(filter_merged, aes(x=Size, y=Rating, size = Installs, color = Sentiment)) +
        geom_point(alpha=0.5) +
        scale_size(range = c(1, 15), name="Installs (thousands)")

    g






}