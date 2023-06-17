admissions <- function(covid_data){

    covid_hosp <- covid_data %>% filter(!is.na(weekly_hosp_admissions)) %>% filter(!is.na(weekly_icu_admissions))

    df <- covid_hosp %>% arrange(date)

    # Plot the line graph
    g <- df %>% ggplot() +
        geom_line(aes(x = date, y = weekly_icu_admissions, color = "Weekly ICU Admissions")) +
        geom_line(aes(x = date, y = weekly_hosp_admissions, color = "Weekly Hospital Admissions")) +
        labs(x = "Date", y = "Admissions", title = "Weekly ICU and Hospital Admissions", color = "Admissions") +
        scale_color_manual(values = c("blue", "red")) +
        theme_minimal()
    g




}