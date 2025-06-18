
uno <- function(data, top_n_genres , top_n_countries) {



    dos <- data %>%
        filter(!is.na(imdb_score)) %>%
        unnest_longer(genres) %>%
        unnest_longer(production_countries)


    top_genres <- dos %>%
        count(genres, sort = TRUE) %>%
        slice_head(n = top_n_genres) %>%
        pull(genres)


    dos_filtered <- dos %>%
        filter(genres %in% top_genres) %>%
        group_by(production_countries, genres) %>%
        summarise(avg_imdb = mean(imdb_score, na.rm = TRUE), .groups = "drop")


    top_genre <- dos_filtered %>%
        group_by(production_countries) %>%
        slice_max(avg_imdb, n = 1) %>%
        ungroup()


    top_genre_limited <- top_genre %>%
        arrange(desc(avg_imdb)) %>%
        slice_head(n = top_n_countries) %>%
        mutate(production_countries = fct_reorder(production_countries, avg_imdb))


    g1 <- ggplot(top_genre_limited, aes(x = production_countries, y = avg_imdb, fill = genres)) +
        geom_col() +
        coord_flip() +
        labs(
            title = "Top-Rated Genre by Production Country (IMDb Score)",
            x = "Production Country",
            y = "Average IMDb Score",
            fill = "Genre"
        ) +
        theme_minimal() +
        theme(
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            plot.title = element_text(size = 14, face = "bold"),
            legend.position = "bottom"
        )

    g1
}


trend <- function(data) {


 g1 <- data %>% filter(!is.na(imdb_score), release_year > 1970) %>%group_by(release_year) %>%  summarise(avg_score = mean(imdb_score, na.rm = TRUE))



 g2 <- g1 %>%  ggplot() +
        geom_line(aes(x = release_year, y = avg_score), size = 1.2) +
        geom_smooth(aes(x = release_year, y = avg_score), method = "loess", se = FALSE, color = "red", linetype = "dashed") +
        labs(
            title = "Average IMDb Score by Release Year",
            x = "Year",
            y = "Average IMDb Score"
        ) +
        theme_minimal()

    g2

}


