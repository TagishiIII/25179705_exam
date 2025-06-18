
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

matched <- function(data1 , data2) {


    matched_data <- inner_join(data1, data2, by = "title")  %>%
        distinct()

    matched_data

}

    top_director <- function(data1 , data2) {



       top <- matched(data1 , data2) %>%  filter(!is.na(director)) %>% count(director, sort = TRUE) %>%
            slice_max(n, n = 10)


       g1 <- top %>%
            ggplot(aes(x = reorder(director, n), y = n)) +
            geom_col(fill = "darkgreen") +
            coord_flip() +
            labs(title = "Top 10 Most Prolific Directors",
                 x = "Director", y = "Number of Titles") +
            theme_minimal()

       g1




    }


top_trend <- function(data1 , data2 ) {



    top_dir <- matched(data1 , data2)  %>% filter(!is.na(director)) %>%count(director, sort = TRUE) %>%slice_max(n, n = 5) %>%  pull(director)


    g1 <- top_dir %>%  filter(director %in% top_directors, !is.na(imdb_score), !is.na(release_year)) %>%
        group_by(director, release_year) %>%
        summarise(avg_score = mean(imdb_score, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = release_year, y = avg_score, color = director)) +
        geom_line(size = 1.2) +
        geom_point(alpha = 0.6) +
        labs(
            title = "IMDb Score Trends of Top 5 Most Prolific Directors",
            subtitle = "Based on average score per year",
            x = "Release Year",
            y = "Average IMDb Score",
            color = "Director"
        ) +
        theme_minimal(base_size = 13) +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )

    g1

}




top_trend1 <- function(data1, data2) {


    matched_data <- inner_join(data1, data2, by = "title")


    top_directors <- matched_data %>%
        filter(!is.na(director)) %>%
        count(director, sort = TRUE) %>%
        slice_max(n, n = 5) %>%
        pull(director)


    g1 <- matched_data %>%
        filter(director %in% top_directors, !is.na(imdb_score), !is.na(release_year.x)) %>%
        group_by(director, release_year.x) %>%
        summarise(avg_score = mean(imdb_score, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = release_year.x, y = avg_score, color = director)) +
        geom_line(size = 1.2) +
        geom_point(alpha = 0.6) +
        labs(
            title = "IMDb Score Trends of Top 5 Most Prolific Directors",
            x = "Release Year",
            y = "Average IMDb Score",
            color = "Director"
        ) +
        theme_minimal(base_size = 13) +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )

    g1
}



top_producer_facet <- function(data1, data2) {

    matched_data <- inner_join(data1, data2, by = "title")


    top_directors <- matched_data %>% filter(!is.na(director)) %>%count(director, sort = TRUE) %>%  slice_max(n, n = 5) %>%pull(director)


    g <- matched_data %>%
        filter(director %in% top_directors, !is.na(imdb_score)) %>%
        ggplot(aes(x = imdb_score, fill = director, color = director)) +
        geom_density(alpha = 0.6, color = NA) +
        facet_wrap(~ director, scales = "free_y") +
        labs(
            title = "IMDb Score Distribution by Top 5 Most Directors",
            x = "IMDb Score",
            y = "Density"
        ) +
        theme_minimal(base_size = 13) +
        theme(
            strip.text = element_text(face = "bold", size = 11),
            legend.position = "none"
        )

    g
}

