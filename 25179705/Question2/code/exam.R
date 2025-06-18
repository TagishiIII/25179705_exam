# Find the most popular album

most_pop_album  <- function(data) {

  g1 <-   data %>%
        group_by(album) %>%
        summarise(avg_popularity = mean(popularity, na.rm = TRUE)) %>%
        arrange(desc(avg_popularity)) %>%
        slice(1) %>%
        pull(album)

    g1
}




top_album  <- function(data) {


    g1 <-   data %>%
        group_by(album) %>%
        summarise(avg_popularity = mean(popularity, na.rm = TRUE)) %>%
        arrange(desc(avg_popularity)) %>%
        slice(1) %>%
        pull(album)

    g2 <- g1 %>% filter(album == "most_pop_album")

    g2


}





top_album_plot <- function(data) {


    top_album_name <- most_pop_album(data)

    g3 <- data %>% filter(album == top_album_name)

    g4 <- g3 %>%


    ggplot() +
        geom_boxplot(aes(x = album, y = popularity) , fill = "skyblue") +
        labs(
            title = paste("Popularity Distribution for", top_album_name),
            x = NULL,
            y = "Popularity"
        ) +
        theme_minimal()


    g4
}


compare_top_albums <- function(data1, data2) {



    coldplay_top <- most_pop_album(data1)
    metallica_top <- most_pop_album(data2)


    coldplay_filtered <- data1 %>%
        filter(album == coldplay_top) %>%
        mutate(artist = "Coldplay", top_album = coldplay_top)

    metallica_filtered <- data2 %>%
        filter(album == metallica_top) %>%
        mutate(artist = "Metallica", top_album = metallica_top)


    bind_rows(coldplay_filtered, metallica_filtered)



}


plot_album_comparison <- function(data1, data2) {



    top_albums_df <- compare_top_albums(data1, data2)

    g5 <- top_albums_df %>%

    ggplot() +
        geom_boxplot(aes(x = artist, y = popularity, fill = artist)) + geom_jitter(aes(x = artist, y = popularity, fill = artist), width = 0.2, size = 2, shape = 21, color = "black", fill = "white") +
        labs(
            title = "Comparison of Most Popular Albums", subtitle = "Coldplay : Parachutes | Metallica : 72 Seasons "
            ,
            x = NULL,
            y = "Track Popularity"

        )
        theme_minimal()

    g5

}



combined_danceability <- function(data1 , data2) {



combinee <- bind_rows(
    data1 %>% mutate(artist = "Coldplay"),
    data2 %>% mutate(artist = "Metallica"))

combinee

}



dannce_plot <- function(data1 , data2) {


g6 <- combined_danceability(data1 , data2)

g7 <- g6  %>% ggplot() +
    geom_density(aes(x = danceability, fill = artist), alpha = 0.5) +
    labs(
        title = "Danceability Distribution: Coldplay vs Metallica",
        x = "Danceability",
        y = "Density",
        fill = "Artist"
    ) +
    theme_minimal()

g7

}

duration <- function(data1 , data2 ) {



    g8 <- bind_rows(
        data1 %>% mutate(duration_minutes = duration / 60, artist = "Coldplay"),
        data2 %>% mutate(duration_minutes = duration_ms / 60000, artist = "Metallica")
    )


    g9 <- g8 %>%  ggplot() +
        geom_point(aes(x = duration_minutes, y = popularity, color = artist),alpha = 0.6) +
        geom_smooth(aes(x = duration_minutes, y = popularity, color = artist), method = "loess") +
        labs(
            title = "Does Song Length Affect Popularity?",
            x = "Duration (minutes)",
            y = "Popularity"
        ) +
        theme_minimal()

        g9
}

trend <- function(data) {


   g15 <-  data %>%
        group_by(year, genre) %>%
        summarise(avg_danceability = mean(danceability, na.rm = TRUE))

   g16 <- g15 %>%
        ggplot() +
        geom_line(aes(x = year, y = avg_danceability, color = genre)) +
        theme_minimal() +
        labs(title = "Danceability Trends by Genre", x = "Year", y = "Danceability")

   g16
}


danu <- function(data) {


    g20 <- data %>%
        group_by(year) %>%
        summarise(avg_danceability = mean(danceability, na.rm = TRUE))

    g21 <- g20 %>%
        ggplot() +
        geom_point(aes(x = year, y = avg_danceability), color = "steelblue") +
        geom_smooth(aes(x = year, y = avg_danceability), method = "loess", se = FALSE, color = "darkblue", size = 1.2) +
        labs(
            title = "Danceability Trend Over Time",
            x = "Year",
            y = "Average Danceability"
        ) +
        theme_minimal()

    g21

}



