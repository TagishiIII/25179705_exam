



# Make song and HBO names Title Case
HBO_Bill_Names <- function(data1 , data2 , data3) {


    baby<- data1 %>% rename(name = Name)

   bill<-  data2 %>% unnest_tokens(name, song) %>%
       distinct(name) %>%
       mutate(source = "Song")



   hbo <- data3 %>% mutate(name = word(character, 1)) %>%  distinct(name) %>%
                               mutate(source = "HBO")



   combined_media_names <- inner_join(bill, hbo, by = "name")


   baby_media_names <- baby %>%
       inner_join(hbo, by = "name")



     baby_media_names
}



most_common_hbo_names <- function(data1 ,data2 , data3){



    baby<- data1 %>% rename(name = Name)

    bill<-  data2 %>% unnest_tokens(name, song) %>%
        distinct(name) %>%
        mutate(source = "Song")



    hbo <- data3 %>% mutate(name = word(character, 1)) %>%  distinct(name) %>%
        mutate(source = "HBO")



    combined_media_names <- inner_join(bill, hbo, by = "name")


    baby_media_names <- baby %>%
        inner_join(hbo, by = "name")





      common <- baby_media_names  %>% group_by(name) %>%
        summarise(total = sum(.data$Count), .groups = "drop") %>%
        slice_max(total, n = 10)


    g1 <- common %>%
        ggplot(aes(x = reorder(name, total), y = total)) +
        geom_col(fill = "purple") +
        coord_flip() +
        labs(
            title = "Top 10 Character Names Used as Baby Names",
            x = "Name",
            y = "Total Number of Babies"
        ) +
        theme_minimal()

    g1



}


trend <- function(data1 , data2 , data3) {


    g3 <- HBO_Bill_Names(data1 , data2 , data3)  %>%
    filter(name %in% c("James", "John", "Robert" ,"William", "Michael"))

    g4 <- g3 %>% ggplot() +
        geom_line(aes(x = Year, y = Count, color = name)) +
        labs(
            title = "Trend in Top 5 Character-Inspired Baby Names Over Time",
            x = "Year", y = "Baby Count"
        ) +
        theme_minimal()



     g4


}




top_25_names <- function(data){


    top<- data %>% group_by(Year, Gender, Name) %>% summarise(Total = sum(Count), .groups = "drop") %>%  group_by(Year, Gender) %>%
        slice_max(order_by = Total, n = 25) %>%
        mutate(Rank = rank(-Total)) %>%
        select(Year, Gender, Name, Rank)

    top


}


get_spearman_for_year <- function(data, y, g) {

    base <- top_25_names(data) %>%
        filter(Year == y, Gender == g)

    result <- tibble()

    for (offset in 1:3) {
        compare_year <- top_25_names(data) %>%
            filter(Year == y + offset, Gender == g)

        joined <- inner_join(base, compare_year, by = "Name", suffix = c("_base", "_comp"))

        rho <- if (nrow(joined) >= 5) {
            cor(joined$Rank_base, joined$Rank_comp, method = "spearman")
        } else {
            NA
        }

        result <- bind_rows(result, tibble(
            Year = y,
            Gender = g,
            Offset = offset,
            Future_Year = y + offset,
            Spearman = rho
        ))
    }

    return(result)
}

spear1 <- function(data) {


    years <- unique(data$Year)
    years <- years[years >= 1990]

    genders <- unique(data$Gender)

    top_data <- top_25_names(data)


    get_spearman_fast <- function(top_data, y, g) {
        base <- top_data %>% filter(Year == y, Gender == g)

        result <- tibble()

        for (offset in 1:3) {
            compare_year <- top_data %>% filter(Year == y + offset, Gender == g)

            joined <- inner_join(base, compare_year, by = "Name", suffix = c("_base", "_comp"))

            rho <- if (nrow(joined) >= 5) {
                cor(joined$Rank_base, joined$Rank_comp, method = "spearman")
            } else {
                NA
            }

            result <- bind_rows(result, tibble(
                Year = y,
                Gender = g,
                Offset = offset,
                Future_Year = y + offset,
                Spearman = rho
            ))
        }

        return(result)
    }


    correlation_results <- purrr::map_dfr(years, function(y) {
        purrr::map_dfr(genders, function(g) {
            get_spearman_fast(top_data, y, g)
        })
    })

    corplot <- correlation_results %>%

    ggplot() +
        geom_line(aes(x = Year, y = Spearman, color = as.factor(Offset)), linewidth = 1.2) +
        facet_wrap(~Gender) +
        labs(
            title = "Spearman Rank Correlation of Top 25 Baby Names Over Time",
            x = "Base Year",
            y = "Spearman Correlation",
            color = "Years Ahead"
        ) +
        theme_minimal()

    corplot
}
