
##b This the code folder for question 4 ( BIllions)

firstplot <- function(data , Title ) {


    df_plot <- data %>%
        mutate(region = ifelse(`location.country code` == "USA", "USA", "Rest of World")) %>%
        group_by(region, wealth.how.inherited) %>%
        summarise(count = n(), .groups = "drop")

  g1 <- df_plot %>%
      ggplot() +
      geom_bar(
          aes(x = region, y = count, fill = wealth.how.inherited),
          stat = "identity" ) +
      labs(
          title = "Billionaires: Inherited Wealth (USA vs Rest of World)",
          x = "",
          y = "Number of Billionaires",
          fill = "Inherited"
      ) +
      theme_minimal()

  g1

}


second_plot <- function(data , Title) {

    d2 <- data %>% filter(`location.country code` %in% c("USA", "CHN")) %>% arrange(`location.country code`, year) %>%
        group_by(`location.country code`) %>%
        mutate(growth_rate = (location.gdp / lag(location.gdp) - 1) * 100) %>%
        ungroup()


 d2_plot <- d2 %>%
    ggplot() +
        geom_line(aes(x = year, y = growth_rate), color = "steelblue", size = 1) +
        geom_point(aes(x = year, y = growth_rate) , color = "darkred", size = 2) +
        facet_wrap(~`location.country code`, scales = "free_y") +
        labs(
            title = "GDP Growth Rates: USA vs China",
            x = "Year",
            y = "Growth Rate (%)"
        ) +
        theme_minimal()

 d2_plot


}



third_plot <- function (data , Title) {


    d3 <- data %>% filter(wealth.how.inherited == "not inherited") %>% filter(company.founded >= 2000) %>%  count(company.sector) %>% arrange(desc(n)) %>% slice_max(n, n = 10)



d3Plot <- d3 %>%
        ggplot() +
        geom_bar(aes(x = reorder(company.sector, n), y = n) , stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(
            title = Title,
            x = "Sector",
            y = "Number of Billionaires"
        ) +
        theme_minimal()

    d3Plot

}



us_inheritance_table <- function(data) {


    data %>%
    filter(`location.country code` == "USA") %>%
    group_by(wealth.how.inherited) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
        percentage = round(100 * count / sum(count), 1)
    ) %>%
    arrange(desc(percentage))

}

non_us_inheritance_table <- function(data) {


    data %>%
    filter(`location.country code` != "USA") %>%
    group_by(wealth.how.inherited) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
        percentage = round(100 * count / sum(count), 1)
    ) %>%
    arrange(desc(percentage))
}




top5_companies_table <- function(data) {


    data %>%
    filter(!is.na(company.name), company.name != "") %>%
    mutate(region = ifelse(`location.country code` == "USA", "USA", "Rest of World")) %>%
    group_by(company.name, region) %>%
    summarise(billionaire_count = n(), .groups = "drop") %>%
    group_by(company.name) %>%
    summarise(
        total_billionaires = sum(billionaire_count),
        region = paste(region[which.max(billionaire_count)], collapse = ", ")
    ) %>%
    arrange(desc(total_billionaires)) %>%
    slice_head(n = 5)
    }




