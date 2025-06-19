

box_plot <- function(data) {

    g1 <- data %>%
        mutate(Age_Group = cut(Age, breaks = c(18, 30, 45, 60, 100),
                               labels = c("18–30", "31–45", "46–60", "60+")))

    g2 <- g1 %>%
        ggplot(aes(x = `Physical Activity Level`, y = `Weight Change (lbs)`)) +
        geom_boxplot(outlier.shape = NA, fill = "white", color = "gray30") +
        geom_jitter(aes(color = Age_Group), width = 0.2, size = 2) +
        labs(
            title = "Weight Change by Activity Level",
            x = "Physical Activity Level",
            y = "Weight Change (lbs)",
            color = "Age Group"
        ) +
        theme_minimal()

    g2
}

first_model <- function(data) {


    model <- lm(`Weight Change (lbs)` ~ Age + `Physical Activity Level` + `Stress Level` + `Sleep Quality`, data = data)


    summary(model)
}



sleep_chart <- function(data) {



  g1 <- data %>%  count(`Sleep Quality`) %>%
        mutate(prop = n / sum(n),
               label = paste0(`Sleep Quality`, ": ", round(100 * prop, 1), "%"))

  g2 <- g1 %>%
        ggplot(aes(x = "", y = prop, fill = `Sleep Quality`)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
        labs(
            title = "Distribution of Sleep Quality",
            x = NULL, y = NULL, fill = "Sleep Quality"
        ) +
        theme_void()


    g2


}

bmr_model <- function(data) {


    lm(`BMR (Calories)` ~ Age + `Sleep Quality` + `Stress Level`, data = data)


}



bmr_plot <- function(data) {


    model <- bmr_model(data)


    predicted_data <- data %>%
        mutate(predicted_BMR = predict(model, newdata = data))


    g1 <- ggplot(predicted_data, aes(x = Age, y = predicted_BMR, color = `Stress Level`)) +
        geom_line(stat = "smooth", method = "lm", se = FALSE, linewidth = 1.2) +
        facet_wrap(~ `Sleep Quality`) +
        labs(
            title = "Predicted BMR by Age, Stress Level, and Sleep Quality",
            x = "Age",
            y = "Predicted BMR (Calories)",
            color = "Stress Level"
        ) +
        theme_minimal(base_size = 13)

    g1
}



weight_regression <- function(data) {


    lm(`Final Weight (lbs)` ~ `Current Weight (lbs)` + `Sleep Quality`, data = data)
}


weight_plot <- function(data) {
    ggplot(data, aes(x = `Current Weight (lbs)`, y = `Final Weight (lbs)`, color = `Sleep Quality`)) +
        geom_point(alpha = 0.7, size = 2) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(
            title = "Final Weight vs Current Weight by Sleep Quality",
            x = "Current Weight (lbs)",
            y = "Final Weight (lbs)",
            color = "Sleep Quality"
        ) +
        theme_minimal(base_size = 13)
}

