library(ggplot2)
library(paletteer)
library(cowplot)


# Loading the cleaned data ---------------------------------------------------------

df_wbl <- read_rds("parental_leave_clean.rds") 


# Average length of the parental leaves per year

leave_per_year <- df_wbl |>
    group_by(year) |>
    summarise(
        "mean_maternityleave" = mean(maternityleave_length),
        "mean_paternityleave" = mean(paternityleave_length),
        "mean_parentalleave" = mean(shared_length)
    ) |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = mean_maternityleave, color = "Maternity leave"), size = 1) +
    geom_line(aes(y = mean_paternityleave, color = "Paternity leave"), size = 1) +
    geom_line(aes(y = mean_parentalleave, color = "Shared parental leave"), size = 1) +
    labs(
        title = "Parental leave per year",
        x = NULL,
        y = "Length of leave in days",
        colour = NULL,
        caption = "© Heini Järviö"
        ) +
    scale_color_paletteer_d("lisa::AndyWarhol") +
    theme_classic()

# Saving the plot ----------------------------------------------------------------

save_plot("plots/parental_leave_per_year.png", leave_per_year)



