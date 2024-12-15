library(ggplot2)
library(paletteer)
library(cowplot)
library(ggrepel)
library(dplyr)


# Loading the cleaned data ---------------------------------------------------------

df_wbl <- read_rds("parental_leave.rds") 


# Average length of the parental leaves per year -----------------------------------

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

# Number of countries with parental leave per year ----------------------------------

n_countries_leave <- df_wbl |>
    group_by(year) |>
    summarise(
        "sum_paternityleave" = sum(paternityleave_log == 1),
        "sum_maternityleave" = sum(maternityleave_log == 1),
        "sum_parentalleave" = sum(parentalleave_log == 1)
    ) |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = sum_maternityleave, color = "Maternity leave"), size = 1) +
    geom_line(aes(y = sum_paternityleave, color = "Paternity leave"), size = 1) +
    geom_line(aes(y = sum_parentalleave, color = "Shared parental leave"), size = 1) +
    labs(
        title = "Number of countries with parental leave per year",
        x = NULL,
        y = "Countries",
        colour = NULL,
        caption = "© Heini Järviö"
    ) +
    scale_color_paletteer_d("lisa::AndyWarhol") +
    theme_classic()

# Saving the plot ----------------------------------------------------------------

save_plot("plots/n_countries_leave.png", n_countries_leave)

# Countries with the longest parental leaves ---------------------------------------

# Average length of the parental leaves in different regions; modifiable by changing the years 

ggplot_2010 <-
    df_wbl |>
    filter(year == 2010) |>
    mutate(
        # Labeling only countries above mean in both maternity and paternity leave
        label = ifelse(
            paternityleave_length > mean(paternityleave_length) & 
            maternityleave_length > mean(maternityleave_length), 
            as.character(country),
            ""
            ), 
            .by = region) |>
    ggplot( 
        mapping = aes(x = maternityleave_length, y = paternityleave_length, label = 
                          label)
    ) +
    geom_point(color = "#FD814EFF", alpha=0.5)+
    facet_wrap(~region) +
    geom_label_repel(
        size = 3,
        box.padding = 1,
        max.overlaps = Inf
    ) +
    labs(
        title = "Leave length in 2010",
        x = "Length of maternity leave in days",
        y = "Length of paternity leave in days",
        caption = "© Heini Järviö"
    ) +
    theme_minimal(base_size = 12)

ggplot_2010

# Saving the plot ----------------------------------------------------------------
    
save_plot("plots/leave_length2010.png", ggplot_2010)
