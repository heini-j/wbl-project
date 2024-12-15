library(gganimate)
library(gifski)

# Loading the cleaned data ---------------------------------------------------------

df_wbl <- read_rds("parental_leave_clean.rds")

anim_plot <- ggplot(df_wbl, aes(maternityleave_length, paternityleave_length, colour = country)) +
    geom_point(size = 2, alpha = 0.7, show.legend = FALSE) +
    # scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 60)) +
    facet_wrap(~region) +
    shadow_trail(max_frames = 5) +
    labs(title = 'Year: {frame_time}', x = 'Length of maternity leave', y = 'Length of paternity leave') +
    transition_time(as.integer(year)) +
    ease_aes('linear') +
    xlim(0, 200) + 
    ylim(0, 100)

animate(anim_plot, device = 'png', renderer = gifski_renderer())

anim_save("parental_leave2.gif", animation = last_animation(), path = NULL)

?ease_aes

?scale_color_manual

    