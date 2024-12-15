library(gganimate)
library(gifski)

# Loading the cleaned data ---------------------------------------------------------

df_wbl <- read_rds("data/parental_leave.rds")

# Creating the animation -------------------------------------------------------------

anim_plot <- ggplot(df_wbl, aes(maternityleave_length, paternityleave_length, colour = country)) +
    geom_point(size = 2, alpha = 0.7, show.legend = FALSE) +
    scale_size(range = c(2, 60)) +
    facet_wrap(~region) +
    shadow_trail(max_frames = 5) +
    labs(title = 'Year: {frame_time}', x = 'Length of maternity leave', y = 'Length of paternity leave') +
    transition_time(as.integer(year)) +
    ease_aes('linear') +
    xlim(0, 200) + 
    ylim(0, 100)

# Animating the plot ----------------------------------------------------------------

animate(anim_plot, device = 'png', renderer = gifski_renderer())

# Saving the animation -------------------------------------------------------------

anim_save("animation/parental_leave.gif", animation = last_animation(), path = NULL)



    