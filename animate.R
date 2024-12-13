install.packages("gifski")
library(gganimate)
library(gifski)

anim_plot <- ggplot(df_parental_leave_clean, aes(maternityleave_lenght, paternityleave_lenght, colour = country)) +
    geom_point(size = 3, alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 60)) +
    facet_wrap(~region) +
    # Here comes the gganimate specific bits
    shadow_trail(max_frames = 5) +
    labs(title = 'Year: {frame_time}', x = 'Lenght of maternity leave', y = 'Lenght of paternity leave') +
    transition_time(as.integer(year)) +
    ease_aes('linear') +
    xlim(0, 200) + 
    ylim(0, 100)

animate(anim_plot, device = 'png', renderer = gifski_renderer())

anim_save("parental_leave2.gif", animation = last_animation(), path = NULL)

?ease_aes

?transition

    