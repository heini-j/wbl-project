install.packages("gifski")
library(gganimate)
library(gifski)

anim_plot <- ggplot(df_parental_leave_clean, aes(maternityleave_lenght, paternityleave_lenght, colour = country)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 30)) +
    facet_wrap(~region) +
    scale_x_log10() +
    # Here comes the gganimate specific bits
    labs(title = 'Year: {frame_time}', x = 'Lenght of maternity leave', y = 'Lenght of paternity leave') +
    transition_time(as.integer(year), range c(1978, 2024)) +
    ease_aes('linear')

animate(anim_plot, device = 'png', renderer = gifski_renderer())

anim_save("parental_leave.gif", animation = last_animation(), path = NULL)

?ease_aes

?transition

    