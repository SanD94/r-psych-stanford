# ggplot call with global aesthetics
ggplot(
    data = data,
    mapping = aes(
        x = cause,
        y = effect
    )
) +
    # add geometric objects (geoms)
    geom_point() +
    stat_summary(fun = "mean", geom = "point") +
    ... +
    # add text objects
    geom_text() +
    annotate() +
    # adjust axes and coordinates
    scale_x_continuous() +
    scale_y_continuous() +
    coord_cartesian() +
    # define plot title, and axis titles
    labs(
        title = "Title",
        x = "Cause",
        y = "Effect"
    ) +
    # change global aspects of the plot
    theme(
        text = element_text(size = 20),
        plot.margin = margin(t = 1, b = 1, l = 0.5, r = 0.5, unit = "cm")
    ) +
    # save the plot
    ggsave(
        filename = "super_nice_plot.pdf",
        width = 8,
        height = 6
    )