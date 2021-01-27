library("knitr") # for rendering the RMarkdown file
library("patchwork") # for making figure panels
library("ggpol") # for making fancy boxplots
library("ggridges") # for making joyplots
library("gganimate") # for making animations
library("gapminder") # data available from Gapminder.org
library("tidyverse") # for plotting

df.diamonds <- diamonds

theme_set(theme_classic() + theme(text = element_text(size = 20)))

# stacked bar chart
ggplot(df.diamonds, aes(cut, fill = color)) +
    geom_bar(color = "black")


# pie chart
ggplot(df.diamonds, aes(1, fill = cut)) +
    geom_bar() +
    coord_polar("y", start = 0) +
    theme_void()


# comparisons
ggplot(
    df.diamonds[1:150, ],
    aes(color, price)
) +
    geom_point(
        alpha = 0.2,
        color = "blue",
        position = position_jitter(width = 0.1, height = 0)
    ) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "pointrange",
        color = "black",
        fill = "yellow",
        shape = 21,
        size = 1
    )


# boxplots
ggplot(df.diamonds, aes(color, price)) +
    geom_boxplot()

# don't use boxplot because of same represention
# of qualitatively different data
set.seed(1)
ggplot(
    df.diamonds %>% sample_n(1000),
    aes(color, price)
) +
    # displays a boxplot and jittered data next to each other
    ggpol::geom_boxjitter(
        jitter.shape = 1,
        jitter.color = "black",
        jitter.alpha = 0.2,
        outlier.color = NA,
        errorbar.draw = FALSE
    ) +
    stat_summary(
        fun = "mean",
        geom = "point",
        shape = 21,
        color = "black",
        fill = "yellow",
        size = 4
    )

# Violin Plots
ggplot(
    df.diamonds,
    aes(color, price)
) +
    # violin is good when displaying bimodal distributions
    # if data is continuous and the dataset is big, you can use it
    # don't forget that violin is not good when having discrete scale
    # like Likert (1 to 7)
    geom_violin()

set.seed(1)
data <- tibble(
    rating = sample(
        1:7,
        prob = c(0.1, 0.4, 0.1, 0.1, 0.2, 0, 0.1),
        size = 500,
        replace = T
    )
)

ggplot(
    data,
    aes("Likert", rating)
) +
    geom_violin() +
    geom_point(
        position = position_jitter(
            width = 0.05,
            height = 0.1
        ),
        alpha = 0.05
    )

ggplot(
    df.diamonds[1:10000, ],
    aes(cut, price)
) +
    geom_violin() +
    geom_point(
        alpha = 0.1,
        position = position_jitter(width = 0.1)
    ) +
    stat_summary(
        fun = "mean",
        geom = "point",
        fill = "blue",
        size = 5,
        shape = 21
    )



# Joy Plots
ggplot(
    df.diamonds,
    aes(price, color)
) +
    ggridges::geom_density_ridges(scale = 1.5)

# Scatter Plots
ggplot(
    df.diamonds,
    aes(carat, price, color = color)
) +
    geom_point()

# Raster Plots
ggplot(
    df.diamonds,
    aes(color, clarity, z = carat)
) +
    stat_summary_2d(fun = "mean", geom = "tile")

ggplot(
    df.diamonds,
    aes(color, clarity, z = carat)
) +
    stat_summary_2d(
        fun = "mean",
        geom = "tile",
        color = "black"
    ) +
    scale_fill_gradient(low = "white", high = "black") +
    labs(fill = "carat")


# temporal data
df.plot <- txhousing %>%
    filter(city %in% c("Dallas", "Fort Worth", "San Antonio", "Houston")) %>%
    mutate(city = factor(city,
        levels = c("Dallas", "Fort Worth", "San Antonio", "Houston")
    ))

ggplot(
    df.plot,
    aes(year, median, color = city, fill = city)
) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "ribbon",
        alpha = 0.2,
        linetype = 0
    ) +
    stat_summary(
        fun = "mean",
        geom = "line"
    ) +
    stat_summary(
        fun = "mean",
        geom = "point"
    )

# Advanced Version of temporal data
df.plot <- txhousing %>%
    filter(city %in% c("Dallas", "Fort Worth", "San Antonio", "Houston")) %>%
    mutate(city = factor(city,
        levels = c("Dallas", "Fort Worth", "San Antonio", "Houston")
    ))

df.text <- df.plot %>%
    filter(year == max(year)) %>%
    group_by(city) %>%
    summarize(year = mean(year) + 0.2, median = mean(median))

ggplot(
    df.plot,
    aes(year, median, fill = city, color = city)
) +
    # draws dashed lines in the background
    geom_hline(
        yintercept = seq(
            from = 100000,
            to = 250000,
            by = 50000
        ),
        linetype = 2,
        alpha = 0.2
    ) +
    # draws ribbon
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "ribbon",
        alpha = 0.2,
        linetype = 0
    ) +
    # draws lines connecting the means
    stat_summary(
        fun = "mean",
        geom = "line"
    ) +
    # draws means as points
    stat_summary(
        fun = "mean",
        geom = "point"
    ) +
    # adds the city names
    geom_text(
        data = df.text,
        mapping = aes(label = city),
        hjust = 0,
        size = 5
    ) +
    # sets y-axis labels
    scale_y_continuous(
        breaks = seq(
            from = 100000,
            to = 250000,
            by = 50000
        ),
        labels = str_c(
            "$",
            seq(
                from = 100,
                to = 250,
                by = 50
            ),
            "K"
        )
    ) +
    # sets x-axis labels
    scale_x_continuous(
        breaks = seq(
            from = 2000,
            to = 2015,
            by = 5
        )
    ) +
    # sets the limits for coordinates
    coord_cartesian(
        xlim = c(1999, 2015),
        clip = "off",
        expand = F
    ) +
    # sets the plot title and axes titles
    labs(
        title = "Change of median house sale in Texas",
        x = "Year",
        y = "Median house sale price",
        fill = "",
        color = ""
    ) +
    theme(
        title = element_text(size = 16),
        legend.position = "none",
        plot.margin = margin(r = 1, unit = "in")
    )


# customizing plots
ggplot(
    df.diamonds,
    aes(cut, price)
) +
    stat_summary(
        fun = "mean",
        geom = "bar",
        color = "black"
    ) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "linerange"
    )

# advanced version
ggplot(
    df.diamonds,
    aes(cut, price)
) +
    # changes the color of the fill
    stat_summary(
        fun = "mean",
        geom = "bar",
        color = "black",
        fill = "lightblue",
        width = 0.85
    ) +
    # makes error bars thicker
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "linerange",
        size = 1.5
    ) +
    # adjusts y axis
    scale_y_continuous(
        breaks = seq(from = 0, to = 4000, by = 2000),
        labels = seq(from = 0, to = 4000, by = 2000)
    ) +
    # adjusts range
    coord_cartesian(
        xlim = c(0.25, 5.75),
        ylim = c(0, 5000),
        expand = F
    ) +
    # adds a title, subtitle and label
    labs(
        title = "Price as a function of quality of cut",
        subtitle = "Note : The price is in US dollars",
        tag = "A",
        x = "Quality of the cut",
        y = "Price"
    ) +
    theme(
        text = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 0.2, unit = "inch")),
        axis.title.y = element_text(margin = margin(r = 0.1, unit = "inch")),
        plot.subtitle = element_text(
            margin = margin(b = 0.3, unit = "inch"),
            color = "gray70"
        ),
        plot.tag = element_text(face = "bold"),
        plot.tag.position = c(0.05, 0.99)
    )

# changing the order of things
ggplot(
    df.diamonds,
    aes(reorder(cut, price), price)
) +
stat_summary(
    fun = "mean",
    geom = "bar",
    color = "black",
    fill = "lightblue",
    width = 0.85
) +
stat_summary(
    fun.data = "mean_cl_boot",
    geom = "linerange",
    size = 1.5
) +
labs(x = "cut")


# dealing with legends
ggplot(
    df.diamonds,
    aes(color, price, color = clarity)
) +
stat_summary(fun = "mean", geom = "point")

ggplot(
    df.diamonds,
    aes(color, price, color = clarity)
) +
stat_summary(fun = "mean", geom = "point") +
theme(legend.position = "bottom")


ggplot(
    df.diamonds,
    aes(color, price, color = clarity)
) +
stat_summary(fun = "mean", geom = "point") +
theme(legend.position = "bottom") +
guides(color = guide_legend(
    nrow = 3,
    reverse = TRUE,
    override.aes = list(size = 6)
))

# saving plots
p1 <- ggplot(
    df.diamonds,
    aes(cut, price)
) +
stat_summary(
    fun = "mean",
    geom = "bar",
    color = "black",
    fill = "lightblue"
) +
stat_summary(
    fun.data = "mean_cl_boot",
    geom = "linerange",
    size = 1
)

# usage of pdf files is better than any other format, 
# it does not affect from resolution.
ggsave(
    filename = "figures/diamond_plot.pdf",
    plot = p1,
    width = 8,
    height = 6
)


# creating figure panels
# first plot
p1 = ggplot(df.diamonds, aes(y, fill = color)) +
geom_density(bw = 0.2, show.legend = F) +
facet_grid(cols = vars(color)) +
coord_cartesian(xlim = c(3, 10), expand = F) +
labs(title = "Width of differently colored diamonds", tag = "A")

# second plot
p2 = ggplot(df.diamonds, aes(color, clarity, z = carat)) +
stat_summary_2d(fun = "mean", geom = "tile") +
labs(
    title = "Carat values",
    subtitle = "For different color and clarity",
    x = "Color",
    tag = "B"
)

# third plot
p3 = ggplot(df.diamonds, aes(cut, price)) +
stat_summary(
    fun = "mean",
    geom = "bar",
    color = "black",
    fill = "lightblue",
    width = 0.85
) + 
stat_summary(
    fun.data = "mean_cl_boot",
    geom = "linerange",
    size = 1.5
) +
scale_x_discrete(labels = c("fair", "good", "very\ngood", "premium", "ideal")) +
labs(
    title = "Price as a function of cut",
    subtitle = "Note: The price is in US dollars",
    tag = "C",
    x = "Quality of the cut",
    y = "Price"
) +
coord_cartesian(
    xlim = c(0.25, 5.75),
    ylim = c(0, 5000),
    expand = F
)

# combine the plots
p1 + (p2 + p3) +
plot_layout(ncol = 1) &
theme_classic() &
theme(plot.tag = element_text(face = "bold", size = 20))

ggsave("figures/combined_plot.pdf", width = 10, height = 6)

# peeking behind the scenes
# for debugging purposes
build = ggplot_build(p)
df.plot_info <- build$data[[1]]
dim(df.plot_info)


# making animations
ggplot(
    gapminder,
    aes(gdpPercap, lifeExp, size = pop, color = country)
) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    geom_text(
        data = gapminder %>%
            filter(country %in%
                c("United States", "China", "India")),
        aes(label = country),
        color = "black",
        vjust = -0.75,
        show.legend = FALSE
    ) +
    scale_color_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    scale_x_log10(
        breaks = c(1e3, 1e4, 1e5),
        labels = c("1,000", "10,000", "100,000")
    ) +
    theme_classic() +
    theme(text = element_text(size = 23)) +
    # gganimate specific bits
    labs(
        title = "Year: {frame_time}",
        x = "GDP Per capita",
        y = "life expectancy"
    ) +
    transition_time(year) +
    ease_aes("linear")

anim_save(filename = "figures/life_gdp_animation.gif")


