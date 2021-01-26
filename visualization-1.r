library("knitr")
library("tidyverse")

df.diamonds <- diamonds

?diamonds

ggplot(data = df.diamonds)
ggplot(
        data = df.diamonds,
        mapping = aes(x = color, y = price)
) # aes means aesthetics

ggplot(
        data = df.diamonds,
        mapping = aes(x = color, y = price)
) +
        stat_summary(fun = "mean", geom = "bar")
# ggplot helps us adding more specific information to the plot
# as what we have in mind

help(stat_summary)


ggplot(
        data = df.diamonds,
        mapping = aes(x = color, y = price)
) +
        stat_summary(fun = "median", geom = "bar")


ggplot(df.diamonds, aes(x = color, y = price)) +
        stat_summary(fun = "mean", geom = "point")

# What they suggest is usage of argument specifiers at first,
# then dropping them later.

theme_set(theme_classic() + # set the theme
        theme(text = element_text(size = 20))) # set the default text size


ggplot(df.diamonds, aes(x = carat, y = price)) +
        geom_point()

ggplot(df.diamonds, aes(carat, price, color = color)) +
        geom_point()

vignette("ggplot2-specs") # Shows a tutorial in the browser

# practice 1
ggplot(df.diamonds, aes(depth, table)) +
        geom_point()


ggplot(df.diamonds, aes(cut, price, group = 1)) +
        stat_summary(fun = "mean", geom = "line")
# parameter group = 1 specificies that all the levels in x parameter can be
# treated as coming from the same group. The real reason is that `cut` is a
# factor rather than a numeric variable. By default, `ggplot2` draws
# a seperate line for each factor level.


# adding error bars
ggplot(df.diamonds, aes(clarity, price)) +
        stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
        # plot bootstrapped error bars first
        stat_summary(fun = "mean", geom = "point") # add points with means

help(mean_cl_boot)

ggplot(df.diamonds, aes(clarity, price)) +
        stat_summary(
                fun.data = "mean_cl_boot",
                geom = "linerange",
                fun.args = list(conf.int = .999, B = 2000)
        ) +
        # plot bootstrapped error bar first
        stat_summary(
                fun = "mean",
                geom = "point"
        ) # add points with means


ggplot(df.diamonds, aes(clarity, price)) +
        stat_summary(fun.data = "mean_cl_boot", geom = "pointrange")

ggplot(df.diamonds, aes(clarity, price)) +
        stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
        stat_summary(fun = "mean", geom = "point", color = "red")


# practice 2
ggplot(df.diamonds, aes(clarity, price)) +
        stat_summary(fun = "mean", geom = "bar") +
        stat_summary(fun.data = "mean_cl_boot", geom = "linerange")

# groping data
ggplot(df.diamonds, aes(color, price, group = cut)) +
        stat_summary(fun = "mean", geom = "line")


ggplot(df.diamonds, aes(color, price, group = cut, color = cut)) +
        stat_summary(fun = "mean", geom = "line", size = 2)
# size argument makes lines thicker

# wrong plot because all the bars are top of each other
ggplot(df.diamonds, aes(color, price, group = cut, color = cut)) +
        stat_summary(fun = "mean", geom = "bar")

ggplot(df.diamonds, aes(color, price, group = cut, fill = cut)) +
        stat_summary(
                fun = "mean",
                geom = "bar",
                position = position_dodge()
        ) +
        scale_fill_manual(
                values = c(
                        "lightblue",
                        "blue",
                        "orangered",
                        "red",
                        "black"
                )
        )


ggplot(df.diamonds, aes(color, price, group = cut, fill = cut)) +
        stat_summary(
                fun = "mean",
                geom = "bar",
                position = position_dodge(width = 0.9),
                color = "black"
        ) +
        stat_summary(
                fun.data = "mean_cl_boot",
                geom = "linerange",
                position = position_dodge(width = 0.9)
        )


# practice 3
ggplot(df.diamonds, aes(color, price, group = clarity, color = clarity)) +
        stat_summary(
                fun = "mean",
                geom = "line",
                size = 2
        ) +
        stat_summary(
                fun.data = "mean_cl_boot",
                geom = "linerange"
        )

# making facets
# even though a plot is pretty, having dense information in it makes people
# hardly understand the topic. Instead of using having much information,
# facets can be used

ggplot(df.diamonds, aes(x = y)) +
        geom_histogram(binwidth = 0.1) +
        coord_cartesian(xlim = c(3, 10))

ggplot(df.diamonds, aes(x = y)) +
        geom_density() +
        coord_cartesian(xlim = c(3, 10))

ggplot(df.diamonds, aes(x = y)) +
        geom_density(bw = 0.5) +
        coord_cartesian(xlim = c(3, 10))

# pretty but dense, not understandable
ggplot(df.diamonds, aes(x = y, group = color, fill = color)) +
        geom_density(bw = 0.2, alpha = 0.2) +
        coord_cartesian(xlim = c(3, 10))

# facets come in...
ggplot(df.diamonds, aes(x = y, fill = color)) +
        geom_density(bw = 0.2) +
        facet_grid(cols = vars(color)) +
        coord_cartesian(xlim = c(3, 10))

# practice 4
ggplot(df.diamonds, aes(color, price, group = cut, fill = cut)) +
        stat_summary(
                fun = "mean",
                geom = "bar",
                color = "black",
                position = position_dodge(width = 0.9)
        ) +
        stat_summary(
                fun.data = "mean_cl_boot",
                geom = "linerange",
                position = position_dodge(width = 0.9)
        ) +
        facet_grid(rows = vars(clarity))

# global, local and setting aes()

ggplot(df.diamonds, aes(carat, price, color = color)) +
        geom_point() +
        geom_smooth(method = "lm", se = F)


# what you call in ggplot applies to the all remaining functions
# in order to overwrite the features, write new arguments to the functions
ggplot(df.diamonds, aes(carat, price)) +
        geom_point(mapping = aes(color = color)) +
        geom_smooth(method = "lm")


ggplot(df.diamonds, aes(carat, price, color = color)) +
        geom_point() +
        geom_smooth(method = "lm", color = "black")
