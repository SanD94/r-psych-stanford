library("knitr") # for rendering the RMarkdown file
library("patchwork") # for making figure panels
library("ggpol") # for making fancy boxplots
library("ggridges") # for making joyplots
library("gganimate") # for making animations
library("gapminder") # data available from Gapminder.org
library("tidyverse") # for plotting

df.diamonds <- diamonds

theme_set(theme_classic() + theme(text = element_text(size = 20)))