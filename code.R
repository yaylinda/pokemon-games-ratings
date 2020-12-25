setwd("~/Developer/pokemon-games-ratings")

library(ggplot2)
library(ggthemes)

data = read.csv("data.csv")
data = na.omit(data)

# Aggregate scores by platform
avg_score_by_platform = aggregate(
  data$score,
  by = list(
    platform = data$platform
  ),
  mean
)

# Order platform by average score
avg_score_by_platform = avg_score_by_platform[order(-avg_score_by_platform$x), ]

# Aggregate scores by platform and year
avg_score = aggregate(
  data$score,
  by = list(
    platform = data$platform,
    year = data$year
  ),
  mean
)

# Tile Plot
ggplot(
  avg_score, 
  aes(
    x = year, 
    y = factor(platform, rev(avg_score_by_platform[, 1])), 
    fill = x,
  )
) + 
geom_tile(color = "white", alpha = 0.8) + 
geom_text(
  aes(label = x), 
  size = 3
) +
coord_equal(ratio = 1) + 
labs(
  y = "Platform",
  x = "Year\n",
  title = "Pokemon Game Metacritic Ratings",
  subtitle = "",
  fill = "Metacritic Score",
  caption = "Data visualization by randomo_redditor"
) + 
# scale_fill_gradient2(
#   "Number of Times Awarded", 
#   limits = c(0, max(aggregated_not_other$x)), 
#   low = "#762A83", 
#   mid = "white", 
#   high = "#1B7837"
# ) +
scale_x_discrete(
  position = "top"
) +
theme_stata() +
theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.y = element_text(angle = 360),
  axis.text.x = element_text(angle = 45,  hjust = -0.005),
  plot.title = element_text(size = 20),
  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 20)),
  axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
  plot.subtitle = element_text(margin = margin(t = 5, r = 0, b = 20, l = 0), color = "darkgray"),
  plot.margin = unit(c(1, 5, 2, 1), "cm"),
  plot.caption = element_text(color = "darkgray", vjust = -15, hjust = -0.4)
)

# Box Plot (Platform)
ggplot(
  data, 
  aes(
    x = factor(platform, rev(avg_score_by_platform[, 1])), 
    y = score, 
    fill = platform,
  )
) + 
  geom_boxplot() +
  labs(
    x = "Platform",
    y = "Metacritic Score",
    title = "Pokemon Game Metacritic Ratings",
    caption = "Data visualization by randomo_redditor"
  ) + 
  theme(
    legend.position="none"
  )

# Box Plot (Year)
ggplot(
  data, 
  aes(
    x = as.factor(year),
    y = score,
  )
) + 
  geom_boxplot() +
  labs(
    x = "Year",
    y = "Metacritic Score",
    title = "Pokemon Game Metacritic Ratings",
    caption = "Data visualization by randomo_redditor"
  ) + 
  theme(
    legend.position="none"
  )
