setwd("~/Developer/pokemon-games-ratings")

library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts()

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

avg_score$score = floor(avg_score$x)

avg_score$color = ifelse(
  avg_score$score >= 75, 
  "#66cc33", 
  ifelse(
    avg_score$score >= 50, 
    "#ffcc33", 
    "#ff0000"
  )
)

avg_score$label = ifelse(
  avg_score$score >= 75, 
  "Generally Favorable", 
  ifelse(
    avg_score$score >= 50, 
    "Mixed or Average", 
    "Generally Unfavorable"
  )
)

# Tile Plot
ggplot(
  avg_score, 
  aes(
    x = year, 
    y = factor(
      platform, 
      levels = rev(avg_score_by_platform[, 1])
    ), 
    fill = factor(
      label, 
      levels = c("Generally Favorable", "Mixed or Average", "Generally Unfavorable")
    )
  )
) + 
  geom_tile(color = "white") + 
  geom_text(
    aes(label = score), 
    size = 7,
    color = "white",
    fontface = "bold",
    family = "mono"
  ) +
  coord_equal(ratio = 1) + 
  scale_fill_manual(values = c("#66cc33", "#ffcc33", "#ff0000")) +
  labs(
    y = "Platform",
    x = "Year",
    title = "Gotta Play and Rate 'em All",
    subtitle = "Metacritic Ratings of Pokémon Games across various platforms",
    caption = "Data visualization by randomo_redditor",
    fill = ""
  ) +
  theme_economist() +
  theme(
    text = element_text(family = "mono"),
    axis.line.y.left = element_line(color = "black", lineend = "round"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.title.y = element_text(size = rel(1.3), margin = margin(t = 0, r =20, b = 0, l = 0)),
    axis.title.x = element_text(size = rel(1.3), margin = margin(t = 20, r = 0, b = 10, l = 0)),
    axis.text.y = element_text(size = rel(1.5), hjust = 1, face = "bold"),
    axis.text.x = element_text(size = rel(1.5), face = "bold"),
    plot.title = element_text(size = rel(2.5), hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = rel(1.5),  margin = margin(t = 10, b = 30), hjust = 0),
    axis.text = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1.5)),
    legend.margin = margin(b = 20)
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
