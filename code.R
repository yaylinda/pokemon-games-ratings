setwd("~/Developer/pokemon-games-ratings")

library(ggplot2)
library(ggthemes)
library(ggtext)
library(extrafont)
loadfonts()

data = read.csv("data.csv")
data = na.omit(data)

data$platform_url = ""
data$platform_url[which(data$platform == 'N64')] = 
  'https://cdn.freebiesupply.com/images/large/2x/n64-logo-png-transparent.png'

data$platform_url[which(data$platform == 'GBA')] = 
  'https://www.pngkit.com/png/full/142-1424510_source-nintendo-game-boy-advance-logo.png'

data$platform_url[which(data$platform == 'Switch')] = 
  'https://i.dlpng.com/static/png/6897050_preview.png'

data$platform_url[which(data$platform == '3DS')] = 
  'https://www.gamingmad.com/wp-content/uploads/2018/07/Nintendo_3DS_logo-300x168.png'

data$platform_url[which(data$platform == 'DS')] = 
  'https://www.logolynx.com/images/logolynx/aa/aa91597ddbb4c97fe02004240c1a3b17.png'

data$platform_url[which(data$platform == 'GC')] = 
  'https://www.pinclipart.com/picdir/big/144-1446693_boy-logo-vector-and-clip-art-inspiration-nintendo.png'

data$platform_url[which(data$platform == 'iOS')] = 
  'https://cdn.magicbytesolutions.com/assets/img/common/ios-app.png'

data$platform_url[which(data$platform == 'WII')] = 
  'https://lh3.googleusercontent.com/-tYqTehc2Jsw/Wp6awtYxyFI/AAAAAAAAEp0/XRbmZ_G1eosqBrieOSaLLraddowgdEwNwCKgBGAs/s640/nintendo%2Bwii.png'

data$platform_url[which(data$platform == 'WIIU')] = 
  'https://www.pikpng.com/pngl/b/320-3203051_gamestop-logo-transparent-nintendo-wii-u-logo-clipart.png'

data$platform_html = paste("<img src='", data$platform_url, "' width='50' />", sep = "")

# Aggregate scores by platform
avg_score_by_platform = aggregate(
  data$score,
  by = list(
    platform = data$platform,
    platform_html = data$platform_html
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
    platform_html = data$platform_html,
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

avg_score$platform_test_html = "<img src='https://www.logolynx.com/images/logolynx/aa/aa91597ddbb4c97fe02004240c1a3b17.png' width='50'/>"

# Tile Plot
ggplot(
  avg_score, 
  aes(
    x = year, 
    # y = platform_test_html,
    y = factor(platform_html, levels = rev(avg_score_by_platform$platform_html)),
    fill = factor(
      label, 
      levels = c("Generally Favorable", "Mixed or Average", "Generally Unfavorable")
    )
  )
) + 
  geom_tile(color = "white") + 
  geom_text(
    aes(label = score), 
    size = 7.5,
    color = "white",
    fontface = "bold",
    family = "mono"
  ) +
  coord_equal(ratio = 1) + 
  scale_fill_manual(values = c("#66cc33", "#ffcc33", "#ff0000")) +
  labs(
    y = "",
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
    axis.title.y = element_text(size = rel(1.4), margin = margin(t = 0, r =20, b = 0, l = 0)),
    axis.title.x = element_text(size = rel(1.4), margin = margin(t = 20, r = 0, b = 10, l = 0)),
    axis.text.y = element_markdown(),
    axis.text.x = element_text(size = rel(1.4), face = "bold"),
    plot.title = element_text(size = rel(3), hjust = 0, face = "bold"),
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
