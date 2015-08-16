library(ggplot2)
data(diamonds)
ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point()
with(diamonds, cor.test(price, x))
with(diamonds, cor.test(price, y))
with(diamonds, cor.test(price, z))

ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(43, 79, 2))

with(diamonds, cor.test(price, depth))

ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point() +
  coord_cartesian(xlim = c(0, quantile(diamonds$carat, 0.99)),
                  ylim = c(0, quantile(diamonds$price, 0.99)))

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = x*y*z, y = price), data = diamonds) +
  geom_point()

# detach("package:plyr", unload=TRUE)
library(plyr); library(dplyr)
count(diamonds$volume == 0)

with(subset(diamonds, volume > 0 & volume <= 800),
    cor.test(volume, price))

ggplot(aes(x = volume, y = price), data = subset(diamonds, volume > 0 & volume <= 800)) +
  geom_point(alpha = 1/10) +
  stat_smooth(method = "lm", formula = y ~ I(x^3))

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = as.integer(median(price)),
            min_price = min(price),
            max_price = max(price),
            n = n())

diamondsByClarity <- arrange(summarise(group_by(diamonds, clarity),
                               mean_price = mean(price),
                               median_price = as.integer(median(price)),
                               min_price = min(price),
                               max_price = max(price),
                               n = n()), clarity)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity")
p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")
grid.arrange(p1, p2, ncol = 1)