library(reshape2)
data(diamonds)

## 1
ggplot(aes(x = price, fill = cut), data = diamonds) +
  geom_histogram() +
  facet_wrap(~color)+
  scale_fill_brewer(type = "qual")

## 2
ggplot(aes(x = table, y = price, color = cut), data = diamonds) +
  geom_point() +
  scale_color_brewer(type = "qual") +
  coord_cartesian(xlim = c(50, 70))

## 3
diamonds <- transform(diamonds, volume = x*y*z)
ggplot(aes(x = volume, y = price, color = clarity), data = diamonds) +
  geom_point() +
  scale_y_log10() +
  scale_color_brewer(type = "div") +
  coord_cartesian(xlim = c(0, quantile(diamonds$volume, 0.99)))

## 4
pf <- read.csv('/Users/Yuji/Library/Mobile Documents/com~apple~CloudDocs/Course/Data Analysis with R/pseudo_facebook.tsv', sep="\t")
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count
ggplot(aes(x = tenure, y = prop_initiated, color = year_joined.bucket), data = subset(pf, !is.na(pf$prop_initiated))) +
  geom_line(stat = "summary", fun.y = median)

ggplot(aes(x = tenure, y = prop_initiated, color = year_joined.bucket), data = subset(pf, !is.na(pf$prop_initiated))) +
  geom_smooth()

mean(pf[pf$year_joined.bucket=="(2012,2014]",]$prop_initiated, na.rm=T)

## 5
ggplot(aes(x = cut, y = price/carat, color = color), data = diamonds) +
  geom_jitter() +
  facet_wrap(~clarity)