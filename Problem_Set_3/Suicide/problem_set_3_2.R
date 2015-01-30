library(ggplot)
library(reshape2)
suicide <- read.csv("suicide.csv", na.strings = "NA", header = T, row.names = 1)
colnames(suicide) <- c(1950:2005)
rowMeans(suicide, na.rm = T)
colMeans(suicide, na.rm = T)
nrow(suicide[rowSums(!is.na(suicide)) == 56,])
data <- suicide[rowSums(!is.na(suicide)) >= 30,]
data.2 <- suicide[rowSums(!is.na(suicide)) == 56,]

data <- data.frame(t(data), year = c(1950:2005))
data.2 <- data.frame(t(data.2), year = c(1950:2005))
df <- melt(data, id="year", variable.name = "country")
df.2 <- melt(data.2, id="year", variable.name = "country")
df$year <- factor(df$year)
df.2$year <- factor(df.2$year)

ggplot(aes(x = X2005), data = suicide) +
  geom_histogram()

ggplot(aes(x = X1990), data = suicide) +
  geom_histogram()

ggplot(aes(x = year, y = value), data = df) +
  geom_boxplot()

ggplot(aes(x = year,y = value), data = df.2) +
  geom_line(aes(color = country, group = country)) +
  theme(axis.text.x = element_text(angle = 90))
