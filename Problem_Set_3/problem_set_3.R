library(ggplot)
data(diamonds)

# Q1
summary(diamonds)

# Q2
ggplot(aes(x = price), data = diamonds) +
  geom_histogram()

# Q3
summary(diamonds$price)

# Q4
sum(diamonds$price < 500)
sum(diamonds$price < 250)
sum(diamonds$price >= 15000)

# Q5
ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 50) +
  scale_x_continuous(limits = c(300, 2000), breaks = seq(300, 2000, 50))

# Q6
by(diamonds$price, diamonds$cut, max)
by(diamonds$price, diamonds$cut, min)
by(diamonds$price, diamonds$cut, median)

# Q7
ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 50) +
  scale_x_continuous(limits = c(300, 2000), breaks = seq(300, 2000, 50)) +
  facet_wrap(~cut, scales = "free_y")

# Q8
ggplot(aes(x = price/carat), data = diamonds) +
  geom_histogram(binwidth = 0.05) +
  scale_x_log10() +
  facet_wrap(~cut, scales = "free_y")

# Q9
ggplot(aes(x = color, y = price/carat), data = diamonds) +
  geom_boxplot()

ggplot(aes(x = cut, y = price/carat), data = diamonds) +
  geom_boxplot()

ggplot(aes(x = clarity, y = price/carat, fill = color), 
       data = diamonds) +
  geom_boxplot()

# Paula Franzini's solution
qplot(x=carat, y = price, data = diamonds) + 
  facet_grid(clarity ~ color)

qplot(x=carat, y = price, 
      data = subset(diamonds, (color=="D" | color == "G" | color =="J")&(clarity=="I1" | clarity=="VS1" | clarity=="IF"))) + 
  facet_grid(clarity ~ color)

# Q11
summary(diamonds$price[diamonds$color == "D"])
summary(diamonds$price[diamonds$color == "J"])
IQR(diamonds$price[diamonds$color == "D"])
IQR(diamonds$price[diamonds$color == "J"])

# Q12
ggplot(aes(x = color, y = price/carat, fill = color), 
       data = diamonds) +
  geom_boxplot() +
  coord_cartesian(ylim = c(2000, 5000))

# Paula Franzini's solution
uniform <- subset(diamonds,cut == "Ideal" & clarity == "IF" & carat < 1.2 & carat > 0.8)
ggplot(aes(x = color, y = price/carat, fill = color), 
       data = uniform) +
  geom_boxplot()

# Q12
ggplot(aes(x = carat), data = diamonds) +
  geom_freqpoly(binwidth=0.005) +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 5, 0.05)) +
  scale_y_continuous(breaks = seq(0, 12000, 1000))

# Use table()
table(diamonds$carat)[table(diamonds$carat)>2000]


