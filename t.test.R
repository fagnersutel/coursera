data(sleep)
g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
difference <- g2 - g1
mn <- mean(difference)
s <- sd(difference)
n <- 10
mn + c(-1,+1) * qt(.975, n-1) * s /sqrt(n)
sort(difference)
boxplot(difference)
t.test(difference)
t.test(g2, g1, paired = T)
head(sleep)
#Extra á a variação no data set e group o classificador dos grupos no dataset
res = t.test(extra ~ I(relevel(group, 2)), paired = T, data = sleep)
summary(res)

mpv =(7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 -2)
mpv
sp = sqrt(mpv)
sp


library(datasets)
data("ChickWeight")
library(reshape2)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
head(wideCW)
names(wideCW)[-(1:2)] <- paste("time", names(wideCW)[-(1:2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW, gain = time21 - time0)
head(wideCW)

library(ggplot2)
g <- ggplot(ChickWeight, aes(x = Time, y = weight, 
                             colour = Diet, group = Chick))
g <- g + geom_line()
g <- g + stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1, col = "black")
g <- g + facet_grid(. ~ Diet)
g


g <- ggplot(wideCW, aes(x = factor(Diet), y = gain, fill = factor(Diet)))
g <- g + geom_violin(col = "black", size = 2)
g


wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
  t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
  t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)

