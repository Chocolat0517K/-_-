install.packages('ggplot2')
library(ggplot2)

# 6.2 二項分布
# plot binomial distribution
df <- data.frame(x = 0:10,
                 y = dbinom(0:10, 10, 0.5))

g1 <- ggplot(df, aes(x = x, y = y)) +
  theme_minimal()

g1 <- g1 +
  geom_bar(stat = 'identity',
           col = 'pink',
           fill = 'pink',
           alpha = 0.8) +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab('x') +
  ylab('Density') +
  ggtitle('Bi(10, 0.5)') +
  theme(plot.title = element_text(hjust = 0.5))
plot(g1)

# plot another binomial distribution
df <- data.frame(x = 0:10,
                 y = dbinom(0:10, 10, 0.3))

g2 <- ggplot(df, aes(x = x, y = y)) +
  theme_minimal()

g2 <- g2 +
  geom_bar(stat = 'identity',
           col = 'lightblue',
           fill = 'lightblue',
           alpha = 0.8) +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab('x') +
  ylab('Density') +
  ggtitle('Bi(10, 0.3)') +
  theme(plot.title = element_text(hjust = 0.5))
plot(g2)


# 6.3 ポアソン分布
# plot poisson distribution
kl <- expand.grid(k = 0:20, lambda = c(1, 4, 10))
pois_df <- data.frame(kl,
                      prob = dpois(kl$k, kl$lambda))
pois_df$lambda = factor(pois_df$lambda)
str(pois_df)

g3 <- ggplot(pois_df,
             aes(x = k,
                 y = prob,
                 colour = lambda,
                 shape = lambda)) +
  theme_minimal()
g3 <- g3 +
  geom_line(size = 1) +
  geom_point(size = 3) +
  xlab('Number of events (k)') +
  ylab('Probability') +
  ggtitle('Po(λ), λ = (1, 4, 10)') +
  theme(plot.title = element_text(hjust = 0.5))
plot(g3)

# 6.4 負の二項分布
k <- 0:40
barplot(dnbinom(k,
               size = 3,
               prob = 0.2),
        names.arg = k)
title('負の二項分布')

# 6.6 正規分布
plot(0:20,
     dbinom(0:20, 20, prob = 0.5),
     xlab = '',
     ylab = '')
par(new = TRUE)
curve(dnorm(x,
            mean = 10,
            sd = sqrt(20 * 0.5 * (1 - 0.5))),
      0,
      20,
      xlab = 'x',
      ylab = '',
      axes = FALSE)
title('正規分布')

# 6.7 指数分布
curve(dexp(x, rate = 2))
title('λ = 2 の指数分布の密度関数')
n <- 10^3
r <- rexp(n, rate = 2.3)
x <- 0
xnum <- 0
count <- 0
time <- 0
for(i in 1:n){
  time <- time + r[i]
  if(time < 1) count <- count + 1
  else {
        x[xnum] <- count
        xnum <- xnum + 1
        time <- 0
        count <- 0
        }
}
barplot(table(x) / xnum)
title('指数分布に従う乱数から生成されるポアソン分布')

# 6.13 ワイブル分布
x <- seq(0, 5, by = 0.01)
curve(dweibull(x, shape = 5, scale = 3),
      0,
      5)
title('ワイブル分布（k = 5, λ = 3）')