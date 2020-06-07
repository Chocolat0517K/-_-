# [a, b]を台に持つ連続一様分布確率密度関数は
# f(x) = 1/(b-a) (a <= x <= b), 0 (otherwise)
# 連続一様分布の確率密度関数のグラフを描く([-1, 2])
install.packages('ggplot2')
library(ggplot2)

uniform_dsb_plot <- function(a, b) {
  xvals <- data.frame(x = c(a, b)) # xの区間を決める
  ggplot(data = data.frame(xvals),
         aes(x = x)) +
    theme_minimal() +
    xlim(c(a-2, b + 2)) +
    ylim(0, 1/(b-a)) +
    stat_function(fun = dunif,
                  args = list(min = a, max = b),
                  geom = 'area',
                  fill = 'blue',
                  alpha = 0.3)  
}
uniform_dsb_plot(a = -1, b = 2)

# 正規分布を描く
normal_dsb <- function(v_mean, v_sd) {
  xvals <- seq(v_mean-4, v_mean+4, length = 50)
  fx <- dnorm(x = xvals, mean = v_mean, sd = v_sd)
  data <- data.frame(x = xvals, y = fx)
  ggplot(data,
         aes(x = xvals,
             y = fx)) +
    geom_line() +
    theme_minimal()
}
normal_dsb(v_mean = 0, v_sd = 1)

# 対数正規分布を描く
lognormal_dsb <- function(v_meanlog, v_sdlog) {
  xvals <- seq(0.01, 10, length = 1000)
  fx <- dlnorm(x = xvals, meanlog = v_meanlog, sd = v_sdlog)
  data <- data.frame(x = xvals, y = fx)
  ggplot(data,
         aes(x = xvals,
             y = fx)) +
    geom_line() +
    theme_minimal()
}
lognormal_dsb(v_meanlog = 0, v_sd = 1)
