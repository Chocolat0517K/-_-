install.packages("ggplot2")
library(ggplot2)

# 因果関係であっても相関関係がない例(p.51)
# y = 2x - x^2に平均0、標準偏差0.2の疑似乱数を加える
set.seed(123)
df <- data.frame(
  x <- c(seq(0, 2, by = 0.01)),
  y <- c(2 * x - x ^ 2 + rnorm(length(x), 0, 0.2))
)

# 散布図を作成する
g1 <- ggplot(df, aes(x = x,
                    y = y))
g1 <- g1 +
  geom_point(shape = 21,
             size = 1.5) +
  ggtitle('y = 2x - x^2') +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate('text',
           size = 5,
           x = 0.25,
           y = 1.35,
           label = paste('cor = ', sprintf('%5.3f',
                                           cor(x, y))))
plot(g1)

# ピアソンの積率相関係数を求め、相関がないことを確認する
cor.test(x, y)
# スピアマンの順位相関係数で計算する
cor.test(x, y, method = 'spearman')
# ケンドールの順位相関係数で計算する
cor.test(x, y, method = 'kendall')

# スピアマンの順位相関係数とケンドールの順位相関係数の関係を確認する
# 0から1までの一様乱数を10個発生させて、それぞれの相関係数を計算する
M <- 10
N <- 200
x <- matrix(runif(M * N, 0, 1), nrow = M, ncol = N)
y <- matrix(runif(M * N, 0, 1), nrow = M, ncol = N)
spearman_cor <- cor(x, y, method = 'spearman')
kendall_cor <- cor(x, y, method = 'kendall')

#データフレームに格納して散布図を描く
df_cor <- data.frame(
  spearman_cor <- c(spearman_cor),
  kendall_cor <- c(kendall_cor)
)
g2 <- ggplot(df_cor,
            aes(x = spearman_cor,
                y = kendall_cor))
g2 <- g2 +
  geom_point(shape = 21,
             size = 0.5) +
  ggtitle('スピアマンの相関係数とケンドールの相関係数の関係') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Spearman') +
  ylab('Kendall')
plot(g2)

# 順位相関係数はサンプルサイズを大きくすると、漸近的に
# （ケンドールの相関係数） ??? 2/3（スピアマンの順位相関係数）が成り立つ
M_2 <- 50
N_2 <- 200
x <- matrix(runif(M_2 * N_2, 0, 1), nrow = M_2, ncol = N_2)
y <- matrix(runif(M_2 * N_2, 0, 1), nrow = M_2, ncol = N_2)
spearman_cor <- cor(x, y, method = 'spearman')
kendall_cor <- cor(x, y, method = 'kendall')

#データフレームに格納して散布図を描く
df_cor <- data.frame(
  spearman_cor <- c(spearman_cor),
  kendall_cor <- c(kendall_cor)
)
g3 <- ggplot(df_cor,
            aes(x = spearman_cor,
                y = kendall_cor))
g3 <- g3 +
  geom_point(shape = 21,
             size = 0.5) +
  ggtitle('スピアマンの相関係数(ρ)とケンドールの相関係数(τ)の関係：\n
          サンプルサイズが大きいと漸近的に τ ??? 2/3 ρ') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Spearman') +
  ylab('Kendall')
g3 <- g3 +
  stat_function(fun = function(x) 2 * x / 3)
plot(g3)


# 時系列と自己相関(p.55)
install.packages('dplyr')
install.packages('forecast')
library(dplyr)
library(forecast)

# 人為的データを作成する(p.57)
data <- c(4.96, 11.15, 14.37, 9.25, 1.01, -0.38, 7.49, 16.83, 11.21, 3.11,
          3.03, 8.70, 16.47, 14.29, 1.89, -7.99, -5.91, 6.58, 12.30, 14.35,
          4.65, -1.31, -0.71, 8.65, 16.97, 16.11, 4.64, -1.01, 0.78, 8.20,
          12.95, 9.55, 8.61, 4.88, -1.45, 0.99, 3.35, 2.78, 4.68, 2.71,
          7.11, 13.22, 16.01, 14.78, 3.08, -7.12, -0.53, 11.28, 17.80, 13.18,
          5.51, -2.82, -7.13, 1.91, 15.28, 16.08, 9.64, 2.60, 1.16, 3.51)

# 時系列データに変換する
data_ts <- ts(data, start = c(1:50), frequency = 1)
class(data_ts)

# ggplot2で可視化するためにデータフレームに変換する
data_ts_to_df <- data.frame(
 x = time(data_ts),
 y = data_ts
)

# 時系列データをプロットする
# 5, 6時点後に元に戻る周期性が見受けられる
g4 <- ggplot(data_ts_to_df,
            aes(x = x))
g4 <- g4 +
  geom_line(aes(y = y))+
  ggtitle('時系列データのプロット（人為的データ）') +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0, 65, 5)) +
  scale_y_continuous(breaks = seq(-10, 20, 2))
plot(g4)

# 自己相関係数のコレログラムを作成する
# ラグh = 6でピークが見られ、周期性が確かめられる
acf <- forecast::ggAcf(data_ts,
                lag.max = 12)
acf <- acf +
  ggtitle('自己相関係数のコレログラム') +
  theme(plot.title = element_text(hjust = 0.5))
plot(acf)

# 練習問題
# 3.1_社会経済指標と投票行動
自民党得票率 <- c(41.4, 76.3, 59.2, 51.8, 52.5, 53.2, 62.4, 55.0, 57.7, 63.2,
                  37.5, 48.5, 32.4, 20.5, 47.9, 68.9, 68.5, 52.5, 63.3, 58.8,
                  59.7, 48.4, 40.7, 51.0, 50.9, 34.3, 25.8, 32.1, 34.4, 55.1,
                  60.3, 57.0, 45.6, 54.2, 55.1, 55.7, 70.3, 61.8, 47.6, 42.5,
                  71.3, 55.2, 65.2, 42.9, 54.7, 62.0, 48.2)
持ち家比率 <-   c(52.8, 71.2, 72.6, 63.7, 81.3, 81.8, 70.9, 74.0, 73.2, 72.9,
                  66.7, 65.7, 43.7, 55.5, 79.6, 85.7, 75.3, 80.5, 73.0, 77.0,
                  77.5, 69.2, 60.0, 78.2, 79.5, 61.8, 49.6, 59.6, 72.1, 71.0,
                  76.3, 72.8, 71.8, 60.7, 67.0, 71.8, 71.2, 68.3, 68.5, 54.8,
                  76.0, 65.8, 69.4, 66.9, 69.7, 71.2, 59.6)
data_3_1 <- data.frame(
  x <- 自民党得票率,
  y <- 持ち家比率
)
# 散布図を描く
g5 <- ggplot(data_3_1,
            aes(x = x,
                y = y)
            )
g5 <- g5 +
  geom_point() +
  ggtitle('1983年総選挙での自民党得票率と持ち家比率') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('自民党得票率（%）') +
  ylab('持ち家比率（%）')
plot(g5)

# 相関係数を求める
cor_3_1 <- cor(自民党得票率, 持ち家比率)
cor_3_1

# 散布図に回帰直線を引く
g5 <- g5 +
  stat_smooth(method = lm)
plot(g5)

# 3.4_ブートストラップ法
men_h <-   c(71, 68, 66, 67, 70, 71, 70, 73, 72, 65, 66)
women_h <- c(69, 64, 65, 63, 65, 62, 65, 64, 66, 59, 62)
data_3_4 <- data.frame(
  x <- men_h,
  y <- women_h
)

# 散布図を描く
g6 <- ggplot(data_3_4,
            aes(x = x,
                y = y)
            )
g6 <- g6 +
  geom_point(position = position_jitter(height = 0, width = 0.1)) +
  ggtitle("11家族内での兄弟と姉妹の身長の組") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('兄弟の身長 (x)') +
  ylab('姉妹の身長 (y)')
plot(g6)

# i) [1, 11]に属する整数の乱数を発生させる
random_int <- sample(1:11, replace = TRUE)
random_int

# ii) i)を11回繰り返して実行し、
# data_3_4からその番号のデータを取り出し（同一番号は重複して取り出す）、
# 相関係数rを計算する
random_int_11times <- sample(1:11, 11, replace = TRUE)
men_h[random_int_11times]
women_h[random_int_11times]
# 相関係数rを計算する
cor(men_h[random_int_11times],
    women_h[random_int_11times])

# iii) ii)を200回繰り返し、相関係数r1, ..., r200を得て、
# そのヒストグラムを作る（ブートストラップ）

# データの格納
r <- numeric(200)

#ブートストラップ
for (i in 1:200) {
random_int_11times <- sample(1:11, 11, replace = TRUE)
r[i] <- cor(men_h[random_int_11times],
            women_h[random_int_11times])
}
# rをデータフレームに変換する
r_df <- data.frame(x <- c(r))

# ヒストグラムを描く
g <- ggplot(r_df,
            aes(x = r))
g <- g +
  geom_histogram(bins = 10, alpha = 0.7) +
  ggtitle("11家族内での兄弟と姉妹の身長の組の相関（ブートストラップ）") +
  theme(plot.title = element_text(hjust = 0.5))
plot(g)