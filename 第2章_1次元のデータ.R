install.packages('ggplot2')
library(ggplot2)

# X ~ N(50, 10^2)から抽出された標本(n = 373)
set.seed(123)
random_data_X <- data.frame(
  value = rnorm(n = 373, mean = 50, sd = 10)
)

# データの確認
head(random_data_X)

# Xのヒストグラムを書く
g <- ggplot(random_data_X,
            aes(x = value))
g <- g +
  geom_histogram(fill = 'grey50') +
  ggtitle('Histgram without any adjustments') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('score') 
plot(g)

# スタージェスの公式によって階級数を決定する(p.22)
k <- 1 + log2(373)
k

# 上のヒストグラムの階級数：k = 10とする
g <- ggplot(random_data_X,
            aes(x = value))
g <- g +
  geom_histogram(bins = 10, fill = 'grey50') +
  ggtitle("Histgram with Sturges' formula") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('score')
plot(g)

# ローレンツ曲線(Lorenz Curve as LC)を描く
# x軸：事業所数の累積相対度数(CRF_x)
# y軸：従業者数の累積相対度数(CRF_y)
CRF_x <- c(0.682, 0.854, 0.963, 0.982, 0.993, 0.999, 0.999, 1.000, 1.000)
CRF_y <- c(0.194, 0.341, 0.568, 0.663, 0.767, 0.884, 0.919, 0.953, 1.000)

# ローレンツ曲線を描く関数を作る
LC = function (a, b) {
  # ジニ係数の算出
  GI <- sprintf('%5.3f',
                2 * sum(a - b) / length(a)
                )
  a <- c(0.000, a)
  b <- c(0.000, b)
  df <- data.frame(a, b)
  Graph <- ggplot(df, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_line(data = data.frame(x = (0:100) / 100),
              aes(x = x, y = x),
              linetype = 'dotted',
              color = 'red',
              size = 1) +
    # ジニ係数をグラフに表示する
    annotate('text',
             size = 5,
             x = 0.25,
             y = 0.90,
             label = paste("GINI index = ", GI)) +
    ggtitle('Lorenz Curve') +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab('Cumulative Relative Frequency_x') +
    ylab('Cumulative Relative Frequency_y')
  plot(Graph)
}

# 事業所規模のローレンツ曲線をグラフへ出力する
# 大多数の中小零細事業所とごく少数の大規模事業書が存在することがわかる
LC(CRF_x, CRF_y)

# 練習問題_2.2
# 1次元データのジニ係数を算出する関数を作る
gini_index <- function(x) {
  #引数のベクトルの全組み合わせをデータフレームに収容する
  expand_df <- expand.grid(x, x)
  sum(abs(expand_df[, 1] - expand_df[, 2])) / (2 * length(x) ^ 2 * mean(x))
}

# データの登録(p.35)
data_a <- c(0, 3, 3, 5, 5, 5, 5, 7, 7, 10)
data_b <- c(0, 1, 2, 3, 5, 5, 7, 8, 9, 10)
data_c <- c(3, 4, 4, 5, 5, 5, 5, 6, 6, 7)

# 登録したデータのジニ係数を計算する
gini_index(data_a)
gini_index(data_b)
gini_index(data_c)


# 練習問題_2.3
# エントロピーを計算する関数を作る
entropy <- function(target_data) {
  freq <- target_data / sum(target_data)
  log_freq <- log2(freq)
  # log(0) = 0と定義する
  log_freq[log_freq == -Inf] <- 0
  # エントロピーを計算する
  ent <- -1 * sum(freq * log_freq)
  return(ent)
}

# データの登録(p.40)
data_this_year <- c(32, 19, 10, 24, 15)
data_10years_ago <- c(28, 13, 18, 29, 12)

# 登録したデータのエントロピーを計算する
entropy(data_this_year)
entropy(data_10years_ago)

# 練習問題_2.4
#データB(p.35, data_b)の標準得点と偏差値得点を求める
# 初めに標本標準偏差を求める関数を作る
sd <- function(x) {
  var(x) * (length(x) - 1) / length(x)
}
# 標準得点を求める関数を作る
standard_score <- function(target_data) {
  std_score <- (target_data - mean(target_data)) / sd(target_data)
  return(std_score)
}

# データBの標準得点を求める
standard_score(data_b)

# データBの偏差値得点を求める
10 * standard_score(data_b) + 50
