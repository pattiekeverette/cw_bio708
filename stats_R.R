#Descriptive statistics

library(tidyverse)

x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)


#central tendency 


#Arithmetic mean
mu_x <- sum(x) / length(x)
mean(x)
mu_y <- sum(y) / length(y)
mean(y)

#geometric mean
(mu_x_ge <- prod(x)^(1/length(x)))

exp(sum(log(x)) / length(x))

(mu_y_ge <- exp(sum(log(y)) / length(y)))

#median
med_x <- median(x)
med_y <- median(y)

variance
(var_x <- sum((x - mean(x))^2) / length(x))
(var_y <- sum((y - mean(y))^2) / length(x))

sqrt(var_x)
sqrt(var_y)

#inter-quartile range
x_1 <- quantile (x, .25)
x_h <- quantile(x, 0.75)
(iqr_x <- xh - x_1)

y_q <- quantile(y, c(.25, .75))
(iqr_y <- y_q[2] - y_q[1])


#MAD
ad_x <- abs(x - median(x))
median(ad_x)

ad_y <- abs(y - median(y_))
median(ad_y)

#Coefficient of variation
sd_x <- sqrt(var_x)
sd_y <- sqrt(var_y)
(cv_x <- sd_x / mu_x)


#extra ---------------------------------------------------------------------------------------

mycv <- function(v) {
  mu <- mean(v)
  s <- sd/(v)
  cv <- s / mu
  
  return(cv)
}

mycv(x)



2
4
8
16
32
64
128
256
512
1024
2048
4096
8192

