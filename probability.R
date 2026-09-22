# Probavility distribution

rm(list=ls())
pacman::p_load(tidyverse, patchwork)

df_h0 <- read_csv("data_src/data_plant_height.csv")

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_vline(aes(xintercept = mean(height)))

#draw probabilty distribution

x <- seq(min(df_h0$height), max(df_h0$height), length = 100)


mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

tibble(y = pd, x = x) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  labs(y = "Probability density", x= "plant height")


#Converting probaility density to frequency

p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10)

p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)

p20_10 <- p20 - p10
print(p20_10)

x_min <- floor(min(df_h0$height))
x_max <- ceiling(max(df_h0$height))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p_up <- pnorm(bin[i+1], mean = mu, sd = sigma) 
  p_low <- pnorm(bin[i], mean = mu, sd = sigma)
}

p[i] <- p_up-p_low #calculate the difference

#combine data and PDF

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))


df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")

df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin


#Poisson fit
x <- seq(0, 10, by = 1)


lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)


#figure

tibble(y = pm, x = x) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") +
  labs(y = "Probability",
       x = "Count")
df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count))
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed", color = "steelblue") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq, color = "steelblue"))
