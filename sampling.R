# Sampling

rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)

#Dataframe 1


h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10,
                height = h,
                unit = "cm")
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)


#Dataframe 2


h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


#parameter inference----------------------------------------------
df_h0 <- read_csv("data_src/data_plant_height.csv")
print(df_h0)

#calcualting true mean
mu <- mean(df_h0$height)
#calcualting true variance
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)
print(sigma2)


#random sampling (rows)

df_i <- df_h0 %>% 
  sample_n(size = 10)

print(df_i)

#for loop, randomly selecting 10 individuals and then mean and variance for selected subset
mu_i <- var_i <- NULL
for (i in 1:1000) {
  df_i <- df_h0 %>% 
   sample_n(size = 10)
  mu_i[i] <- mean(df_i$height)
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
}


#visualizing 
df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

#histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

#histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# layout vertically and understanding patchwork
g_hor <- g_mu + g_var #from class R script but not in the textbook
g_mu / g_var


#Correcting bias
#From class R script and textbook

var_ub_i <- NULL
for (i in 1:1000) {
df_i <- df_h0 %>% 
sample_n(size = 10)
var_ub_i[i] <- var(df_i$height)}

#this adds a new column
df_sample <- df_sample %>% 
  mutate(var_ub_hat = var_ub_i)

g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

#This makes a figure for variance, unbiased variance, and mean

g_var <- g_var + scale_x_continuous(limits = c(0, 80))
g_var_ub <- g_var_ub + scale_x_continuous(limits = c(0, 80))
g_mu / g_var / g_var_ub

