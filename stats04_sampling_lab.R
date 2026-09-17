#We used 10 plants to estimate sample means and variances. Obtain 100 sub-datasets with 50 and 100 measures each, and draw histograms of sample means and unbiased variances (use var()).

#Sample means and unbiased variances are unbiased if samples are randomly selected.
#What happens if samples are non-random? Suppose the investigator was unable to find 
#plants less than 10 cm in height – the following code excludes those less than 10 cm in height:
 
#Repeat step 1 with df_h10 instead of df_h0 and compare the results.

  library(tidyverse)
 

rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)

df_h0 <- read_csv("data_src/data_plant_height.csv")

mu <- mean(df_h0$height)

sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)



mu_i_50 <- var_i_50 <- NULL

for (i in 1:100) {
  df_i_50 <- df_h0 %>% 
    sample_n(size = 50)
  
  mu_i_50[i] <- mean(df_i_50$height)
  

  var_i_50[i] <- var(df_i_50$height)
}

df_i_50 <- tibble(mu_hat_50 = mu_i_50,
                    var_hat_50 = var_i_50)

g_mu_50 <- df_i_50 %>% 
  ggplot(aes(x = mu_hat_50)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var_50 <- df_i_50 %>% 
  ggplot(aes(x = var_hat_50)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_mu_50
g_var_50

mu_i_100 <- var_i_100 <- NULL

for (i in 1:100) {
  df_i_100 <- df_h0 %>% 
    sample_n(size = 100)
  
  mu_i_100[i] <- mean(df_i_100$height)
  
  
  var_i_100[i] <- var(df_i_100$height)
}

df_i_100 <- tibble(mu_hat_100 = mu_i_100,
                  var_hat_100 = var_i_100)

g_mu_100 <- df_i_100 %>% 
  ggplot(aes(x = mu_hat_100)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var_100 <- df_i_100 %>% 
  ggplot(aes(x = var_hat_100)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_mu_100
g_var_100


# part 2 ------------------------------------------------------------------




df_h10 <- df_h0 %>% 
  filter(height >= 10)

mu_2 <- mean(df_h10$height)

sigma2_2 <- sum((df_h10$height - mu_2)^2) / nrow(df_h10)

mu_i_50_2 <- var_i_50_2 <- NULL

for (i in 1:100) {
  df_i_50_2 <- df_h10 %>% 
    sample_n(size = 50)
  
  mu_i_50_2[i] <- mean(df_i_50_2$height)
  
  
  var_i_50_2[i] <- var(df_i_50_2$height)
}

df_i_50_2 <- tibble(mu_hat_50_2 = mu_i_50_2,
                  var_hat_50_2 = var_i_50_2)

g_mu_50_2 <- df_i_50_2 %>% 
  ggplot(aes(x = mu_hat_50_2)) +
  geom_histogram() +
  geom_vline(xintercept = mu_2)

g_var_50_2 <- df_i_50_2 %>% 
  ggplot(aes(x = var_hat_50_2)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2_2)

g_mu_50_2
g_var_50_2

mu_i_100_2 <- var_i_100_2 <- NULL

for (i in 1:100) {
  df_i_100_2 <- df_h10 %>% 
    sample_n(size = 100)
  
  mu_i_100_2[i] <- mean(df_i_100_2$height)
  
  
  var_i_100_2[i] <- var(df_i_100_2$height)
}

df_i_100_2 <- tibble(mu_hat_100_2 = mu_i_100_2,
                    var_hat_100_2 = var_i_100_2)

g_mu_100_2 <- df_i_100_2 %>% 
  ggplot(aes(x = mu_hat_100_2)) +
  geom_histogram() +
  geom_vline(xintercept = mu_2)

g_var_100_2 <- df_i_100_2 %>% 
  ggplot(aes(x = var_hat_100_2)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2_2)

g_mu_100_2
g_var_100_2


