#Normal Distribution
#The function rnorm() produces a random variable that follows a Normal distribution with a specified mean and SD. Using this function,

#Generate a variable with 50 observations.
#Create a figure similar to Figure 9.3


pacman:: p_load(tidyverse, patchwork)

x <- rnorm(n = 50, mean = 80, sd = 2)

mu <- mean(x)
sigma <- sd(x)
pd <- dnorm(x, mean = mu, sd = sigma)


x_min <- floor(min(x))
x_max <- ceiling(max(x))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p_up <- pnorm(bin[i+1], mean = mu, sd = sigma) 
  p_low <- pnorm(bin[i], mean = mu, sd = sigma)
  
  p[i] <- p_up-p_low 
}



df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * 50)



df_x <-tibble(x)


  df_x %>% ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "darkblue") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "darkblue")

#  9.4.2 Poisson Distribution
#The function rpois() produces a random variable that follows a Poisson distribution with a specified mean. Using this function,
  
#Generate a variable with 1000 observations.
#Create a figure similar to Figure 9.7
  
  #Poisson fit
 z <- rpois( n = 1000, lambda = 15)
  
 
 lambda <- mean(z)
 bin <- seq(min(z), max(z), by = 1)
 
 pm <- dpois( x = bin, lambda = lambda)
 
 df_z <- tibble(z = z)
 
 df_prob <- tibble( pm = pm, bin= bin) %>% mutate(freq = pm * nrow(df_z))
 

 df_z %>%  
 ggplot(
  aes(x = z)) +
 geom_histogram(
  binwidth = 0.5,
center = 0
 ) +
   geom_point( data = df_prob, aes( x= bin, y = freq), color = "skyblue" ) +
   geom_line(data = df_prob,
       aes(
       x= bin,
       y= freq
       ), color = "skyblue",
       linetype = "dashed")

