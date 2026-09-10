#Descriptive statistics lab

library(tidyverse)

# Central tendency --------------------------------------------------------
#7.3.1 Comparing Central Tendency Measures
#What are the differences of the three measures of central tendency? To investigate this further, let’s perform the following exercise.

#Create a new vector z with length 100 as exp(rnorm(n = 100, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median of z.

z <- exp(rnorm(n = 100, mean = 0, sd = 0.1))
arithmetic_mean_z <- mean(z)
geometric_mean_z <- prod(z)^(1/length(100))
median_z <- median(z)



#Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().

#Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline() .

#Visually compare the values of the central tendency measures with the vertical lines drawn by geom_vline().
z_tibble <- tibble(z)

z_tibble %>% 
  ggplot(mapping = aes(x = z )) +
  geom_histogram() +
geom_vline(xintercept = arithmetic_mean_z, color = "pink") +
geom_vline(xintercept = geometric_mean_z, color = "darkblue") +
geom_vline(xintercept = median_z, color= "red")


#Create a new vector z_rev as -z + max(z) + 0.5, and repeat step 1 – 4.

z_rev <- (z + max(z) + .5)

z_rev <- exp(rnorm(n = 100, mean = 0, sd = 0.1))
arithmetic_mean_z_rev <- mean(z_rev)
geometric_mean_z_rev <- prod(z_rev)^(1/length(100))
median_z_rev <- median(z_rev)

z_rev_tibble <- tibble(z_rev)

z_rev_tibble %>% 
  ggplot(mapping = aes(x = z_rev )) +
  geom_histogram() +
  geom_vline(xintercept = arithmetic_mean_z_rev, color = "green") +
  geom_vline(xintercept = geometric_mean_z_rev, color = "orange") +
  geom_vline(xintercept = median_z_rev, color= "purple")



# Variation ---------------------------------------------------------------

#7.3.2 Comparing Variation Measures
Why do we have absolute (variance, SD, MAD, IQR) and relative measures (CV, MAD/Median) of variation? To understand this, suppose we have 100 measurements of fish weight in unit “gram.” (w in the following script)

#w <- rnorm(100, mean = 10, sd = 1)
#head(w) # show first 10 elements in w
## [1]  8.734939  9.313147  9.554338 11.224082 10.359814 10.400771
#Using this data, perform the following exercise:

#Convert the unit of w to “milligram” and create a new vector m.

#Calculate SD and MAD for w and m.

#Calculate CV and MAD/Median for w and m.


