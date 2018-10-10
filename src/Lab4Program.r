# Name:
# Date:

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

#Question 1.

dat <- us_contagious_diseases %>% 
    mutate(per100000rate = count * 100000 * weeks_reporting / (population * 52)) %>%
    filter(disease == "Measles", state != "Alaska" & state != "Hawaii")

#Question 2.

ggplot(data = filter(dat, state == "California")) 
+ geom_point(mapping = aes(x = year, y = per100000rate)) 
+ geom_vline(xintercept = 1965)

#Question 3.

dat_caliFocus <- filter(us_contagious_diseases, state == "California")

dat_caliFocus$yearBlock[dat_caliFocus$year == 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year == 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year == 1970] <- "1970’s"

ggplot(data = dat_caliFocus) 
+ geom_bar(mapping = aes(x = state,y = count, fill = yearBlock), 
           position = "dodge", stat = "identity") 
+ theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust=-0.01))

ggplot(data = dat_caliFocus)
+ geom_bar(mapping = aes(x = state,y = sqrt(count), fill = yearBlock), 
           position = "dodge", stat = "identity") 
+ theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust=-0.01))

#Question 4.

us_contagious_diseases$yearBlock[us_contagious_diseases$year == 1950] <- "1950’s"
us_contagious_diseases$yearBlock[us_contagious_diseases$year == 1960] <- "1960’s"
us_contagious_diseases$yearBlock[us_contagious_diseases$year == 1970] <- "1970’s"

ggplot(data = us_contagious_diseases) 
+ geom_bar(mapping = aes(x = state,y = sqrt(count), fill = yearBlock), 
           position = "dodge", stat = "identity") 
+ theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust=-0.01))

#Question 5.

dat <- us_contagious_diseases %>% 
  mutate(per100000rate = count * 100000 * weeks_reporting / (population * 52)) 

ggplot(data = dat, mapping = aes(x = year,y = state)) 
+ geom_tile(mapping = aes(fill = per100000rate))

#Question 6.





