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

ggplot(data = filter(dat, state == "California")) + 
  geom_point(mapping = aes(x = year, y = per100000rate)) + 
  geom_vline(xintercept = 1965)

#Question 3.

dat_caliFocus <- filter(us_contagious_diseases, 
                        state == "California", between(year,1950,1979))

dat_caliFocus$yearBlock[dat_caliFocus$year < 1960 & dat_caliFocus$year > 1949] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year < 1970 & dat_caliFocus$year > 1959] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year < 1980 & dat_caliFocus$year > 1969] <- "1970’s"

# without square root
ggplot(data = dat_caliFocus) + 
  geom_bar(mapping = aes(x = state,y = count, fill = yearBlock), 
           position = "dodge", stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust=-0.01))

# square root count
ggplot(data = dat_caliFocus) + 
  geom_bar(mapping = aes(x = state,y = sqrt(count), fill = yearBlock), 
           position = "dodge", stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust=-0.01))

#Question 4.

dat_stateFocus <- filter(us_contagious_diseases, between(year,1950,1979))

dat_stateFocus$yearBlock[1960 > dat_stateFocus$year && dat_stateFocus$year > 1949] <- "1950’s"
dat_stateFocus$yearBlock[1970 > dat_stateFocus$year && dat_stateFocus$year > 1959] <- "1960’s"
dat_stateFocus$yearBlock[1980 > dat_stateFocus$year && dat_stateFocus$year > 1969] <- "1970’s"

ggplot(data = dat_stateFocus) + 
  geom_bar(mapping = aes(x = state,y = sqrt(count), fill = yearBlock), 
           position = "dodge", stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust=-0.01))

#Question 5.

dat_stateRate <- dat_stateFocus %>% 
  mutate(per100000rate = count * 100000 * weeks_reporting / (population * 52))

#fill = square root count, yearBlock/state
ggplot(data = dat_stateRate, mapping = aes(x = yearBlock,y = state)) + 
  geom_tile(mapping = aes(fill = sqrt(count), colour = "grey"))

#fill = square root count, year/state
ggplot(data = dat_stateRate, mapping = aes(x = year,y = state)) + 
  geom_tile(mapping = aes(fill = sqrt(count), colour = "grey"))

#fill = rate, year/state
ggplot(data = dat_stateRate, mapping = aes(x = year,y = state)) + 
  geom_tile(mapping = aes(fill = per100000rate, colour = "grey"))

#fill = rate, yearBlock/state
ggplot(data = dat_stateRate, mapping = aes(x = yearBlock,y = state)) + 
  geom_tile(mapping = aes(fill = per100000rate, colour = "grey"))

#Question 6.
library(readr)
autismReport <- read_csv("autismReport.csv")
# https://www.ncbi.nlm.nih.gov/books/NBK332896/
# https://www.dds.ca.gov/Autism/docs/AutismReport_2007.pdf
ggplot(data = autismReport, mapping = aes(x = year, y = year_s_pop)) + geom_line()

# This is the only yearly data I found on the internet, it's from 1986 to 2006, collected in California
# We can see a very obvious increasement since then. However, the graph we made earlier are
# from 1950 to 1970. So this autism data can only shows the cases increased after 1970, which
# could be irrelevant to the vaccine. But we cannot conclude anything from this for now.




