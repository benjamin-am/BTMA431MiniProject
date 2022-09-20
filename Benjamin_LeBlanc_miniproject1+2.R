#### Ben LeBlanc Mini Project Presentation

library(httr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(stringr)
library(plotly)



url <-'https://data.calgary.ca/resource/vdjc-pybd.json?$limit=50000&$offset=0'

business.license.data <- httr::GET(url = url)

business.license.data <- content(business.license.data, as = "text")

busn.license <- do.call("rbind.data.frame", lapply(business.license.data, fromJSON))

# i want to change the address column to just include SW, SE, NW, NE. So I can do an analysis based on the four subsections
# of calgary. I will also use the neighbourhood column, if the 4 quadrants are not enough.

busn.license$quadrant <- str_sub(busn.license$address , start= -2)

# Licensed - All requirements have been met, the invoice has been paid and this new business is currently licensed.
# Renewal Licensed - This is a renewal licence that has been paid.
# we are going to focus on these two types of licenses

busn.license <- busn.license %>%
  filter((tolower(jobstatusdesc) == 'renewal licensed' | tolower(jobstatusdesc) == 'licensed' )) 


busn.license$licencetypes <- as.factor(busn.license$licencetypes)

levels(busn.license$licencetypes)

# [3] "ALCOHOL BEVERAGE MANUFACTURER"                                 
# [4] "ALCOHOL BEVERAGE SALES (DRINKING EST/RESTAURANT)"              
# [5] "ALCOHOL BEVERAGE SALES (DRINKING ESTABLISHMENT)"               
# [6] "ALCOHOL BEVERAGE SALES (RESTAURANT)"      
# [15] "CANNABIS FACILITY"                                             
# [16] "CANNABIS STORE" 

busn.license <- busn.license %>%
  filter((tolower(licencetypes) == 'alcohol beverage manufacturer' | tolower(licencetypes) == 'alcohol beverage sales (drinking est/restaurant)'
          |  tolower(licencetypes) == 'alcohol beverage sales (drinking establishment)' | tolower(licencetypes) == 'alcohol beverage sales (restaurant)'
          | tolower(licencetypes) == 'cannabis facility'  | tolower(licencetypes) == 'cannabis store'))



busn.license$licencetypes <- ifelse(grepl(pattern = "ALCOHOL", busn.license$licencetypes), "ALCOHOL", "CANNABIS")

busn.license$licencetypes <- as.factor(busn.license$licencetypes)
busn.license$quadrant <- as.factor(busn.license$quadrant)
busn.license$comdistnm <- as.factor(busn.license$comdistnm)

# first we want to see the levels of alcohol per quadrant

busn.license.alcohol <- busn.license %>%
  filter(licencetypes == "ALCOHOL") 

table(busn.license.alcohol$quadrant)

as.numeric(table(busn.license.alcohol$quadrant))

p <- plot_ly(
  x = c("NE", "NW", "SE", "SW"), 
  y = as.numeric(table(busn.license.alcohol$quadrant)),
  name = "Where to Buy Alcohol via Quadrant",
  type = "bar"
) %>%
  layout(yaxis = list(title = 'Count of Alcohol Shops'), barmode = 'group')
p



# we can also look via community


# as we look, it seems that the south side of the city has more places to buy alcohol/to get intoxicated.
# now let's look at cannabis

busn.license.canna <- busn.license %>%
  filter(licencetypes == "CANNABIS") 

table(busn.license.canna$quadrant)

as.numeric(table(busn.license.canna$quadrant))

p1 <- plot_ly(
  x = c("NE", "NW", "SE", "SW"), 
  y = as.numeric(table(busn.license.canna$quadrant)),
  name = "Where to Buy Alcohol via Quadrant",
  type = "bar"
) %>%
  layout(yaxis = list(title = 'Count of Cannabis Shops'), barmode = 'group')
p1
# comparing the two it looks quite similar. The south quadrants of the city have the higher count of stores, while the NE is catching up and the NW falls behind. 



quadrants <- c("NE", "NW", "SE", "SW")
alcohol <- as.numeric(table(busn.license.alcohol$quadrant))
cannabis <- as.numeric(table(busn.license.canna$quadrant))
data <- data.frame(quadrants, alcohol, cannabis)

p2 <- plot_ly(data, x = ~quadrants, y = ~alcohol, type = 'bar', name = 'alcohol') %>%
  add_trace(y = ~cannabis, name = 'cannabis') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p2


# now that we have visualed our question as to which quadrant of the city contains the most accessible alcohol and cannabis, lets put it towards a hypothesis test
# does each quadrant have a similar number of shops to the city average, or is there a statistical difference?
# for this test we will be using both cannabis and alcohol, not seperating it


city.mean <- mean(as.numeric(table(busn.license$quadrant)))


# now we will conduct 4 seperate hypothesis tests, to see if any quadrant of the city is statistically different to the mean of the city
# for the 2 Northern quadrants I will test to see if they are less than average
# for the two southern quadrants I will do a hypothesis test to see if they are more than average
# more specifically a proportion test

table(busn.license$quadrant)
as.numeric(table(busn.license$quadrant))[1]

nrow(busn.license)



NEt.test <- binom.test(x = as.numeric(table(busn.license$quadrant))[1], n = nrow(busn.license) , p = 0.25, alternative = "less")

NEt.test
# P-value = 2.166e-08

NWt.test <- binom.test(x = as.numeric(table(busn.license$quadrant))[2], n = nrow(busn.license), p = 0.25, alternative = "less")

NWt.test
# p-value < 2.2e-16


# now for the two southern quadrants I will do greater than tests, as we saw in the visualizations they had higher counts than the northern quadrants

SEt.test <- binom.test(x = as.numeric(table(busn.license$quadrant))[3], n = nrow(busn.license) , p = 0.25, alternative = "greater")

SEt.test
# p-value = 5.419e-05

SWt.test <- binom.test(x = as.numeric(table(busn.license$quadrant))[4], n = nrow(busn.license), p = 0.25, alternative = "greater")

SWt.test
# p-value < 2.2e-16

# all the p-values come back, and we can reject the null-hypothesis on each one of them.
# So in the Northern quadrants in the city, we can say that they have less stores to purchase alcohol and cannabis,
# while the Southern quadrants of the city have more stores to purchase alcohol and cannabis.

# in conclusion, the part of the city that parties the most, based off of visualization and hypothesis testing is the SW.

