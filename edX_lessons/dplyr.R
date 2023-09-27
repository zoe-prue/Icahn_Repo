# dplyr
# ZMP
paste("#" ,format(Sys.Date(),format="%d %b %Y"))

# Reading in .csv
library(readr)
dat <- read_csv("raw_data/femaleMiceWeights.csv")
head(dat)

#filter only the data of mice with "chow" diet
library(dplyr) 
chow <- filter(dat, Diet=="chow") #keep only the ones with chow diet
head(chow)

# select only the weights of the mice
chowVals <- select(chow,Bodyweight)
head(chowVals)

# using pipes (%>%)
# e.g. selecting only chow diet rows, then getting the weights
chowVals <- filter(dat, Diet=="chow") %>% select(Bodyweight)
head(chowVals)

#dplyr returns a dataframe if it received a dataframe. 
# turn object into class numeric at last step using unlist
chowVals <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
class( chowVals )

#equivalent in R
chowVals <- dat[ dat$Diet=="chow", colnames(dat)=="Bodyweight"]
head(chowVals)
# creating a dataframe that is a subset of dat, 
# where the rows are included if == "chow", and 
# columns are bodyweight values


