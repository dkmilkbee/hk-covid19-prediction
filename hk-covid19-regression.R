# R Script for Data Analytics in Finance using R project
# Ver. final (initial: 2022-4-14, debug test: 2022-4-30, commit date: 2022-4-30)
# File Update Date: 2022-4-30

#########################################################################
# Initialize & import required library
rm(list=ls())
if("dplyr" %in% rownames(installed.packages()) == FALSE)
  install.packages("dplyr")
library(dplyr)
if("tidyr" %in% rownames(installed.packages()) == FALSE)
  install.packages("tidyr")
library(tidyr)
if("ggplot2" %in% rownames(installed.packages()) == FALSE)
  install.packages("ggplot2")
library(ggplot2)
if("gridExtra" %in% rownames(installed.packages()) == FALSE)
  install.packages("gridExtra")
library(gridExtra)

# Import dataset (master)
# Data source: https://github.com/owid/covid-19-data
ds <- read.csv("owid-covid-data.csv")

# Normalize and cleaning
# Note: 
# -Keep empty data as NA for R tidy up (must avoid group to infinite)
# -Do not normalize zero value
# -Factor data when required

# Normalize Date/time
ds$date <- as.Date(ds$date, "%Y-%m-%d");

# Prepare data, filter location data and statistic data
dataLocation <- ds %>%
  filter(!grepl('^OWID.*',iso_code)) %>% # Remove other data not related to continent
  filter(date >= '2020-03-01' & date <= '2022-03-31') # Use data start from March20 (Since Feb20's data is incomplete)
dataStat <- ds %>%
  filter(grepl('^OWID.*',iso_code)) %>% # OWID_* is statistic data from OurWorld, we will not use this data. 
  filter(date >= '2020-03-01' & date <= '2022-03-31')

# ********************************************** #
# Prepare data for simple graphs

# Count total death by Continent / Country
vacc_ct <- dataLocation %>%
  group_by(continent) %>%
  summarize(mydata = sum(new_vaccinations_smoothed, na.rm=T))
death_ct <- dataLocation %>%
  group_by(continent) %>%
  summarize(mydata = sum(new_deaths_smoothed, na.rm=T))

# Plot some simple graph from above data
g1 <- ggplot(data = death_ct) + 
  geom_col(aes(x=continent, y=mydata, fill=continent)) +
  ggtitle("No. of deaths by Continent") +
  xlab("Continent") + ylab("No. of people") +
  theme(plot.title = element_text(hjust = 0.5))

g2 <- ggplot(data = vacc_ct) + 
  geom_col(aes(x=continent, y=mydata, fill=continent)) +
  ggtitle("No. of Vaccinated people by Continent") +
  xlab("Continent") + ylab("No. of people") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot
g1
g2
# grid.arrange(g1, g2, nrow=1)


# Seems too many location data in one graph, filter them by continent
# Prepare data
ct <- "Asia"
vacc_loc <- dataLocation %>%
  filter(continent == ct) %>%
  group_by(location) %>%
  summarize(mydata = sum(new_vaccinations_smoothed, na.rm=T))
death_loc <- dataLocation %>%
  filter(continent == ct) %>%
  group_by(location) %>%
  summarize(mydata = sum(new_deaths_smoothed, na.rm=T))

#Plot graph in Asia (Deaths/Vaccinate)
g1 <- ggplot(data = death_loc) +
  geom_col(aes(x=location, y=mydata, fill=location)) + 
  ggtitle("No. of deaths in Asia") +
  xlab("Location") + ylab("No. of people") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme(legend.position="none")

g2 <- ggplot(data = vacc_loc) +
  geom_col(aes(x=location, y=mydata, fill=location)) + 
  ggtitle("No. of vaccinated people in Asia") +
  xlab("Location") + ylab("No. of people") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme(legend.position="none")

grid.arrange(g1, g2, nrow=1) # One screen two plots

# ********************************************** #
# Focus on HK
# Prepare data
dataHK <- dataLocation %>%
  filter(iso_code == "HKG")

# Plot the number of death vs cases against by date (from a specific date range)
# Select values for plot
dataHK.p <- dataHK %>% 
  select(date, new_cases_smoothed, new_tests_smoothed)
#dataHK.p <- dataHK.p[complete.cases(dataHK.p),] # Select non-NA rows only
dataHK.p <- na.omit(dataHK.p)

# View(dataHK.p)
ggplot(data = dataHK.p) + 
  geom_line(aes(x=date, y=new_tests_smoothed), colour="red") +
  geom_line(aes(x=date, y=new_cases_smoothed), colour="blue") +
  scale_x_date(limit=c(as.Date("2021-03-01"), as.Date("2022-03-31"))) +
  ggtitle("Case and death against date in HK") +
  xlab("Date") + ylab("COVID19 Cases and Death") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("red", "blue"))


# Graph the total cases and vaccinations, preparing data
dataHK.p2 <- dataHK %>%
  select(date, total_cases, total_vaccinations, new_tests_smoothed, total_deaths)

# nrow(dataHK.p2)
# View(dataHK.p2)
# Plot some simple graphs about HK (cases/vaccinations/tests/deaths)
g1 <- ggplot(data = dataHK.p2) +
  geom_line(aes(x=date, y=total_cases), colour="red") + labs(title="cases vs date")
g2 <- ggplot(data = dataHK.p2) +
  geom_line(aes(x=date, y=total_vaccinations), colour="blue") + labs(title="vaccinations vs date")
g3 <- ggplot(data = dataHK.p2) +
  geom_line(aes(x=date, y=new_tests_smoothed), colour="black")+ labs(title="new tests vs date")
g4 <- ggplot(data = dataHK.p2) +
  geom_line(aes(x=date, y=total_deaths), colour="green") + labs(title="total deaths vs date")
grid.arrange(g1, g2, g3, g4, nrow=2, ncol=2)

# ********************************************** #
# Plot graph comparing HK data to selected location (mostly developed country) in Asia
# Prepare data
loc <- c("HKG", "TWN", "SGP", "KOR", "JPN", "MYS")
analysis <- c("new_cases_smoothed_per_million", "new_vaccinations_smoothed_per_million", 
              "new_tests_smoothed_per_thousand", "new_deaths_smoothed_per_million")
dataSel <- dataLocation %>%
  select(iso_code, location, date, any_of(analysis)) %>%
  filter(iso_code %in% loc)

dataSel <- na.omit(dataSel) # Remove NA

# Plot against cases/vaccinations/tests/deaths in HK
g1 <- ggplot() +
  geom_line(data = filter(dataSel, iso_code == "HKG"), aes(x=date, y=new_cases_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "TWN"), aes(x=date, y=new_cases_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "SGP"), aes(x=date, y=new_cases_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "KOR"), aes(x=date, y=new_cases_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "JPN"), aes(x=date, y=new_cases_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "MYS"), aes(x=date, y=new_cases_smoothed_per_million, group=iso_code, colour=iso_code)) +
  labs(title="New cases against date")
g2 <- ggplot() +
  geom_line(data = filter(dataSel, iso_code == "HKG"), aes(x=date, y=new_vaccinations_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "TWN"), aes(x=date, y=new_vaccinations_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "SGP"), aes(x=date, y=new_vaccinations_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "KOR"), aes(x=date, y=new_vaccinations_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "JPN"), aes(x=date, y=new_vaccinations_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "MYS"), aes(x=date, y=new_vaccinations_smoothed_per_million, group=iso_code, colour=iso_code)) +
  labs(title="New vaccinations against date")
g3 <- ggplot() +
  geom_line(data = filter(dataSel, iso_code == "HKG"), aes(x=date, y=new_tests_smoothed_per_thousand, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "TWN"), aes(x=date, y=new_tests_smoothed_per_thousand, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "SGP"), aes(x=date, y=new_tests_smoothed_per_thousand, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "KOR"), aes(x=date, y=new_tests_smoothed_per_thousand, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "JPN"), aes(x=date, y=new_tests_smoothed_per_thousand, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "MYS"), aes(x=date, y=new_tests_smoothed_per_thousand, group=iso_code, colour=iso_code)) +
  labs(title="New tests against date")
g4 <- ggplot() +
  geom_line(data = filter(dataSel, iso_code == "HKG"), aes(x=date, y=new_deaths_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "TWN"), aes(x=date, y=new_deaths_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "SGP"), aes(x=date, y=new_deaths_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "KOR"), aes(x=date, y=new_deaths_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "JPN"), aes(x=date, y=new_deaths_smoothed_per_million, group=iso_code, colour=iso_code)) +
  geom_line(data = filter(dataSel, iso_code == "MYS"), aes(x=date, y=new_deaths_smoothed_per_million, group=iso_code, colour=iso_code)) +
  labs(title="New deaths against date")

grid.arrange(g1, g4, nrow=2)
grid.arrange(g3, g2, nrow=2)

# ********************************************** #
# Graph the world map, import required libraries
if("maps" %in% rownames(installed.packages()) == FALSE)
  install.packages("maps")
library(maps)
if("RColorBrewer" %in% rownames(installed.packages()) == FALSE)
  install.packages("RColorBrewer")
library(RColorBrewer)
if("forcats" %in% rownames(installed.packages()) == FALSE)
  install.packages("forcats")
library(forcats)

# Create world map
world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region, n=3) # Convert region code to ISO code 3 digit

# Prepare data
# Total deaths percentage by location
df <- dataLocation %>%
  select(iso_code, total_deaths, population) %>%
  filter(total_deaths >= 0) %>%
  group_by(iso_code) %>%
  summarize(data = max(100*(total_deaths/population), na.rm=T))

# Define Map scales
df$data_scale <- cut(df$data,
                      breaks = c(-Inf, 0.005, 0.01, 0.05, 0.1, 0.3, 0.5, Inf), 
                      labels = c("Less than 0.005%", "0.01%-0.005%", "0.05%-0.01%", 
                                 "0.1%-0.05%", "0.3%-0.1%", "0.5-0.3%", "More than 0.5%"))

# Plot the map of death %, with 7 level scales in red, country border in black and do not fill area
ggplot(df) +
  geom_map(aes(map_id = iso_code, fill = fct_rev(data_scale)), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(name = "Total Deaths %", values = rev(brewer.pal(7, name = "Reds"))) +
  theme_void() +
  coord_fixed()

# Vaccinated people % by location
df <- dataLocation %>%
  select(iso_code, people_vaccinated, population) %>%
  filter(people_vaccinated >= 0) %>%
  group_by(iso_code) %>%
  summarize(data = max(people_vaccinated/population, na.rm=T))

df$data_scale <- cut(df$data, 
                     breaks = c(-Inf, 0.05, 0.1, 0.2, 0.4, 0.6, 0.9, Inf), 
                     labels = c("Less than 5%", "10%-5%", "20%-10%", "40%-20%", 
                                "60%-40%", "90%-60%", "More than 90%"))

world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region, n=3)

ggplot(df) +
  geom_map(aes(map_id = iso_code, fill = fct_rev(data_scale)), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(name = "Vaccination %", values = rev(brewer.pal(7, name = "Greens"))) +
  theme_void() +
  coord_fixed()

#################################################################################################
# Correlation analysis
# Prepare new data, customize the date range
ds1 <- ds %>%
  filter(!grepl('^OWID.*',iso_code)) %>% 
  filter(date >= '2020-03-01' & date <= '2022-03-31')

# Selected factors to analysis
ds1 <- ds1 %>% 
  select(location, total_cases_per_million, stringency_index, icu_patients_per_million, hosp_patients_per_million, weekly_icu_admissions_per_million,
         weekly_hosp_admissions_per_million, population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita, 
         extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities,
         hospital_beds_per_thousand, human_development_index) %>%
  group_by(location) %>%
  summarise_if(is.numeric, max, na.rm=T) # One location one record, use max as grouped data
# View(ds1)
ds1 <- do.call(data.frame, lapply(ds1, function(x) replace(x, is.infinite(x), NA))) # Replace infinite value to NA
# class(ds1)

# Graph the correlation on each factors compared to total_cases_per_million
par(mfrow=c(2,2)) # 2 by 2 in one plot
plot(ds1$stringency_index, ds1$total_cases_per_million)
plot(ds1$human_development_index, ds1$total_cases_per_million)
plot(ds1$gdp_per_capita, ds1$total_cases_per_million)
plot(ds1$extreme_poverty, ds1$total_cases_per_million)
#
par(mfrow=c(3,2))
plot(ds1$icu_patients_per_million, ds1$total_cases_per_million)
plot(ds1$hosp_patients_per_million, ds1$total_cases_per_million)
plot(ds1$weekly_icu_admissions_per_million, ds1$total_cases_per_million)
plot(ds1$weekly_hosp_admissions_per_million, ds1$total_cases_per_million)
plot(ds1$hospital_beds_per_thousand, ds1$total_cases_per_million)
#
par(mfrow=c(2,2))
plot(ds1$population_density, ds1$total_cases_per_million)
plot(ds1$median_age, ds1$total_cases_per_million)
plot(ds1$aged_65_older, ds1$total_cases_per_million)
plot(ds1$aged_70_older, ds1$total_cases_per_million)
#
par(mfrow=c(3,2))
plot(ds1$cardiovasc_death_rate, ds1$total_cases_per_million)
plot(ds1$diabetes_prevalence, ds1$total_cases_per_million)
plot(ds1$female_smokers, ds1$total_cases_per_million)
plot(ds1$male_smokers, ds1$total_cases_per_million)
plot(ds1$handwashing_facilities, ds1$total_cases_per_million)
# reset plot
par(mfrow=c(1,1))

# Correlation matrix
cor(ds1$stringency_index, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$human_development_index, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$gdp_per_capita, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$extreme_poverty, ds1$total_cases_per_million, use="na.or.complete")
#
cor(ds1$icu_patients_per_million, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$hosp_patients_per_million, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$weekly_icu_admissions_per_million, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$weekly_hosp_admissions_per_million, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$hospital_beds_per_thousand, ds1$total_cases_per_million, use="na.or.complete")
#
cor(ds1$population_density, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$median_age, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$aged_65_older, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$aged_70_older, ds1$total_cases_per_million, use="na.or.complete")
#
cor(ds1$cardiovasc_death_rate, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$diabetes_prevalence, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$female_smokers, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$male_smokers, ds1$total_cases_per_million, use="na.or.complete")
cor(ds1$handwashing_facilities, ds1$total_cases_per_million, use="na.or.complete")
#
#########################################################################
# Use Decision tree to check the factor for death rate / new cases
# Import required libraries
if("caret" %in% rownames(installed.packages()) == FALSE)
  install.packages("caret")
library(caret)
if("rpart" %in% rownames(installed.packages()) == FALSE)
  install.packages("rpart")
library(rpart)
# Library for plot beautiful decision tree
if("rattle" %in% rownames(installed.packages()) == FALSE)
  install.packages("rattle")
library(rattle)
# Prepare data (for death rate)
df <- ds %>%
  filter(!grepl('^OWID.*',iso_code))  %>% 
  filter(date >= '2020-03-01' & date <= '2022-03-31')
df <- df %>%
  select(new_deaths_smoothed_per_million, stringency_index, icu_patients_per_million, hosp_patients_per_million, weekly_icu_admissions_per_million,
         weekly_hosp_admissions_per_million, population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita, 
         extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities,
         hospital_beds_per_thousand, human_development_index)
# df <- do.call(data.frame, lapply(df, function(x) replace(x, is.infinite(x), NA)))
# View(df)
# na.omit(df)
# df[ , colSums(is.na(df)) == 0]
# Factor the death rate by mean + 1 std dev.
factor_mean <- mean(df$new_deaths_smoothed_per_million, na.rm=T) # + sd(df$new_deaths_smoothed_per_million, na.rm=T)

df$is_high <- ifelse(df$new_deaths_smoothed_per_million > factor_mean, 1, 0) # 1 = high death rate, 0 = low death rate
df <- df %>% drop_na(is_high)
df$is_high <- factor(df$is_high)
#
df <- df[-1] # Remove source factor column

set.seed(10)  # fix the randomizer
inTraining <- createDataPartition(df$is_high, p = 0.75, list = FALSE) 

# split data in 75%-25% ratio
training <- df[inTraining,] #70% data for model training  
testing <- df[-inTraining,] #30% for model testing

# nrow(df); nrow(training); nrow(testing)

# Build decision tree using training set
fit <- rpart(is_high ~ ., data=training, method="class", 
             parms=list(split="information"))
# summary(fit)

pred <- predict(fit, type="class", newdata=testing)   # Predict Class
ConfusionMatrix <- table(pred, testing$is_high)      # Confusion Matrix Table
ConfusionMatrix
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)       # calculate the accuracy

# Plot the decision tree, black line, yes/no decision
fancyRpartPlot(fit, yesno=2, split.col="black", nn.col="black", 
               caption="", palette="Set2", branch.col="black")

# ********************************************** #
# Prepare data (for new cases rate)
df <- ds %>%
  filter(!grepl('^OWID.*',iso_code))  %>% 
  filter(date >= '2020-03-01' & date <= '2022-03-31')
df <- df %>% 
  select(new_cases_smoothed_per_million, stringency_index, icu_patients_per_million, hosp_patients_per_million, weekly_icu_admissions_per_million,
         weekly_hosp_admissions_per_million, population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita, 
         extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities,
         hospital_beds_per_thousand, human_development_index)

factor_mean <- mean(df$new_cases_smoothed_per_million, na.rm=T) + sd(df$new_cases_smoothed_per_million, na.rm=T)
df$is_high <- ifelse(df$new_cases_smoothed_per_million > factor_mean, 1, 0)
df <- df %>% drop_na(is_high)
df$is_high <- factor(df$is_high)
#
df <- df[-1]

set.seed(10)  # fix the randomizer
inTraining <- createDataPartition(df$is_high, p = 0.75, list = FALSE)

# split data in 75%-25% ratio
training <- df[inTraining,] #75% data for model training  
testing <- df[-inTraining,] #25% for model testing

# nrow(df); nrow(training); nrow(testing)

# Build decision tree (new cases)

fit <- rpart(is_high ~ ., data=training, method="class", 
             parms=list(split="information"))
# summary(fit)

pred <- predict(fit, type="class", newdata=testing)   # Predict Class
ConfusionMatrix <- table(pred, testing$is_high)      # Confusion Matrix Table
ConfusionMatrix
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)       # calculate the accuracy

# Plot the decision tree
fancyRpartPlot(fit, yesno=2, split.col="black", nn.col="black", 
               caption="", palette="Set2", branch.col="black")


######################################################################################
# Predict HK cases using non-linear regression

# Prepare data
ds_hk <- ds %>%
  filter(iso_code %in% c("HKG"))

ds_hk$date <- as.Date(ds_hk$date, "%Y-%m-%d") # Convert date from string to Date() type

# Parameters test (debug only)
# period <- 90
# forecast <- 30
# end_date <- summarise(ds_hk, date = max(date))
# end_date <- end_date[1,1]
# start_date <- end_date - period
# class(start_date)

# ds2 <-na.omit(ds2)
# View(ds2)
# str(ds2)

# Prepare data for building regression model using no.2-4 COVID-19 waves in HK
# Wave 4th
wave4 <- ds_hk %>%
  select(date, new_cases_smoothed) %>%
  filter(date>="2020-11-15" & date<="2021-02-15")

# Wave 3rd
wave3 <- ds_hk %>%
  select(date, new_cases_smoothed) %>%
  filter(date>="2020-07-01" & date<="2020-09-10")

# Wave 2nd
wave2 <- ds_hk %>%
  select(date, new_cases_smoothed) %>%
  filter(date>="2020-03-01" & date<="2020-04-30")

g1 <- ggplot(data=wave4) +
  geom_line(aes(x=date, y=new_cases_smoothed)) +
  ggtitle("Wave 4th")
g2 <- ggplot(data=wave3) +
  geom_line(aes(x=date, y=new_cases_smoothed)) +
  ggtitle("Wave 3rd")
g3 <- ggplot(data=wave2) +
  geom_line(aes(x=date, y=new_cases_smoothed)) +
  ggtitle("Wave 2nd")

grid.arrange(g3,g2,g1, ncol=3, nrow=1) # Plot 3 waves graphs to find suitable model

# Prepare regression function
# Define a Gaussian function (of four parameters)
# Ref: https://en.wikipedia.org/wiki/Gaussian_function
f <- function(x, theta)  { 
  m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
  a*exp(-0.5*((x-m)/s)^2) + b
}

# Regression the HK 2nd wave 
wave2$num_days <- round(as.numeric(difftime(wave2$date, "2020-03-01" ,units="days"))) # Change date to number of days

x <- wave2$num_days
y <- wave2$new_cases_smoothed

# Estimate some starting values
m.0 <- x[which.max(y)]; s.0 <- (max(x)-min(x))/4; b.0 <- min(y); a.0 <- (max(y)-min(y))
#
# Do the fit
fit2 <- nls(y ~ f(x,c(m,s,a,b)), data.frame(x,y), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
#
# Display the estimated location of the peak and its SE
# summary(fit2)$parameters["m", 1:2]
summary(fit2)

# Graph the estimated curve and real data point
par(mfrow=c(1,1))
plot(c(x,0),c(y,f(coef(fit2)["m"],coef(fit2))), main="COVID19 HK 2nd wave regression", type="n",
     xlab="number of days", ylab="New cases")
curve(f(x, coef(fit2)), add=TRUE, col="Red", lwd=2)
points(x,y, pch=19)

# ********************************************** #
# Regression 3rd wave 
wave3$num_days <- round(as.numeric(difftime(wave3$date, "2020-07-01" ,units="days")))

x <- wave3$num_days
y <- wave3$new_cases_smoothed

# Estimate some starting values
m.0 <- x[which.max(y)]; s.0 <- (max(x)-min(x))/4; b.0 <- min(y); a.0 <- (max(y)-min(y))
#
# Do the fit
fit3 <- nls(y ~ f(x,c(m,s,a,b)), data.frame(x,y), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
#
# Display the estimated location of the peak and its SE
# summary(fit3)$parameters["m", 1:2]
summary(fit3)

# Graph the estimated curve and real data point
par(mfrow=c(1,1))
plot(c(x,0),c(y,f(coef(fit3)["m"],coef(fit3))), main="COVID19 HK 3rd wave regression", type="n",
     xlab="number of days", ylab="New cases")
curve(f(x, coef(fit3)), add=TRUE, col="Red", lwd=2)
points(x,y, pch=19)

# ********************************************** #
# Regression 4th wave 
wave4$num_days <- round(as.numeric(difftime(wave4$date, "2020-11-15" ,units="days")))

x <- wave4$num_days
y <- wave4$new_cases_smoothed

# Estimate some starting values
m.0 <- x[which.max(y)]; s.0 <- (max(x)-min(x))/4; b.0 <- min(y); a.0 <- (max(y)-min(y))
#
# Do the fit
fit4 <- nls(y ~ f(x,c(m,s,a,b)), data.frame(x,y), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
#
# Display the estimated location of the peak and its SE
# summary(fit4)$parameters["m", 1:2]
summary(fit4)
# plot(x, resid(fit4), main="Residuals")

# Graph the estimated curve and real data point
par(mfrow=c(1,1))
plot(c(x,0),c(y,f(coef(fit4)["m"],coef(fit4))), main="COVID19 HK 4th wave regression", type="n",
     xlab="number of days", ylab="New cases")
curve(f(x, coef(fit4)), add=TRUE, col="Red", lwd=2)
points(x,y, pch=19)

# ********************************************** #
# Prediction based on above regression fits
# HK Wave 5 data, choose 2022-1-31 as start date
# Training data end at 2022-3-31, will use latest data for prediction
wave5 <- ds_hk %>%
  select(date, new_cases_smoothed) %>%
  filter(date>="2022-01-31" & date<="2022-04-15")

wave5$num_days <- round(as.numeric(difftime(wave5$date, "2022-01-31" ,units="days")))

# predict use fit of wave 2nd
new_cases2 = predict(fit2, wave5, interval = "prediction")

# predict use fit of wave 3rd
new_cases3 = predict(fit3, wave5, interval = "prediction")

# predict use fit of wave 4th
new_cases4 = predict(fit4, wave5, interval = "prediction")

# Plot all the above prediction together and the real data of 5th wave 
plot(wave5$num_days[1:length(new_cases2)], new_cases2, type="l", col="green", xlab="", ylab="")
par(new=TRUE)
plot(wave5$num_days[1:length(new_cases3)], new_cases3, type="l", col="orange", xlab="", ylab="")
par(new=TRUE)
plot(wave5$num_days[1:length(new_cases4)], new_cases4, type="l", col="red", xlab="", ylab="")
par(new=TRUE)
plot(wave5$num_days, wave5$new_cases_smoothed, col="blue", xlab="Number of days", ylab="New cases", main="Regression fit curve to 5th wave from 2-4 waves")

