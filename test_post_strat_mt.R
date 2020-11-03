library(haven)
library(tidyverse)

#grab survey data
setwd("C:/Users/irfan/Documents/University/Year 3/Fall 2020/STA304/Problem Set/PS3")
raw_data <- read_dta("inputs/ns20200625/ns20200625.dta")
raw_data <- labelled::to_factor(raw_data)


#select the predictor variables that we are interested in
survey_reduced_data <- 
  raw_data %>% 
  select(vote_2020,
         race_ethnicity,
         age,
         gender,
         census_region,
         education,
         household_income,
         employment)


#quantify the response variable y: if the individual vote for Trump in 2020, y = 1; otherwise, y = 0
#survey_reduced_data<-
  #survey_reduced_data %>%
  #mutate(vote_trump = 
           #ifelse(vote_2020=="Donald Trump", 1, 0))

#quantify the response variable y: if the individual vote for Trump in 2020, y = 1; otherwise, y = 0
survey_reduced_data<-
  survey_reduced_data %>%
  mutate(vote_trump = case_when(
    vote_2020 == "Donald Trump" ~ 1,
    vote_2020 == "Joe Biden" ~ 0
  ))


# filters out NA cases caused by mutate in above chunk (basically not Trump or Biden)
survey_reduced_data <- survey_reduced_data[complete.cases(survey_reduced_data),]
#the above line is tsany's modification

#quantify race_ethnicity
survey_reduced_data<-
  survey_reduced_data %>%
  mutate(race_ethnicity_code = case_when(
    race_ethnicity=="White" ~ 1,
    race_ethnicity=="Black, or African American" ~ 2,
    race_ethnicity=="American Indian or Alaska Native" ~ 3,
    race_ethnicity=="Asian (Asian Indian)" ~ 4,
    race_ethnicity=="Asian (Chinese)" ~ 4,
    race_ethnicity=="Asian (Filipino)" ~ 4,
    race_ethnicity=="Asian (Japanese)" ~ 4,
    race_ethnicity=="Asian (Korean)" ~ 4,
    race_ethnicity=="Asian (Vietnamese)" ~ 4,
    race_ethnicity=="Asian (Other)" ~ 4,
    race_ethnicity=="Pacific Islander (Native Hawaiian)" ~ 4,
    race_ethnicity=="Pacific Islander (Guamanian)" ~ 4,
    race_ethnicity=="Pacific Islander (Samoan)" ~ 4,
    race_ethnicity=="Pacific Islander (Other)" ~ 4,
    race_ethnicity=="Some other race" ~ 5
  ))

#quantify education
survey_reduced_data<-
  survey_reduced_data %>%
  mutate(education_code = case_when(
    education=="3rd Grade or less" ~ 1,
    education=="Middle School - Grades 4 - 8" ~ 1,
    education=="Completed some high school" ~ 2,
    education=="High school graduate" ~ 2,
    education=="Other post high school vocational training" ~ 2,
    education=="Completed some college, but no degree" ~ 2,
    education=="Associate Degree" ~ 3,
    education=="College Degree (such as B.A., B.S.)" ~ 3,
    education=="Completed some graduate, but no degree" ~ 3,
    education=="Masters degree" ~ 4,
    education=="Doctorate degree" ~ 4
  ))

#quantify employment
survey_reduced_data<-
  survey_reduced_data %>%
  mutate(employment_code = case_when(
    employment=="Full-time employed" ~ 1,
    employment=="Homemaker" ~ 3,
    employment=="Retired" ~ 3,
    employment=="Unemployed or temporarily on layoff" ~ 2,
    employment=="Part-time employed" ~ 1,
    employment=="Permanently disabled" ~ 3,
    employment=="Student" ~ 3,
    employment=="Self-employed" ~ 1
  ))

test_model <- glm(vote_trump ~ as.factor(race_ethnicity_code) + as.factor(education_code) + as.factor(employment_code), data = survey_reduced_data, family = binomial())
summary(test_model)
write_csv(survey_reduced_data, "outputs/survey_data.csv")


# post strat
library(haven)
library(tidyverse)

#grab census data
raw_data <- read_dta("inputs/usa_00004.dta.gz")

raw_data <- labelled::to_factor(raw_data)

# select the same predictor variables that we are interested in
census_reduced_data <- 
  raw_data %>% 
  select(race, age, educ, empstat, region)

#quantify race in the same way
census_reduced_data <- 
  census_reduced_data %>% 
  mutate(race_ethnicity_code = case_when(
    race=="white" ~ 1,
    race=="black/african american/negro" ~ 2,
    race=="american indian or alaska native" ~ 3,
    race=="chinese" ~ 4,
    race=="japanese" ~ 4,
    race=="other asian or pacific islander" ~ 4,
    race=="other race, nec" ~ 5,
    race=="two major races" ~ 5,
    race=="three or more major races" ~ 5
  ))


#quantify education in the same way
census_reduced_data <- 
  census_reduced_data %>% 
  mutate(education_code = case_when(
    educ=="n/a or no schooling" ~ 1,
    educ=="nursery school to grade 4" ~ 1,
    educ=="grade 5, 6, 7, or 8" ~ 1,
    educ=="grade 9" ~ 2,
    educ=="grade 10" ~ 2,
    educ=="grade 11" ~ 2,
    educ=="grade 12" ~ 2,
    educ=="1 year of college" ~ 3,
    educ=="2 years of college" ~ 3,
    educ=="3 years of college" ~ 3,
    educ=="4 years of college" ~ 3,
    educ=="5+ years of college" ~ 4
  ))

#quantify employment in the same way
census_reduced_data <- 
  census_reduced_data %>% 
  mutate(employment_code = case_when(
    empstat=="n/a" ~ 3,
    empstat=="employed" ~ 1,
    empstat=="unemployed" ~ 2,
    empstat=="not in labor force" ~ 3
  ))

# divide the census data into cells; count the number of individuals in each cell
census_reduced_data <- 
  census_reduced_data %>%
  count(race_ethnicity_code, education_code, employment_code) %>%
  group_by(race_ethnicity_code, education_code, employment_code)

write_csv(census_reduced_data, "outputs/census_data_group.csv")

#Apply the model fitted above to our poststratification data: we get the log(P/(1-P)) of each cell
# P is the probability of voting for Trump in 2020 election
census_reduced_data$logodds_estimate <-
  test_model %>%
  predict(newdata = census_reduced_data)

# since we used logistic model, we have to convert log(P/(1-P)) to P
census_reduced_data$estimate <-
  exp(census_reduced_data$logodds_estimate)/(1+exp(census_reduced_data$logodds_estimate))

census_data_predict <- census_reduced_data %>%
  mutate(predict_prop = estimate * n) %>%
  summarise(our_predict = sum(predict_prop)/sum(n))

#sum of y_hat
y_hat_PS <- sum(census_data_final$n*census_data_final$Yhat_values)/sum(census_data_final$n)


# this graph is to assess our logit model, it should be an S-shaped curve
# resources
# https://stackoverflow.com/questions/36685921/plot-logistic-regression-curve-in-r
# wasnt able to get the code working


plot_mt <- ggplot(aes(y=race_ethnicity_code, x=logodds_estimate), data=census_reduced_data) +
  theme_minimal() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

plot_mt


#regression model table
broom::tidy(test_model) #why is it missing a bunch of stuff? o.o

#some tables with distribution

ethnic_predicts <- census_data_predict %>%
  group_by(race_ethnicity_code) %>%
  summarise(mean = mean(our_predict), 
            lower = quantile(our_predict, 0.025), 
            upper = quantile(our_predict, 0.975))

employment_predicts <- census_data_predict %>%
  group_by(employment_code) %>%
  summarise(mean = mean(our_predict), 
            lower = quantile(our_predict, 0.025), 
            upper = quantile(our_predict, 0.975))

education_predicts <- census_data_predict %>%
  group_by(education_code) %>%
  summarise(mean = mean(our_predict), 
            lower = quantile(our_predict, 0.025), 
            upper = quantile(our_predict, 0.975))

predicts_tables <- c(ethnic_predicts, employment_predicts, education_predicts)
knitr::kable(predicts_tables, digits = 5, caption = "Yhat Values per Dependent Variable")
# not sure what the above table looks like in rmd

#some visualization ideas

#histogram with ggplot
census_data_predict %>%
  ggplot(aes(x = our_predict)) +
  theme_minimal() +
  geom_histogram(bins = 30,
                 colour = "black",
                 fill = "transparent") +
  labs(title = "Histogram of Yhat Values",
       x = "Yhat Values",
       y = "Estimated Density")

#boxplot with ggplot
census_data_predict %>%
  ggplot(aes(y = our_predict)) + 
  theme_classic() +
  geom_boxplot(width = .1) +
  labs(title = "Boxplot of Yhat Values Grouped by Education",
       y = "Yhat Values") +
  coord_cartesian(xlim = c(-.3,.3)) +
  theme(axis.text.x = element_blank())

#simpler way to graph a boxplot, hist and scatterplot
par(mfrow=c(2,2))
boxplot(census_data_predict$our_predict, main="Boxplot of Yhat Values")
hist(census_data_predict$our_predict, main="Histogram of Yhat Values")
plot(census_data_predict$our_predict, main="Scatterplot of Yhat Values")


census_data_final <- cbind(census_reduced_data, Yhat_values = census_data_predict$our_predict)

#this is a table of all the final values together from census_reduced_data and the yhat values
knitr::kable(census_data_final, digits = 3, caption = "Census Data Weights",
             col.names = c("Race Ethnicity Code", "Education Code", "Employment Codes", "n", "Log Odds", "Estimates", "Yhat Values"))


