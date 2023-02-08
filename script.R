

#title: Guided Projects R from Dataquest -  Investigating COVID-19 Virus Trends
#author: "Thais Lovisi"
#date: 09/12/2020
#output:  html_document


#Introduction
  #   A pandemia first internationally reported from Wuhan China on  December 31st,  
  # 2019 , without the exact origin determined, had put the world under constant 
  # signal of alert. This virus, known as COVID-19 which stands for COronaVIrus 
  # Disease, has put the world on trought a constant alert due its high level of 
  # transmissibility. 

  #   Since its first alert in 2019, the world has been engaged in the fight 
  # against this pandemic. Several measures had been taken across the world to
  # "flatten the infection curve". Between those measures, we can highlight the 
  # social distancing and the lockdown. However, despite the measures many 
  # people have passed away or had long-term aftereffects due to the infection. 

  #   In solidarity with this unprecedented global crisis, several organizations
  # did not hesitate to share several datasets allowing the conduction of 
  # several kinds of analysis in order to understand this pandemic.  

  #   Analyzing the dataset provided by
  # https://www.kaggle.com/datasets/lin0li/covid19testing we aim to  build our 
  # skills and understanding of the data analysis workflow by evaluating the 
  # COVID-19 situation and answer the following questions: 
  
       # 1. Which countries have had the highest number of positive cases 
       # against the number of tests?
  
#This dataset was collected between the 20th of January and the 1st of June 2020. 
###


#This project aims to investigate Which countries have had the highest number of positive cases against the number of tests.

#Step1. Understanding the Data

install.packages("readr")
library("readr")
library("tidyverse")
originalFile <- read.csv("covid19.csv")
dim(originalFile) # gives number of rows vs number of cols
vector_cols <- colnames(originalFile) # give us the var. names 
class(vector_cols) # variable type
head(originalFile)# Returns the first 6 obs of a vector, matrix, table, data frame or function. Used to see the data type
glimpse(originalFile) #  makes it possible to see every column in a data frame. Used to see whole data set
summary(originalFile) # descriptive statistics

#Step2. Isolating the Rows We Need

covid_df_all_states <- filter(originalFile, Province_State == "All States") #create a data set only with data Province_State == "All States"

covid_df_all_states_daily <- covid_df_all_states[c(1,4,11,12,13,14)] 

#Step3. Extracting top 10 test cases countries

#Arrange the tested column in descending order (returns tibble table, descending order)

covid_df_all_states_daily_sum <- arrange((covid_df_all_states_daily %>% 
  group_by(Country_Region) %>% #takes an existing tbl and converts it into a grouped tbl where operations are performed "by group".
  summarize(tested = sum(daily_tested),
           positive = sum(daily_positive),
           active = sum(active),
           hospitalized = sum(hospitalizedCurr))), desc(tested))

covid_top_10 <- head(covid_df_all_states_daily_sum, 10) #extracting top 10

#Step4. Identifying the Highest number of Positive against Tested Cases

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

#naming cols

names(tested_cases) <- countries
tested_cases
names(positive_cases) <- countries
positive_cases
names(active_cases) <- countries
active_cases
names(hospitalized_cases) <- countries
hospitalized_cases

top_3 <- positive_cases/tested_cases
top_3 <- sort(top_3, decreasing = TRUE)                           

positive_tested_top_3 <- head(top_3, 3)
positive_tested_top_3

#Step5. Keeping relevant information

united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

#creating a matrix
covid_mat <- rbind(united_kingdom, united_states,turkey)
mat_names <- c("Ratio","tested","positive","active","hospitalized")
colnames(covid_mat) <- mat_names

#Step6. putting all togheter

question <-"Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

dataframe_list <- list(originalFile, covid_df_all_states, covid_df_all_states_daily,covid_top_10)
matrix_list <- list(covid_mat)
list_vectors <- list(vector_cols, countries)
data_structure_list <- list(dataframe_list, matrix_list, list_vectors)

covid_analysis_list <- list(question, answer, data_structure_list)#Result is displayed on this object

