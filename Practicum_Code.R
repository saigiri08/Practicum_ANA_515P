install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)

#Reading sheet 1
mental_health_survey_sheet1 <- read_excel("C:/Users/91738/Downloads/Mental_Health_Survey.xlsx" , sheet = 1)

#Reading sheet 1
mental_health_survey_sheet2 <- read_excel("C:/Users/91738/Downloads/Mental_Health_Survey.xlsx" , sheet = 2)

#changing the data type of sheet 2 to bind it with sheet 1
mental_health_survey_sheet2$Timestamp <- as.double(mental_health_survey_sheet2$Timestamp)

#combining both sheet 1 and sheet2
mental_health_survey_combine <- bind_rows(mental_health_survey_sheet1,mental_health_survey_sheet2)

#Filtering age more than 0 as there will be no negative value for age
mental_health_survey <- mental_health_survey_combine %>% filter(Age > 0)

# Display Results
head(mental_health_survey)
summary(mental_health_survey)
str(mental_health_survey)

#Renaming the columns to make them more understandable
mental_health_survey_col_rename <- mental_health_survey %>% rename(Co_Workers = coworkers)


#Removing duplicates to avoid repeatation of data
mental_health_survey_remove <- unique(mental_health_survey_col_rename)

#Replacing missing value with mean of that particular column to get better results
mental_health_survey_replace <- mental_health_survey_col_rename %>% mutate_if(is.numeric , ~ifelse(is.na(.), mean(., na.rm = TRUE), .))


# to handle the values like NA 
mental_health_survey_sorted <- mental_health_survey_replace %>% na.omit(mental_health_survey_replace)


#Handeling outliers to get accurate results
mental_health_survey_age_outlier <- mental_health_survey_sorted[mental_health_survey_sorted$Age < 60, ]

#Box plot 
Box_plot <- ggplot(mental_health_survey_age_outlier, aes(x = 1, y = Age)) +
  geom_boxplot(color = "skyblue") +
  ggtitle("Outliers in Numeric Column")

#Histogram
histogram <- ggplot(mental_health_survey_age_outlier, aes(x = Age)) +
  geom_histogram(bins = 25, fill = "orange", color = "blue") +
  labs(title = "Mental Health according to Age",
       x = "Age",
       y = "count")


#writing the cleaned and processed Data to new file
write.csv(mental_health_survey_age_outlier, "cleaned_Mental_Health_Survey_dataset.csv", row.names = FALSE)


