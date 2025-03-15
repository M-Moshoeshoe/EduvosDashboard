#loading the required library
library(tidyverse)

#reading the dataset
df <- read.csv("C:\\Users\\HP\\Downloads\\graduate_survey.csv", stringsAsFactors = FALSE)

#Selecting only the relevant columns
df <- df %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases,
         Platform, WebFramework, Industry, AISearch, AITool, Employment)

#displaying the first few rows to confirm selection
head(df)

#droping rows where essential columns are missing
df <- df %>%
  filter(!is.na(Campus) & !is.na(StudyField) & !is.na(Branch) & !is.na(Role) & !is.na(EduLevel))

#replace missing values in non essential columns with "Unknown"
df[is.na(df)] <- "Unknown"

#Checking the missing values after cleaning
colSums(is.na(df))
# Standardizing campus names (merging Durban and Umhlanga)
df$Campus <- ifelse(df$Campus %in% c("Umhlanga"), "Durban", df$Campus)

#converting all categorical columns to lowercase create consistency
df <- df %>%
  mutate(across(where(is.character), tolower))

#displaying unique campus values to verify
unique(df$Campus)
#counting the number of responses per campus
top_campuses <- df %>%
  group_by(Campus) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) #selecting the top 5 campuses

#filtering the dataset to only include responses from these top campuses
df <- df %>% filter(Campus %in% top_campuses$Campus)

#display the top campuses
print(top_campuses)
#save the cleaned dataset
write.csv(df, "cleaned_graduate_survey.csv", row.names = FALSE)

#confirm that the file was saved successfully
print("Cleaned dataset saved as 'cleaned_graduate_survey.csv'")

