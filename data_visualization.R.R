#loading the required library
library(tidyverse)

#reading the cleaned dataset
df <- read.csv("C:\\Users\\HP\\Documents\\Question1\\cleaned_graduate_survey.csv", stringsAsFactors = FALSE)

#ensuring that the categorical columns are treated as factors
df <- df %>%
  mutate(across(where(is.character), as.factor))

#function to count occurrences of tools that are used (handles multiple selections)
count_tools <- function(column) {
  df %>%
    separate_rows({{ column }}, sep = ";") %>%
    group_by({{ column }}) %>%
    summarise(count = n(), .groups = "drop") %>%  # Fixed warning here
    arrange(desc(count))
}

#i)the most Popular Tools Used by Graduates

#programming languages
prog_lang_count <- count_tools(ProgLang)
ggplot(prog_lang_count, aes(x = reorder(ProgLang, count), y = count, fill = ProgLang)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top programming languages used by graduates are:",
       x = "Programming Language",
       y = "Count") +
  theme_minimal()

#databases
database_count <- count_tools(Databases)
ggplot(database_count, aes(x = reorder(Databases, count), y = count, fill = Databases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top databases used by graduates are:",
       x = "Database",
       y = "Count") +
  theme_minimal()

#web frameworks
webframework_count <- count_tools(WebFramework)
ggplot(webframework_count, aes(x = reorder(WebFramework, count), y = count, fill = WebFramework)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Web frameworks used by graduates are:",
       x = "Web Framework",
       y = "Count") +
  theme_minimal()

#cloud platforms
platform_count <- count_tools(Platform)
ggplot(platform_count, aes(x = reorder(Platform, count), y = count, fill = Platform)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top cloud platforms used by graduates are:",
       x = "Cloud Platform",
       y = "Count") +
  theme_minimal()

#AI search tools
aisearch_count <- count_tools(AISearch)
ggplot(aisearch_count, aes(x = reorder(AISearch, count), y = count, fill = AISearch)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top AI search tools used by graduates are:",
       x = "AI Search Tool",
       y = "Count") +
  theme_minimal()

#AI Developer tools
aitool_count <- count_tools(AITool)
ggplot(aitool_count, aes(x = reorder(AITool, count), y = count, fill = AITool)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top AI developer tools used by graduates are:",
       x = "AI Developer Tool",
       y = "Count") +
  theme_minimal()


#ii)the most Popular Industries for each study field
industry_count <- df %>%
  separate_rows(Industry, sep = ";") %>%
  group_by(StudyField, Industry) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(StudyField, desc(count))

ggplot(industry_count, aes(x = reorder(Industry, count), y = count, fill = StudyField)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~StudyField, scales = "free_y") +
  labs(title = "Top Industries by study field are:",
       x = "Industry",
       y = "Count") +
  theme_minimal()


#iii)the most common job roles categorized by study field
role_count <- df %>%
  group_by(StudyField, Role) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(StudyField, desc(count))

ggplot(role_count, aes(x = reorder(Role, count), y = count, fill = StudyField)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~StudyField, scales = "free_y") +
  labs(title = "Top job roles by study field are:",
       x = "Job Role",
       y = "Count") +
  theme_minimal()


#iv)the employment rate of graduates by study field
employment_rate <- df %>%
  group_by(StudyField, Employment) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(employment_rate, aes(x = StudyField, y = percentage, fill = Employment)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Employment Rate by study field is:",
       x = "Study Field",
       y = "Employment Rate (%)") +
  theme_minimal()
#Printing the plots
print(prog_lang_count)
print(database_count)
print(webframework_count)
print(platform_count)
print(aisearch_count)
print(aitool_count)
print(industry_count)
print(role_count)
print(employment_rate)
