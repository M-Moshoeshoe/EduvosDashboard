#loading the necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

#loading the dataset
file_path <- "C:\\Users\\HP\\Documents\\Question1\\cleaned_graduate_survey.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

#define function in order to process categorical columns
split_and_count <- function(column){
  df %>%
    select(all_of(column)) %>%
    separate_rows(all_of(column), sep = ";") %>%
    count(all_of(column), sort = TRUE, name = "count") %>%
    na.omit()
}
#UI Layout
ui <- dashboardPage(
  dashboardHeader(title = "Eduvos IT Graduate Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Programming Languages", tabName = "prog_lang_count", icon = icon("code")),
      menuItem("Databases", tabName = "database_count", icon = icon("database")),
      menuItem("Web Frameworks", tabName = "webframework_count", icon = icon("globe")),
      menuItem("Platforms", tabName = "platform_count", icon = icon("cloud")),
      menuItem("Employment Analysis", tabName = "employment_rate", icon = icon("briefcase"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "prog_lang_count",
              fluidRow(
                box(title = "The most used programming languages", status = "primary", solidHeader = TRUE,
                    plotOutput("prog_lang_count"))
              )
      ),
      tabItem(tabName = "database_count",
              fluidRow(
                box(title = "The most used databases", status = "primary", solidHeader = TRUE,
                    plotOutput("database_count"))
              )
      ),
      tabItem(tabName = "webframework_count",
              fluidRow(
                box(title = "The most used web frameworks", status = "primary", solidHeader = TRUE,
                    plotOutput("webframework_count"))
              )
      ),
      tabItem(tabName = "platform_count",
              fluidRow(
                box(title = "The most used platforms", status = "primary", solidHeader = TRUE,
                    plotOutput("platform_count"))
              )
      ),
      tabItem(tabName = "employment_rate",
              fluidRow(
                box(title = "The employment rate by study field", status = "primary", solidHeader = TRUE,
                    plotOutput("employment_rate"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
#server function
output$prog_lang_count <- renderPlot({
  data <- split_and_count("ProgLang")
  ggplot(data, aes(x = reorder(!!sym(names(data)[1]), -count), y = count)) +
    geom_bar(stat = "identity", fill = "green") +
    coord_flip() +
    labs(title = "Top programming languages", x = "Programming Language", y = "Count")
})

output$database_count <- renderPlot({
  data <- split_and_count("Databases")
  ggplot(data, aes(x = reorder(!!sym(names(data)[1]), -count), y = count)) +
    geom_bar(stat = "identity", fill = "grey") +
    coord_flip() +
    labs(title = "Top databases", x = "Database", y = "Count")
})

output$webframework_count <- renderPlot({
  data <- split_and_count("WebFramework")
  ggplot(data, aes(x = reorder(!!sym(names(data)[1]), -count), y = count)) +
    geom_bar(stat = "identity", fill = "purple") +
    coord_flip() +
    labs(title = "Top web frameworks", x = "Web Framework", y = "Count")
})

output$platform_count <- renderPlot({
  data <- split_and_count("Platform")
  ggplot(data, aes(x = reorder(!!sym(names(data)[1]), -count), y = count)) +
    geom_bar(stat = "identity", fill = "red") +
    coord_flip() +
    labs(title = "Top cloud platforms", x = "Platform", y = "Count")
})

output$employment_rate <- renderPlot({
  employment_rate <- df %>%
    group_by(StudyField) %>%
    summarise(Employed = sum(Employment != "Unemployed"), Total= n(), EmploymentRate = (Employed/Total)*100)

  ggplot(employment_rate, aes(x = StudyField, y = EmploymentRate, fill = StudyField)) +
    geom_bar(stat = "identity") +
    labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)") +
    theme_minimal()
})
}
#Running the ShinnyApp
shinyApp(ui = ui, server = server)
