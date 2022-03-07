# Load Packages --------------------------------------------------------------
library(tidyverse)
library(priceR)
library(ggplot2)
library(ggthemes)
library(scales)
library(plotly)
library(DT)
library(htmltools)
library(htmlwidgets)
library(rsconnect)
library(shiny)
library(shinydashboard)

# Import and Clean Data ------------------------------------------------------
# Use readr package to read in imdb and metacritic csv
imdb_data <- read_csv("imdb.csv")
meta_data <- read_csv("metacritic.csv")

# Use dplyr to select relevant columns and rename them for the imdb data
imdb_data <-  imdb_data %>%
  select(name, year, score, budget) %>%
  rename(title = name, imdb_score = score)

# Use dplyr to select relevant columns and rename them for the meta data 
meta_data <-  meta_data %>%
  select(movie_title, metascore) %>%
  rename(title = movie_title, meta_score = metascore)

# Merge budget data with metacritic data by movie title
# Name the merged data frame master_data
master_data <- merge(imdb_data, meta_data, by = "title", all = T)

# Drop duplicate movie titles so we can adjust for inflation
master_data <- master_data[!duplicated(master_data$title), ]

# Use priceR package to adjust movie budgets for inflation up to 2020
# Note: We use 2020 since this was the last year for the IMDb dataset
master_data$budget_adj <- adjust_for_inflation(
  price = master_data$budget, 
  from_date = master_data$year, 
  country = "US", 
  to_date = 2020)

# Remove old budget variable and year now that we adjusted
master_data <- master_data %>%
  select(-budget, -year)

# Filter out NAs in title, imdb_score, meta_score, and budget_adj
master_data <- master_data %>%
  filter(!is.na(title)) %>%
  filter(!is.na(imdb_score)) %>%
  filter(!is.na(meta_score)) %>%
  filter(!is.na(budget_adj))

# Create UI -----------------------------------------------------------------
ui <- dashboardPage(
  # Create a page color
  skin = "yellow",
  
  # Add a title
  dashboardHeader(
    title = "Cinema!"
  ),
  
  # Create the sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "Main"),
      menuItem("IMDb Score", tabName = "imdb_score"),
      menuItem("Metacritic Score", tabName = "meta_score")
    )
  ),
  
  # Create the body
  dashboardBody(
    tabItems(
      tabItem("Main",
              h1("Background"),
              p("I have always wondered how a movie's budget relates to its 
                 reception, both by critics and the audience. Does an audience 
                 favorite tend to have a larger budget? Does a film trashed by 
                 critics usually have a smaller budget? To investigate this, 
                 I will use two data sets from Kaggle. To assess budget and 
                 audience score, I will use a data set that from IMDb that 
                 contains thousands of movies in the last 40 years (1980-2020). 
                 In conjunction with the IMDb data set, I will also use a data 
                 set from Metacritic, one of the most popular websites for 
                 seeing critics' numerical scores of movies."),
              p("I will combine the data frames and create interactive plots 
                 that assess the relationship between budget and IMDb score and
                 budget and Metacritic score. To supplement these 
                 visualizations, I will also provide two tables that allow
                 filtering, sorting, and searching of the data used for each 
                 plot."),
              p("IMDb data set: 
                 https://www.kaggle.com/danielgrijalvas/movies"),
              p("Metacritc data set:
                 https://www.kaggle.com/miazhx/metacritic-movie-reviews")
      ),
      
      tabItem("imdb_score",
              h2("IMDb Score"),
              p("This section shows IMDb Score and inflation-adjusted budget
                 for movies from 1980 to 2020."),
              box(plotlyOutput("p_imdb"), width = "500"),
              box(dataTableOutput("t_imdb"), width = "500")
      ),
      
      tabItem("meta_score",
              h2("Metacritic Score"),
              p("This section shows Metacritic Score and inflation-adjusted 
              budget for movies from 1980 to 2020."),
              box(plotlyOutput("p_meta"), width = "500"),
              box(dataTableOutput("t_meta"), width = "500")
      )
    )
  )
)

server <- function(input, output) {
  output$p_imdb <- renderPlotly({
    p_imdb <- ggplot(
      data = master_data, 
      mapping = aes(
        x = imdb_score, 
        y = budget_adj,
        label = title, 
        text = paste(
          "Title:", title,
          "\nIMDb Score:", imdb_score,
          "\nBudget:", round(budget_adj / 1000000, digits = 0), "M"))) +
      geom_point(
        alpha = .8, 
        shape = 21, 
        size = 2, 
        fill = "gold", 
        color = "black", 
        position = position_jitter(width = .05)) +
      theme_fivethirtyeight() +
      scale_x_continuous(
        breaks = seq(0, 10, by = 1), 
        limits = c(0, 10.5), 
        expand = c(0, 0)) +
      scale_y_continuous(
        breaks = seq(0, 400000000, by = 50000000),
        limits = c(0, 400000000),
        expand = c(0, 0),
        labels = unit_format(unit = "M", scale = 1e-6)) +
      xlab("IMDb Score") +
      ylab("Inflation-Adjusted Budget ($USD)") +
      ggtitle("Inflation-Adjusted Movie Budget by IMDb Score") +
      theme(axis.title.x = element_text(size = 15, vjust = 0)) +
      theme(axis.title.y = element_text(size = 15, vjust = 1)) +
      theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 1))
    
    ggplotly(p_imdb, tooltip = "text")
  })
  
  output$t_imdb <- renderDataTable(
    t_imdb <- master_data %>%
      select(-meta_score) %>%
      mutate(budget_adj = round(budget_adj / 1000000, 2)) %>%
      rename(Title = title) %>%
      rename(`IMDb Score` = imdb_score) %>%
      rename(`Inflation-Adjusted Budget (Millions $USD)` = budget_adj)
  )
  
  output$p_meta <- renderPlotly({
    p_meta <- ggplot(
      data = master_data, 
      mapping = aes(
        x = meta_score, 
        y = budget_adj,
        label = title, 
        text = paste(
          "Title:", title,
          "\nMetacritic Score:", meta_score,
          "\nBudget:", round(budget_adj / 1000000, digits = 0), "M"))) +
      geom_point(
        alpha = .8, 
        shape = 21, 
        size = 2, 
        fill = "light blue", 
        color = "black", 
        position = position_jitter(width = .05)) +
      theme_fivethirtyeight() +
      scale_x_continuous(
        breaks = seq(0, 100, by = 10), 
        limits = c(0, 105), 
        expand = c(0, 0)) +
      scale_y_continuous(
        breaks = seq(0, 400000000, by = 50000000),
        limits = c(0, 400000000),
        expand = c(0, 0),
        labels = unit_format(unit = "M", scale = 1e-6)) +
      xlab("Metacritic Score") +
      ylab("Inflation-Adjusted Budget ($USD)") +
      ggtitle("Inflation-Adjusted Movie Budget by Metacritic Score") +
      theme(axis.title.x = element_text(size = 15, vjust = 0)) +
      theme(axis.title.y = element_text(size = 15, vjust = 1)) +
      theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 1))
    
    ggplotly(p_meta, tooltip = "text")
  })
  
  output$t_meta <- renderDataTable(
    t_imdb <- master_data %>%
      select(-imdb_score) %>%
      mutate(budget_adj = round(budget_adj / 1000000, 2)) %>%
      rename(Title = title) %>%
      rename(`Metacritic Score` = meta_score) %>%
      rename(`Inflation-Adjusted Budget (Millions $USD)` = budget_adj)
  )
  
}

shinyApp(ui, server)