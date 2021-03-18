library(shiny)
library(shinythemes)
library(rsconnect)
library(packrat)
library(htmltools)
library(DT)
library(tidyverse)
library(scales)

# Data Import 
sd1_zcta <- read.csv("~/R/R Projects/supervisorial-district-census-data/Shiny_app/data/sd1_zcta.csv", 
                     stringsAsFactors = FALSE)

sd2_zcta <- read.csv("~/R/R Projects/supervisorial-district-census-data/Shiny_app/data/sd2_zcta.csv",
                     stringsAsFactors = FALSE)

sd3_zcta <- read.csv("~/R/R Projects/supervisorial-district-census-data/Shiny_app/data/sd3_zcta.csv",
                     stringsAsFactors = FALSE)

sd4_zcta <- read.csv("~/R/R Projects/supervisorial-district-census-data/Shiny_app/data/sd4_zcta.csv",
                     stringsAsFactors = FALSE)

sd5_zcta <- read.csv("~/R/R Projects/supervisorial-district-census-data/Shiny_app/data/sd5_zcta.csv",
                     stringsAsFactors = FALSE)

# SD zip codes 
sd1_zip <- sd1_zcta$Zip_Code
sd2_zip <- sd2_zcta$Zip_Code
sd3_zip <- sd3_zcta$Zip_Code
sd4_zip <- sd4_zcta$Zip_Code
sd5_zip <- sd5_zcta$Zip_Code


# Universal column names 
display_columns <- c("Zip_Code",
                     "Sq_Miles",
                     "Supervisor_District",
                     "SD",
                     "Sq_Miles_in_SD",
                     "Proportion_in_SD",
                     "Median_HH_Income",
                     "Per_Capita_Income",
                     "Population",
                     "Latinx",
                     "Black",
                     "Asian",
                     "American_Indian",
                     "Pacific_Islander",
                     "Other",
                     "White",
                     "Native_Born",
                     "Immigrant_Citizen",
                     "Immigrant_Undocumented",
                     "Housing_Units",
                     "Owner_Units",
                     "Renter_Units",
                     "Rent_Burden",
                     "Severe_Rent_Burden",
                     "Population_over_25",
                     "Less_than_HH",
                     "HS_Diploma",
                     "Bachelors_or_Higher",
                     "Poor_or_Struggling",
                     "Doing_Okay")

# Change column names 
colnames(sd1_zcta) <- display_columns
colnames(sd2_zcta) <- display_columns
colnames(sd3_zcta) <- display_columns
colnames(sd4_zcta) <- display_columns
colnames(sd5_zcta) <- display_columns

# Category selection
population <- c("Zip_Code", "Proportion_in_SD", "Population")
race <- c("Zip_Code", "Population", "Latinx", "Black", "Asian", "White")
immigration <- c("Zip_Code", "Population", "Native_Born", "Immigrant_Citizen", "Immigrant_Undocumented")
housing <- c("Zip_Code", "Housing_Units", "Owner_Units", "Renter_Units", "Rent_Burden", "Severe_Rent_Burden")
education <- c("Zip_Code", "Population_over_25", "Less_than_HH", "HS_Diploma", "Bachelors_or_Higher")
income <- c("Zip_Code", "Median_HH_Income", "Per_Capita_Income")
poverty <- c("Zip_Code", "Population", "Poor_or_Struggling", "Doing_Okay")

# Display column names for DT 
dt_pop <- c("Zip Code", "Proportion in District", "Population")
dt_race <- c("Zip Code", "Population", "Latinx", "Black", "Asian", "White")
dt_edu <- c("Zip Code", "Population over 25", "Less than HH", "HS Diploma", "Bachelors or higher")
dt_imm <- c('Zip Code', 'Population', 'Native Born', 'Immigrant Citizen', 'Immigrant Undocumented')
dt_housing <- c('Zip Code', 'Housing Units', 'Owners', 'Renters', 'Rent Burden', 'Severe Rent Burden')
dt_income <- c('Zip Code', 'Median HH Income', 'Per Capita Income')
dt_poverty <- c('Zip Code', 'Population', 'Poor or Struggling', 'Doing Okay')


# UI
ui <- fluidPage(
  
  # theme
  theme = shinytheme("simplex"),
  
  # Application title, subtitle, and byline
  titlePanel(shiny::tags$h1(HTML("<strong>Los Angeles County Supervisorial District Census Data<strong/>"))),
  titlePanel(h3("Proportionally Allocated by Zip Code Tabulated Areas (ZCTA)")),
  titlePanel(h5("Tabulated by Joel Montano")),
  
  
  # Sidebar layout 
  sidebarLayout(
    sidebarPanel(
      
      # Supervisorial district selection 
      selectInput("sd",
                  label = "Supervisor District",
                  choices = c("District 1", 
                              "District 2",
                              "District 3",
                              "District 4",
                              "District 5"),
                  width = "50%"),
      
      # Zip code(s) selection
      selectInput("zips",
                  label = "Zip Codes",
                  choices = c("All", 
                              "Custom"),
                  selected = "All",
                  width = "50%"),
      
      # Conditional panel if District 1 is selected
      conditionalPanel(
        condition = "input.zips == 'Custom' && input.sd == 'District 1'",
        selectInput("zip1",
                    label = "Zip Code Selection",
                    choices = sort(unique(sd1_zip)),
                    selected = 90001,
                    multiple = TRUE,
                    width = "50%")
      ),
      
      # Conditional panel if District 2 is selected 
      conditionalPanel(
        condition = "input.zips == 'Custom' && input.sd == 'District 2'",
        selectInput("zip2",
                    label = "Zip Code Selection",
                    choices = sort(unique(sd2_zip)),
                    selected = 90001,
                    multiple = TRUE,
                    width = "50%")
      ),
      
      # Conditional panel if District 3 is selected
      conditionalPanel(
        condition = "input.zips == 'Custom' && input.sd == 'District 3'",
        selectInput("zip3",
                    label = "Zip Code Selection",
                    choices = sort(unique(sd3_zip)),
                    selected = 90004,
                    multiple = TRUE,
                    width = "50%")
      ),
      
      # Conditional panel if District 4 is selected
      conditionalPanel(
        condition = "input.zips == 'Custom' && input.sd == 'District 4'",
        selectInput("zip4",
                    label = "Zip Code Selection",
                    choices = sort(unique(sd4_zip)),
                    selected = 90045,
                    multiple = TRUE,
                    width = "50%")
      ),
      
      # Conditional panel if District 5 is selected
      conditionalPanel(
        condition = "input.zips == 'Custom' && input.sd == 'District 5'",
        selectInput("zip5",
                    label = "Zip Code Selection",
                    choices = sort(unique(sd5_zip)),
                    selected = 91001,
                    multiple = TRUE,
                    width = "50%")
      ),
      
      # Radio button selection of categories
      radioButtons("category",
                   "Category",
                   choices = c("Population",
                               "Race",
                               "Education",
                               "Immigration",
                               "Housing",
                               "Income",
                               "Poverty"),
                   selected = "Population"),
      
      width = 3
      
    ),
    
    
    # Data Table
    mainPanel(
      dataTableOutput("table"),
      
      width = 9
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # Supervisor District selected    
  sd_selected <- reactive({
    
    switch(input$sd, 
           "District 1" = sd1_zcta,
           "District 2" = sd2_zcta,
           "District 3" = sd3_zcta,
           "District 4" = sd4_zcta,
           "District 5" = sd5_zcta)
  })
  # Zip Code selected
  zip_selected <- reactive({
    
    if(input$zips == "All") {
      zip_selected <- sd_selected()
    }
    else if(input$sd == 'District 1' & input$zip1 %in% sort(unique(sd1_zip))){
      zip_selected <- sd_selected() %>%
        filter(Zip_Code %in% input$zip1)
    } 
    else if(input$sd == 'District 2' & input$zip2 %in% sort(unique(sd2_zip))){
      zip_selected <- sd_selected() %>% 
        filter(Zip_Code %in% input$zip2)
    }
    else if(input$sd == 'District 3' & input$zip3 %in% sort(unique(sd3_zip))){
      zip_selected <- sd_selected() %>% 
        filter(Zip_Code %in% input$zip3)
    }
    else if(input$sd == 'District 4' & input$zip4 %in% sort(unique(sd4_zip))){
      zip_selected <- sd_selected() %>% 
        filter(Zip_Code %in% input$zip4)
    }
    else if(input$sd == 'District 5' & input$zip5 %in% sort(unique(sd5_zip))){
      zip_selected <- sd_selected() %>% 
        filter(Zip_Code %in% input$zip5)
    }
  })
  
  # Category selected & datatable creation
  category_selected <- reactive({
    
    if(input$category == "Population"){
      category_selected <- zip_selected() %>% 
        select(population) %>% 
        datatable(rownames = FALSE, 
                  colnames = dt_pop,
                  caption = "Data Source: 2019 American Community Survey (5-Year Estimates)",
                  extensions = "Buttons",
                  options = list(order = list(list(0, 'asc')),
                                 dom = "lfrtBip",
                                 buttons = list(list(extend = 'csv', 
                                                     filename = paste("Supervisor", 
                                                                      input$sd, 
                                                                      input$category,
                                                                      "by ZCTA"))),
                                 lengthMenu = list(20, 40, 60, 80),
                                 pageLength = 20)) %>% 
        formatPercentage('Proportion_in_SD') %>% 
        formatCurrency('Population',
                       currency = "",
                       digits = 0,
                       interval = 3,
                       mark = ",",
                       dec.mark = ".")
      
    }
    else if(input$category == "Race"){
      category_selected <- zip_selected() %>% 
        select(race) %>% 
        datatable(rownames = FALSE, 
                  colnames = dt_race,
                  caption = "Data Source: 2019 American Community Survey (5-Year Estimates)",
                  extensions = "Buttons",
                  options = list(order = list(list(0, 'asc')),
                                 dom = "lfrtBip",
                                 buttons = list(list(extend = 'csv', 
                                                     filename = paste("Supervisor", 
                                                                      input$sd, 
                                                                      input$category,
                                                                      "by ZCTA"))),
                                 lengthMenu = list(20, 40, 60, 80),
                                 pageLength = 20)) %>% 
        formatPercentage(c('Latinx', 'Black', 'Asian', 'White')) %>% 
        formatCurrency('Population',
                       currency = "",
                       digits = 0,
                       interval = 3,
                       mark = ",",
                       dec.mark = ".")
      
    } 
    else if(input$category == "Education"){
      category_selected <- zip_selected() %>% 
        select(education) %>% 
        datatable(rownames = FALSE, 
                  colnames = dt_edu,
                  caption = "Data Source: 2019 American Community Survey (5-Year Estimates)",
                  extensions = "Buttons",
                  options = list(order = list(list(0, 'asc')),
                                 dom = "lfrtBip",
                                 buttons = list(list(extend = 'csv', 
                                                     filename = paste("Supervisor", 
                                                                      input$sd, 
                                                                      input$category,
                                                                      "by ZCTA"))),
                                 lengthMenu = list(20, 40, 60, 80),
                                 pageLength = 20)) %>% 
        formatPercentage(c('Less_than_HH', 'HS_Diploma', 'Bachelors_or_Higher')) %>% 
        formatCurrency('Population_over_25',
                       currency = "",
                       digits = 0,
                       interval = 3,
                       mark = ",",
                       dec.mark = ".")
    }
    else if(input$category == "Immigration"){
      category_selected <- zip_selected() %>% 
        select(immigration) %>% 
        datatable(rownames = FALSE, 
                  colnames = dt_imm,
                  caption = "Data Source: 2019 American Community Survey (5-Year Estimates)",
                  extensions = "Buttons",
                  options = list(order = list(list(0, 'asc')),
                                 dom = "lfrtBip",
                                 buttons = list(list(extend = 'csv', 
                                                     filename = paste("Supervisor", 
                                                                      input$sd, 
                                                                      input$category,
                                                                      "by ZCTA"))),
                                 lengthMenu = list(20, 40, 60, 80),
                                 pageLength = 20)) %>%
        formatPercentage(c('Native_Born', 'Immigrant_Citizen', 'Immigrant_Undocumented')) %>% 
        formatCurrency('Population',
                       currency = "",
                       digits = 0,
                       interval = 3,
                       mark = ",",
                       dec.mark = ".")
    }
    else if(input$category == "Housing"){
      category_selected <- zip_selected() %>% 
        select(housing) %>% 
        datatable(rownames = FALSE, 
                  colnames = dt_housing,
                  caption = "Data Source: 2019 American Community Survey (5-Year Estimates)",
                  extensions = "Buttons",
                  options = list(order = list(list(0, 'asc')),
                                 dom = "lfrtBip",
                                 buttons = list(list(extend = 'csv', 
                                                     filename = paste("Supervisor", 
                                                                      input$sd, 
                                                                      input$category,
                                                                      "by ZCTA"))),
                                 lengthMenu = list(20, 40, 60, 80),
                                 pageLength = 20)) %>%
        formatPercentage(c('Owner_Units', 'Renter_Units', 'Rent_Burden', 'Severe_Rent_Burden')) %>% 
        formatCurrency('Housing_Units',
                       currency = "",
                       digits = 0,
                       interval = 3,
                       mark = ",",
                       dec.mark = ".")
    }
    else if(input$category == "Income"){
      category_selected <- zip_selected() %>% 
        select(income) %>% 
        datatable(rownames = FALSE, 
                  colnames = dt_income,
                  caption = "Data Source: 2019 American Community Survey (5-Year Estimates)",
                  extensions = "Buttons",
                  options = list(order = list(list(0, 'asc')),
                                 dom = "lfrtBip",
                                 buttons = list(list(extend = 'csv', 
                                                     filename = paste("Supervisor", 
                                                                      input$sd, 
                                                                      input$category,
                                                                      "by ZCTA"))),
                                 lengthMenu = list(20, 40, 60, 80),
                                 pageLength = 20)) %>% 
        formatCurrency(c('Median_HH_Income', 'Per_Capita_Income'),
                       digits = 0)
    }
    else if(input$category == "Poverty"){
      category_selected <- zip_selected() %>% 
        select(poverty) %>% 
        datatable(rownames = FALSE, 
                  colnames = dt_poverty,
                  caption = "Data Source: 2019 American Community Survey (5-Year Estimates)",
                  extensions = "Buttons",
                  options = list(order = list(list(0, 'asc')),
                                 dom = "lfrtBip",
                                 buttons = list(list(extend = 'csv', 
                                                     filename = paste("Supervisor", 
                                                                      input$sd, 
                                                                      input$category,
                                                                      "by ZCTA"))),
                                 lengthMenu = list(20, 40, 60, 80),
                                 pageLength = 20)) %>% 
        formatPercentage(c('Poor_or_Struggling', 'Doing_Okay')) %>% 
        formatCurrency('Population',
                       currency = "",
                       digits = 0,
                       interval = 3,
                       mark = ",",
                       dec.mark = ".")
      
    }
    
  })
  
  # Render Data Table
  output$table <- renderDataTable({
    
    category_selected()
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
