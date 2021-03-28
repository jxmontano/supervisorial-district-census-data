library(shiny)
library(shinythemes)
library(rsconnect)
library(packrat)
library(htmltools)
library(DT)
library(sf)
library(leaflet)
library(leafpop)
library(tidyverse)
library(scales)

# Data Import 
sd1_zcta <- read.csv("data/sd1_zcta.csv", 
                     stringsAsFactors = FALSE)

sd2_zcta <- read.csv("data/sd2_zcta.csv",
                     stringsAsFactors = FALSE)

sd3_zcta <- read.csv("data/sd3_zcta.csv",
                     stringsAsFactors = FALSE)

sd4_zcta <- read.csv("data/sd4_zcta.csv",
                     stringsAsFactors = FALSE)

sd5_zcta <- read.csv("data/sd5_zcta.csv",
                     stringsAsFactors = FALSE)

# SD zip codes 
sd1_zip <- sd1_zcta$Zip_Code
sd2_zip <- sd2_zcta$Zip_Code
sd3_zip <- sd3_zcta$Zip_Code
sd4_zip <- sd4_zcta$Zip_Code
sd5_zip <- sd5_zcta$Zip_Code


# Universal column names for datasets 
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

# Import geojson files 
sd1_zcta_geo <- st_read("data/sd1_zcta.geojson")
sd2_zcta_geo <- st_read("data/sd2_zcta.geojson")
sd3_zcta_geo <- st_read("data/sd3_zcta.geojson")
sd4_zcta_geo <- st_read("data/sd4_zcta.geojson")
sd5_zcta_geo <- st_read("data/sd5_zcta.geojson")

# Universal column names for geojson maps 
display_geo_columns <- c("Zip_Code",
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
                         "Doing_Okay",
                         "geometry")

colnames(sd1_zcta_geo) <- display_geo_columns
colnames(sd2_zcta_geo) <- display_geo_columns
colnames(sd3_zcta_geo) <- display_geo_columns
colnames(sd4_zcta_geo) <- display_geo_columns
colnames(sd5_zcta_geo) <- display_geo_columns

# Capture unique zip codes
sd1_zips_geo <- sort(unique(sd1_zcta_geo$Zip_Code))
sd2_zips_geo <- sort(unique(sd2_zcta_geo$Zip_Code))
sd3_zips_geo <- sort(unique(sd3_zcta_geo$Zip_Code))
sd4_zips_geo <- sort(unique(sd4_zcta_geo$Zip_Code))
sd5_zips_geo <- sort(unique(sd5_zcta_geo$Zip_Code))


# Palettes 
pal_pop <- colorBin(palette = "Blues",
                    domain = sd2_zcta_geo$Population,
                    bins = 4,
                    alpha = TRUE)


pal_race <- colorBin(palette = "YlGnBu",
                     domain = sd1_zcta_geo$Latinx,
                     bins = 4,
                     alpha = TRUE)

pal_edu <- colorBin(palette = "YlOrBr",
                    domain = sd3_zcta_geo$Bachelors_or_Higher,
                    bins = 4,
                    alpha = TRUE)

pal_imm <- colorBin(palette = "RdPu",
                    domain = sd1_zcta_geo$Immigrant_Undocumented,
                    bins = 4,
                    alpha = TRUE)

pal_housing <- colorBin(palette = "BuPu",
                        domain = sd5_zcta_geo$Rent_Burden,
                        bins = 4,
                        alpha = TRUE)

pal_income <- colorBin(palette = "GnBu",
                       domain = sd3_zcta_geo$Median_HH_Income,
                       bins = 4,
                       alpha = TRUE)

pal_poverty <- colorBin(palette = "YlOrRd",
                        domain = sd3_zcta_geo$Poor_or_Struggling,
                        bins = 4,
                        alpha = TRUE)

# Convert to numeric
sd1_zcta_geo$Latinx <- as.numeric(sd1_zcta_geo$Latinx)
sd1_zcta_geo$Black <- as.numeric(sd1_zcta_geo$Black)
sd1_zcta_geo$Asian <- as.numeric(sd1_zcta_geo$Asian)
sd1_zcta_geo$White <- as.numeric(sd1_zcta_geo$White)
sd1_zcta_geo$Less_than_HH <- as.numeric(sd1_zcta_geo$Less_than_HH)
sd1_zcta_geo$HS_Diploma <- as.numeric(sd1_zcta_geo$HS_Diploma)
sd1_zcta_geo$Bachelors_or_Higher <- as.numeric(sd1_zcta_geo$Bachelors_or_Higher)
sd1_zcta_geo$Native_Born <- as.numeric(sd1_zcta_geo$Native_Born)
sd1_zcta_geo$Immigrant_Citizen <- as.numeric(sd1_zcta_geo$Immigrant_Citizen)
sd1_zcta_geo$Immigrant_Undocumented <- as.numeric(sd1_zcta_geo$Immigrant_Undocumented)
sd1_zcta_geo$Renter_Units <- as.numeric(sd1_zcta_geo$Renter_Units)
sd1_zcta_geo$Rent_Burden <- as.numeric(sd1_zcta_geo$Rent_Burden)
sd1_zcta_geo$Severe_Rent_Burden <- as.numeric(sd1_zcta_geo$Severe_Rent_Burden)
sd1_zcta_geo$Poor_or_Struggling <- as.numeric(sd1_zcta_geo$Poor_or_Struggling)

sd2_zcta_geo$Latinx <- as.numeric(sd2_zcta_geo$Latinx)
sd2_zcta_geo$Black <- as.numeric(sd2_zcta_geo$Black)
sd2_zcta_geo$Asian <- as.numeric(sd2_zcta_geo$Asian)
sd2_zcta_geo$White <- as.numeric(sd2_zcta_geo$White)
sd2_zcta_geo$Less_than_HH <- as.numeric(sd2_zcta_geo$Less_than_HH)
sd2_zcta_geo$HS_Diploma <- as.numeric(sd2_zcta_geo$HS_Diploma)
sd2_zcta_geo$Bachelors_or_Higher <- as.numeric(sd2_zcta_geo$Bachelors_or_Higher)
sd2_zcta_geo$Native_Born <- as.numeric(sd2_zcta_geo$Native_Born)
sd2_zcta_geo$Immigrant_Citizen <- as.numeric(sd2_zcta_geo$Immigrant_Citizen)
sd2_zcta_geo$Immigrant_Undocumented <- as.numeric(sd2_zcta_geo$Immigrant_Undocumented)
sd2_zcta_geo$Renter_Units <- as.numeric(sd2_zcta_geo$Renter_Units)
sd2_zcta_geo$Rent_Burden <- as.numeric(sd2_zcta_geo$Rent_Burden)
sd2_zcta_geo$Severe_Rent_Burden <- as.numeric(sd2_zcta_geo$Severe_Rent_Burden)
sd2_zcta_geo$Poor_or_Struggling <- as.numeric(sd2_zcta_geo$Poor_or_Struggling)

sd3_zcta_geo$Latinx <- as.numeric(sd3_zcta_geo$Latinx)
sd3_zcta_geo$Black <- as.numeric(sd3_zcta_geo$Black)
sd3_zcta_geo$Asian <- as.numeric(sd3_zcta_geo$Asian)
sd3_zcta_geo$White <- as.numeric(sd3_zcta_geo$White)
sd3_zcta_geo$Less_than_HH <- as.numeric(sd3_zcta_geo$Less_than_HH)
sd3_zcta_geo$HS_Diploma <- as.numeric(sd3_zcta_geo$HS_Diploma)
sd3_zcta_geo$Bachelors_or_Higher <- as.numeric(sd3_zcta_geo$Bachelors_or_Higher)
sd3_zcta_geo$Native_Born <- as.numeric(sd3_zcta_geo$Native_Born)
sd3_zcta_geo$Immigrant_Citizen <- as.numeric(sd3_zcta_geo$Immigrant_Citizen)
sd3_zcta_geo$Immigrant_Undocumented <- as.numeric(sd3_zcta_geo$Immigrant_Undocumented)
sd3_zcta_geo$Renter_Units <- as.numeric(sd3_zcta_geo$Renter_Units)
sd3_zcta_geo$Rent_Burden <- as.numeric(sd3_zcta_geo$Rent_Burden)
sd3_zcta_geo$Severe_Rent_Burden <- as.numeric(sd3_zcta_geo$Severe_Rent_Burden)
sd3_zcta_geo$Poor_or_Struggling <- as.numeric(sd3_zcta_geo$Poor_or_Struggling)

sd4_zcta_geo$Latinx <- as.numeric(sd4_zcta_geo$Latinx)
sd4_zcta_geo$Black <- as.numeric(sd4_zcta_geo$Black)
sd4_zcta_geo$Asian <- as.numeric(sd4_zcta_geo$Asian)
sd4_zcta_geo$White <- as.numeric(sd4_zcta_geo$White)
sd4_zcta_geo$Less_than_HH <- as.numeric(sd4_zcta_geo$Less_than_HH)
sd4_zcta_geo$HS_Diploma <- as.numeric(sd4_zcta_geo$HS_Diploma)
sd4_zcta_geo$Bachelors_or_Higher <- as.numeric(sd4_zcta_geo$Bachelors_or_Higher)
sd4_zcta_geo$Native_Born <- as.numeric(sd4_zcta_geo$Native_Born)
sd4_zcta_geo$Immigrant_Citizen <- as.numeric(sd4_zcta_geo$Immigrant_Citizen)
sd4_zcta_geo$Immigrant_Undocumented <- as.numeric(sd4_zcta_geo$Immigrant_Undocumented)
sd4_zcta_geo$Renter_Units <- as.numeric(sd4_zcta_geo$Renter_Units)
sd4_zcta_geo$Rent_Burden <- as.numeric(sd4_zcta_geo$Rent_Burden)
sd4_zcta_geo$Severe_Rent_Burden <- as.numeric(sd4_zcta_geo$Severe_Rent_Burden)
sd4_zcta_geo$Poor_or_Struggling <- as.numeric(sd4_zcta_geo$Poor_or_Struggling)

sd5_zcta_geo$Latinx <- as.numeric(sd5_zcta_geo$Latinx)
sd5_zcta_geo$Black <- as.numeric(sd5_zcta_geo$Black)
sd5_zcta_geo$Asian <- as.numeric(sd5_zcta_geo$Asian)
sd5_zcta_geo$White <- as.numeric(sd5_zcta_geo$White)
sd5_zcta_geo$Less_than_HH <- as.numeric(sd5_zcta_geo$Less_than_HH)
sd5_zcta_geo$HS_Diploma <- as.numeric(sd5_zcta_geo$HS_Diploma)
sd5_zcta_geo$Bachelors_or_Higher <- as.numeric(sd5_zcta_geo$Bachelors_or_Higher)
sd5_zcta_geo$Native_Born <- as.numeric(sd5_zcta_geo$Native_Born)
sd5_zcta_geo$Immigrant_Citizen <- as.numeric(sd5_zcta_geo$Immigrant_Citizen)
sd5_zcta_geo$Immigrant_Undocumented <- as.numeric(sd5_zcta_geo$Immigrant_Undocumented)
sd5_zcta_geo$Renter_Units <- as.numeric(sd5_zcta_geo$Renter_Units)
sd5_zcta_geo$Rent_Burden <- as.numeric(sd5_zcta_geo$Rent_Burden)
sd5_zcta_geo$Severe_Rent_Burden <- as.numeric(sd5_zcta_geo$Severe_Rent_Burden)
sd5_zcta_geo$Poor_or_Struggling <- as.numeric(sd5_zcta_geo$Poor_or_Struggling)


# UI
ui <- navbarPage("Tabulated and Developed by Joel Montano (@jxmontano)",
                 
                 tabPanel("",
                          
                          fluidPage(
                            
                            # theme
                            theme = shinytheme("yeti"),
                            
                            # Application title & subtitle
                            titlePanel(h1(HTML("<strong>Los Angeles County Supervisorial District Census Data<strong/>")), windowTitle = "Supervisorial District Census Data by ZCTA"),
                            titlePanel(h3("Proportionally Allocated by Zip Code Tabulated Areas (ZCTA)")),
                            #titlePanel(h5("Tabulated by Joel Montano")),
                            
                            
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
                                            selected = "District 1",
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
                                
                                # Sidebar panel ends
                              ),
                              
                              
                              # Main Panel
                              mainPanel(
                                
                                dataTableOutput("table"),
                                
                                
                                width = 9
                                # Main panel ends
                              )
                              # Sidebar layout ends                
                            )
                            # Fluid page ends
                          )
                          # Tab Panel ends
                 ),
                 tabPanel("",
                          
                          fluidPage(
                            
                            
                            # Title
                            titlePanel("Spatial Visualization"),
                            
                            # Sidebar layout 
                            sidebarLayout(
                              sidebarPanel(
                                # Supervisorial district selection 
                                selectInput("sd_geo",
                                            label = "Supervisor District",
                                            choices = c("District 1", 
                                                        "District 2",
                                                        "District 3",
                                                        "District 4",
                                                        "District 5"),
                                            width = "50%"),
                                # Zip code(s) selection
                                selectInput("zips_geo",
                                            label = "Zip Codes",
                                            choices = c("All", 
                                                        "Custom"),
                                            selected = "All",
                                            width = "50%"),
                                # Conditional panel if District 1 is selected
                                conditionalPanel(
                                  condition = "input.zips_geo == 'Custom' && input.sd_geo == 'District 1'",
                                  selectInput("zip1_geo",
                                              label = "Zip Code Selection",
                                              choices = sort(unique(sd1_zips_geo)),
                                              selected = 90001,
                                              multiple = TRUE,
                                              width = "50%")
                                ),
                                # Conditional panel if District 2 is selected
                                conditionalPanel(
                                  condition = "input.zips_geo == 'Custom' && input.sd_geo == 'District 2'",
                                  selectInput("zip2_geo",
                                              label = "Zip Code Selection",
                                              choices = sort(unique(sd2_zips_geo)),
                                              selected = 90001,
                                              multiple = TRUE,
                                              width = "50%")
                                ),
                                # Conditional panel if District 3 is selected
                                conditionalPanel(
                                  condition = "input.zips_geo == 'Custom' && input.sd_geo == 'District 3'",
                                  selectInput("zip3_geo",
                                              label = "Zip Code Selection",
                                              choices = sort(unique(sd3_zips_geo)),
                                              selected = 90004,
                                              multiple = TRUE,
                                              width = "50%")
                                ),
                                # Conditional panel if District 4 is selected
                                conditionalPanel(
                                  condition = "input.zips_geo == 'Custom' && input.sd_geo == 'District 4'",
                                  selectInput("zip4_geo",
                                              label = "Zip Code Selection",
                                              choices = sort(unique(sd4_zips_geo)),
                                              selected = 90045,
                                              multiple = TRUE,
                                              width = "50%")
                                ),
                                # Conditional panel if District 5 is selected
                                conditionalPanel(
                                  condition = "input.zips_geo == 'Custom' && input.sd_geo == 'District 5'",
                                  selectInput("zip5_geo",
                                              label = "Zip Code Selection",
                                              choices = sort(unique(sd5_zips_geo)),
                                              selected = 91001,
                                              multiple = TRUE,
                                              width = "50%")
                                ),
                                
                                # Radio button selection of categories
                                radioButtons("category_geo",
                                             label = "Category",
                                             choices = c("Population",
                                                         "Race",
                                                         "Education",
                                                         "Immigration",
                                                         "Housing",
                                                         "Income",
                                                         "Poverty"),
                                             selected = "Population"),
                                
                                # Race selection 
                                conditionalPanel(
                                  condition = "input.category_geo == 'Race'",
                                  selectInput("race_geo",
                                              label = "Race",
                                              choices = c("Latinx",
                                                          "Black",
                                                          "Asian",
                                                          "White"),
                                              selected = "Latinx",
                                              width = "50%")
                                ),
                                # Education selection
                                conditionalPanel(
                                  condition = "input.category_geo == 'Education'",
                                  selectInput("edu_geo",
                                              label = "Education Level",
                                              choices = c("Less than High School",
                                                          "High School Diploma",
                                                          "Bachelors or higher"),
                                              selected = "High School Diploma",
                                              width = "50%")
                                ),
                                # Immigration selection
                                conditionalPanel(
                                  condition = "input.category_geo == 'Immigration'",
                                  selectInput("imm_geo",
                                              label = "Immigrant Population",
                                              choices = c("Documented",
                                                          "Undocumented"),
                                              selected = "Documented",
                                              width = "50%")
                                ),
                                # Housing selection
                                conditionalPanel(
                                  condition = "input.category_geo == 'Housing'",
                                  selectInput("housing_geo",
                                              label = "Renter Population",
                                              choices = c("Rent Burden",
                                                          "Severe Rent Burden"),
                                              selected = "Rent Burden",
                                              width = "50%")),
                                
                                width = 3
                                
                                # Sidebar panel ends
                              ),
                              
                              # Main panel
                              mainPanel(
                                
                                leafletOutput("map"),
                                
                                width = 9
                                # Main panel ends  
                              )
                              # Sidebar layout ends            
                            )
                            # Fluid page ends     
                          )
                          # Tab panel ends
                 )
                 # Navbar page ends
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
  
  # SD geo selection
  sd_geo_selected <- reactive({
    
    switch(input$sd_geo,
           "District 1" = sd1_zcta_geo,
           "District 2" = sd2_zcta_geo,
           "District 3" = sd3_zcta_geo,
           "District 4" = sd4_zcta_geo,
           "District 5" = sd5_zcta_geo)
    
  })
  
  # Zip code selection
  zip_geo_selected <- reactive({
    
    if(input$zips_geo == "All"){
      sd_geo_selected() 
    }
    else if(input$sd_geo == "District 1" & input$zip1_geo %in% sort(unique(sd1_zips_geo))){
      sd_geo_selected() %>% 
        filter(Zip_Code %in% input$zip1_geo)
    }
    else if(input$sd_geo == "District 2" & input$zip2_geo %in% sort(unique(sd2_zips_geo))){
      sd_geo_selected() %>% 
        filter(Zip_Code %in% input$zip2_geo)
    }
    else if(input$sd_geo == "District 3" & input$zip3_geo %in% sort(unique(sd3_zips_geo))){
      sd_geo_selected() %>% 
        filter(Zip_Code %in% input$zip3_geo)
    }
    else if(input$sd_geo == "District 4" & input$zip4_geo %in% sort(unique(sd4_zips_geo))){
      sd_geo_selected() %>% 
        filter(Zip_Code %in% input$zip4_geo)
    }
    else if(input$sd_geo == "District 5" & input$zip5_geo %in% sort(unique(sd5_zips_geo))){
      sd_geo_selected() %>% 
        filter(Zip_Code %in% input$zip5_geo)
    }
  })
  
  # Category selections
  category_geo_selected <- reactive({
    
    if(input$category_geo == "Population"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_pop(Population),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Proportion in SD: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Proportion_in_SD),
                                   "<br/>Population: ",
                                   prettyNum(zip_geo_selected()$Population, 
                                             big.mark = ","))) %>% 
        addLegend(position = "topright",
                  title = "Population",
                  pal = pal_pop,
                  values = ~Population,
                  opacity = 1)
    }
    else if(input$category_geo == "Race" & input$race_geo == "Latinx"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_race(Latinx),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population: ",
                                   prettyNum(zip_geo_selected()$Population, big.mark = ","),
                                   "<br/>Latinx: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Latinx))) %>% 
        addLegend(position = "topright",
                  title = "Latinx",
                  pal = pal_race,
                  values = ~Latinx,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
      
    }
    else if(input$category_geo == "Race" & input$race_geo == "Black"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_race(Black),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population: ",
                                   prettyNum(zip_geo_selected()$Population, big.mark = ","),
                                   "<br/>Black: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Black))) %>% 
        addLegend(position = "topright",
                  title = "Black",
                  pal = pal_race,
                  values = ~Black,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Race" & input$race_geo == "Asian"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_race(Asian),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population: ",
                                   prettyNum(zip_geo_selected()$Population, big.mark = ","),
                                   "<br/>Asian: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Asian))) %>% 
        addLegend(position = "topright",
                  title = "Asian",
                  pal = pal_race,
                  values = ~Asian,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Race" & input$race_geo == "White"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_race(White),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population: ",
                                   prettyNum(zip_geo_selected()$Population, big.mark = ","),
                                   "<br/>White: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$White))) %>% 
        addLegend(position = "topright",
                  title = "White",
                  pal = pal_race,
                  values = ~White,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Education" & input$edu_geo == "Less than High School"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_edu(Less_than_HH),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population 25 and older: ",
                                   prettyNum(zip_geo_selected()$Population_over_25, big.mark = ","),
                                   "<br/>Less than High School: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Less_than_HH))) %>% 
        addLegend(position = "topright",
                  title = "Less than High School",
                  pal = pal_edu,
                  values = ~Less_than_HH,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Education" & input$edu_geo == "High School Diploma"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_edu(HS_Diploma),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population 25 and older: ",
                                   prettyNum(zip_geo_selected()$Population_over_25, big.mark = ","),
                                   "<br/>High School Diploma: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$HS_Diploma))) %>% 
        addLegend(position = "topright",
                  title = "High School Diploma",
                  pal = pal_edu,
                  values = ~HS_Diploma,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Education" & input$edu_geo == "Bachelors or higher"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_edu(Bachelors_or_Higher),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population 25 and older: ",
                                   prettyNum(zip_geo_selected()$Population_over_25, big.mark = ","),
                                   "<br/>Bachelors or higher: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Bachelors_or_Higher))) %>% 
        addLegend(position = "topright",
                  title = "Bachelors or higher",
                  pal = pal_edu,
                  values = ~Bachelors_or_Higher,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Immigration" & input$imm_geo == "Documented"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_imm(Immigrant_Citizen),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population: ",
                                   prettyNum(zip_geo_selected()$Population, big.mark = ","),
                                   "<br/>Immigrants Documented: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Immigrant_Citizen))) %>% 
        addLegend(position = "topright",
                  title = "Documented",
                  pal = pal_imm,
                  values = ~Immigrant_Citizen,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Immigration" & input$imm_geo == "Undocumented"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_imm(Immigrant_Undocumented),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Population: ",
                                   prettyNum(zip_geo_selected()$Population, big.mark = ","),
                                   "<br/>Immigrants Undocumented: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Immigrant_Undocumented))) %>% 
        addLegend(position = "topright",
                  title = "Undocumented",
                  pal = pal_imm,
                  values = ~Immigrant_Undocumented,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Housing" & input$housing_geo == "Rent Burden"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_housing(Rent_Burden),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Housing Units: ",
                                   prettyNum(zip_geo_selected()$Housing_Units, big.mark = ","),
                                   "<br/>Renters: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Renter_Units),
                                   "<br/>Rent Burden: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Rent_Burden))) %>% 
        addLegend(position = "topright",
                  title = "Rent Burden",
                  pal = pal_housing,
                  values = ~Rent_Burden,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Housing" & input$housing_geo == "Severe Rent Burden"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_housing(Severe_Rent_Burden),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Housing Units: ",
                                   prettyNum(zip_geo_selected()$Housing_Units, big.mark = ","),
                                   "<br/>Renters: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Renter_Units),
                                   "<br/>Severe Rent Burden: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Severe_Rent_Burden))) %>% 
        addLegend(position = "topright",
                  title = "Severe Rent Burden",
                  pal = pal_housing,
                  values = ~Severe_Rent_Burden,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
    else if(input$category_geo == "Income"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_income(Median_HH_Income),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Median Household Income: ",
                                   label_dollar()(zip_geo_selected()$Median_HH_Income))) %>% 
        addLegend(position = "topright",
                  title = "Median Household Income",
                  pal = pal_income,
                  values = ~Median_HH_Income,
                  opacity = 1,
                  labFormat = labelFormat(prefix = "$"))
    }
    else if(input$category_geo == "Poverty"){
      leaflet(zip_geo_selected()) %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(stroke = TRUE,
                    color = "black",
                    weight = 2,
                    opacity = 0.3,
                    fillColor = ~pal_poverty(Poor_or_Struggling),
                    fillOpacity = 0.8,
                    label = zip_geo_selected()$Zip_Code,
                    labelOptions = labelOptions(textsize = "14px"),
                    highlightOptions = highlightOptions(color = "red", weight = 5, bringToFront = TRUE),
                    popup = paste0("Supervisor District: ",
                                   zip_geo_selected()$SD,
                                   "<br/>Zip Code: ",
                                   zip_geo_selected()$Zip_Code,
                                   "<br/>Income Below 200% of the FPL: ",
                                   label_percent(accuracy = 1)(zip_geo_selected()$Poor_or_Struggling))) %>% 
        addLegend(position = "topright",
                  title = HTML("Income Below 200% <br/> of the Federal Poverty Level"),
                  pal = pal_poverty,
                  values = ~Poor_or_Struggling,
                  opacity = 1,
                  labFormat = labelFormat(transform = function(x) x * 100,
                                          suffix = "%"))
    }
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    
    category_geo_selected()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
