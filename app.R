options(repos = c(CRAN = "https://cran.rstudio.com"))

install.packages("leaflet")
install.packages("raster")
install.packages("sf")
install.packages("shiny")
install.packages("tidyverse")
install.packages("readxl")
install.packages("writexl")
install.packages("wesanderson")
install.packages("bs4Dash")
install.packages("DT")
library(leaflet) #for making the interactive map
library(raster) #for importing raster data
library(scales)
library(lattice)
library(sf)
library(shiny)
library(readxl)
library(writexl)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(bslib)
library(shinythemes)
library(bs4Dash)
library(DT)
library(wesanderson)

###########Cleaning Dataset ###################
#Loading NFHS Shape Files

setwd('C:/Users/Nikhila Vijay/Desktop/Janaagraha/shiny_nfhs')
nfhs_district <- read_sf("IAGE7AFL.shp")

#Renaming variables
nfhs_district <- nfhs_district %>% 
  rename(`shdist` = `DHSREGNA`)

#Changing  district to lower case
nfhs_district <- nfhs_district %>% 
  mutate(shdist = tolower(shdist))

#Keeping only urban variables
nfhs_district <- nfhs_district %>% 
  filter(URBAN_RURA == "U")

#Loading boundary data of Sub-district
india <- read_sf("SUBDISTRICT_11.shp")



#Changing district to lower case
india <- india %>% 
  mutate(shdist = tolower(DISTRICT))

#Loading NFHS cluster data file
nfhs_data <- read_excel("nfhs_data_clster.xlsx",sheet = "Sheet1")

#Renaming variables
nfhs_data <- nfhs_data %>% 
  rename(`DHSCLUST` = `hv001`)

#Loading NFHS district data file
nfhs_data_district <- read_excel("nfhs_data.xlsx",sheet = "Sheet1")

#Merging NFHS data with sub district shape file
nfhs_shape <- left_join(india, nfhs_data_district, by = c("shdist" ="shdist"), relationship ="many-to-many")

#Merging NFHS data with NFHS shape file
nfhs_all <- left_join(nfhs_district,nfhs_data, by = c("DHSCLUST" ="DHSCLUST"), relationship ="many-to-many")

#Keeping only Odisha State file

odisha_cluster <- nfhs_data %>% 
  filter(hv024 == "odisha")

odisha_shape <- nfhs_shape %>% 
  filter(hv024 == "odisha")

odisha_all <- nfhs_all %>% 
  filter(hv024 == "odisha")





########################################################
########### R SHINY DASHBOARD #########################

########## Cleaning ##################################


ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                  "Clean Energy Pathways: Odisha", id ="nav",
                  tabPanel("Dashboard Guide",
                           navset_pill_list(
                             nav_panel(title = "Fuel Choice & Health", tags$h2(tags$b("Fuel Choice & Health Outcomes: Guide")),
                                       card(card_header(tags$h3("Background & Objective")),
                                            tags$div(
                                            "As per the", a("World Health Organisation", href="https://www.who.int/news-room/fact-sheets/detail/household-air-pollution-and-health#:~:text=Impacts%20on%20health,air%20pollution%20data%20for%20details)"), ", household air pollution (HAP) was responsible for 3.2 million deaths per year in 2020, including 7% of deaths of children under the age of 5.",
                                            tags$br(),tags$br(),
                                            "Solid cooking fuel is the main source of household air pollution because of incomplete combustion and poor ventilation.", a("Traditional Chulhas and cooking fuel produce numerous toxic pollutants", href ="https://main.mohfw.gov.in/sites/default/files/5412023661450432724_0.pdf"), "such as formaldehyde, benzene, poly-aromatic hydrocarbons, dioxins etc.",
                                            tags$br(),tags$br(),
                                            "A study published in the", a("Lancet", href="https://cpcb.nic.in/Actionplan/Bhubaneswar.pdf"), "in 2018 attributed air pollution to 31,118 deaths in Odisha in 2017, out of which 57% of the deaths were due to household air pollution. 
                                            The main health impacts of HAP include lung cancer, heart disease, stroke, chronic obstructive pulmonary disease, and child pneumonia."
                                            )),
                                       card(card_header(h3("Approach")),
                                            tags$div("We are using National Family Health Survey 2019-20 (NFHS) data to map cooking fuel choice with health outcomes associated with Household Air Pollution (HAP). 
                                                     Since most of the health outcomes are associated with women and children, we are looking at the incidence of the following health outcomes among women:",
                                                     tags$br(),
                                                     tags$ul(
                                                     tags$li("Chronic Respiratory Disease/Asthma"),
                                                     tags$li("Hypertension"),
                                                     tags$li("Heart Disease"),
                                                     tags$li("Anemia")),
                                                     tags$br(),
                                                     "We are looking to see if there is any correlation between usage of traditional fuels and higher incidence of the health outcomes associated with such fuels in Odisha"
                                            )
                                       ),
                                       card(card_header(h3("Data Guide")),
                                            "We are considering only Urban Population in Odisha for our analysis:",
                                            tags$br(),tags$br(),
                                            tags$div(HTML("<b>Base Map</b>: We are using % HH fuel choice at the district level."),
                                                     tags$br(),
                                                     tags$ul(
                                                       tags$li("The total number of Districts in Odisha: 30"),
                                                       tags$li("The total number of HHs in Odisha: 3707")
                                                       ),
                                                     HTML("<b>Scatter Plot</b>: We are using the % of cooking fuel choice used as x axis and the incidence of health outcome as the y axis. The data for the scatter plot is at the cluter level."),
                                                     tags$br(),
                                                     tags$ul(
                                                       tags$li("The total number of clusters in Odisha: 184")),
                                                     HTML("<b>Circles Plot</b>: The circles represent the % incidence of selected health outcome among adult women."),
                                                     tags$br(),
                                                     tags$ul(
                                                       tags$li("The total persons among the HHs sampled in Odisha: 15313"),
                                                       tags$li("The total adult women among the HHs sampled in Odisha: 4296")),
                                                     tags$br(),tags$br()
                                                       
                                       ))),
                           )
                           ),
                  tabPanel("Fuel Choice & Health: NFHS",
                           leafletOutput("map", height= 700, width = 1300),
                           absolutePanel(id = "controls", fixed = FALSE,
                                         draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
                                         width = 300, height = "auto",
                                         tags$h4("Base Indicators"),
                                         selectInput("fuel", "Cooking Fuel Choice (District)", choices = c("Electricity","LPG","Natural Gas","Biogas","Kerosene","Coal,lignite","Charcoal","Wood","Straw/Shrub/Grass","Agri Crop","Animal Dung","No Food cooked in House"),
                                                     selected = "Charcoal",multiple = FALSE),
                                         selectInput("fuelcl", "Cooking Fuel (Cluster)", choices = c("Electricity","LPG","Natural Gas","Biogas","Kerosene","Coal,lignite","Charcoal","Wood","Straw/Shrub/Grass","Agri Crop","Animal Dung","No Food cooked in House",""),
                                                     selected = "Charcoal",multiple = FALSE),
                                         tags$h4("Health Indicators"),
                                         selectInput("health", "Health outcomes associated with HH Air Pollution: Women", choices = c("Chronic Respiratory Disease/Asthma","Hypertension","Heart Disease","Anemia level: Severe","Anemia level: Moderate","Anemia level: Mild",""),
                                                     selected = "Chronic Respiratory Disease/Asthma",multiple = FALSE),
                                         plotOutput("scatterbasehealth", height = 300, width = 300)
                           )
                  )
                ))


server <- function(input, output, session) {
  
  odisha_shape <- nfhs_shape %>% filter(hv024 == "odisha")
  odisha_all <- nfhs_all %>% filter(hv024 == "odisha")
  
  # Reactive expressions for indicators
  fuelindicator <- reactive({
    switch(input$fuel,
           "Electricity" = odisha_shape$cooking_fuel_d1_dist_pc,
           "LPG" = odisha_shape$cooking_fuel_d2_dist_pc,
           "Natural Gas" = odisha_shape$cooking_fuel_d3_dist_pc,
           "Biogas" = odisha_shape$cooking_fuel_d4_dist_pc,
           "Kerosene" = odisha_shape$cooking_fuel_d5_dist_pc,
           "Coal,lignite" = odisha_shape$cooking_fuel_d6_dist_pc,
           "Charcoal" = odisha_shape$cooking_fuel_d7_dist_pc,
           "Wood" = odisha_shape$cooking_fuel_d8_dist_pc,
           "Straw/Shrub/Grass" = odisha_shape$cooking_fuel_d9_dist_pc,
           "Agri Crop" = odisha_shape$cooking_fuel_d10_dist_pc,
           "Animal Dung" = odisha_shape$cooking_fuel_d11_dist_pc,
           "No Food cooked in House" = odisha_shape$cooking_fuel_d12_dist_pc
    )
  })
  
  fuel_cluster_indicator <- reactive({
    switch(input$fuelcl,
           "Electricity" = odisha_all$cooking_fuel_d1_cl_pc,
           "LPG" = odisha_all$cooking_fuel_d2_cl_pc,
           "Natural Gas" = odisha_all$cooking_fuel_d3_cl_pc,
           "Biogas" = odisha_all$cooking_fuel_d4_cl_pc,
           "Kerosene" = odisha_all$cooking_fuel_d5_cl_pc,
           "Coal,lignite" = odisha_all$cooking_fuel_d6_cl_pc,
           "Charcoal" = odisha_all$cooking_fuel_d7_cl_pc,
           "Wood" = odisha_all$cooking_fuel_d8_cl_pc,
           "Straw/Shrub/Grass" = odisha_all$cooking_fuel_d9_cl_pc,
           "Agri Crop" = odisha_all$cooking_fuel_d10_cl_pc,
           "Animal Dung" = odisha_all$cooking_fuel_d11_cl_pc,
           "No Food cooked in House" = odisha_all$cooking_fuel_d12_cl_pc
    )
  })
  
  healthindicator <- reactive({
    switch(input$health,
           "Chronic Respiratory Disease/Asthma" = odisha_all$resp_asthma_d2_cl_pc,
           "Hypertension" = odisha_all$hypertension_d2_cl_pc,
           "Heart Disease" = odisha_all$heart_disease_d2_cl_pc,
           "Anemia level: Severe" = odisha_all$anemia_d1_cl_pc,
           "Anemia level: Moderate" = odisha_all$anemia_d2_cl_pc,
           "Anemia level: Mild" = odisha_all$anemia_d3_cl_pc
    )
  })
  
  output$scatterbasehealth <- renderPlot({
    plot(healthindicator() ~ fuel_cluster_indicator(), data = odisha_all, xlab = "Fuel Indicator %",
         ylab = "Health Indicator %",pch = 10, col = "orange")
    abline(lm(healthindicator() ~ fuel_cluster_indicator(), data=odisha_all), col = "lightblue")
  })
  
  pal <- reactive({
    colorBin(palette = "PuRd", domain = fuelindicator(), bins = 10)
  })
  qpal <- reactive({
    colorBin("Blues", domain = healthindicator(), bins = 6)
  })
  
  # Render initial Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addPolygons(data = odisha_shape,
                  fillColor = ~pal()(fuelindicator()),
                  popup = paste0("<strong>District: </strong>", odisha_shape$DISTRICT, 
                                 "<br><strong>Sub District: </strong>", odisha_shape$NAME, 
                                 "<br><strong>% of Cases:</strong>", fuelindicator(),
                                 "<br><strong>Total HHs in District</strong>", odisha_shape$hh_count),
                  color = "gray",
                  fillOpacity = 0.8,
                  weight = 1)  %>% 
      addTiles() %>% 
      addLegend(
        position = "topleft",
        pal = pal(), values = fuelindicator(), title = "Fuel Choice %"
      ) %>% 
      addCircleMarkers(data = odisha_all,
                       weight = 6, color = ~qpal()(healthindicator()),
                       popup = paste0("<strong>District: </strong>", odisha_all$shdist.x, 
                                      "<br><strong>% of Cases: </strong>", healthindicator(),
                                      "<br><strong>Total women in cluster: </strong>", odisha_all$women_count_cl,
                                      "<br><strong>Total adult in cluster: </strong>", odisha_all$person_count_cl),
                       fillOpacity = 0.2) %>% 
      addLegend(
        position = "topleft",
        pal = qpal(), values = healthindicator(), title = "% Health Incidence"
      )
  
  })
  
  # Update map when fuel input changes
  observeEvent(input$fuel, {
    leafletProxy("map", data = odisha_shape) %>% 
      clearShapes() %>% 
      addPolygons(fillColor = ~pal()(fuelindicator()),
                  popup = paste0("<strong>District: </strong>", odisha_shape$DISTRICT, 
                                 "<br><strong>Sub District: </strong>", odisha_shape$NAME, 
                                 "<br><strong>% of Cases: </strong>", fuelindicator(),
                                 "<br><strong>Total HHs in District: </strong>", odisha_shape$hh_count),
                  color = "gray",
                  fillOpacity = 0.8,
                  weight = 1) %>% 
      addTiles() %>% 
      addCircleMarkers(data = odisha_all, lng = odisha_all$LONGNUM, lat = odisha_all$LATNUM,
                       weight = 6, color = ~qpal()(healthindicator()),
                       popup = paste0("<strong>District: </strong>", odisha_all$shdist.x, 
                                      "<br><strong>% of Cases: </strong>", healthindicator(),
                                      "<br><strong>Total women in cluster: </strong>", odisha_all$women_count_cl,
                                      "<br><strong>Total adult in cluster: </strong>", odisha_all$person_count_cl),
                       fillOpacity = 0.2)  
    })
  
  
}

shinyApp(ui, server)


























