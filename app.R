##Data 
library(leaflet)

if (!require(maps)) install.packages('maps')
library(maps)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(benford.analysis)
library(BenfordTests)
load("gun.RData")
load("db_map.RData")
load("statesPop.RData")
# gun$year<-as.numeric(gun$year)
year_list <- c(unique(as.character(gun$year)))

db_map$year <- as.character(db_map$year)
db_map<-na.omit(db_map)

##

# db_2017<-db_map%>%
#   filter(year==2017)
# 
# mapStates = map("state", fill = TRUE, plot = FALSE)
# map_2017<-leaflet(data=mapStates)%>%
#   addTiles()%>%
#   addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>%
#   addMarkers(clusterOptions = markerClusterOptions(),data = db_2017)
# 
# bins <- c(0, 100000, 200000, 500000, 1000000, 2000000, 5000000, 10000000, Inf)
# pal <- colorBin("YlOrRd", domain = statesPop$Count, bins = bins)
# quakes <- db_2017 %>%
#   dplyr::mutate(mag.level = cut(num_kill,c(3,4,5,6),
#                                 labels = c('>3 & <=4', '>4 & <=5', '>5 & <=6')))
# 
# quakes.df <- split(quakes, quakes$mag.level)
# 
# names(quakes.df) %>%
#   purrr::walk( function(df) {
#     l <<- map_2017 %>%
#       addMarkers(data=quakes.df[[df]],
#                  lng=~longitude, lat=~latitude,
#                  label=~as.character(num_kill),
#                  popup=~as.character(num_kill),
#                  group = df,
#                  clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
#                  labelOptions = labelOptions(noHide = F,
#                                              direction = 'auto'))
#   })



##ui part

ui <- dashboardPage(
  skin = "black",
  # Application title
  dashboardHeader(title = "Gun violence"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Interactive map", tabName = "a", icon = icon("globe")),
      menuItem("Trends by time", tabName = "b", icon = icon("bar-chart-o")),
      menuItem("Benford Law Test", tabName = "c", icon = icon("stethoscope"))
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "a",
      fluidRow(
                      
        box(title = "Gun violence map in America",width = NULL, solidHeader = TRUE,
                     selectInput("selectyear2",label = "Select Year:",choices=year_list),
                     leafletOutput("plot1", height = 500))
              
      )
    ),tabItem(
      tabName = "c",
      plotOutput("plot2",height = 600)
    ),
    tabItem(
      tabName = "b",
      fluidRow(width = 8,
                      box(title = "Trends by year",width = NULL, solidHeader = TRUE,
                          selectInput("selectyear1",label = "Select Year:",choices=year_list),
                          plotOutput("plot3",height = 500))
                      
      )
    
    )))
  )


# Define server 
server <- function(input, output) {
  output$plot2 <- renderPlot({
    c <- benford(statesPop$Count)
    plot(c)
  })
  output$plot1<-renderLeaflet({
    
    
    db_2017<-db_map%>%
      filter(year==input$selectyear2)
    
    mapStates = maps::map("state", fill = TRUE, plot = FALSE)
    map_2017<-leaflet(data=mapStates)%>%
      addTiles()%>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>%
      addMarkers(clusterOptions = markerClusterOptions(),data = db_2017)
    
    bins <- c(0, 100000, 200000, 500000, 1000000, 2000000, 5000000, 10000000, Inf)
    pal <- colorBin("YlOrRd", domain = statesPop$Count, bins = bins)
    quakes <- db_2017 %>%
      dplyr::mutate(mag.level = cut(num_kill,c(3,4,5,6),
                                    labels = c('>3 & <=4', '>4 & <=5', '>5 & <=6')))
    
    quakes.df <- split(quakes, quakes$mag.level)
    
    # names(quakes.df) %>%
    #   purrr::walk( function(df) {
    #     l <<- map_2017 %>%
    #       addMarkers(data=quakes.df[[df]],
    #                  lng=~longitude, lat=~latitude,
    #                  label=~as.character(num_kill),
    #                  popup=~as.character(num_kill),
    #                  group = df,
    #                  clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
    #                  labelOptions = labelOptions(noHide = F,
    #                                              direction = 'auto'))
    #   })
    
    
    
    
    map_go<-leaflet(data = mapStates)%>%
    addTiles()%>% addPolygons(
      fillColor = ~pal(statesPop$Count),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7)%>% addPolygons(
        fillColor = ~pal(statesPop$Count),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE))%>%
    addMarkers(clusterOptions = markerClusterOptions(),data = db_2017)%>%
    addLayersControl(
      overlayGroups = names(quakes.df),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  map_go
    
  })
  output$plot3<-renderPlot({
    temp=gun %>% 
      filter(year==input$selectyear1) %>%
      group_by(month) %>% 
      summarise(count=n())
    # ggplot(temp,aes(month,count))+
    #   geom_line(size=0.9)+
    #   geom_point(alpha=0.5)+theme_bw()+
    #   theme(legend.position="bottom")+
    #   labs(x="Month",y="Count of incidents")
      ggplot(temp) +
        geom_bar(mapping = aes(x=month, y=count,fill=month),stat = 'identity')+
        labs(x="Month",y="Count of incidents")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


