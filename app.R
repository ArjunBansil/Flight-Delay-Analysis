library(nycflights13)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(geosphere)
library(sparklyr)
library("shinyTime")
library(scales)

sc = spark_connect(master = "local",version = "2.1.0")

copy_to(sc, flights, "flights_s", overwrite = TRUE)
flights_tbl = tbl(sc, "flights_s")

copy_to(sc, airlines, "airlines_s", overwrite = TRUE)
airlines_tbl = tbl(sc, "airlines_s")

model_data <- flights_tbl %>%
  filter(!is.na(arr_delay) & !is.na(dep_delay) & !is.na(distance)) %>%
  filter(dep_delay > 15 & dep_delay < 240) %>%
  filter(arr_delay > -60 & arr_delay < 360) %>%
  left_join(airlines_tbl, by = c("carrier" = "carrier")) %>%
  mutate(gain = dep_delay - arr_delay) %>%
  select(origin, dest, carrier, airline = name, distance, dep_delay, arr_delay, gain)

partitions = model_data %>% 
  sdf_partition(train_data = 0.5, valid_data = 0.5, seed = 777)

lm1 <- ml_linear_regression(partitions$train_data, gain ~ distance + dep_delay + carrier)

pred_tbl <- sdf_predict(lm1, partitions$valid_data)

lookup_tbl <- pred_tbl %>%
  group_by(origin, dest, carrier, airline) %>%
  summarize(
    flights = n(),
    distance = mean(distance),
    avg_dep_delay = mean(dep_delay),
    avg_arr_delay = mean(arr_delay),
    avg_gain = mean(gain),
    pred_gain = mean(prediction)
  )

sdf_register(lookup_tbl, "lookup")
tbl_cache(sc, "lookup")

carrier_origin <- c("JFK", "LGA", "EWR")
carrier_dest <- c("BOS", "DCA", "DEN", "HNL","LAX","SEA","SFO","STL")

ui <- fluidPage(
  tags$script(' var setInitialCodePosition = function(){
              setCodePosition(false,false);};'),
  
  titlePanel("NYCFlights13 Time Gained in Flight"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("origin", "Flight Origin: ", 
                   carrier_origin, selected = "JFK"),
      br(),
      
      radioButtons("dest", "Flight Destination: ",
                   carrier_dest, selected = "SFO")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Map", leafletOutput("map")),
                  tabPanel("Data", dataTableOutput("datatable")),
                  tabPanel("Estimated Flight Time", uiOutput("calc"))
      )
    )
  )
  )

server <- function(input, output){
  origin <- reactive({
    req(input$origin)
    filter(nycflights13::airports, faa==input$origin)
  })
  
  dest <- reactive({
    req(input$dest)
    filter(nycflights13::airports, faa==input$dest)
  })
  
  plot_data <- reactive({
    req(input$origin, input$dest)
    lookup_tbl %>%
      filter(origin==input$origin & dest==input$dest) %>%
      ungroup() %>%
      select(airline, flights, distance, avg_gain, pred_gain) %>%
      collect
  })
  
  output$plot <- renderPlot({
    ggplot(plot_data(), aes(factor(airline), pred_gain))+
      geom_bar(stat = "identity", fill ="#F44336") +
      geom_point(aes(factor(airline),avg_gain)) + 
      coord_flip() +
      labs(x = "", y = "Time gained in flight (minutes)") +
      labs(title = "Observed gain (plot) vs Predicted gain (bar)")
  })
  
  output$map <- renderLeaflet({
    gcIntermediate(
      select(origin(), lon, lat),
      select(dest(), lon, lat),
      n=100, addStartEnd=TRUE, sp=TRUE
    ) %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolylines()
  })
  
  output$datatable <- renderDataTable(
    datatable(plot_data()) %>% 
      formatRound(c("flights", "distance"),0) %>%
      formatRound(c("avg_gain","pred_gain"),1)
  )
  
  
  output$calc <- renderUI({
    fluidPage({
      timeInput("t1","Time: ")
      textOutput("f","Summary")
    })
  })
  
}

shinyApp(ui = ui, server = server)






