# Library yang digunakan
library(shiny)
library(leaflet)
library(tidyverse)
# library(rsconnect)
# library(cowplot)
library(shinydashboard)
library(shinyWidgets)
# library(dplyr)
# library(lubridate)
# library(ggplot2)
# library(gganimate)
# library(gifski)
# library(av)
library(shinyDatetimePickers)
# library(DT)
# library(tidyverse)
# library(leaflet)
library(secr)
library(sp)
# library(leaflet.extras2)
# library(shiny)
# library(leaflet.extras)
library(shinyWidgets)
# library(shinythemes)
library(shinyFeedback)
library(echarts4r)
library(trajr)
library(geosphere)

addResourcePath("image", 'C:/Users/INFINIX/Downloads/ANTASENA v2/ANTASENA')

ais_data <- read_csv("file_ais_20k.csv")
maritime_sec <- read_csv(file = "alki2.csv")
raw_data <- read_csv("file_ais_ext_aa.csv")

y_coord <- c(17,  18, 20, 21)
x_coord <- c(-71, -67, -65,-73)
xym <- cbind(x_coord, y_coord)

y_coord2 <- c(34,  31, 22, 26)
x_coord2 <- c(-123, -128, -117,-110)
xym2 <- cbind(x_coord2, y_coord2)

y_coord3 <- c(25,  20, 23, 26)
x_coord3 <- c(-86, -84, -80,-82)
xym3 <- cbind(x_coord3, y_coord3)

list_data <- list(xym, xym2, xym3)

polygon_list<-list()
for (i in 1:length(list_data)) {
  p = Polygon(list_data[[i]])
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  data_poly = data.frame(anomaly=99.9)
  polygon_list[[i]] = SpatialPolygonsDataFrame(sps,data_poly)
}

footer <- fluidRow(
  div(
    style = "
      width: 100vw;
      padding: 10px;
      position: fixed;
      bottom: 0;
      left: 0;
      background-color: #222;
      margin-top: 20px;
      z-index: 10000;
    ",
    p(
      style="
        text-align: center; 
        color: white; margin: 0;",
      "Copyright TNI AL ANTASENA 2023 Version 2.0")
  )
)

# Dashboard 1
ui <- dashboardPage( skin = "blue",
                     dashboardHeader(title = "ANTASENA", titleWidth = 230),
                     dashboardSidebar(
                       tags$head(tags$style((HTML(".skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrappe {background-color:#ffffff;")))),
                       tags$figure(align = "center", 
                                   br(),
                                   #tags$img(src="https://i.postimg.cc/dtmsXcSt/antasena.png",
                                   tags$img(src="image/LOGO-ANTASENA.png",
                                            width = 200, 
                                            height = 180)),
                       sidebarMenu(
                         menuItem("COORDINATE GENERATOR", tabName = "coordinate_generator"),
                         menuItem("ANOMALY DETECTION", tabName = "anomaly_detection"),
                         menuItem("KONSERVASI", tabName = "konservasi"),
                         menuItem("STRUKTUR ORGANISASI", tabName = "struktur")
                       )
                     ),
                     dashboardBody(
                       footer, bookmarkButton(),
                       
                       tabItems(
                         
                         tabItem(
                           tabName = "coordinate_generator", 
                           
                           fluidRow(style = "margin-top : 20px",
                             box(width = 12,
                                 title = "Input",
                                 fluidRow(
                                   column(width =2,
                                          numericInput("latitude","LATITUDE:",value=10)
                                          
                                   ),
                                   column(width =2,
                                          numericInput("longitude","LONGITUDE:",value=10)
                                   ),
                                   
                                   column(width = 2,
                                          numericInput("num3", "MMSI:",value=10)
                                          
                                   ),
                                   column(
                                     width = 4,
                                     datetimeMaterialPickerInput
                                     ("dtmpicker",label = "Base Date Time", disablePast = TRUE)
                                   )
                                 )
                             )
                           ),
                           
                           fluidRow(
                             box(width = 12,
                                 title = "Map",
                                 
                                 br(),
                                 downloadButton('download','DOWNLOAD'),
                                 actionButton("generate","GENERATE", class = "btn btn-default action-button"),
                                 actionButton("ukur","MEASURE"),
                                 actionButton('reset','RESET'),
                                 fluidRow(
                                   column(
                                     align = "left",
                                     height = 2,
                                     width = 8,
                                     h1(uiOutput("waktu"))
                                   )
                                 ),
                                 fluidRow(
                                   column(
                                     width = 9,
                                     leafletOutput("map", height = "800px")
                                   ),
                                   column(
                                     width = 3,
                                     #tableOutput("data_table")
                                     DT::dataTableOutput("data_table")
                                   )
                                 ) 
                             )
                             
                           )
                           
                         ),
                         
                         tabItem(
                           tabName = "anomaly_detection",
                           fluidRow(style = "margin-top : 20px",
                             box(
                               width = 6,
                               title = "Peta",
                               leafletOutput(outputId = "View_maps")
                             ),
                             box(
                               width = 6,
                               title = "Details Trajectory",
                               uiOutput("trajectory_plot")
                             )  ),
                           fluidRow(
                             valueBoxOutput("value_jumlah_kapal"),
                             valueBoxOutput("value_alert"),
                             # valueBoxOutput("tinggi_gelombang")
                             valueBoxOutput("time",width=4)
                             ) ,
                           
                           fluidRow(
                             plotOutput("nav1_trajectory_plot")
                           ),
                           fluidRow(
                             title="INPUT",
                             #box(
                             #selectInput("lat", "MMSI", choices = raw_data$MMSI%>%unique() ),width = 4),
                             column(
                               width = 4,
                               textInput("text", "Filter Berdasarkan MMSI:"),
                               actionButton(
                                 inputId = "Filter", 
                                 label = "Filter"
                               ))
                           ),
                           fluidRow(
                             column(
                               width = 12,
                               box(
                                 p("KAPAL YANG MELANGGAR"), width = 12,height = "600px",
                                 DT::dataTableOutput("table_profile")),
                               box(
                                 uiOutput("data_kapal")
                               )),
                       
                             fluidRow(
                               column(
                                 width = 12,
                                 leafletOutput(outputId = "peta_kecepatan", height = "50vh")
                               )
                             )
                          )),
                         tabItem(
                           tabName = "konservasi",
                           fluidRow(
                             column(
                               width = 3,
                               airDatepickerInput(
                                 inputId = "nav2_datetimepicker_input_1",
                                 label = "Input Date (After)",
                                 value = min(maritime_sec$timestamp),
                                 timepicker = T
                               )
                             ),
                             column(
                               width = 3,
                               airDatepickerInput(
                                 inputId = "nav2_datetimepicker_input_2",
                                 label = "Input Date (Before)",
                                 value = min(maritime_sec$timestamp) + 3600*24*30,
                                 timepicker = T
                               )
                             )
                           ),
                           fluidRow(
                             valueBoxModuleUI(
                               id = "nav2_last_date", 
                               subtitle = "Last Date",
                               icon = icon("calendar"),
                               width = 4
                             ),
                             valueBoxModuleUI(
                               id = "nav2_total_ships",
                               subtitle = "Total Ships",
                               icon = icon("ship"),
                               width = 4
                             ),
                             valueBoxModuleUI(
                               id = "nav2_days_to_ffd",
                               subtitle = "Days to Next FFD",
                               icon = icon("fish"),
                               width = 4
                             )
                             # valueBoxModuleUI(
                             #   id = "nav2_tanggal_pasang",
                             #   subtitle = "Gelompang Pasang Selanjutnya",
                             #   icon = icon("exclamation-triangle"),
                             #   width = 3
                             # )
                           ),
                           fluidRow(
                             column(
                               width = 12,
                               leafletOutput(outputId = "nav2_aismap", height = "50vh")
                             )
                           ),
                           fluidRow(
                             style = "margin-top : 20px",
                             box(
                               width = 3,
                               tags$h1("Total Alerts Recorded"),
                               echarts4rOutput(outputId = "nav2_total_alert_output")
                             ),
                             box(
                               width = 3,
                               tags$h1("Vessel Type Distribution"),
                               echarts4rOutput(outputId = "nav2_total_ship_types")
                             ),
                             box(
                               width = 3,
                               tags$h1("Hourly Activity"),
                               echarts4rOutput(outputId = "nav2_hourly_activity")
                             ),
                             box(
                               width = 3,
                               tags$h1("Transceiver Class Distrib"),
                               echarts4rOutput(outputId = "nav2_transceiver_class_activity")
                             )
                           )
                           
                         ),
                         
                         tabItem(
                           tabName = "struktur",  class='active', role="figure",
                           title = "STRUKTUR ORGANISASI",
                           tags$img(src="image/Screenshot_278.png", width = "100%")
                         )
                         
                       )
                     )
                     )              




server <- function(input, output, session) {
  # 
  # # server logic untuk dashboard 1
  
  output$map <- renderLeaflet({
    if(is.null(nrow(data$click_data))) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 106.8114, lat = -6.0228, zoom = 9)
    }
    else {data$click_data %>% 
        leaflet() %>%
        addTiles() %>%
        setView(lng = 106.8114, lat = -6.0228, zoom = 9) %>%
        addCircles(lng = ~longitude, lat = ~latitude) 
    }
  }
  )
  
  output$leaflet1<-renderLeaflet({
    leaf <- leaflet() %>%
      addTiles() %>%
      setView(lng = 106.8114, lat = -6.0228, zoom=9)%>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "maroon",
        localization = "id"
      )
  }
  )
  
  leafletOutput("leaflet1")
  
  # Membuat data kosong
  data <- reactiveValues(click_data = 
                           data.frame(MMSI = numeric(),
                                      latitude = numeric(),
                                      longitude = numeric(),
                                      time = numeric()))
  
  # Memfungsikan tombol download
  output$download <- downloadHandler(
    filename = function() {
      paste("Chrt_Gen", '.csv', sep='')
    },
    content = function(file) {
      write.csv(data$click_data, file)
    })
  
  # Memfungsikan tombol measure
  observeEvent(input$ukur,{
    leafletProxy("map")%>%
      addMeasure() 
  })
  
  # Menambahkan fitur klik ke peta dan memperbarui tabel dengan nilai yang diklik
  observeEvent(input$map_click, {
    click_data <- data.frame( MMSI = input$num3,
                              latitude = input$map_click$lat,
                              longitude = input$map_click$lng,
                              time = input$dtmpicker)
    data$click_data <- rbind(data$click_data, click_data)
  })
  
  # Menampilkan tabel data
  output$data_table <- DT::renderDataTable({
    data$click_data
  })
  
  # Mengaktifkan fungsi bergerak setiap detik  
  output$waktu <- renderText({
    invalidateLater(1000, session) 
    paste0(Sys.time())
  })
  
  # Mengaktifkan fungsi generate  
  observeEvent(input$generate, {
    click_data <- data.frame(MMSI = input$num3,
                             latitude = input$latitude,
                             longitude = input$longitude,
                             time=input$dtmpicker)
    data$click_data <- rbind(data$click_data, click_data)
    
    leafletProxy("map", data = NULL) %>%
      setView(lng = input$longitude, lat = input$latitude, zoom = 9) %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  # Mengaktifkan fungsi download  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("Chart_Gen", '.csv', sep='') },
    content = function(file) {
      write.csv(data, file)
    })
  # Memfungsikan tombol reset
  observeEvent(input$reset,{
    leafletProxy("map", data = NULL) %>%
      clearShapes()
  })
  # Memfungsikan tombol reset2
  observe({
    if (input$reset == 0)
      return()
    data$click_data <- NULL
  })
  # Aksi saat peta di klik
  observeEvent(input$map_click,{
    click <- input$map_click
    leafletProxy("map", data = NULL) %>%
      setView(lng = click$lng, lat = click$lat, zoom = 9) %>%
      addMarkers(lng = click$lng, lat = click$lat)
  })
  # server logic untuk dashboard 2
  output$peta_1 <- renderLeaflet({
    data_koordinat  %>% 
      select(MMSI, LON, LAT, BaseDateTime) %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == min(BaseDateTime)) %>% 
      ungroup() %>% 
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~LON, lat = ~LAT )%>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "maroon",
        localization = "id"
      )
  })
  raw_data_filter <- reactive({
    raw_data %>% 
      group_by(MMSI) %>% 
      count() %>% 
      ungroup() %>% 
      filter(n >= 200) %>% pull(MMSI) -> mmsi_for_filter
    raw_data %>% 
      filter(MMSI %in% mmsi_for_filter) -> ais_data_for_app
    ais_data_for_app %>% 
      select(MMSI, BaseDateTime, LON, LAT, VesselType,VesselName,SOG,COG,Heading,IMO,CallSign,Status,Length,Width,Draft,Cargo,TransceiverClass) %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == max(BaseDateTime)) %>% 
      ungroup() %>% 
      select(MMSI, LON, LAT, BaseDateTime, VesselType,VesselName,SOG,COG,Heading,IMO,CallSign,Status,Length,Width,Draft,Cargo,TransceiverClass) -> ais_data_for_app
    ais_data_for_app %>% 
      select(LON, LAT) %>% 
      as.matrix() -> lon_lat_matrix
    
    
    rownames(lon_lat_matrix) <- ais_data_for_app$MMSI
    
    pointsInPolygon(lon_lat_matrix, xym) -> is_anomaly
    
    df <- data.frame(matrix(ncol = 3, nrow = length(is_anomaly)))
    
    #provide column names
    colnames(df) <- c('var1', 'var2', 'var3')
    
    for (i in 1:length(list_data)) {
      pointsInPolygon(lon_lat_matrix, list_data[[i]]) -> is_anomaly
      df[i] <- is_anomaly
    }
    
    ais_data_for_app %>% 
      bind_cols(df) %>% 
      mutate(is_anomaly = var1 + var2 + var3) %>% 
      mutate(is_anomaly = as.logical(is_anomaly)) %>% 
      select(MMSI, LON, LAT,BaseDateTime, is_anomaly, VesselType,VesselName,SOG,COG,Heading,IMO,CallSign,Status,Length,Width,Draft,Cargo,TransceiverClass) %>% 
      mutate(is_anomaly = factor(is_anomaly))
  })
  
  output$View_maps <- renderLeaflet({
    pal <- colorFactor(c("blue","red"), domain = c(TRUE,FALSE))
    raw_data_filter() %>% 
      mutate(
        cntnt=paste0('<strong>Name: </strong>',VesselName,
                     '<br><strong>MMSI:</strong>', MMSI,
                     '<br><strong>Last Observed:</strong>', BaseDateTime,
                     '<br><strong>SOG:</strong>', SOG,
                     '<br><strong>COG:</strong>', COG,
                     '<br><strong>Heading:</strong>', Heading,
                     '<br><strong>IMO:</strong>', IMO,
                     '<br><strong>Call Sign:</strong>', CallSign,
                     '<br><strong>Vessel Type:</strong>', VesselType,
                     '<br><strong>Status:</strong>', Status,
                     '<br><strong>Cargo:</strong>', Cargo,
                     '<br><strong>Transceiver Class:</strong>', TransceiverClass)
      ) %>% 
      leaflet() %>% 
      addTiles() -> peta
    for (i in 1:length(polygon_list)) {
      peta %>% 
        addPolygons(data=polygon_list[[i]])->peta
    }
    peta %>% 
      # setView(lng = 106.8114, lat = -6.0228, zoom = 9) %>%
      addCircleMarkers(lng = ~LON, lat = ~LAT, color = ~pal(is_anomaly),layerId = ~MMSI,popup = ~cntnt, clusterOptions = markerClusterOptions())
   
  })
  
  data_trajectory <- reactive({
    req(input$View_maps_marker_click$id)
    raw_data %>% 
      filter(MMSI == input$View_maps_marker_click$id) %>% 
      # arrange dataset based on BaseDateTime ascendingly
      arrange(BaseDateTime) %>% 
      # get LON, LAT, and BaseDateTime column
      select(MMSI, LON, LAT, BaseDateTime) %>% 
      inner_join(
        raw_data_filter() %>% 
          select(MMSI, is_anomaly), by=c("MMSI"="MMSI")  ) 
  })
  
  output$trajectory_plot <- renderUI({
    req(data_trajectory)
    if(nrow(data_trajectory() == 0)){
      print(data_trajectory())  
      data_trajectory() %>% pull(is_anomaly) %>% unique() -> status
      if(status == FALSE){
        data_trajectory() %>% 
          leaflet() %>% 
          addTiles() %>% 
          leaflet.extras2::addAntpath(lng = ~LON, lat = ~LAT, color = "blue") %>% 
          addMeasure()
      }else{
        data_trajectory() %>% 
          leaflet() %>% 
          addTiles() %>% 
          leaflet.extras2::addAntpath(lng = ~LON, lat = ~LAT, color = "red") %>% 
          addMeasure()
      }
    } else{
      paste0("template")   }   
  })
  
  output$nav1_trajectory_plot <- renderPlot({
    req(input$View_maps_marker_click$id)
    raw_data %>% 
      filter(MMSI == input$View_maps_marker_click$id) %>% 
      # arrange dataset based on BaseDateTime ascendingly
      arrange(BaseDateTime) %>% 
      # get LON, LAT, and BaseDateTime column
      select(LON, LAT, BaseDateTime) -> traj_data
    
    print(traj_data)
    
    # define new variable: time
    traj_data$time <- traj_data$BaseDateTime - traj_data$BaseDateTime[1]
    
    # from traj_data
    traj_data %>% 
      # get LON, LAT, and time
      select(LON, LAT, time) %>% 
      # change time to integer
      mutate(time = as.integer(time))-> traj_data
    
    # check traj_data
    traj_data
    
    # create trajectory from data
    TrajFromCoords(
      track = traj_data,
      xCol = "LON",
      yCol = "LAT",
      timeCol = "time",
      timeUnits = "s"
    ) -> traj_data
    
    print(traj_data)
    
    traj_data %>% 
      ggplot(aes(x = x, y = y)) + 
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      theme_minimal()
  })
  
  output$value_jumlah_kapal <- renderValueBox({
    valueBox(
      value = raw_data %>% pull(MMSI) %>% unique() %>% length(),
      subtitle = "Jumlah Kapal", 
      icon = icon("ship"),
      color = "aqua")
  })
  output$value_alert <- renderValueBox({
    valueBox(
      value = raw_data_filter() %>%  filter(is_anomaly == TRUE)%>% pull(MMSI) %>% unique() %>% length(),
      subtitle = "Jumlah Alert", 
      icon = icon("triangle-exclamation"),
      color = "maroon"  )
  })
  
  
  callModule(
    valueBoxModule,
    "nav2_last_date",
    value = "2022-12-24"
  )
  
  callModule(
    valueBoxModule,
    "nav2_total_ships",
    value = maritime_sec %>% 
      pull(ship_id) %>%
      unique() %>% 
      length()
  )
  
  callModule(
    valueBoxModule,
    "nav2_days_to_ffd",
    value = paste0(15, " days ahead")
  )
  
  callModule(
    valueBoxModule,
    "nav2_tanggal_pasang",
    value = "2023-01-24"
  )
  
  output$nav2_aismap <- renderLeaflet({
    ais_data %>% 
      mutate(
        cntnt=paste0('<strong>Name: </strong>',VesselName,
                     '<br><strong>MMSI:</strong>', MMSI,
                     '<br><strong>Last Observed:</strong>', BaseDateTime,
                     '<br><strong>SOG:</strong>', SOG,
                     '<br><strong>COG:</strong>', COG,
                     '<br><strong>Heading:</strong>', Heading,
                     '<br><strong>IMO:</strong>', IMO,
                     '<br><strong>Call Sign:</strong>', CallSign,
                     '<br><strong>Vessel Type:</strong>', VesselType,
                     '<br><strong>Status:</strong>', Status,
                     '<br><strong>Cargo:</strong>', Cargo,
                     '<br><strong>Transceiver Class:</strong>', TransceiverClass)
      ) %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == max(BaseDateTime)) %>% 
      ungroup() %>%
      leaflet() %>% 
      addTiles() %>%
      addCircleMarkers(
        lng = ~LON, 
        lat = ~LAT, 
        clusterOptions = markerClusterOptions(),
        popup = ~cntnt
      )
  })
  
  output$nav2_total_alert_output <- renderEcharts4r({
    maritime_sec %>%
      filter(
        timestamp >= input$nav2_datetimepicker_input_1,
        timestamp <= input$nav2_datetimepicker_input_2
      ) %>% 
      group_by(jenis_alert) %>% 
      count(name = "Total") %>% 
      ungroup() %>% 
      arrange(Total) %>% 
      e_charts(jenis_alert) %>% 
      e_bar(Total) %>% 
      e_flip_coords() %>% 
      e_tooltip("item")
  })
  
  output$nav2_total_ship_types <- renderEcharts4r({
    maritime_sec %>%
      filter(
        timestamp >= input$nav2_datetimepicker_input_1,
        timestamp <= input$nav2_datetimepicker_input_2
      ) %>% 
      group_by(ais_type_summary) %>% 
      count(name = "Total") %>% 
      ungroup() %>% 
      arrange(Total) %>% 
      e_charts(ais_type_summary) %>% 
      e_bar(Total) %>% 
      e_flip_coords() %>% 
      e_tooltip("item")
  })
  
  output$nav2_hourly_activity <- renderEcharts4r({
    maritime_sec %>% 
      filter(
        timestamp >= input$nav2_datetimepicker_input_1,
        timestamp <= input$nav2_datetimepicker_input_2
      ) %>% 
      mutate(
        hourly = lubridate::hour(timestamp), 
        day = lubridate::day(timestamp),
        month = lubridate::month(timestamp),
        year = lubridate::year(timestamp)
      ) %>% 
      select(ship_id, timestamp, day, month, year, hourly) %>% 
      arrange(timestamp) %>% 
      mutate(hourly_dt = lubridate::make_datetime(year = year, month = month, day = day, hour = hourly)) %>% 
      group_by(hourly_dt) %>%
      mutate(hourly_dt = as.character(hourly_dt)) %>%
      summarise(
        activity_count = n()
      ) %>% 
      ungroup() %>% 
      e_charts(hourly_dt) %>% 
      e_line(activity_count) %>% 
      e_tooltip("axis")
  })
  
  str(maritime_sec)
  ## Vessel Type Distribution
  output$nav2_total_ship_types <- renderEcharts4r({
    maritime_sec %>%
      # filter(
      #         timestamp >= input$nav2_datetimepicker_input_1,
      #         timestamp <= input$nav2_datetimepicker_input_2
      #       ) %>%
      # raw_data %>%
      group_by(ais_type_summary) %>% 
      summarise(Total_Kapal = n_distinct(mmsi))%>%
      arrange(Total_Kapal) %>%
      # ungroup() %>% 
      # arrange(Total) %>% 
      e_charts(ais_type_summary) %>% 
      e_polar() %>%
      e_angle_axis() %>%
      e_radius_axis(ais_type_summary) %>%
      e_bar(Total_Kapal, coord_system = "polar") %>% 
      e_tooltip("axis")
  })
  
  ## Transceiver Class Distribution
  output$nav2_transceiver_class_activity <- renderEcharts4r({
    raw_data %>%
      count(TransceiverClass) %>% 
      e_charts(TransceiverClass) %>% 
      e_pie(n) %>%
      e_tooltip("axis")
  })
  
  #UPDATE TIME EVERY 1SECOND
  timer <- reactiveTimer(1000)
  get_system_time <-function(){
    format(Sys.time(), "%H:%M:%S")
  }
  output$time <- renderValueBox({
    timer()
    valueBox(
      get_system_time(),
      "Current Time", icon = icon("clock"),color = "green"
    )
  })
  #DATA KAPAL YANG MELANGGAR
  ship_violate <-reactive({
    req(raw_data_filter() %>%  filter(is_anomaly == TRUE))
    raw_data_filter() %>%  filter(is_anomaly == TRUE) %>% 
      # arrange dataset based on BaseDateTime ascendingly
      arrange(BaseDateTime) %>% 
      # get LON, LAT, and BaseDateTime column
      select(MMSI,LON, LAT, BaseDateTime, VesselType,VesselName,SOG,COG,Heading,IMO,CallSign,Status,Length,Width,Draft,Cargo,TransceiverClass)
  })
  output$table_profile<- DT::renderDataTable({
    DT::datatable(ship_violate()%>%
                    distinct_all())
  })
  ev <- eventReactive(input$Filter,{
    ship_violate() %>% 
      filter(MMSI == input$text) %>% 
      arrange(BaseDateTime)# %>% 
    #select(MMSI,LON, LAT, BaseDateTime, VesselName, VesselType)
  })
  output$data_kapal <- renderUI({
    tagList(
      p(paste("MMSSI", ev()$MMSI)),
      p(paste("VesselName", ev()$VesselName)),
      p(paste("VesselType", ev()$VesselType)),
      p(paste("SOG", ev()$SOG)),
      p(paste("COG", ev()$COG)),
      p(paste("Status", ev()$Status)),
    )
  })
  output$nav2_aismap <- renderLeaflet({
    ais_data %>% 
      mutate(
        cntnt=paste0('<strong>Name: </strong>',VesselName,
                     '<br><strong>MMSI:</strong>', MMSI,
                     '<br><strong>Last Observed:</strong>', BaseDateTime,
                     '<br><strong>SOG:</strong>', SOG,
                     '<br><strong>COG:</strong>', COG,
                     '<br><strong>Heading:</strong>', Heading,
                     '<br><strong>IMO:</strong>', IMO,
                     '<br><strong>Call Sign:</strong>', CallSign,
                     '<br><strong>Vessel Type:</strong>', VesselType,
                     '<br><strong>Status:</strong>', Status,
                     '<br><strong>Cargo:</strong>', Cargo,
                     '<br><strong>Transceiver Class:</strong>', TransceiverClass)
      ) %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == max(BaseDateTime)) %>% 
      ungroup() %>%
      leaflet() %>% 
      addTiles() %>%
      addCircleMarkers(
        lng = ~LON, 
        lat = ~LAT, 
        clusterOptions = markerClusterOptions(),
        popup = ~cntnt)
  })
  ev2<-eventReactive(input$Filter,{
    raw_data %>% 
      filter(MMSI==input$text)
  })
  output$peta_kecepatan <- renderLeaflet({
    ev2()%>% 
      arrange(BaseDateTime) %>% 
      select(MMSI, LON, LAT, BaseDateTime) %>% 
      mutate(
        lag_LON = lag(LON), lag_LAT = lag(LAT), lag_datetime = lag(BaseDateTime)
      ) %>% 
      rowwise() %>% 
      mutate(
        time_diff = BaseDateTime - lag_datetime,
        distance = distHaversine(c(lag_LON, lag_LAT), c(LON, LAT))
      ) %>% 
      ungroup() %>% 
      mutate(
        speed = distance / as.numeric(time_diff)
      ) %>% 
      leaflet() %>% 
      addTiles() %>% 
      leaflet.extras2::addArrowhead(
        lng = ~LON,
        lat = ~LAT,
        label = ~paste0("speed: ", round(speed, 2))
      )
  })
  
}


shinyApp(ui, server, enableBookmarking = "url")