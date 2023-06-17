library(shiny)
library(astsa)
library(tidyverse)
library(worldmet)
library(REmap)
library(leaflet)
library(shinyjs)

options(remap.ak = "VcPRIFeGAiDSftG3foncLV3cGAO2bxGs")

ui <- fluidPage(
  titlePanel("NOAA数据导入"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      textInput("city", "输入城市名称", placeholder = "青岛"),
      numericInput("start_year", "起始年份", value = 2023),
      numericInput("end_year", "结束年份", value = 2023),
      actionButton("submit", "导入数据")
    ),
    mainPanel(
      verbatimTextOutput("clock"),
      tabsetPanel(
        tabPanel("地图", leafletOutput("map")),
        tabPanel("数据", 
                 fluidRow(
                   column(width = 8, tableOutput("data_table")),
                   column(width = 4, align = "center", 
                          downloadButton("download", "下载数据")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    disable("submit")
    latlon <- get_city_coord(input$city)
    citylist <- getMeta(lat = latlon[2], lon = latlon[1])
    Select_city <- subset(citylist, dist == min(dist))
    
    data <- importNOAA(code = Select_city$code, year = input$start_year:input$end_year)
    data <- data %>%
      filter(complete.cases(air_temp)) %>%
      select(date, air_temp)
    
    file_name <- paste0(Select_city$station, ".csv")
    file_name <- gsub("/", "_", file_name)
    write.csv(data, file = file_name)
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lat = latlon[2], lng = latlon[1], zoom = 10) %>%
        addMarkers(lat = latlon[2], lng = latlon[1], popup = input$city)
    })
    
    output$data_table <- renderTable({
      data
    })
    
    output$download <- downloadHandler(
      filename = file_name,
      content = function(file) {
        file.copy(file_name, file)
      }
    )
    
    observe({
      if (!is.null(data)) {
        enable("submit")
      }
    })
  })
  
  # 创建一个反应性计时器，每秒触发一次
  clockTimer <- reactiveTimer(1000)
  
  output$clock <- renderPrint({
    clockTimer()  # 每秒触发一次反应式计时器
    currentTime <- Sys.time()  # 获取当前时间
    paste("当前时间：", format(currentTime, "%Y-%m-%d %H:%M:%S"))  # 格式化显示时间
  })
}

shinyApp(ui = ui, server = server)
