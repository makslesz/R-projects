library(shiny)
library(data.table)
library(googleVis)
library(plotly)
library(dplyr)
library(tidyr)

server <- shinyServer(function(input, output) {
  
  observeEvent(input$selectKraj,{
    outVar$selectKrajVar <- input$selectKraj
  })
  
  # wybór roku
  outVar <- reactiveValues(
    selectYearVar = "2022")
  observeEvent(input$selectYear,{
    outVar$selectYearVar <- input$selectYear})
  
  # wybór płci
  outVar <- reactiveValues(
    selectPlecVar = "T")
  observeEvent(input$selectPlec,{
    if(input$selectPlec == "Mężczyzna"){
      outVar$selectPlecVar <- "M"}
    else if(input$selectPlec == "Kobieta"){
      outVar$selectPlecVar <- "F"}
    else if(input$selectPlec == "Razem"){
      outVar$selectPlecVar <- "T"}
  }) 
  
  # pobieranie danych
  v <- reactiveValues(dataLoadDownload = FALSE)
  observeEvent(input$getDataFromServer,{
    v$dataLoadDownload <- !v$dataLoadDownload
  })
  
  # ramka  danych  
  dataIn <- reactive({
    if(v$dataLoadDownload==TRUE){
      retData <- (
        read.table(file="https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/DEMO_R_MWK_TS/?format=SDMX-CSV&i",sep=",",dec=".",header=T,stringsAsFactors=F)
      )
      
      retData <- retData[c("geo", "sex", "TIME_PERIOD", "OBS_VALUE")]
      colnames(retData) <- c("KRAJ", "PŁEĆ", "TYDZIEŃ", "LICZBA")
      retData <- retData %>% separate(TYDZIEŃ, c("ROK", "TYDZIEŃ"), sep = "-W")
      retData <- retData[as.character(retData$ROK)==as.character(outVar$selectYearVar),]
      retData <- retData[as.character(retData$PŁEĆ)==as.character(outVar$selectPlecVar),]
      retData <- retData[as.character(retData$KRAJ)%in%as.character(outVar$selectKrajVar),]
      return(retData[c("KRAJ", "PŁEĆ", "TYDZIEŃ", "LICZBA")])
    }
    return(data.frame())
  })
  
  
  # wypisywanie danych 
  output$daneSample <- renderTable({
    tmpData <- dataIn()
    return(tmpData)
  },include.rownames=FALSE)
  
  
  # mapka
  output$view <- renderGvis({
    
    data <- dataIn()
    data_sum <- data %>% group_by(KRAJ) %>% summarise(LICZBA=sum(LICZBA))
    
    gvisGeoChart(data_sum, "KRAJ", "LICZBA",
                 options=list(region="150",
                              displayMode="regions",
                              width=850, height=850))
  })
  
  # plotly
  output$plot <- renderPlotly({
    
    img <-(
      ggplot(data=dataIn(),aes(x=as.integer(factor(TYDZIEŃ)),y=LICZBA))
      + 
        geom_line()
      + 
        facet_wrap(~KRAJ,ncol=1,scales="free_y")
      +
        xlab("Tydzień")
      +
        ylab("Liczba zgonów"))
    
    ggplotly(img, height=800)
    
  })
  
  
})


ui <- shinyUI(fluidPage(
  
  titlePanel("PiWD/Projekt/123301"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "selectKraj",
        "Kraje",
        choices = c("AD","AL","AM","AT","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","GE","HR","HU","IE","IS","IT","LI","LT","LU","LV","ME","MT","NL","NO","PL","PT","RO","RS","SE","SI","SK","UK"),
        selected = c("AD","AL","AM","AT","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","GE","HR","HU","IE","IS","IT","LI","LT","LU","LV","ME","MT","NL","NO","PL","PT","RO","RS","SE","SI","SK","UK"),
        multiple = TRUE),
      
      selectInput("selectYear",
                  label = "Rok danych",
                  choices = as.vector(as.character(2022:2000),mode="list")),
      
      selectInput("selectPlec",
                  label = "Płeć",
                  choices = as.vector(c("Razem", "Mężczyzna", "Kobieta"),mode="list")),                  
      
      actionButton("getDataFromServer", "Pobierz dane")
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Dane", tableOutput("daneSample")),
                  tabPanel("Mapka",htmlOutput("view")),
                  tabPanel("Wykresy",plotlyOutput("plot"))
      )
    )
    
    
  )
  
))



print(shinyApp(ui = ui, server = server))
