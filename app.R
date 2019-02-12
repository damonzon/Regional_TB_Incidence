# Tuberculosis Incidence by Region in 2016
library(shiny)
library(shinythemes)
library(data.table)
library(plotly)

regions<- fread("https://raw.githubusercontent.com/damonzon/WHO_TB_Burden/master/tb_regions.csv")
all_regions <- data.frame(unique(regions$region_code))
colnames(all_regions) <- "region"
all_regions <- arrange(all_regions,region)
all_regions <- all_regions[-c(23), ] 
all_regions <- data.frame(regions=all_regions)

ui <- navbarPage(
  title = "Tuberculosis: Kifua kikuu",
  theme = shinytheme("darkly"),
  
  navbarMenu(
    "Videos",
    tabPanel(title = "Epidemiology",
             htmlOutput("video_1e")),
    tabPanel(title = "Pathogenesis",
             htmlOutput("video_1p")),
    tabPanel(title = "Risk Factors",
             htmlOutput("video_1rf"))
  ),
  
  navbarMenu(
    "Trends",
    tabPanel(
      title = "Barplots",
      uiOutput("Choose_region"),
      plotlyOutput("plot1", height = 600)
     
    )
  )
)

server = function(input, output, session) {
  
  output$Choose_region <- renderUI({
    selectInput("select",
                "Select a Region",
                choices = all_regions,
                selected = all_regions[1])
  })
  
  get_data <- reactive({
    region_selected = input$select
    region_data = subset(regions, 
            year == 2016 &region_code == region_selected)
    region_data  = region_data[, c("Country", "population", "incidence")]
    return(region_data)
  })
  
   output$video_1e <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                src = "https://www.youtube.com/embed/FcCk-GlZIC0")
  })
  
  output$video_1p <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                src="https://www.youtube.com/embed/S0Kak0qQFgM")
  })
  
  output$video_1rf <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
      src = "https://www.youtube.com/embed/svvNKlnKtTE")
  })
  
  output$plot1 <- renderPlotly({
    region_data = get_data()
    p <- ggplot(region_data, aes(x=reorder(Country,incidence), 
         y=incidence,country=Country, pop=population,
         Incidence=incidence)) +
      geom_bar(stat='identity', color="black", fill="blue") +
      coord_flip() +
      ggtitle("Population in Millions: Incidence = Cases per 100,000") +
      xlab("") +
      ylab("Tuberculosis Incidence in 2016\nWHO Data") +
      theme_bw() 
    ggplotly(p,tooltip = c("country","pop","Incidence"))
    
  })
}

shinyApp(ui = ui, server = server)
