library(shiny)
library(shinythemes)
library(data.table)
library(plotly)
regions<- fread("https://raw.githubusercontent.com/damonzon/Regional_TB_Incidence/master/tb_regions.csv")
all_regions <- data.frame(unique(regions$region_code))
colnames(all_regions) <- "region"
all_regions <- arrange(all_regions,region)
all_regions <- all_regions[-c(23), ] 
all_regions <- data.frame(regions=all_regions)

ui <- navbarPage(
  titlePanel(title=div(img(src="RStudio-Ball.png",
      width="40px",height="40px"),"Worldwide Tuberculosis")),
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
    "Plots",
    tabPanel(
      title = "Barplots: 2016",
      uiOutput("Choose_region"),
      plotlyOutput("plot1", height = 500)),
    
    tabPanel(
      title = "Boxplots: 2016",
      plotlyOutput("plot2", height = 500)),
    
    tabPanel(
      title = "Incidence: 2000-2016",
      plotOutput("plot3")),
    
    tabPanel(
      title = "Population: 2000-2016",
      plotOutput("plot4"))
    
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
  
  get_data2 <- reactive({
  region_selected = input$select
  region_data2 = subset(regions, 
            region_code == region_selected)
  region_data2  = region_data2[, c("Country", "year", "population","incidence")]
  return(region_data2)
  })
  
  output$Choose2_region <- renderUI({
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
      ylab("\nTuberculosis Incidence in 2016") +
      theme_bw() 
    ggplotly(p,tooltip = c("country","pop","Incidence"))
    
  })
  
  output$plot2 <- renderPlotly({
    region_data = get_data()
    p <- ggplot(region_data, aes(x="",y=incidence,
        country=Country,pop=population,
        Incidence=incidence)) + 
      geom_boxplot() +
      ggtitle("Population in Millions: Incidence = Cases per 100,000") +
      xlab(paste(input$select)) +
      ylab("Tuberculosis Incidence in 2016") +
      theme_bw() +
      geom_jitter(shape=16, position=position_jitter(0.1),color="blue")+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    ggplotly(p,tooltip = c("country","pop","Incidence"))
    
  })
 
  output$plot3 <- renderPlot({
    region_data2 = get_data2()
  p <- ggplot(data=region_data2, aes(x=year, y=incidence, Country
         )) +
    geom_line(aes(color = Country)) +
    geom_point(aes(shape = Country)) +
    ggtitle(paste(input$select))+
    xlab("Years 2000-2016") +
    ylab("TB Incidence") +
    theme_bw(base_size = 20)
  p
  }) 
  
  output$plot4 <- renderPlot({
    region_data2 = get_data2()
    p <- ggplot(data=region_data2, aes(x=year, y=population, Country
    )) +
      geom_line(aes(color = Country)) +
      geom_point(aes(shape = Country)) +
      ggtitle(paste(input$select),"Population")+
      xlab("Years 2000-2016") +
      ylab("Millions") +
      theme_bw(base_size = 20)
    p
  
  }) 
  
}

shinyApp(ui = ui, server = server)
