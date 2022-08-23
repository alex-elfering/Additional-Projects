library(shiny)

source("C:/Users/alexe/Desktop/new stardew varibales.R")
source("C:/Users/alexe/Desktop/new stardew functions.R")

ui <- fluidPage(
  sidebarLayout(position = "left",
                sidebarPanel(
                  actionButton("go", 
                               "Run code"),
                  selectInput("level", 
                              "Farm Level",
                              1:14),
                  selectInput("fertilizer", 
                              "Fertilizer:",
                              0:3),
                  selectInput("season", 
                              "Season:",
                              choices = ''),
                  uiOutput("spring"),
                  uiOutput("summer"),
                  uiOutput("fall"),
                  h5("Table created by Alex Elfering"),
                  width=2),
                mainPanel()
  )) 

server = shinyServer(function(input, output, session) {
  
  observe({
    updateSelectInput(session, inputId = "season", label = "Season:", choices = c('Spring', 'Summer', 'Fall'))
  })
  output$spring <- renderUI({
    
    if (!input$season == 'Spring') return(NULL) else {
      tagList(sliderInput("blue_jazz", "Blue Jazz:", min = 0, max = 90, value = 10), 
              sliderInput("cauliflower", "Cauliflower:", min = 0, max = 90, value = 10), 
              sliderInput("garlic", "Garlic:", min = 0, max = 90, value = 10), 
              sliderInput("green_bean", "Green Bean:", min = 0, max = 90, value = 10), 
              sliderInput("kale", "Kale:", min = 0, max = 90, value = 10), 
              sliderInput("parsnip", "Parsnip:", min = 0, max = 90, value = 10), 
              sliderInput("potato", "Potato:", min = 0, max = 90, value = 10), 
              sliderInput("tulip", "Tulip:", min = 0, max = 90, value = 10))
    }
    
  })
  output$summer <- renderUI({
    
    if (!input$season == 'Summer') return(NULL) else {
      tagList(sliderInput("blueberry", "Blueberry:", min = 0, max = 90, value = 10), 
              sliderInput("corn", "Corn:", min = 0, max = 90, value = 10), 
              sliderInput("hops", "Hops:", min = 0, max = 90, value = 10), 
              sliderInput("hot_pepper", "Hot Pepper:", min = 0, max = 90, value = 10), 
              sliderInput("melon", "Melon:", min = 0, max = 90, value = 10), 
              sliderInput("poppy", "Poppy:", min = 0, max = 90, value = 10), 
              sliderInput("radish", "Radish:", min = 0, max = 90, value = 10), 
              sliderInput("red_cabbage", "Red Cabbage:", min = 0, max = 90, value = 10), 
              sliderInput("summer_spangle", "Summer Spangle:", min = 0, max = 90, value = 10), 
              sliderInput("sunflower", "Sunflower:", min = 0, max = 90, value = 10), 
              sliderInput("tomato", "Tomato:", min = 0, max = 90, value = 10), 
              sliderInput("wheat", "Wheat:", min = 0, max = 90, value = 10))
    }
    
  })
  output$fall <- renderUI({
    
    if (!input$season == 'Fall') return(NULL) else {
      tagList(sliderInput("amaranth", "Amaranth:", min = 0, max = 90, value = 10), 
              sliderInput("artichoke", "Artichoke:", min = 0, max = 90, value = 10), 
              sliderInput("bok_choy", "Bok Choy:", min = 0, max = 90, value = 10), 
              sliderInput("cranberries", "Cranberries:", min = 0, max = 90, value = 10), 
              sliderInput("eggplant", "Eggplant:", min = 0, max = 90, value = 10), 
              sliderInput("fairy_rose", "Fairy Rose:", min = 0, max = 90, value = 10), 
              sliderInput("grape", "Grape:", min = 0, max = 90, value = 10), 
              sliderInput("pumpkin", "Pumpkin:", min = 0, max = 90, value = 10), 
              sliderInput("yam", "Yam:", min = 0, max = 90, value = 10), 
              sliderInput("wheat", "Wheat:", min = 0, max = 90, value = 10))
    }
    
  })
  
  renderTable(
    
    if (!input$season == 'Spring'){
      
    }
    
  )
  
})

shinyApp(ui,server)