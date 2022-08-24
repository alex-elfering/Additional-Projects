library(shiny)

source("~/GitHub/Additional-Projects/Stardew Valley Crop Return/new stardew variables.R")
source("~/GitHub/Additional-Projects/Stardew Valley Crop Return/new stardew functions.R")

ui <- fluidPage(
  sidebarLayout(position = "left",
                sidebarPanel(
                  actionButton("go", 
                               "Run code"),
                  selectInput("level", 
                              "Farm Level",
                              0:14),
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
                mainPanel(plotOutput("plot", width = "100%"))
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
  
  output$plot <- renderPlot({
    
    farm_levels <- read.csv('~/farm levels and fertilizer.csv') %>%
      filter(fertilizer == input$fertilizer,
             level == input$level) %>%
      select(regular,
             silver,
             gold,
             iridium)
    
    return_crop_printer <- rbindlist(crop_printer(i = select_crops, n = 10000))
    return_crop_quality <- crop_quality(return_crop_printer, farm_level = input$level, fertilizer_level = input$fertilizer)
    return_additional <- additional_crops(return_crop_quality)
    return_crop_revenue <- crop_revenue(return_additional)
    
    total_rev_chart <- return_crop_revenue %>%
      mutate(total_revenue = init_revenue + (additional *add_revenue)) %>%
      group_by(iter) %>%
      summarise(total_revenue = sum(total_revenue)) %>%
      ungroup() %>%
      ggplot(aes(total_revenue)) +
      geom_histogram(color = 'white',
                     fill = 'steelblue') +
      scale_x_continuous(labels = function(x){ paste0(scales::comma(x), 'g') }) +
      scale_y_continuous(labels = scales::comma,
                         expand = c(0, 0)) +
      labs(title = 'Total Revenue Gained from Growing',
           subtitle = 'Based on 10,000 simulations',
           y = '',
           x = 'Revenue (gold)') +
      theme(plot.title = element_text(face = 'bold', size = 16),
            plot.subtitle = element_text(size = 14),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 12),
            plot.title.position = "plot",
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 16, color = '#969696'),
            axis.text.x.bottom = element_text(size = 12, color = 'black'),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", linetype = "solid"),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed")) 
    
    total_rev_chart
    
  }, height = 850)
  
})

shinyApp(ui,server)