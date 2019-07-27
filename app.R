#LATEST!


library(shiny)
library(shinyjs)
library("plotly", lib.loc="/usr/lib/R/site-library")
library(ggplot2)
library(RColorBrewer)
library(lubridate)

newData <- function() {
  rbind(base,data)
}

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

#setwd("~/Desktop/shinyFormLatest/data")
#setwd("/srv/shiny-server/appdata")


# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  # Application title
  titlePanel("Reenoel Restaurant Ratings"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Chart controls", div(
          id = "form2",
          selectInput("metric", "Choose Reenoel metric",
                      choices= c('Score','Food','Atmosphere','Character','Service')),
          selectInput("color_change", "Colour by:",
                      choices= c('Restaurant','Meal','Month'))
        )
        ),
        tabPanel("Data Input", div(
          id = "form",
          textInput("Restaurant", labelMandatory("Restaurant"), ""),
          dateInput("Date", label = "Date Visited", value = Sys.Date(), format = "dd MM yyyy"),
          selectInput("Meal", "Time of Day", c("",  "Breakfast", "Lunch", "Dinner")),
          sliderInput("Food", "Food", min = 0, max = 10, value = 5, step = 0.5),
          sliderInput("Atmosphere", "Atmosphere", min = 0, max = 10, value = 5, step = 0.5),
          sliderInput("Character", "Character", min = 0, max = 10, value = 5, step = 0.5),
          sliderInput("Service", "Service", min = 0, max = 10, value = 5, step = 0.5),
          actionButton("submit", "Submit", class = "btn-primary")
        ))
      )),
    # Show a plot of the generated distribution
    mainPanel(
    tabsetPanel(
      tabPanel("Top scorers",
               h3(textOutput("selected_var")),
               plotlyOutput("barPlot")
      ),
      
      tabPanel("Data Table", tableOutput('summary'))
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$table <- renderTable(restaurants, colnames = TRUE)
  data <- eventReactive(c(input$agent,input$submit), {        
    rbind( data.frame(read.csv("/home/ubuntu/appdata/restaurants.csv",header=FALSE))
           ,cbind(input$Restaurant, format(input$Date, "%d/%m/%Y"), input$Meal, input$Food, input$Atmosphere, input$Character, input$Service
                  , (input$Food * 0.5), (input$Atmosphere * 0.2), (input$Character * 0.2), (input$Service * 0.1)
                  , ((input$Food * 0.5)+(input$Atmosphere * 0.2)+(input$Character * 0.2)+(input$Service * 0.1))
                  , (input$Atmosphere * 0.2)+(input$Character * 0.2)+(input$Service * 0.1)
                  , format(input$Date, "%b - %Y")))
  })
  
  
  #write data back to folder
  observeEvent(input$submit, {
    write.table(data(), file="/home/ubuntu/appdata/restaurants.csv", sep=",", col.names= FALSE, row.names=F)
  })
  
  
  #Create datatable to be displayed
  
  datatable<-eventReactive(c(input$agent,input$submit), {         
    data.frame(read.table("/home/ubuntu/appdata/restaurants.csv",sep=",",header=T))
  })
  
  
  #Create table
  output$summary <- renderTable({ datatable() }, digits=2,align = "c" )
  
  #Reset form
  observeEvent(input$submit, {
    reset("form")
  })
  
  
  #some text
  output$selected_var <- renderText({
    paste("Reenoel Metric: ",input$metric)
  })
  
  #Create bar chart
  output$barPlot<-renderPlotly({
    plot_ly(({ datatable() }),
            x = ({ datatable() })[,input$metric],
            y = ~reorder(({ datatable() })$Restaurant
                         , ({ datatable() })[,input$metric]),
            type = 'bar', orientation = 'h',
            hoverinfo = "text",
            text = (paste(({ datatable() })$Restaurant, "<br>",
                          paste(input$metric,"= ", ({ datatable() })[,input$metric]), "<br>",
                          "Date: ", ({ datatable() })$Date, "<br>",
                          "Time: ", ({ datatable() })$Meal)),
            color = ({ datatable() })[,input$color_change]
            #({ datatable() })[,input$color_change]
    ) %>%
      layout(margin = list(l = 180, r = 50, b = 90, t = 30, pad = 4),
             width = 800, height = 620,
             yaxis = list(title = "",showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = TRUE),
             showlegend= FALSE,
             paper_bgcolor = 'rgb(248, 248, 255)',
             plot_bgcolor = 'rgb(248, 248, 255)') %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = ({ datatable() })[,input$metric] + 0.5,
                      text = round(({ datatable() })[,input$metric], 1),
                      font = list(family = 'Arial', size = 15),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'paper', yref = 'paper',
                      x = -0.20, y = -0.07,
                      text = paste('Created to celebrate the 34th birthday of Reena Chieng'),
                      font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                      showarrow = FALSE)
  })
}


# Run the application
shinyApp(ui = ui, server = server)

