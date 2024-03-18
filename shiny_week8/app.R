library(shiny)
library(ggplot2)

week8_tbl <- readRDS("../shiny_week8/week8_tbl.rds") #load data

# Define UI
ui <- fluidPage(
  titlePanel("The relationship between Q1-Q6 Mean and Q8-Q10 Mean"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      selectInput("error_band", "Error Band:",
                  choices = c("Display Error Band", "Suppress Error Band"),
                  selected = "Display Error Band"),
      selectInput("date_filter", "Date Filter:",
                  choices = c("Include before July 1, 2017", "Exclude before July 1, 2017"),
                  selected = "Include before July 1, 2017")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server<-function(input, output){
  output$plot<-renderPlot({
    if (input$gender != "All") {
      week8_tbl <-week8_tbl[ week8_tbl$gender == input$gender, ] #if gender selected other than all, the data will filter accordingly
    }
    if (input$date_filter == "Include before July 1, 2017") {
      week8_tbl <-week8_tbl[week8_tbl$date < as.POSIXct("2017-07-01"), ]
    } else {
      week8_tbl <-week8_tbl[week8_tbl$date >= as.POSIXct("2017-07-01"), ]
    }
    if(input$error_band == "Display Error Band") {
      week8_tbl %>% 
      ggplot(aes(x=mean_q1_6, y=mean_q8_q10))+
        geom_point()+
        geom_smooth(method="lm", color="purple", se = ifelse(input$error_band == "Display Error Band", TRUE, FALSE))+
        labs(x = "Q1-Q6 Mean", y = "Q8-Q10 Mean")
    }
}
)
}

# Run the application
shinyApp(ui = ui, server= server)
