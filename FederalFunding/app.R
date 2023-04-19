library(tidyverse)
library(shiny)
library(DT)
library(bslib)
library(plotly)
library(crosstalk)
library(ggsci)
library(RColorBrewer)
library(usethis)
fed <- read_csv("./data/FederalFunding.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("United States Federal Spending"),
    fluidRow(
      column(4, selectizeInput("agency", "Choose up to 3 Agencies:", unique(fed$AgencyName), 
                            multiple = TRUE, 
                            selected = "Department of Education",
                            options = list(maxItems = 3))),
      column(6, checkboxGroupInput("measure", "Choose a Measure:", 
                             choices = list("Gross Cost" = "Gross Cost", 
                                            "Earned Revenue" = "Earned Revenue",
                                            "Subtotal" = "Subtotal",
                                            "Net Cost" = "Net Cost"),
                             selected = "Gross Cost", inline= T))),
    fixedRow(column(6, plotlyOutput("scatter1")),
             column(6, dataTableOutput("DT")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  user_fed2 <- reactive({
    fed %>%
      filter(AgencyName %in% c(input$agency),
             RestatementFlag == "Y", 
             Measures == input$measure) %>%
      select(-c(RestatementFlag, RecordDate))
  })
  
  shared_fed <- SharedData$new(user_fed2)
  
  output$scatter1 <- renderPlotly({
    plot_ly(data = shared_fed, 
            x =~FiscalYear, 
            y = ~`Amount (in Billions)`,
            color = ~AgencyName,
            colors = "Set1", 
            mode = "lines+markers", 
            name = ~AgencyName, 
            linetype = ~Measures) %>%
      layout(hovermode = "x unified", 
             xaxis = list(title = "Fiscal Year"),
             yaxis = list(title = "Amount in Billions"), 
             height = 500) %>%
      add_trace(type = 'scatter',
                mode = 'lines+markers',
                x = ~FiscalYear,
                y = ~`Amount (in Billions)`,
                text = ~Measures,
                hovertemplate = paste('$%{y:.2f} Billion<br>%{text}'),
                showlegend = FALSE
      ) 
  })
  
  
  output$DT <- renderDataTable(datatable(shared_fed, filter="none", 
                                         colnames = c("Agency", "Year", "Measure", "Amount"),
                                         rownames = FALSE, 
                                         #style = "bootstrap",
                                         options = list(pageLength = 10, dom = "tip")), server = FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)
