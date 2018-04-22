library(shiny)
library(tidyverse)
library(tidycensus)

census_api_key("60d90b082e1b11ec178ffa07d99ca2e6865449ac")

# user interface
ui <- fluidPage(
  titlePanel("EDA of American Community Survey"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # a choice of which state to plot
      selectInput("State", "State",choices = state.abb, selected = "NJ"),
      
      # a choice of plotting median household income/median gross rent/ratio of median gross rent to median household income
      radioButtons("Type", "Type",
                   choices = list("Median Gross Rent","Median Household Income", "Ratio"), 
                   selected = "Median Gross Rent")
    ),
    
    mainPanel(plotOutput("Plot"))
  )
)

# server logic
server <- function(input, output) {
  
  newdat <- reactive(
    {
      get_acs(
        variables = c(median_gross_rent = "B25064_001", median_household_income = "B19013_001"),
        state = input$State,
        geography = "tract",
        geometry = TRUE
      ) %>% .[, -5] %>% data.frame() %>% spread(key=variable, value=estimate) %>% 
        mutate(ratio = median_gross_rent/median_household_income)
    }
  )
  
  output$Plot <- renderPlot(
    {
      newdat() %>% 
        ggplot(aes_string(fill = input$Type)) + geom_sf() + ggtitle(input$Type) + scale_fill_gradientn(colours = terrain.colors(10))
    }
  )
}

shinyApp(ui = ui, server = server)
