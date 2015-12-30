library(ggplot2)
library(ggvis)
library(dplyr)
library(shiny)
library(data.table)

tmp <- data.table(
  maand   = factor(month.name %>% rep(2), levels = month.name),
  bedrag  = rnorm(12, 3000, 200) %>% c(rep(100, 12)),
  bron    = rep(c('Salaris', 'Zorgtoeslag'), each = 12)
)


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Financial Overview",
             ggvisOutput('Income')
             ),
    
    tabPanel("Add New Data",
             "hier komt de data invoer"
             ),
    
    tabPanel("Adjust Current Data",
             "hier komt de data verwijdering")
  )
)

server <- function(input, output){
  
  Income_plot <- reactive({
    tmp %>% 
      tbl_df %>%
      ggvis(x = ~maand, y = ~bedrag, fill = ~as.factor(bron)) %>%
      layer_bars()  
  })
  
  Income_plot %>% bind_shiny("Income")
  
}
  
shinyApp(ui = ui,
         server = server)

