library(ggvis)
library(dplyr)
library(shiny)
library(shinydashboard)
library(magrittr)
library(data.table)

ui <- dashboardPage(
  
  dashboardHeader(
    title = 'Finance Planner'
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
  tabsetPanel(
    
    tabPanel("Financial Overview",
             
             h3('Overview of all Income'),
             ggvisOutput('income'),
             
             h3("Overview of all Expenditure"),
             ggvisOutput('expenditure'),
             
             h3("Net Results"),
             ggvisOutput('net_month'),
             
             h3("Savings Increase or Decrease"),
             ggvisOutput('savings')
             ),
    
    
    # Place widgets next to each other: SO 20637248
    
    tabPanel("Data",
             
             h3("Data Source"),
             
             div(style="display:inline-block", 
                 textInput("data_source",
                           NULL,
                           width = '500px')
                 ),
             
             div(style="display:inline-block", 
                 actionButton("load", 
                             "Load data from file",
                             width = '150')),
             
             h3("Data Destination"),
             
             div(style="display:inline-block", 
                 textInput("data_save", 
                           NULL,
                           width = '500px')),
             
             div(style="display:inline-block", 
                 actionButton("save", 
                             "Save data to file",
                             width = '150')),
             
             h3("Enter New Data"),
             
             selectInput("month_entry",
                         "Months",
                         choices  = month.name,
                         multiple = TRUE,
                         width = '700'),
             
             div(style="display:inline-block", 
                 numericInput('amount_entry', 
                          'Amount',
                          value = 0,
                          width = '350')),
             
             div(style="display:inline-block",
                 selectInput("in_out_entry",
                             "In or Out",
                             choices  = c("in", 'out'),
                             width = '350')),
             
             div(style="display:inline-block", 
                 textInput("type_entry",
                           "Type",
                           width = '550')),
             
             div(style="display:inline-block", 
                 actionButton("add",
                              "Add to dataset",
                              width = '150')),
             
             h3('Current Data'),
             
             dataTableOutput('finance_data')
             )
  )
  )
)


server <- function(input, output){
  ################
  # Data section #
  ################
  
  # start with an empty object that can be added to or replaced by a load
  values <- reactiveValues()
  values$finance_data <-  data.table(
    month  = factor(c('January', 'January'), levels = month.name), 
    amount = c(0, 0),
    type   = rep('plac', 2),
    in_out = c('in', 'out')
  )
  
  # adding lines to the current data.frame
  observe({
    if(input$add > 0) {
      new_data <- isolate(data.table(
        month  = input$month_entry, 
        amount = input$amount_entry,
        type   = input$type_entry, 
        in_out = input$in_out_entry
      ))
   
      isolate(values$finance_data <- rbind(values$finance_data ,
                                           new_data))
    }
  })
  
  output$finance_data <- renderDataTable({values$finance_data})
  
  # load data from a file into the current data.frame
  observe({
    if(input$load > 0){
      values$finance_data <- read.table(
        input$data_source,
        header           = TRUE,
        stringsAsFactors = FALSE,
        sep              = ';'
      )  %>% mutate(month = factor(month, levels = month.name))
    }
  })
  
  
  # save data to file
  observe({
    if(input$save > 0) {
      write.table(values$finance_data,
                  input$data_save,
                  row.names = FALSE,
                  quote     = FALSE,
                  sep       = ';'
      )
    }
  })
  
  ################
  # Plot section #
  ################
  
  plots_tooltip <- function(x){
    if (is.null(x)) return(NULL)
    
    paste0(x[2] %>% as.character,"<br>",
           x[1] %>% as.character, "<br>",
           (x[4]-x[3]) %>% as.character)}
  
  plots_width  <- 1000
  plots_height <- 200
  
  income_plot <- reactive({
    values$finance_data %>% 
      filter(in_out == 'in') %>%
      tbl_df %>%
      ggvis(x = ~month, y = ~amount, fill = ~as.factor(type)) %>%
      layer_bars() %>% 
      add_tooltip(plots_tooltip, 'hover') %>%
      set_options(height = plots_height, width = plots_width) %>%
      hide_legend('fill') %>% 
      add_axis("x", title = '') %>%
      add_axis("y", title = '')
  })
  
  income_plot %>% bind_shiny("income")
  
  expenditure_plot <- reactive({
    values$finance_data %>% 
      filter(in_out == 'out') %>%
      tbl_df %>%
      ggvis(x = ~month, y = ~amount, fill = ~as.factor(type)) %>%
      layer_bars() %>% 
      add_tooltip(plots_tooltip, 'hover') %>%
      set_options(height = plots_height, width = plots_width) %>%
      hide_legend('fill') %>% 
      add_axis("x", title = '') %>%
      add_axis("y", title = '')
  })
  
  expenditure_plot %>% bind_shiny("expenditure")
  
  net_month_plot <- reactive({
    values$finance_data %>% 
      mutate(amount = ifelse(in_out == 'in', amount, -amount)) %>% 
      group_by(month) %>% 
      summarise(net_result = sum(amount)) %>% 
      ggvis(x = ~month, y = ~net_result) %>% 
      layer_points() %>% 
      set_options(height = plots_height, width = plots_width) %>%
      add_axis("x", title = '') %>%
      add_axis("y", title = '')
    })
  
  net_month_plot %>% bind_shiny("net_month")
  
  savings_plot <- reactive({
    values$finance_data %>% 
      mutate(amount = ifelse(in_out == 'in', amount, -amount)) %>% 
      group_by(month) %>% 
      summarise(net_result = sum(amount)) %>% 
      ungroup %>%
      mutate(savings = net_result %>% cumsum) %>% 
      ggvis(x = ~month, y = ~savings) %>% 
      layer_lines() %>% 
      set_options(height = plots_height, width = plots_width) %>%
      add_axis("x", title = '') %>%
      add_axis("y", title = '')
  })
  
  savings_plot %>% bind_shiny("savings")
    
} # closes the server
  
shinyApp(ui = ui,
         server = server)


