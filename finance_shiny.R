library(ggvis)
library(dplyr)
library(shiny)
library(data.table)

ui <- fluidPage(
  tabsetPanel(
    
    tabPanel("Financial Overview",
             ggvisOutput('income'),
             ggvisOutput('expenditure'),
             ggvisOutput('net_month'),
             ggvisOutput('savings')
             ),
    
    tabPanel("Data",
             
             textInput("data_source",
                       label = h3("Data Source")),
             
             actionButton("load",
                          "Load file"),
             
             actionButton("create",
                          "Create empty file"),
             
             actionButton("save",
                          "Save changes to file"),
             
             selectInput("month_entry",
                         "Months",
                         choices  = month.name,
                         multiple = TRUE),
             
             numericInput('amount_entry', 
                          'Amount',
                          value = 0),
             
             textInput("type_entry",
                       "Type"),
             
             selectInput("in_out_entry",
                         "In or Out",
                         choices  = c("in", 'out')
                         ),
             
             actionButton("add",
                          "Add to dataset"),
             
             dataTableOutput('finance_data_init')
             )
  )
)


server <- function(input, output){
  
  ################
  # Plot section #
  ################
  
  # tooltip for the income en expenditure plots
  plots_tooltip <- function(x){
    if (is.null(x)) return(NULL)
    
    paste0(x[2] %>% as.character,"<br>",
           x[1] %>% as.character, "<br>",
           (x[4]-x[3]) %>% as.character)
  }
  
  plots_width  <- 1000
  plots_height <- 200
  
  income_plot <- reactive({
    tmp %>% 
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
    tmp %>% 
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
    tmp %>% 
      mutate(amount = ifelse(in_out == 'in', amount, -amount)) %>% 
      group_by(month) %>% 
      summarise(net_result = sum(amount)) %>% 
      ggvis(x = ~month, y = ~net_result) %>% 
      layer_lines() %>% 
      set_options(height = plots_height, width = plots_width) %>%
      add_axis("x", title = '') %>%
      add_axis("y", title = '')
    })
  
  net_month_plot %>% bind_shiny("net_month")
  
  savings_plot <- reactive({
    tmp %>% 
      mutate(amount = ifelse(in_out == 'in', amount, -amount)) %>% 
      group_by(month) %>% 
      summarise(net_result = sum(amount)) %>% 
      ungroup %>%
      mutate(savings = net_result %>% cumsum) %>% 
      ggvis(x = ~month, y = ~0, y2 = ~savings) %>% 
      layer_ribbons() %>% 
      set_options(height = plots_height, width = plots_width) %>%
      add_axis("x", title = '') %>%
      add_axis("y", title = '')
  })
  
  savings_plot %>% bind_shiny("savings")
  
  ################
  # Data section #
  ################
  
  # Load an existing file
  finance_data_load <- eventReactive(input$load,{
    read.table(input$data_source,
               header = TRUE)
  })
  
  output$finance_data <- 
    renderDataTable({
      finance_data_load()
      })
  
  # create a new file
  finance_data_create <- eventReactive(input$create,{
    data.table(
      month  = factor(levels = month.name), 
      amount = numeric(),
      type   = character(), 
      in_out = character()
    )
  })

  observeEvent(input$create, {
    write.table(finance_data_create(), 
                input$data_source,
                quote = FALSE)
  })
  
  # add new lines to the data set
  
  finance_data_add <- eventReactive(input$add,{
    data.table(
      month  = input$month_entry,
      amount = input$amount_entry,
      type   = input$type_entry,
      in_out - input$in_out_entry
    )
    
    output$finance_data <- 
      renderDataTable({
        finance_data_load()
      })
    
    observeEvent(input$add, {
      
    })
    
  })
}
  
shinyApp(ui = ui,
         server = server)

