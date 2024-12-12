library(shiny)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(openxlsx)
library(cluster)

ui <- fluidPage(
  titlePanel("Explanatory Data Analysis with Palmer Penguins"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Create New Column"),
      textInput("new_col_name", "New Column Name", ""),
      textInput("new_col_condition", "Condition/Formula for New Column", 
                "Example: ifelse(body_mass_g > 4000, 'Large', 'Small')"),
      actionButton("add_column", "Add New Column"),
      
      h3("Select Visualization"),
      selectInput("plot_type", "Select Plot Type", 
                  choices = c("Box Plot", "Histogram", "Pie Chart", 
                              "Scatter Plot", "Bar Plot", "Stacked Bar Plot", "Line Chart")),
      selectInput("visualization_option", "Select Visualization Type",
                  choices = c("One Variable", "Two Variables")),
      selectInput("x_var", "Select Variable", choices = names(penguins)),
      conditionalPanel(
        condition = "input.visualization_option == 'Two Variables'",
        selectInput("y_var", "Select Y Variable", choices = names(penguins), selected = "")
      ),
      textInput("plot_title", "Plot Title", ""),
      textInput("x_label", "X-Axis Label", ""),
      textInput("y_label", "Y-Axis Label", ""),
      selectInput("color_theme", "Color Theme", 
                  choices = c("Default", "viridis", "Set3", "Dark2")),
      
      h3("Download Options"),
      downloadButton("download_plot", "Download Plot as PNG"),
      downloadButton("download_data", "Download Clean Data as CSV")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset", dataTableOutput("data_table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary_output")),
        tabPanel("Visualizations", plotOutput("data_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(penguins)
  
  # Clean data automatically when the app starts
  observe({
    cleaned_data <- data() %>%
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
    
    numeric_data <- cleaned_data %>% select(where(is.numeric))
    kmeans_result <- kmeans(numeric_data, centers = 3, nstart = 25)
    
    cleaned_data <- cleaned_data %>%
      mutate(across(where(is.character), ~ ifelse(is.na(.), 
                                                  factor(kmeans_result$cluster), 
                                                  .)))
    
    data(cleaned_data)
  })
  
  output$data_table <- renderDataTable({
    data()
  })
  
  observe({
    numeric_summary <- data() %>% select(where(is.numeric)) %>% summary()
    categorical_summary <- data() %>% select(where(is.character)) %>%
      summarise(across(everything(), ~ as.list(table(.))))
    
    output$summary_output <- renderPrint({
      list(Numeric_Summary = numeric_summary, Categorical_Summary = categorical_summary)
    })
  })
  
  observeEvent(input$add_column, {
    new_col_name <- input$new_col_name
    condition <- parse(text = input$new_col_condition)
    
    updated_data <- data() %>% mutate(!!new_col_name := eval(condition))
    data(updated_data)
  })
  
  observeEvent(input$x_var, {
    if (input$visualization_option == "Two Variables") {
      updateSelectInput(session, "y_var", choices = names(penguins), selected = "")
    }
  })
  
  current_plot <- reactive({
    req(input$x_var)
    plot_data <- data()
    p <- NULL
    
    if (input$plot_type == "Box Plot") {
      p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_boxplot() + labs(title = input$plot_title, x = input$x_label, y = input$y_label)
    } else if (input$plot_type == "Histogram") {
      p <- ggplot(plot_data, aes_string(x = input$x_var)) +
        geom_histogram(binwidth = 30, fill = "blue", color = "black") +
        labs(title = input$plot_title, x = input$x_label, y = input$y_label)
    } else if (input$plot_type == "Pie Chart") {
      p <- ggplot(plot_data, aes_string(x = "factor(1)", fill = input$x_var)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(title = input$plot_title, x = input$x_label, y = input$y_label)
    } else if (input$plot_type == "Scatter Plot") {
      req(input$y_var)
      p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() + labs(title = input$plot_title, x = input$x_label, y = input$y_label)
    } else if (input$plot_type == "Bar Plot") {
      p <- ggplot(plot_data, aes_string(x = input$x_var, fill = input$x_var)) +
        geom_bar() + labs(title = input$plot_title, x = input$x_label, y = input$y_label)
    } else if (input$plot_type == "Stacked Bar Plot") {
      p <- ggplot(plot_data, aes_string(x = input$x_var, fill = input$y_var)) +
        geom_bar() + labs(title = input$plot_title, x = input$x_label, y = input$y_label)
    } else if (input$plot_type == "Line Chart") {
      p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var, group = 1)) +
        geom_line() + labs(title = input$plot_title, x = input$x_label, y = input$y_label)
    }
    
    if (input$color_theme == "viridis") {
      p <- p + scale_fill_viridis_d()
    } else if (input$color_theme == "Set3") {
      p <- p + scale_fill_brewer(palette = "Set3")
    } else if (input$color_theme == "Dark2") {
      p <- p + scale_fill_brewer(palette = "Dark2")
    }
    
    return(p)
  })
  
  # Render plot in UI
  output$data_plot <- renderPlot({
    current_plot()
  })
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() { "plot.png" },
    content = function(file) {
      ggsave(file, plot = current_plot())  # Save the current plot
    }
  )
  
  # Download cleaned data as CSV
  output$download_data <- downloadHandler(
    filename = function() { "cleaned_data.csv" },
    content = function(file) {
      write.csv(data(), file)
    }
  )
}

shinyApp(ui, server)
