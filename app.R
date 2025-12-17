library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

ui <- fluidPage(
  
  titlePanel("Cluster Sampling – Population Mean Estimation"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("N", "Population Size (N):", value = 300, min = 50),
      numericInput("C", "Total Number of Clusters:", value = 10, min = 2),
      numericInput("B", "Allowable Error / Sampling Bias (B):", value = 4, min = 1),
      
      selectInput("Z", "Confidence Level:",
                  choices = c("90%" = 1.645,
                              "95%" = 1.96,
                              "99%" = 2.576),
                  selected = 1.96),
      
      numericInput("cost", "Cost per Cluster:", value = 100, min = 1),
      numericInput("time", "Time per Cluster (hours):", value = 2, min = 0.1),
      
      actionButton("calc", "Generate Cluster Design")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Cluster Information",
                 DTOutput("cluster_table")
        ),
        
        tabPanel("Sampling Summary",
                 tableOutput("summary")
        ),
        
        tabPanel("Design of Experiment",
                 plotOutput("cluster_plot", height = "450px")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$calc, {
    
    set.seed(123)
    
    # POPULATION
    units_per_cluster <- ceiling(input$N / input$C)
    
    population <- data.frame(
      Unit = 1:input$N,
      y = rnorm(input$N, mean = 60, sd = 15),
      Cluster = rep(paste0("C", 1:input$C), each = units_per_cluster)[1:input$N],
      Cost = rep(input$cost, input$N),
      Time = rep(input$time, input$N)
    )
    
    # Cluster-level summaries
    cluster_info <- population %>%
      group_by(Cluster) %>%
      summarise(
        Cluster_Size = n(),
        Mean = mean(y),
        Variance = var(y),
        Cost = first(Cost),
        Time = first(Time),
        .groups = "drop"
      )
    
    M <- nrow(cluster_info)           # total clusters
    S2 <- var(cluster_info$Mean)      # between-cluster variance
    Z <- as.numeric(input$Z)
    B <- input$B
    
    # REQUIRED NUMBER OF CLUSTERS
    m <- ceiling((Z^2 * S2) / (B^2))
    m <- min(m, M)
    
    # Select clusters (SRS of clusters)
    selected_clusters <- sample(cluster_info$Cluster, m)
    
    population$Selected <- ifelse(
      population$Cluster %in% selected_clusters,
      "Selected", "Not Selected"
    )
    
    # COST & TIME CALCULATION
    cost_time <- cluster_info %>%
      filter(Cluster %in% selected_clusters) %>%
      summarise(
        Total_Clusters_Selected = m,
        Expected_Cost = sum(Cost),
        Expected_Time = sum(Time)
      )
    
    # OUTPUTS
    output$cluster_table <- renderDT({
      datatable(cluster_info, options = list(pageLength = 10))
    })
    
    output$summary <- renderTable({
      data.frame(
        Parameter = c("Total Clusters",
                      "Clusters Selected",
                      "Allowable Error (B)",
                      "Confidence Level (Z)",
                      "Expected Cost",
                      "Expected Time"),
        Value = c(M,
                  m,
                  B,
                  Z,
                  cost_time$Expected_Cost,
                  cost_time$Expected_Time)
      )
    })
    
    output$cluster_plot <- renderPlot({
      ggplot(population,
             aes(x = Unit, y = y, color = Selected)) +
        geom_point(size = 2) +
        facet_wrap(~Cluster, scales = "free_x") +
        scale_color_manual(
          values = c("Selected" = "#1F78B4",
                     "Not Selected" = "#D9D9D9")
        ) +
        labs(
          title = "Cluster Sampling Design",
          subtitle = paste("Selected Clusters:", paste(selected_clusters, collapse = ", ")),
          x = "Population Units",
          y = "Observation Value",
          color = "Cluster Status"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_text(face = "bold")
        )
    })
    
  })
  
}

shinyApp(ui, server)
