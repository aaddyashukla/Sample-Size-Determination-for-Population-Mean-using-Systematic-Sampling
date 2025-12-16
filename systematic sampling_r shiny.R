library(shiny)
library(ggplot2)
library(dplyr)

# UI Definition
ui <- fluidPage(
  titlePanel("Optimal Sample Size Calculator: Cost, Time & Bias Adjusted"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Global Parameters"),
      numericInput("target_mse", "Target Mean Squared Error (MSE):", value = 0.5, min = 0.0001),
      numericInput("bias", "Estimated Sampling Bias (B):", value = 0.1, min = 0),
      numericInput("fixed_cost", "Fixed Overhead Cost:", value = 1000),
      
      tags$hr(),
      h4("Subgroup (Strata) Configuration"),
      helpText("Define parameters for up to 3 subgroups for simulation."),
      
      # Subgroup 1
      h5("Subgroup 1"),
      splitLayout(
        numericInput("N1", "Pop Size (N1)", 1000),
        numericInput("S1", "Std Dev (S1)", 10),
        numericInput("C1", "Cost/Unit (C1)", 20),
        numericInput("T1", "Time/Unit (T1)", 5)
      ),
      
      # Subgroup 2
      h5("Subgroup 2"),
      splitLayout(
        numericInput("N2", "Pop Size (N2)", 2000),
        numericInput("S2", "Std Dev (S2)", 15),
        numericInput("C2", "Cost/Unit (C2)", 15),
        numericInput("T2", "Time/Unit (T2)", 4)
      ),
      
      # Subgroup 3
      h5("Subgroup 3"),
      splitLayout(
        numericInput("N3", "Pop Size (N3)", 1500),
        numericInput("S3", "Std Dev (S3)", 12),
        numericInput("C3", "Cost/Unit (C3)", 25),
        numericInput("T3", "Time/Unit (T3)", 6)
      ),
      
      actionButton("calc", "Calculate Optimal Design", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results & Design Plot",
                 br(),
                 verbatimTextOutput("summary_text"),
                 br(),
                 plotOutput("allocation_plot"),
                 br(),
                 h4("Cost & Time Analysis"),
                 tableOutput("resource_table")
        ),
        tabPanel("Methodology (White Paper)",
                 uiOutput("whitepaper_view")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  observeEvent(input$calc, {
    
    # 1. Consolidate Input Data
    strata_data <- data.frame(
      Subgroup = c("Group 1", "Group 2", "Group 3"),
      Nh = c(input$N1, input$N2, input$N3),
      Sh = c(input$S1, input$S2, input$S3),
      Ch = c(input$C1, input$C2, input$C3),
      Th = c(input$T1, input$T2, input$T3)
    )
    
    # 2. Adjust Target Variance for Bias
    # MSE = Variance + Bias^2  =>  Target Variance = MSE - Bias^2
    target_var <- input$target_mse - (input$bias^2)
    
    output$summary_text <- renderPrint({
      if(target_var <= 0) {
        cat("ERROR: Bias squared is greater than or equal to Target MSE.\n")
        cat("Impossible to achieve target MSE. Reduce Bias or increase Target MSE.")
      } else {
        
        # 3. Calculate Lagrange Multiplier components for Optimum Allocation
        # Formula: nh varies with (Nh * Sh) / sqrt(Ch)
        
        # Calculate Numerator Sum: Sum(Nh * Sh * sqrt(Ch))
        num_sum <- sum(strata_data$Nh * strata_data$Sh * sqrt(strata_data$Ch))
        
        # Calculate Denominator Sum: Sum(Nh * Sh / sqrt(Ch))
        den_sum <- sum(strata_data$Nh * strata_data$Sh / sqrt(strata_data$Ch))
        
        # Total Sample Size n (for Min Cost given fixed Variance V)
        # V_desired = (1/N^2) * Sum(Nh^2 * Sh^2 / nh) - (1/N)Sum(Nh * Sh^2) [Ignoring FPC for simplicity or assuming N is large]
        # Simplified optimum n formula:
        # n = (Sum(Nh * Sh * sqrt(Ch)) * Sum(Nh * Sh / sqrt(Ch))) / (N^2 * Target_Var + Sum(Nh * Sh^2)) 
        
        N <- sum(strata_data$Nh)
        numerator <- sum(strata_data$Nh * strata_data$Sh / sqrt(strata_data$Ch)) * sum(strata_data$Nh * strata_data$Sh * sqrt(strata_data$Ch))
        # Note: Using absolute variance target V (not standard error)
        denominator <- (N^2 * target_var) + sum(strata_data$Nh * strata_data$Sh^2)
        
        n_total <- numerator / denominator
        
        # Allocation per stratum
        strata_data$nh <- n_total * ( (strata_data$Nh * strata_data$Sh / sqrt(strata_data$Ch)) / sum(strata_data$Nh * strata_data$Sh / sqrt(strata_data$Ch)) )
        strata_data$nh <- ceiling(strata_data$nh) # Round up
        
        # Calculate Resources
        strata_data$Total_Cost <- strata_data$nh * strata_data$Ch
        strata_data$Total_Time <- strata_data$nh * strata_data$Th
        
        total_project_cost <- sum(strata_data$Total_Cost) + input$fixed_cost
        total_project_time <- sum(strata_data$Total_Time)
        
        cat("--- OPTIMAL DESIGN SUMMARY ---\n")
        cat("Total Required Sample Size (n):", sum(strata_data$nh), "\n")
        cat("Adjusted Target Variance (V):", round(target_var, 5), "\n")
        cat("Estimated Total Cost:", total_project_cost, "\n")
        cat("Estimated Total Man-Hours:", total_project_time, "\n")
        
        # Store data for plotting
        output$allocation_plot <- renderPlot({
          ggplot(strata_data, aes(x=Subgroup, y=nh, fill=Subgroup)) +
            geom_bar(stat="identity", width=0.5) +
            geom_text(aes(label=nh), vjust=-0.5) +
            labs(title = "Optimal Sample Allocation per Subgroup",
                 subtitle = paste("Based on Cost Optimization. Total n =", sum(strata_data$nh)),
                 y = "Sample Size (n)") +
            theme_minimal()
        })
        
        output$resource_table <- renderTable({
          strata_data[, c("Subgroup", "nh", "Total_Cost", "Total_Time")]
        })
      }
    })
  })
  
  output$whitepaper_view <- renderUI({
    withMathJax(
      HTML("
        <h3><b>White Paper: Cost-Optimal Stratified Sampling Design</b></h3>
        <p><b>1. Introduction</b><br>
        This methodology determines the required sample size to estimate a population mean while minimizing cost and accounting for sampling bias.</p>
        
        <p><b>2. The Objective Function</b><br>
        We aim to minimize the Variance of the estimator \\(\\bar{y}_{st}\\) subject to a cost constraint, or conversely, minimize Cost for a fixed Variance.</p>
        
        <p><b>3. Bias Adjustment</b><br>
        The Mean Squared Error (MSE) is defined as:
        $$MSE(\\bar{y}) = V(\\bar{y}) + B^2$$
        where \\(B\\) is the sampling bias. To calculate the required sample size, we first derive the Target Variance:
        $$V_{target} = MSE_{target} - B^2$$
        </p>
        
        <p><b>4. Optimum Allocation (Neyman with Cost)</b><br>
        The sample size \\(n_h\\) for stratum \\(h\\) is allocated proportional to the population size and variability, and inversely proportional to the cost:
        $$n_h \\propto \\frac{N_h S_h}{\\sqrt{C_h}}$$
        
        The total sample size \\(n\\) is calculated as:
        $$n = \\frac{(\\sum W_h S_h \\sqrt{C_h}) \\sum (W_h S_h / \\sqrt{C_h})}{V_{target} + \\frac{1}{N} \\sum W_h S_h^2}$$
        where \\(W_h = N_h / N\\).
        </p>
      ")
    )
  })
}

shinyApp(ui = ui, server = server)