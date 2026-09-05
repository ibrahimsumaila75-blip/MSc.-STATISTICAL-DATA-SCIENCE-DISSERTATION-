project_path <- "C:/Users/USER/Desktop/MSC DISSERTATION"

setwd(project_path)

cat("Project directory:\n")
cat(project_path, "\n\n")

raw_data_path <- file.path(project_path, "DATA", "RAW")

clean_data_path <- file.path(project_path, "DATA", "CLEAN")

results_path <- file.path(project_path, "RESULTS")

tables_path <- file.path(results_path, "Tables")

figures_path <- file.path(results_path, "Figures")

models_path <- file.path(results_path, "Models")

scripts_path <- file.path(project_path, "CODE")

folders <- c(
  
  clean_data_path,
  
  results_path,
  
  tables_path,
  
  figures_path,
  
  models_path,
  
  scripts_path
  
)

for(folder in folders){
  
  if(!dir.exists(folder)){
    
    dir.create(
      
      folder,
      
      recursive = TRUE,
      
      showWarnings = FALSE
      
    )
    
  }
  
}

cat("Project folders verified.\n\n")

required_packages <- c(
  
  "haven",
  
  "dplyr",
  
  "glmnet",
  
  "caret",
  
  "car",
  
  "corrplot",
  
  "ggplot2"
  
)

installed <- rownames(installed.packages())

missing_packages <-
  
  required_packages[
    
    !(required_packages %in% installed)
    
  ]

if(length(missing_packages) > 0){
  
  install.packages(missing_packages)
  
}

invisible(
  
  lapply(
    
    required_packages,
    
    library,
    
    character.only = TRUE
    
  )
  
)

cat("Packages loaded successfully.\n\n")

set.seed(123)

cat("Random seed set to 123.\n\n")

options(
  
  stringsAsFactors = FALSE,
  
  scipen = 999
  
)

cat("Global options configured.\n\n")

cat("-------------------------------------------\n")

cat("Setup Completed Successfully\n")

cat("-------------------------------------------\n")

cat("Project Path:\n")

print(project_path)

cat("\nRaw Data:\n")

print(raw_data_path)

cat("\nClean Data:\n")

print(clean_data_path)

cat("\nResults:\n")

print(results_path)

cat("\nTables:\n")

print(tables_path)

cat("\nFigures:\n")

print(figures_path)

cat("\nModels:\n")

print(models_path)

cat("-------------------------------------------\n")

performance_metrics <- function(actual, predicted) {
  
  predicted <- as.vector(predicted)
  
  rmse <- sqrt(
    mean((actual - predicted)^2)
  )
  
  mae <- mean(
    abs(actual - predicted)
  )
  
  r2 <- 1 -
    sum((actual - predicted)^2) /
    sum((actual - mean(actual))^2)
  
  data.frame(
    RMSE = round(rmse, 2),
    MAE = round(mae, 2),
    R2 = round(r2, 3)
  )
}

plot_predictions <- function(
    actual,
    predicted,
    title
) {
  
  predicted <- as.vector(predicted)
  
  prediction_table <- data.frame(
    Actual = actual,
    Predicted = predicted
  )
  
  prediction_table$Error <-
    prediction_table$Predicted -
    prediction_table$Actual
  
  ggplot(
    prediction_table,
    aes(
      x = Actual,
      y = Predicted,
      colour = Error
    )
  ) +
    geom_point(
      alpha = 0.65,
      size = 2.5
    ) +
    scale_colour_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      name = "Prediction Error"
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = title,
      x = "Actual Age (Years)",
      y = "Predicted Age (Years)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    )
}

plot_residuals <- function(actual, predicted, title) {
  
  predicted <- as.vector(predicted)
  
  residuals <- actual - predicted
  
  residual_data <- data.frame(
    Predicted = predicted,
    Residual = residuals
  )
  
  ggplot(
    residual_data,
    aes(x = Predicted, y = Residual)
  ) +
    geom_point(
      colour = "#2874A6",
      alpha = 0.25,
      size = 1.5
    ) +
    geom_hline(
      yintercept = 0,
      colour = "#C0392B",
      linetype = "dashed",
      linewidth = 1.1
    ) +
    geom_smooth(
      method = "loess",
      se = TRUE,
      colour = "#8E44AD",
      fill = "#D7BDE2",
      linewidth = 1
    ) +
    labs(
      title = title,
      x = "Predicted Age (Years)",
      y = "Residual (Observed − Predicted)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 15,
        hjust = 0.5
      ),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}