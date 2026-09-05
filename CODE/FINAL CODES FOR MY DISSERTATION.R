library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(caret)
library(corrplot)
library(readr)

project_path <- getwd()

raw_data_path <- file.path(project_path, "DATA", "RAW")
clean_data_path <- file.path(project_path, "DATA", "CLEAN")

results_path <- file.path(project_path, "RESULTS")

tables_path <- file.path(results_path, "Tables")
figures_path <- file.path(results_path, "Figures")
models_path <- file.path(results_path, "Models")

dir.create(clean_data_path,
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(tables_path,
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(figures_path,
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(models_path,
           recursive = TRUE,
           showWarnings = FALSE)

set.seed(123)

performance_metrics <- function(actual, predicted) {
  
  predicted <- as.vector(predicted)
  
  rmse <- sqrt(mean((actual - predicted)^2))
  
  mae <- mean(abs(actual - predicted))
  
  r2 <- 1 - sum((actual - predicted)^2) /
    sum((actual - mean(actual))^2)
  
  data.frame(
    RMSE = round(rmse, 2),
    MAE = round(mae, 2),
    R2 = round(r2, 3)
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
      axis.title = element_text(
        face = "bold"
      ),
      panel.grid.minor = element_blank()
    )
  
}

plot_predictions <- function(actual,
                             predicted,
                             title){
  
  predicted <- as.vector(predicted)
  
  prediction_table <- data.frame(
    Actual = actual,
    Predicted = predicted
  )
  
  ggplot(prediction_table,
         aes(x = Actual,
             y = Predicted)) +
    
    geom_point(alpha = 0.4,
               colour = "steelblue") +
    
    geom_abline(intercept = 0,
                slope = 1,
                colour = "red",
                linewidth = 1) +
    
    labs(
      title = title,
      x = "Actual Age (Years)",
      y = "Predicted Age (Years)"
    ) +
    
    theme_minimal()
  
}

source("SCRIPT/00_Setup.R")

import_nhanes <- function(cycle){
  
  demo_path <- file.path(raw_data_path, cycle, "DEMO")
  exam_path <- file.path(raw_data_path, cycle, "EXAM")
  lab_path  <- file.path(raw_data_path, cycle, "LAB")
  
  demo <- read_xpt(file.path(
    demo_path,
    paste0("DEMO_", cycle, ".xpt")
  ))
  
  bmx <- read_xpt(file.path(
    exam_path,
    paste0("BMX_", cycle, ".xpt")
  ))
  
  bpx <- read_xpt(file.path(
    exam_path,
    paste0("BPX_", cycle, ".xpt")
  ))
  
  biopro <- read_xpt(file.path(
    lab_path,
    paste0("BIOPRO_", cycle, ".xpt")
  ))
  
  glu <- read_xpt(file.path(
    lab_path,
    paste0("GLU_", cycle, ".xpt")
  ))
  
  hdl <- read_xpt(file.path(
    lab_path,
    paste0("HDL_", cycle, ".xpt")
  ))
  
  tchol <- read_xpt(file.path(
    lab_path,
    paste0("TCHOL_", cycle, ".xpt")
  ))
  
  nhanes_cycle <- demo %>%
    left_join(bmx, by = "SEQN") %>%
    left_join(bpx, by = "SEQN") %>%
    left_join(biopro, by = "SEQN") %>%
    left_join(glu, by = "SEQN") %>%
    left_join(hdl, by = "SEQN") %>%
    left_join(tchol, by = "SEQN")
  
  return(nhanes_cycle)
  
}

cycles <- c(
  "2009_2010",
  "2011_2012",
  "2013_2014",
  "2015_2016",
  "2017_2018"
)

nhanes_list <- lapply(cycles, import_nhanes)

sapply(nhanes_list, nrow)
nhanes_all <- bind_rows(nhanes_list)

nrow(nhanes_all)
saveRDS(
  nhanes_all,
  file.path(clean_data_path, "nhanes_all.rds")
)
summary(nhanes_all)

analysis_data <- nhanes_all %>%
  select(
    SEQN,
    RIDAGEYR,
    RIAGENDR,
    RIDRETH1,
    DMDEDUC2,
    INDFMPIR,
    BMXBMI,
    BMXWAIST,
    BPXSY1, BPXSY2, BPXSY3, BPXSY4,
    BPXDI1, BPXDI2, BPXDI3, BPXDI4,
    LBXTC,
    LBDHDD,
    LBXSCR,
    LBXSAL,
    LBXSUA,
    LBXSBU,
    LBXGLU
  )

analysis_data<- analysis_data %>%
  rename(
    
    Participant_ID = SEQN,
    
    Age = RIDAGEYR,
    
    Sex = RIAGENDR,
    
    Race_Ethnicity = RIDRETH1,
    
    Education_Level = DMDEDUC2,
    
    Income_Poverty_Ratio = INDFMPIR,
    
    BMI = BMXBMI,
    
    Waist_Circumference = BMXWAIST,
    
    Total_Cholesterol = LBXTC,
    
    HDL_Cholesterol = LBDHDD,
    
    Creatinine = LBXSCR,
    
    Albumin = LBXSAL,
    
    Uric_Acid = LBXSUA,
    
    Blood_Urea_Nitrogen = LBXSBU,
    
    Glucose = LBXGLU
    
  )

analysis_data <- analysis_data %>%
  mutate(
    
    Systolic_BP = rowMeans(
      select(., BPXSY1:BPXSY4),
      na.rm = TRUE
    ),
    
    Diastolic_BP = rowMeans(
      select(., BPXDI1:BPXDI4),
      na.rm = TRUE
    )
    
  )

analysis_data <- analysis_data %>%
  select(
    -BPXSY1:-BPXSY4,
    -BPXDI1:-BPXDI4
  )

analysis_data <- analysis_data %>%
  filter(Age >= 20)

analysis_data <- analysis_data %>%
  filter(!Education_Level %in% c(7, 9))

n_complete_with_glucose <- sum(complete.cases(analysis_data))
n_complete_with_glucose

analysis_data <- analysis_data %>%
  select(-Glucose)

model_complete <- analysis_data %>%
  filter(complete.cases(.))

n_complete_without_glucose <- nrow(model_complete)
n_complete_without_glucose

n_complete_without_glucose - n_complete_with_glucose
saveRDS(
  model_complete,
  file.path(clean_data_path, "model_complete.rds")
)

source("SCRIPT/00_Setup.R")

model_complete <- readRDS(
  file.path(clean_data_path, "model_complete.rds")
)

continuous_vars <- model_complete %>%
  select(
    Age,
    Income_Poverty_Ratio,
    BMI,
    Waist_Circumference,
    Total_Cholesterol,
    HDL_Cholesterol,
    Creatinine,
    Albumin,
    Uric_Acid,
    Blood_Urea_Nitrogen,
    Systolic_BP,
    Diastolic_BP
  )

descriptive_table <- data.frame(
  
  Variable = names(continuous_vars),
  
  Mean = sapply(continuous_vars, mean),
  
  SD = sapply(continuous_vars, sd),
  
  Median = sapply(continuous_vars, median),
  
  Minimum = sapply(continuous_vars, min),
  
  Maximum = sapply(continuous_vars, max)
  
)

descriptive_table[-1] <- round(descriptive_table[-1], 2)
descriptive_table

write.csv(
  descriptive_table,
  file.path(
    tables_path,
    "Table_4_1_Descriptive_Statistics.csv"
  ),
  row.names = FALSE
)

sex_counts <- table(model_complete$Sex)

sex_table <- data.frame(
  Category = names(sex_counts),
  Frequency = as.vector(sex_counts),
  Percentage = round(
    as.vector(prop.table(sex_counts)) * 100,
    2
  )
)
sex_table

ggplot(sex_table, aes(x = Category, y = Frequency, fill = Category)) +
  geom_col() +
  geom_text(
    aes(label = paste0(Frequency, " (", Percentage, "%)")),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Distribution of Participants by Sex",
    x = "Sex",
    y = "Frequency"
  ) +
  scale_fill_manual(
    values = c(
      "Male" = "blue",
      "Female" = "pink"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "none")

race_table <- data.frame(
  
  Category = names(table(model_complete$Race_Ethnicity)),
  
  Frequency = as.vector(
    table(model_complete$Race_Ethnicity)
  ),
  
  Percentage = round(
    
    prop.table(
      table(model_complete$Race_Ethnicity)
    )*100,
    
    2
    
  )
  
)
race_table

education_table <- data.frame(
  
  Category = names(table(model_complete$Education_Level)),
  
  Frequency = as.vector(
    table(model_complete$Education_Level)
  ),
  
  Percentage = round(
    
    prop.table(
      table(model_complete$Education_Level)
    )*100,
    
    2
    
  )
  
)
education_table
cor_matrix <- cor(
  model_complete %>%
    dplyr::select(
      Age,
      Income_Poverty_Ratio,
      BMI,
      Waist_Circumference,
      Total_Cholesterol,
      HDL_Cholesterol,
      Creatinine,
      Albumin,
      Uric_Acid,
      Blood_Urea_Nitrogen,
      Systolic_BP,
      Diastolic_BP
    ),
  use = "complete.obs"
)

par(
  oma = c(0, 0, 4, 0),
  mar = c(1, 1, 1, 1)
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.8,
  col = colorRampPalette(
    c("blue", "white", "red")
  )(200),
  cl.pos = "n"
)

mtext(
  "Correlation Matrix of Chronological Age and Selected Predictors",
  side = 3,
  outer = TRUE,
  line = 1,
  font = 2,
  cex = 1.3
)
legend(
  "right",
  legend = c(
    "Strong negative",
    "Moderate negative",
    "Weak / no correlation",
    "Moderate positive",
    "Strong positive"
  ),
  fill = c(
    "blue",
    "lightblue",
    "white",
    "pink",
    "red"
  ),
  title = "Correlation strength",
  bty = "n",
  cex = 0.8
)


source("SCRIPT/00_Setup.R")

model_complete <- readRDS(
  file.path(clean_data_path, "model_complete.rds")
)
set.seed(123)

train_index <- createDataPartition(
  model_complete$Age,
  p = 0.80,
  list = FALSE
)

train_data <- model_complete[train_index, ]

test_data <- model_complete[-train_index, ]
nrow(model_complete)
nrow(train_data)
nrow(test_data)
saveRDS(
  train_data,
  file.path(clean_data_path, "train_data.rds")
)

saveRDS(
  test_data,
  file.path(clean_data_path, "test_data.rds")
)


mlr_model <- lm(
  
  Age ~ BMI +
    Waist_Circumference +
    Total_Cholesterol +
    HDL_Cholesterol +
    Creatinine +
    Albumin +
    Uric_Acid +
    Blood_Urea_Nitrogen +
    Systolic_BP +
    Diastolic_BP +
    Income_Poverty_Ratio +
    Sex +
    Race_Ethnicity +
    Education_Level,
  
  data = train_data
  
)
summary(mlr_model)

mlr_predictions <- predict(
  mlr_model,
  newdata = test_data
)
mlr_results <- performance_metrics(
  
  actual = test_data$Age,
  
  predicted = mlr_predictions
  
)
mlr_results

plot_predictions(
  
  actual = test_data$Age,
  
  predicted = mlr_predictions,
  
  title = "Multiple Linear Regression"
  
)
prediction_table <- data.frame(
  
  Actual_Age = test_data$Age,
  
  Predicted_Age = mlr_predictions
  
)
prediction_table
write.csv(
  
  prediction_table,
  
  file.path(
    
    tables_path,
    
    "MLR_Predictions.csv"
    
  ),
  
  row.names = FALSE
  
)
figure_MLR_residual <- plot_residuals(
  actual = test_data$Age,
  predicted = mlr_predictions,
  title = "Residual Plot — Multiple Linear Regression"
)

figure_MLR_residual

source("SCRIPT/00_Setup.R")

train_data <- readRDS(
  file.path(clean_data_path, "train_data.rds")
)

test_data <- readRDS(
  file.path(clean_data_path, "test_data.rds")
)

predictor_vars <- c(
  "BMI",
  "Waist_Circumference",
  "Total_Cholesterol",
  "HDL_Cholesterol",
  "Creatinine",
  "Albumin",
  "Uric_Acid",
  "Blood_Urea_Nitrogen",
  "Systolic_BP",
  "Diastolic_BP",
  "Income_Poverty_Ratio",
  "Sex",
  "Race_Ethnicity",
  "Education_Level"
)

x_train <- model.matrix(
  ~ .,
  data = train_data[, predictor_vars]
)[,-1]

x_test <- model.matrix(
  ~ .,
  data = test_data[, predictor_vars]
)[,-1]

y_train <- train_data$Age

y_test <- test_data$Age

set.seed(123)

cv_ridge <- cv.glmnet(
  
  x_train,
  
  y_train,
  
  alpha = 0,
  
  nfolds = 10
  
)

plot(
  cv_ridge,
  main = "10-Fold Cross-Validation for Ridge Regression",
  xlab = "log(lambda)",
  ylab = "Mean Cross-Validated Error"
)

coef(cv_ridge, s = "lambda.min")
ridge_model <- glmnet(
  
  x_train,
  
  y_train,
  
  alpha = 0,
  
  lambda = cv_ridge$lambda.min
  
)

ridge_predictions <- predict(
  
  ridge_model,
  
  newx = x_test,
  
  s = cv_ridge$lambda.min
  
)

ridge_results <- performance_metrics(
  
  actual = y_test,
  
  predicted = ridge_predictions
  
)
ridge_results

plot_predictions(
  
  actual = y_test,
  
  predicted = ridge_predictions,
  
  title = "Ridge Regression"
  
)
figure_ridge_pred <- plot_residuals(
  actual = y_test,
  predicted = ridge_predictions,
  title = "Residual Plot — Ridge Regression"
)
figure_ridge_pred

source("SCRIPT/00_Setup.R")

train_data <- readRDS(
  file.path(clean_data_path, "train_data.rds")
)

test_data <- readRDS(
  file.path(clean_data_path, "test_data.rds")
)

predictor_vars <- c(
  "BMI",
  "Waist_Circumference",
  "Total_Cholesterol",
  "HDL_Cholesterol",
  "Creatinine",
  "Albumin",
  "Uric_Acid",
  "Blood_Urea_Nitrogen",
  "Systolic_BP",
  "Diastolic_BP",
  "Income_Poverty_Ratio",
  "Sex",
  "Race_Ethnicity",
  "Education_Level"
)

x_train <- model.matrix(
  ~ .,
  data = train_data[, predictor_vars]
)[,-1]

x_test <- model.matrix(
  ~ .,
  data = test_data[, predictor_vars]
)[,-1]

y_train <- train_data$Age
y_test <- test_data$Age

set.seed(123)

cv_lasso <- cv.glmnet(
  
  x_train,
  
  y_train,
  
  alpha = 1,
  
  nfolds = 10
  
)

plot(
  cv_lasso,
  main = "10-Fold Cross-Validation for LASSO Regression",
  xlab = "log(lambda)",
  ylab = "Mean Cross-Validated Error"
)

lasso_model <- glmnet(
  
  x_train,
  
  y_train,
  
  alpha = 1,
  
  lambda = cv_lasso$lambda.min
  
)
lasso_coefficients <- as.matrix(
  coef(lasso_model, s = "lambda.min")
)

coef(lasso_model, s = "lambda.min")

lasso_predictions <- predict(
  
  lasso_model,
  
  newx = x_test,
  
  s = cv_lasso$lambda.min
  
)

lasso_results <- performance_metrics(
  
  actual = y_test,
  
  predicted = lasso_predictions
  
)

lasso_results

plot_predictions(
  
  actual = y_test,
  
  predicted = lasso_predictions,
  
  title = "LASSO Regression"
  
)


write.csv(
  
  lasso_coefficients,
  
  file.path(
    
    tables_path,
    
    "LASSO_Coefficients.csv"
    
  )
  
)

figure_lasso_predictions <- plot_residuals(
  actual = y_test,
  predicted = lasso_predictions,
  title = "Residual Plot — LASSO Regression"
)
figure_lasso_predictions

source("SCRIPT/00_Setup.R")

train_data <- readRDS(
  file.path(clean_data_path, "train_data.rds")
)

test_data <- readRDS(
  file.path(clean_data_path, "test_data.rds")
)

predictor_vars <- c(
  "BMI",
  "Waist_Circumference",
  "Total_Cholesterol",
  "HDL_Cholesterol",
  "Creatinine",
  "Albumin",
  "Uric_Acid",
  "Blood_Urea_Nitrogen",
  "Systolic_BP",
  "Diastolic_BP",
  "Income_Poverty_Ratio",
  "Sex",
  "Race_Ethnicity",
  "Education_Level"
)

x_train <- model.matrix(
  ~ .,
  data = train_data[, predictor_vars]
)[,-1]

x_test <- model.matrix(
  ~ .,
  data = test_data[, predictor_vars]
)[,-1]

y_train <- train_data$Age
y_test <- test_data$Age

set.seed(123)

cv_elastic <- cv.glmnet(
  
  x_train,
  
  y_train,
  
  alpha = 0.5,
  
  nfolds = 10
  
)

plot(
  cv_elastic,
  main = "10-Fold Cross-Validation for Elastic Net Regression",
  xlab = "log(lambda)",
  ylab = "Mean Cross-Validated Error"
)

elastic_model <- glmnet(
  
  x_train,
  
  y_train,
  
  alpha = 0.5,
  
  lambda = cv_elastic$lambda.min
  
)

elastic_predictions <- predict(
  
  elastic_model,
  
  newx = x_test,
  
  s = cv_elastic$lambda.min
  
)

elastic_results <- performance_metrics(
  
  actual = y_test,
  
  predicted = elastic_predictions
  
)

elastic_results

plot_predictions(
  
  actual = y_test,
  
  predicted = elastic_predictions,
  
  title = "Elastic Net Regression"
  
)

pred_elastic <- as.vector(
  predict(
    elastic_model,
    newx = x_test,
    s = "lambda.min"
  )
)

figure_pred_elastic <- plot_residuals(
  actual = y_test,
  predicted = pred_elastic,
  title = "Residual Plot — Elastic Net Regression"
)

figure_pred_elastic

ggplot(model_complete, aes(x = Age, fill = after_stat(x))) +
  geom_histogram(
    bins = 30,
    colour = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradient(
    low = "#56CCF2",
    high = "#6A1B9A"
  ) +
  labs(
    title = "Distribution of Chronological Age",
    x = "Chronological Age (years)",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    axis.title = element_text(
      face = "bold"
    )
  )

model_complete %>%
  select(
    BMI,
    Waist_Circumference,
    Systolic_BP,
    Diastolic_BP
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = "", y = Value, fill = Variable)) +
  geom_boxplot(
    colour = "black",
    alpha = 0.8,
    width = 0.6
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Distribution of Anthropometric and Blood Pressure Variables",
    x = NULL,
    y = "Observed Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

model_complete %>%
  select(
    Total_Cholesterol,
    HDL_Cholesterol,
    Creatinine,
    Albumin,
    Uric_Acid,
    Blood_Urea_Nitrogen
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = "", y = Value, fill = Variable)) +
  geom_boxplot(
    colour = "black",
    alpha = 0.8,
    width = 0.6
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Distribution of Biochemical Variables",
    x = NULL,
    y = "Observed Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )
