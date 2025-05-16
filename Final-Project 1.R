# Diabetic Patients Data Of 130-US hospitals for years 1999-2008

diabetes = read.csv("/Users/nawarajadhikari/Desktop/ASDS/6303/Final Project/diabetic_data.csv", stringsAsFactors = FALSE)
View(diabetes)


#----------------------------------------------------------------------------------------
# Checking the datatypes of the features
str(diabetes)

# As we can see that there are 13 Numerical features and 37 Categorical features
#We can see the '?' in the data set,Which are null value. We can replace them with NA.
# Replace all '?' with NA
diabetes[diabetes == "?"] <- NA
#----------------------------------------------------------------------------------------
# Check for Missing Values
# Only show columns with at least one NA
colSums(is.na(diabetes))[colSums(is.na(diabetes)) > 0]

missing_percent <- colSums(is.na(diabetes)) / nrow(diabetes) * 100
missing_percent[missing_percent > 0]

# We will remove the feature which has more than 35% missing values
# Remove columns with more than 35% missing values
df <- diabetes[, missing_percent <= 35]
View(df)

# Now lets first fill missing values in Race column
unique(df$race)
#As we can see that there is a other column, we will fill missing values with others
df$race[is.na(df$race)] <- "Other"
# For other features we will deal with that afterwards
#---------------------------------------------------------------------------------------
# Statistics of Dataset
summary(df)
#---------------------------------------------------------------------------------------
#Required Libraries for EDA
library(tidyverse) 
library(RColorBrewer) 
library(dlookr) 
library(ggcorrplot) 
library(plyr)  
library(dplyr) 
library(cowplot) 

# 1. Checking the distribution of target Class
ggplot(df, aes(x = readmitted, fill = readmitted)) + 
  geom_bar() + 
  ggtitle("Distribution of Target Class") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.25) + 
  scale_fill_brewer(palette = "Pastel1")

#2. Race
ggplot(df, aes(x = race,fill = race)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Distribution of Race") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) + 
  scale_fill_brewer(palette="Pastel1")

#Clearly, the “Cacucasian” race dominates.

#3. Exploring- gender: (Categorical)
df$gender <- gsub("Unknown/Invalid", "Male", df$gender)
ggplot(df, aes(x = gender,fill = gender)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Distribution of Gender") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) + 
  scale_fill_brewer(palette="Pastel1") 

# 4.Age
## Increasing the color palette 
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(10) 

ggplot(df, aes(x = age,fill = age)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Distribution of Gender") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) + 
  scale_fill_manual(values = mycolors) 

# 5. Admissions types
# Convert 'admission_type_id' to character
df$admission_type_id <- as.character(df$admission_type_id)

# Now select 'admission_type_id' and 'readmitted' columns
new_admission_id <- df %>% select(admission_type_id, readmitted)

# Plotting the data
ggplot(new_admission_id, aes(x = admission_type_id, fill = admission_type_id)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Distribution of Admission ID") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) + 
  scale_fill_brewer(palette="Pastel1")


#6. Discharge dispostion
# Convert 'discharge_disposition_id' to character
df$discharge_disposition_id <- as.character(df$discharge_disposition_id)

# Now select 'discharge_disposition_id' and 'readmitted' columns
diabetes_dispo_id <- df %>% select(discharge_disposition_id, readmitted)

df$discharge_disposition_id <- factor(df$discharge_disposition_id, 
                                      levels = sort(unique(df$discharge_disposition_id)))

# Define color palette
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(30)

# Plotting the data
ggplot(diabetes_dispo_id, aes(x = discharge_disposition_id, fill = discharge_disposition_id)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Distribution of Discharge Disposition Id") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_manual(values = mycolors)


# 7. Admission Source Id
# Convert 'admission_source_id' to character
df$admission_source_id <- as.character(df$admission_source_id)

# Convert to factor with levels in ascending order
df$admission_source_id <- factor(df$admission_source_id, 
                                 levels = sort(unique(df$admission_source_id)))

# Now select 'admission_source_id' and 'readmitted' columns
diabetes_adm_source <- df %>% select(admission_source_id, readmitted)

# Define color palette
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(21)

# Plotting the data
ggplot(diabetes_adm_source, aes(x = admission_source_id, fill = admission_source_id)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Distribution of Admission Source Id") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) + 
  scale_fill_manual(values = mycolors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 8. Numerical Features

selected_columns <- df[, c("time_in_hospital", 
                           "num_lab_procedures", 
                           "num_procedures", 
                           "num_medications", 
                           "number_outpatient", 
                           "number_emergency", 
                           "number_inpatient", 
                           "number_diagnoses")]

# Melt the data into long format for easier plotting
melted_data <- melt(selected_columns)

# Create the plot
plot <- ggplot(melted_data, aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ variable, scales = "free_x") +  # Facet by feature
  ggtitle("Histograms of Selected Features") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")

# Save the plot with larger size
ggsave("histograms_large.png", plot, width = 12, height = 8, dpi = 300)


# Corrrelation Heatmap

numeric = c("time_in_hospital", 
            "num_lab_procedures", 
            "num_procedures", 
            "num_medications", 
            "number_outpatient", 
            "number_emergency", 
            "number_inpatient", 
            "number_diagnoses") 
diabetes_numeric <- select(df, numeric) 
correlation_matrix <- round(cor(diabetes_numeric),2) 
ggcorrplot(correlation_matrix, hc.order = TRUE, lab = TRUE, 
           colors = c("#6D9EC1", "white", "#E46726"))


# Exploring- Features of Medications (Categorical)
## Function to plot distribution of a categorical variable 
cat_distribution <- function(df, atr) { 
  title <- paste("Distribution of",atr, sep=" ") 
  plt <- ggplot(df, aes_string(x = atr, fill = atr)) + 
    geom_bar(show.legend = FALSE) + 
    ggtitle(title) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_text(aes(label = ..count..), stat = "count", vjust=-0.25, size=2) + 
    scale_fill_brewer(palette="Pastel1") 
  plt 
} 

## First 9 Features 
metformin_plot <- cat_distribution(df, "metformin") 
repaglinide_plot <- cat_distribution(df, "repaglinide") 
nateglinide_plot <- cat_distribution(df, "nateglinide") 
chlorpropamide_plot <- cat_distribution(df, "chlorpropamide") 
glimepiride_plot <- cat_distribution(df, "glimepiride") 
acetohexamide_plot <- cat_distribution(df, "acetohexamide") 
glipizide_plot <- cat_distribution(df, "glipizide") 
glyburide_plot <- cat_distribution(df, "glyburide") 
tolbutamide_plot <- cat_distribution(df, "tolbutamide")

## Making a grid 
plot_grid(metformin_plot, 
          repaglinide_plot, 
          nateglinide_plot, 
          chlorpropamide_plot, 
          glimepiride_plot, 
          acetohexamide_plot, 
          glipizide_plot, 
          glyburide_plot, 
          tolbutamide_plot, 
          ncol = 3, labels = "AUTO")


pioglitazone_plot <- cat_distribution(df, "pioglitazone") 
rosiglitazone_plot <- cat_distribution(df, "rosiglitazone") 
acarbose_plot <- cat_distribution(df, "acarbose") 
miglitol_plot <- cat_distribution(df, "miglitol") 
troglitazone_plot <- cat_distribution(df, "troglitazone") 
tolazamide_plot <- cat_distribution(df, "tolazamide") 

plot_grid(pioglitazone_plot, 
          rosiglitazone_plot, 
          acarbose_plot, 
          miglitol_plot, 
          troglitazone_plot, 
          tolazamide_plot, 
          ncol = 3, labels = "AUTO")


## Next 8 Features 
examide_plot <- cat_distribution(df, "examide") 
citoglipton_plot <- cat_distribution(df, "citoglipton") 
insulin_plot <- cat_distribution(df, "insulin") 
glyburide_metformin_plot <- cat_distribution(df, "glyburide.metformin") 
glipizide_metformin_plot <- cat_distribution(df, "glipizide.metformin") 
glimepiride_pioglitazone_plot <- cat_distribution(df, 
                                                  "glimepiride.pioglitazone") 
metformin_rosiglitazone_plot <- cat_distribution(df, "metformin.rosiglitazone") 
metformin_pioglitazone_plot <- cat_distribution(df, "metformin.pioglitazone") 

## Making a grid 
plot_grid(examide_plot, 
          citoglipton_plot, 
          insulin_plot, 
          glyburide_metformin_plot, 
          glipizide_metformin_plot, 
          glimepiride_pioglitazone_plot, 
          metformin_rosiglitazone_plot, 
          metformin_pioglitazone_plot, 
          ncol = 2, labels = "AUTO") 

change_med_plot <- cat_distribution(diabetes, "change") 
change_med_plot

diabetesMed_plot <- cat_distribution(diabetes, "diabetesMed") 
diabetesMed_plot

#---------------------------------------------------------------------------------------
# We will remove the unnecessary columns from the dataset
numeric.features <- c("time_in_hospital","num_lab_procedures","num_procedures","num_medications", 
                      "number_outpatient","number_emergency","number_inpatient","number_diagnoses") 

cat.features <- c("race","gender","age","admission_type_id","discharge_disposition_id", 
                  "admission_source_id","change","diabetesMed") 

medical.features <- c("metformin","repaglinide","nateglinide","chlorpropamide", "glimepiride", 
                      "glipizide","glyburide","tolbutamide","pioglitazone",  
                      "rosiglitazone","acarbose","miglitol","troglitazone","tolazamide", 
                      "insulin","glyburide.metformin","glipizide.metformin") 

target.attr <- c("readmitted") 

diag = c("diag_1","diag_2","diag_3")

# Select the features you need from the dataframe
selected_features <- c(numeric.features, cat.features, medical.features, target.attr)

# Create a subset of the dataframe with the selected features
df_selected <- df[, selected_features]

# Handle missing values
df_selected$readmitted[df_selected$readmitted == "<30"] <- as.integer(0)
df_selected$readmitted[df_selected$readmitted == ">30"] <- as.integer(1)
df_selected$readmitted[df_selected$readmitted == "NO"] <- as.integer(2)

# Convert categorical variables to factors
cat_vars <- c(cat.features, medical.features, target.attr)
df_selected[cat_vars] <- lapply(df_selected[cat_vars], as.factor)

# Convert age to an ordered factor
age_range <- c("[0-10)", "[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)", "[90-100)")
df_selected$age <- factor(df_selected$age, levels = age_range, ordered = TRUE)

# Apply square root transformation to certain numerical columns
cols_to_sqrt <- c("num_lab_procedures", "num_medications")
df_selected[cols_to_sqrt] <- lapply(df_selected[cols_to_sqrt], sqrt)


# Set a seed for reproducibility
set.seed(123)

# Split the dataset into 80% training and 20% testing
library(caret)
trainIndex <- createDataPartition(df_selected$readmitted, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

# Create training and testing datasets
train_data <- df_selected[trainIndex, ]
test_data <- df_selected[-trainIndex, ]

# Separate features (X) and target (y)
X_train <- train_data[, setdiff(names(train_data), "readmitted")]
y_train <- train_data$readmitted
X_test <- test_data[, setdiff(names(test_data), "readmitted")]
y_test <- test_data$readmitted

# Load the nnet package
library(nnet)

# Train a multinomial logistic regression model
multinom_model <- multinom(readmitted ~ ., data = train_data)

# Make predictions
multinom_pred <- predict(multinom_model, newdata = test_data)

# Confusion matrix for training data
multinom_train_pred <- predict(multinom_model, newdata = train_data)
confusionMatrix(multinom_train_pred, as.factor(train_data$readmitted))

# Evaluate the model
confusionMatrix(multinom_pred, as.factor(test_data$readmitted))




#-------------------------------------------------------------------------------
library(rpart)
cart_model <- rpart(readmitted ~ ., data = train_data, method = "class")

# Make predictions
cart_pred <- predict(cart_model, newdata = test_data, type = "class")

# Confusion matrix for training data
cart_train_pred <- predict(cart_model, newdata = train_data, type = "class")
confusionMatrix(cart_train_pred, as.factor(train_data$readmitted))


# Evaluate the model
confusionMatrix(cart_pred, as.factor(test_data$readmitted))

#-------------------------------------------------------------------------------

library(randomForest)
rf_model <- randomForest(readmitted ~ ., data = train_data)

# Make predictions
rf_pred <- predict(rf_model, newdata = test_data)

# Confusion matrix for training data
rf_train_pred <- predict(rf_model, newdata = train_data)
confusionMatrix(rf_train_pred, as.factor(train_data$readmitted))


# Evaluate the model
confusionMatrix(rf_pred, as.factor(test_data$readmitted))

#-------------------------------------------------------------------------------

# XGBoost for multiclass classification
library(xgboost)

# Prepare data
train_matrix <- model.matrix(~ . - 1, data = train_data)
test_matrix <- model.matrix(~ . - 1, data = test_data)

# Convert target to numeric for XGBoost
train_label <- as.numeric(train_data$readmitted) - 1
test_label <- as.numeric(test_data$readmitted) - 1

# Train the XGBoost model
xgb_model <- xgboost(data = train_matrix, label = train_label, 
                     nrounds = 100, objective = "multi:softmax", num_class = 3)

# Make predictions
xgb_pred <- predict(xgb_model, test_matrix)

# Confusion matrix for training data
xgb_train_pred <- predict(xgb_model, model.matrix(~ . - 1, data = train_data))
confusionMatrix(as.factor(xgb_train_pred), as.factor(train_label))

# Evaluate the model
confusionMatrix(as.factor(xgb_pred), as.factor(test_label))

# Load necessary library for plotting
library(ggplot2)

# Get feature importance
importance_matrix <- xgb.importance(model = xgb_model)

# Plot feature importance
xgb.plot.importance(importance_matrix)

#-------------------------------------------------------------------------------

# Train Naive Bayes
library(e1071)
nb_model <- naiveBayes(readmitted ~ ., data = train_data)

# Make predictions
nb_pred <- predict(nb_model, newdata = test_data)

# Confusion matrix for training data
nb_train_pred <- predict(nb_model, newdata = train_data)
confusionMatrix(nb_train_pred, as.factor(train_data$readmitted))


# Evaluate the model
confusionMatrix(nb_pred, as.factor(test_data$readmitted))

