library(car)
library(caTools)
library(data.table) 
library(dplyr)
library(ehaGoF) 
library(ggcorrplot)
library(ggplot2)
library(Metrics)
library(nnet)
library(purrr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(tidyr)


setwd("C:/Users/tyeck/OneDrive/Desktop/School/Y2S1/BC2406 Analytics I Visual & Predictive Techniques/BC2406_Project_Submission_Group4")
emissionsData.dt <- data.table::fread("methane_emissions_data.csv",stringsAsFactors = TRUE, na.strings = c("NA", "missing", "N/A", -99, "", "m", "M", "na", "."))

class(emissionsData.dt)
summary(emissionsData.dt)
levels(emissionsData.dt$ProducingType)
str(emissionsData.dt)
emissionsData.dt <- subset(emissionsData.dt, select = -c(SiteName,`Fracked?`))

sum(is.na(emissionsData.dt))

# Exploratory Data Analysis part 1 -------------------------------------------------------------

# Correlation Exploration
ggcorrplot(cor(na.omit(emissionsData.dt[,-c("Combuster","tracerflux","ProducingType","WellStruct")])), type = "lower")
cor(emissionsData.dt$WaterProduced, emissionsData.dt$GasProduced)
cor(emissionsData.dt$Age, emissionsData.dt$emissionsSum)
cor(emissionsData.dt$MeasuredChemicalInjectionPumps, emissionsData.dt$TotalChemicalInjectionPumps)


# Convert to long format for continuous variables
emissionsData_continuous<- gather(emissionsData.dt, key = "Variable", value = "Value",c("GasProduced", "WaterProduced", "MeasuredChemicalInjectionPumps", "TotalChemicalInjectionPumps"))

# Create a boxplot with faceting
ggplot(emissionsData_continuous, aes(x = Variable, y = Value)) +
  geom_boxplot(fill ="grey") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Variables with Outliers")

#--------------------------------- Data Cleaning —-----------------------------------

## Factoring and converting categorical variables to numeric levels -------------
emissionsData.dt$Combuster <- factor(ifelse(emissionsData.dt$Combuster == "Yes", 1, 0))
emissionsData.dt$tracerflux <- factor(ifelse(emissionsData.dt$tracerflux == "Yes", 1, 0))
emissionsData.dt$ProducingType <- factor(ifelse(emissionsData.dt$ProducingType == "shale gas", 0, ifelse(emissionsData.dt$ProducingType == "Other Tight Reservoir Rock", 1, 2)))
emissionsData.dt$WellStruct <- factor(ifelse(emissionsData.dt$WellStruct == "Horizontal", 0, ifelse(emissionsData.dt$WellStruct == "Directional", 1, 2)))

## Outlier Handling -------------------------------------------------------------

# Identify outlier rows for each column
outlier_rows <- emissionsData.dt %>%
  select_if(is.numeric) %>%
  map(~ which(.x %in% boxplot.stats(.x)$out)) %>%
  unlist() %>%
  unique()

# Remove rows containing outliers
emissionsData.dt <- emissionsData.dt[-outlier_rows, ]


## Missing Data Handling ------------------------------------------------------------------
missing_values <- sapply(emissionsData.dt, function(x) sum(is.na(x)))
print(missing_values)

# Replace NA with mean for continuous
emissionsData.dt <- emissionsData.dt %>%
  mutate(across(-c(Combuster, tracerflux, ProducingType, WellStruct), 
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Check if replaced
sapply(emissionsData.dt, function(x) sum(is.na(x)))


# Logistic Regression for `Combuster` with 2 levels
log.Combuster <- glm(Combuster ~ .,data = emissionsData.dt, family = binomial)
summary(log.Combuster)
# Predict probabilities using the logistic regression model
predicted_probs <- predict(log.Combuster, newdata = emissionsData.dt, type = "response")
# Identify indices of missing values in 'Combuster'
missing_indices <- which(is.na(emissionsData.dt$Combuster))
# Replace missing values using a threshold (e.g., 0.5 for binary classification)
emissionsData.dt$Combuster[missing_indices] <- ifelse(predicted_probs[missing_indices] > 0.5, 1, 0)

# Logistic Regression for `tracerflux` with 2 levels
log.tracerflux <- glm(tracerflux ~ ., data = emissionsData.dt, family = binomial)
# Predict probabilities using the logistic regression model
predicted_probs_tracerflux <- predict(log.tracerflux, newdata = emissionsData.dt, type = "response")
# Identify indices of missing values in 'tracerflux'
missing_indices_tracerflux <- which(is.na(emissionsData.dt$tracerflux))
# Replace missing values using a threshold
emissionsData.dt$tracerflux[missing_indices_tracerflux] <- ifelse(predicted_probs_tracerflux[missing_indices_tracerflux] > 0.5, 1, 0)

# Multinomial Logistic Regression for `ProducingType`
multinom.ProducingType <- multinom(ProducingType ~ . -Combuster -tracerflux -WellStruct, data = emissionsData.dt)
# Predict the class labels using the multinomial logistic regression model
predicted_labels_ProducingType <- predict(multinom.ProducingType, newdata = emissionsData.dt)
# Identify indices of missing values in 'ProducingType'
missing_indices_ProducingType <- which(is.na(emissionsData.dt$ProducingType))
# Replace missing values
emissionsData.dt$ProducingType[missing_indices_ProducingType] <- predicted_labels_ProducingType[missing_indices_ProducingType]


# Check if replaced
sapply(emissionsData.dt, function(x) sum(is.na(x)))

## Check for duplicated rows based on all columns ---------------------------------------
duplicates <- emissionsData.dt[duplicated(emissionsData.dt), ]

# Print out number of duplicated rows
cat("Number of duplicated rows: ", nrow(duplicates), "\n")

# --------------------------------Exploratory Data Analysis part 2 -------------------------------------------

# Convert to long format for continuous variables
emissionsData_continuous<- gather(emissionsData.dt, key = "Variable", value = "Value",-c("Combuster", "tracerflux", "ProducingType", "WellStruct"))

# Create a boxplot with faceting
ggplot(emissionsData_continuous, aes(x = Variable, y = Value)) +
  geom_boxplot(fill ="grey") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Continuous Variables")

# Create a density plot with faceting 
ggplot(emissionsData_continuous, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, color = "white", show.legend = FALSE) +  
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Density Plots of Continuous Variables")

# Create a histogram with faceting
ggplot(emissionsData_continuous, aes(x = Value, fill = Variable)) +
  geom_histogram(binwidth = 10, position = "dodge", show.legend = FALSE) +  
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Continuous Variables")

# Create a violin plot with faceting
ggplot(emissionsData_continuous, aes(x = Variable, y = Value, fill = Variable)) +
  geom_violin(trim = FALSE, scale = "width", color = "white") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Violin Plots of Continuous Variables") +
  theme(legend.position="none") 

# Convert numeric levels back to strings for barplot visualisation
emissionsData.dt$Combuster <- factor(emissionsData.dt$Combuster, levels = c(0, 1), labels = c("No", "Yes"))
emissionsData.dt$tracerflux <- factor(emissionsData.dt$tracerflux, levels = c(0, 1), labels = c("No", "Yes"))
emissionsData.dt$ProducingType <- factor(emissionsData.dt$ProducingType, levels = c(0, 1, 2), labels = c("shale gas", "Other Tight Reservoir Rock", "Other Tight Reservoir Rock & Shale"))
emissionsData.dt$WellStruct <- factor(emissionsData.dt$WellStruct, levels = c(0, 1, 2), labels = c("Horizontal", "Directional", "Vertical"))

emissionsData_categorical<- gather(emissionsData.dt, key = "Variable", value = "Value",c("Combuster", "tracerflux", "ProducingType", "WellStruct"))

# Create a bar chart with faceting
ggplot(emissionsData_categorical, aes(x = Value, fill = Variable)) +
  geom_bar(position = "dodge", show.legend = FALSE) +  
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Bar Charts of Categorical Variables", 
       x = "Categories", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------Saving Cleaned Data ----------------------------------
# write.csv(emissionsData.dt, file = "C:/Users/tyeck/OneDrive/Desktop/School/Y2S1/BC2406 Analytics I Visual & Predictive Techniques/BC2406_Project_Submission_Group4/cleaned_methane_emissions_data.csv", row.names = FALSE)


# --------------------------------- Train-Test split -----------------------------------
set.seed(2001)
train <- sample.split(Y = emissionsData.dt$emissionsSum, SplitRatio = 0.7)
trainset <- subset(emissionsData.dt, train == T)
testset <- subset(emissionsData.dt, train == F)


# -------------------------------- Linear Regression  ----------------------------------
# Feature Selection 

linReg <- lm(emissionsSum ~ ., data = trainset)
summary(linReg)

# Stepwise Selection
linRegSelected <- step(linReg)

# Multicollinearity Check
vif(linRegSelected)
summary(linRegSelected)

# Model Diagnostics
par(mfrow=c(2,2))
plot(linRegSelected)
par(mfrow=c(1,1))


# Testset Prediction
linReg.predict <- predict(linRegSelected, newdata = testset)

# -------------------------------- CART Regression  ----------------------------------
CART.train <- rpart(emissionsSum ~., data = trainset, method = 'anova',
            control = rpart.control(minsplit = 2, cp = 0))

printcp(CART.train)

# print(CART.train)
# rpart.plot(CART.train, nn = T)

plotcp(CART.train)

# Extract the Optimal Tree via code instead of eye power 
# Compute min CVerror + 1SE in maximal tree CART.train
CVerror.cap <- (CART.train$cptable[which.min(CART.train$cptable[,"xerror"]), "xerror"] 
              + CART.train$cptable[which.min(CART.train$cptable[,"xerror"]), "xstd"])

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree.
i <- 1; j<- 4
while (CART.train$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region.
cp.opt = ifelse(i > 1, sqrt(CART.train$cptable[i,1] * CART.train$cptable[i-1,1]), 1)

# Pruning the training model
CART.train_pruned <- prune(CART.train, cp = cp.opt)

rpart.plot(CART.train_pruned, nn=TRUE, main = "Optimal Tree in Emissions")

#Testset Predictions
CART.predict = predict(CART.train_pruned, newdata = testset)

# Checking Variable Importance
summary(CART.train_pruned)
CART.train_pruned$variable.importance
# -------------------------------- Random Forest Regression  ----------------------------------
ranFor <- randomForest(emissionsSum ~ . , data = trainset, importance = T)
plot(ranFor)
ranFor

#Testset Predictions
ranFor.predict <- predict(ranFor, newdata = testset)

# Checking variable importance in the model
varImpPlot(ranFor,type=1)
importance(ranFor)

# ------------------------------- Performance Measures ---------------------------------------

# Calculate RMSE
rmse.linReg <- gofRMSE(testset$emissionsSum, linReg.predict)
rmse.cart <- gofRMSE(testset$emissionsSum, CART.predict)
rmse.ranFor <- gofRMSE(testset$emissionsSum, ranFor.predict)

# Calculate R^2
r2.linReg <- gofRSq(testset$emissionsSum, linReg.predict)
r2.cart <- gofRSq(testset$emissionsSum, CART.predict)
r2.ranFor <- gofRSq(testset$emissionsSum, ranFor.predict)

# Calculate MAE
mae.linReg <- round(mae(testset$emissionsSum, linReg.predict),3)
mae.cart <-round(mae(testset$emissionsSum, CART.predict),3)
mae.ranFor <-round(mae(testset$emissionsSum, ranFor.predict),3)

# Print RMSE MAE and R^2 for each model
cat("\n", 
    "RMSE for Linear Regression:", rmse.linReg, "\n",
    "RMSE for CART:", rmse.cart, "\n",
    "RMSE for Random Forest:",rmse.ranFor, "\n", 
    "MAE for Linear Regression:", mae.linReg, "\n",
    "MAE for CART:", mae.cart, "\n",
    "MAE for Random Forest:",mae.ranFor, "\n", 
    "R^2 for Linear Regression:", r2.linReg, "\n", 
    "R^2 for CART:", r2.cart, "\n",
    "R^2 for Random Forest:", r2.ranFor, "\n")

# Visualising the plots

# Creating a function to plot the graphs
plotypredvstrue <- function(ytrue, ypredicted, title = "Predicted vs True Y with Residuals")
{
  data <- data.frame(ytrue, ypredicted);
  r2 <- gofRSq(ytrue, ypredicted)
  # Create the scatter plot
  scatter_plot <- ggplot(data, aes(x = ytrue, y = ypredicted)) +
    geom_point(shape = 1, color = "blue") + 
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") + 
    geom_segment(aes(x = ytrue, xend = ytrue, y = ypredicted, yend = ytrue), linetype = "dotted", color = "red", linewidth = 0.25) + 
    labs(x = "True Y", y = "Predicted Y") + 
    ggtitle(title) + 
    theme_minimal()+
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, label = paste("R2:", r2), size = 5, color = "red");
  # Display the plot
  print(scatter_plot);
}

# Plot for Linear Regression
plotypredvstrue(testset$emissionsSum, linReg.predict, title = "Predicted vs True Y with Residuals for Linear Regression Model")

# Plot for CART Model
plotypredvstrue(testset$emissionsSum, CART.predict, title = "Predicted vs True Y with Residuals for CART Model")

# Plot for Random Forest
plotypredvstrue(testset$emissionsSum, ranFor.predict, title = "Predicted vs True Y with Residuals for Random Forest")


# ========================================================== END =====================================================================

# Save your Random Forest model
saveRDS(ranFor, "random_forest_model.rds")
