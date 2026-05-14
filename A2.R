# ============================================================
# FIT3152 Assignment 2 – Kai Williams (33739374)
# ============================================================

# ---------- Setup & data creation ---------------------------

rm(list = ls())
set.seed(33739374) # Your Student ID is the random seed
WD_full = read.csv("WVSBinaryExtract.csv")
selected_cols = c(sample(3:49, 30),sample(50:63, 3))
WD_full = WD_full[c(1:2, selected_cols)]
WD_full = WD_full[sample(nrow(WD), 20000, replace = FALSE),]

summary(WD_full)

# ============================================================
# Q1: Explore the Data
# ============================================================

class_vars <- c("CArmedForces", "CMajComp", "CUnions")

# Proportion of High vs Low for each variable
for (cv in class_vars) {
  tbl <- table(WD[[cv]])
  prop <- prop.table(tbl)
  cat(sprintf("\n%s:\n", cv))
  print(tbl)
  cat(sprintf("Low (0): %.2f%%  |  High (1): %.2f%%\n",
              prop["0"] * 100, prop["1"] * 100))
}

# Descriptions of predictor attributes (non-class columns)
predictor_vars <- setdiff(names(WD_full), c("Country", "Wave", class_vars))
summary(WD_full[, predictor_vars])

# class balance visually
par(mfrow = c(1, length(class_vars)))
for (cv in class_vars) {
  barplot(prop.table(table(WD_full[[cv]])) * 100,
          main = cv,
          xlab = "Confidence",
          ylab = "Percentage (%)",
          col = c("tomato", "steelblue"),
          names.arg = c("Low (0)", "High (1)"),
          ylim = c(0, 100))
}
par(mfrow = c(1, 1))

# ============================================================
# Q2: Pre-processing
# ============================================================

# replace negative values with NA
WD_full[WD_full < 0] <- NA

# convert to factors (for classifiers)
WD_full$CArmedForces <- as.factor(WD_full$CArmedForces)
WD_full$CMajComp     <- as.factor(WD_full$CMajComp)
WD_full$CUnions      <- as.factor(WD_full$CUnions)

# convert sex to factor (categorical predictor)
WD_full$Sex <- as.factor(WD_full$Sex)

# drop Country column
length(unique(WD_full$Country))
WD <- WD_full[, !names(WD_full) %in% c("Country")]

summary(WD)

# ============================================================
# Q3: Train and Test data split
# ============================================================

set.seed(33739374) #Student ID as random seed
train.row = sample(1:nrow(WD), 0.7*nrow(WD))
WD.train = WD[train.row,]
WD.test = WD[-train.row,]

# ============================================================
# Q4: Classification Models
# ============================================================

library(rpart)        # Decision Tree
library(e1071)        # Naive Bayes
library(adabag)       # Bagging + Boosting
library(randomForest) # Random Forest

library(rpart.plot)

# Predictor variables — exclude all three class variables
predictors <- setdiff(names(WD), c("CArmedForces", "CMajComp", "CUnions"))

# Helper to build formula for a given target
make_formula <- function(target) {
  as.formula(paste(target, "~ ."))
}

# Create per-target training sets (drop NAs in the target column only)
train_AF <- WD.train[!is.na(WD.train$CArmedForces), c(predictors, "CArmedForces")]
train_MC <- WD.train[!is.na(WD.train$CMajComp),     c(predictors, "CMajComp")]
train_UN <- WD.train[!is.na(WD.train$CUnions),      c(predictors, "CUnions")]

# Ensure complete cases for adabag methods (they don’t like NAs)
train_AF_adb <- train_AF[complete.cases(train_AF), ]
train_MC_adb <- train_MC[complete.cases(train_MC), ]
train_UN_adb <- train_UN[complete.cases(train_UN), ]

# ---------- Decision Tree ----------
dt_AF <- rpart(make_formula("CArmedForces"), data = train_AF, method = "class")
dt_MC <- rpart(make_formula("CMajComp"),     data = train_MC, method = "class")
dt_UN <- rpart(make_formula("CUnions"),      data = train_UN, method = "class")

rpart.plot(dt_AF, main = "Decision Tree: CArmedForces")
rpart.plot(dt_MC, main = "Decision Tree: CMajComp")
rpart.plot(dt_UN, main = "Decision Tree: CUnions")

# ---------- Naive Bayes ----------
nb_AF <- naiveBayes(make_formula("CArmedForces"), data = train_AF)
nb_MC <- naiveBayes(make_formula("CMajComp"),     data = train_MC)
nb_UN <- naiveBayes(make_formula("CUnions"),      data = train_UN)

# ---------- Bagging ----------
set.seed(33739374)
bag_AF <- bagging(CArmedForces ~ ., data = train_AF_adb, mfinal = 10)
bag_MC <- bagging(CMajComp     ~ ., data = train_MC_adb, mfinal = 10)
bag_UN <- bagging(CUnions      ~ ., data = train_UN_adb, mfinal = 10)

# ---------- Boosting ----------
boost_AF <- boosting(CArmedForces ~ ., data = train_AF_adb, mfinal = 10)
boost_MC <- boosting(CMajComp     ~ ., data = train_MC_adb, mfinal = 10)
boost_UN <- boosting(CUnions      ~ ., data = train_UN_adb, mfinal = 10)

# ---------- Random Forest ----------
rf_AF <- randomForest(make_formula("CArmedForces"), data = train_AF,
                      na.action = na.omit, importance = TRUE)
rf_MC <- randomForest(make_formula("CMajComp"),     data = train_MC,
                      na.action = na.omit, importance = TRUE)
rf_UN <- randomForest(make_formula("CUnions"),      data = train_UN,
                      na.action = na.omit, importance = TRUE)

# ============================================================
# Q5: Predictions, Confusion Matrices & Metrics
# ============================================================

# Create per-target test sets (drop NAs in target column only)
test_AF <- WD.test[!is.na(WD.test$CArmedForces), c(predictors, "CArmedForces")]
test_MC <- WD.test[!is.na(WD.test$CMajComp),     c(predictors, "CMajComp")]
test_UN <- WD.test[!is.na(WD.test$CUnions),      c(predictors, "CUnions")]

# --- Helper: compute metrics from confusion matrix ---
get_metrics <- function(actual, predicted) {
  cm    <- table(Actual = actual, Predicted = predicted)
  TP    <- cm["1", "1"]
  TN    <- cm["0", "0"]
  FP    <- cm["0", "1"]
  FN    <- cm["1", "0"]
  acc   <- (TP + TN) / sum(cm)
  prec  <- TP / (TP + FP)
  rec   <- TP / (TP + FN)
  f1    <- 2 * prec * rec / (prec + rec)
  return(list(CM = cm,
              Accuracy  = round(acc,  4),
              Precision = round(prec, 4),
              Recall    = round(rec,  4),
              F1        = round(f1,   4)))
}

# --- Helper: print results for one class variable ---
print_results <- function(label, actual, preds_list) {
  cat("\n============================================================\n")
  cat("Class Variable:", label, "\n")
  cat("============================================================\n")
  results <- data.frame(Model     = character(),
                        Accuracy  = numeric(),
                        Precision = numeric(),
                        Recall    = numeric(),
                        F1        = numeric(),
                        stringsAsFactors = FALSE)
  for (model_name in names(preds_list)) {
    m <- get_metrics(actual, preds_list[[model_name]])
    cat("\n--", model_name, "--\n")
    print(m$CM)
    cat(sprintf("Accuracy: %.4f | Precision: %.4f | Recall: %.4f | F1: %.4f\n",
                m$Accuracy, m$Precision, m$Recall, m$F1))
    results <- rbind(results, data.frame(Model     = model_name,
                                         Accuracy  = m$Accuracy,
                                         Precision = m$Precision,
                                         Recall    = m$Recall,
                                         F1        = m$F1))
  }
  cat("\n--- Summary Table:", label, "---\n")
  print(results, row.names = FALSE)
  return(results)
}

# ---------- Predictions ----------

# Decision Tree
pred_dt_AF <- predict(dt_AF, test_AF, type = "class")
pred_dt_MC <- predict(dt_MC, test_MC, type = "class")
pred_dt_UN <- predict(dt_UN, test_UN, type = "class")

# Naive Bayes
pred_nb_AF <- predict(nb_AF, test_AF)
pred_nb_MC <- predict(nb_MC, test_MC)
pred_nb_UN <- predict(nb_UN, test_UN)

# Bagging
pred_bag_AF <- predict(bag_AF, test_AF)$class
pred_bag_MC <- predict(bag_MC, test_MC)$class
pred_bag_UN <- predict(bag_UN, test_UN)$class

# Boosting
pred_boost_AF <- predict(boost_AF, test_AF)$class
pred_boost_MC <- predict(boost_MC, test_MC)$class
pred_boost_UN <- predict(boost_UN, test_UN)$class

# Random Forest
pred_rf_AF <- predict(rf_AF, test_AF)
pred_rf_MC <- predict(rf_MC, test_MC)
pred_rf_UN <- predict(rf_UN, test_UN)

# ---------- Results ----------

preds_AF <- list("Decision Tree" = pred_dt_AF,
                 "Naive Bayes"   = pred_nb_AF,
                 "Bagging"       = pred_bag_AF,
                 "Boosting"      = pred_boost_AF,
                 "Random Forest" = pred_rf_AF)

preds_MC <- list("Decision Tree" = pred_dt_MC,
                 "Naive Bayes"   = pred_nb_MC,
                 "Bagging"       = pred_bag_MC,
                 "Boosting"      = pred_boost_MC,
                 "Random Forest" = pred_rf_MC)

preds_UN <- list("Decision Tree" = pred_dt_UN,
                 "Naive Bayes"   = pred_nb_UN,
                 "Bagging"       = pred_bag_UN,
                 "Boosting"      = pred_boost_UN,
                 "Random Forest" = pred_rf_UN)

results_AF <- print_results("CArmedForces", test_AF$CArmedForces, preds_AF)
results_MC <- print_results("CMajComp",     test_MC$CMajComp,     preds_MC)
results_UN <- print_results("CUnions",      test_UN$CUnions,      preds_UN)

# ============================================================
# Q6: ROC curves and AUC
# ============================================================

library(pROC)

# Helper: convert factor 0/1 to numeric 0/1
to01 <- function(x) as.numeric(as.character(x))

# ------------------------------------------------------------
# Get predicted probabilities for class "1" (High confidence)
# ------------------------------------------------------------

## ----- CArmedForces -----

# Decision Tree
prob_dt_AF <- predict(dt_AF,  test_AF, type = "prob")[, "1"]

# Naive Bayes
prob_nb_AF <- predict(nb_AF,  test_AF, type = "raw")[, "1"]

# Bagging (adabag)
pred_bag_AF <- predict(bag_AF, newdata = test_AF)
prob_bag_AF <- pred_bag_AF$prob[, 1]

# Boosting (adabag)
pred_boost_AF <- predict(boost_AF, newdata = test_AF)
prob_boost_AF <- pred_boost_AF$prob[, 1]

# Random Forest
prob_rf_AF <- predict(rf_AF,  test_AF, type = "prob")[, "1"]  


## ----- CMajComp -----

prob_dt_MC <- predict(dt_MC, test_MC, type = "prob")[, "1"]
prob_nb_MC <- predict(nb_MC, test_MC, type = "raw")[, "1"]

pred_bag_MC <- predict(bag_MC, newdata = test_MC)
prob_bag_MC <- pred_bag_MC$prob[, 1]

pred_boost_MC <- predict(boost_MC, newdata = test_MC)
prob_boost_MC <- pred_boost_MC$prob[, 1]

prob_rf_MC <- predict(rf_MC, test_MC, type = "prob")[, "1"]  


## ----- CUnions -----

prob_dt_UN <- predict(dt_UN, test_UN, type = "prob")[, "1"]
prob_nb_UN <- predict(nb_UN, test_UN, type = "raw")[, "1"]

pred_bag_UN <- predict(bag_UN, newdata = test_UN)
prob_bag_UN <- pred_bag_UN$prob[, 1]

pred_boost_UN <- predict(boost_UN, newdata = test_UN)
prob_boost_UN <- pred_boost_UN$prob[, 1]

prob_rf_UN <- predict(rf_UN, test_UN, type = "prob")[, "1"] 

# ============================================================
# 3. ROC curves + AUC for each class variable (pROC)
# ============================================================

# Helper to plot all 5 ROC curves on one plot and return AUCs
plot_roc_set <- function(y_true, probs_list, title_text) {
  # y_true must be numeric 0/1
  cols <- c("red", "blue", "darkgreen", "orange", "purple")
  ltys <- c(1, 2, 3, 4, 5)
  
  i <- 1
  aucs <- data.frame(Model = names(probs_list), AUC = NA_real_)
  
  for (name in names(probs_list)) {
    roc_obj <- roc(y_true, probs_list[[name]], quiet = TRUE)   
    if (i == 1) {
      plot(roc_obj,
           col = cols[i], lty = ltys[i],
           main = title_text,
           print.auc = FALSE, legacy.axes = FALSE,
           xlim = c(0, 1),
           ylim = c(0, 1),
           xaxs = "i", yaxs = "i")
    } else {
      plot(roc_obj, col = cols[i], lty = ltys[i], add = TRUE)
    }
    aucs$AUC[aucs$Model == name] <- as.numeric(auc(roc_obj))   
    i <- i + 1
  }
  abline(a = 0, b = 1, lty = 3, col = "grey")
  legend("topright",
         legend = names(probs_list),
         col = cols, lty = ltys, cex = 0.8)
  return(aucs)
}

# ---------- CArmedForces ----------
probs_AF <- list("Decision Tree" = prob_dt_AF,
                 "Naive Bayes"   = prob_nb_AF,
                 "Bagging"       = prob_bag_AF,
                 "Boosting"      = prob_boost_AF,
                 "Random Forest" = prob_rf_AF)

auc_AF <- plot_roc_set(to01(test_AF$CArmedForces),
                       probs_AF,
                       "ROC — Confidence in Armed Forces (CArmedForces)")

# ---------- CMajComp ----------
probs_MC <- list("Decision Tree" = prob_dt_MC,
                 "Naive Bayes"   = prob_nb_MC,
                 "Bagging"       = prob_bag_MC,
                 "Boosting"      = prob_boost_MC,
                 "Random Forest" = prob_rf_MC)

auc_MC <- plot_roc_set(to01(test_MC$CMajComp),
                       probs_MC,
                       "ROC — Confidence in Major Companies (CMajComp)")

# ---------- CUnions ----------
probs_UN <- list("Decision Tree" = prob_dt_UN,
                 "Naive Bayes"   = prob_nb_UN,
                 "Bagging"       = prob_bag_UN,
                 "Boosting"      = prob_boost_UN,
                 "Random Forest" = prob_rf_UN)

auc_UN <- plot_roc_set(to01(test_UN$CUnions),
                       probs_UN,
                       "ROC — Confidence in Unions (CUnions)")

# View AUC tables
auc_AF
auc_MC
auc_UN

# ============================================================
# Q8: Variable Importance
# ============================================================

# ---------- Bagging ----------
# importance is stored directly as a named vector in the model
vi_bag_AF <- bag_AF$importance
vi_bag_MC <- bag_MC$importance
vi_bag_UN <- bag_UN$importance

# ---------- Boosting ----------
vi_boost_AF <- boost_AF$importance
vi_boost_MC <- boost_MC$importance
vi_boost_UN <- boost_UN$importance

# ---------- Random Forest ----------
vi_rf_AF <- importance(rf_AF)
vi_rf_MC <- importance(rf_MC)
vi_rf_UN <- importance(rf_UN)

# Visual plots for RF
varImpPlot(rf_AF,  n.var = ncol(train_AF) - 1, main = "RF Variable Importance: CArmedForces")
varImpPlot(rf_MC,  n.var = ncol(train_AF) - 1, main = "RF Variable Importance: CMajComp")
varImpPlot(rf_UN,  n.var = ncol(train_AF) - 1, main = "RF Variable Importance: CUnions")

library(dplyr)

make_importance_table <- function(vi_bag, vi_boost, vi_rf) {
  # adabag importance is a named numeric vector
  bag_df   <- data.frame(Attribute = names(vi_bag),
                         Bagging   = round(as.numeric(vi_bag), 4))
  boost_df <- data.frame(Attribute = names(vi_boost),
                         Boosting  = round(as.numeric(vi_boost), 4))
  
  # randomForest importance — use MeanDecreaseGini
  rf_df <- as.data.frame(vi_rf) %>%
    tibble::rownames_to_column("Attribute") %>%
    select(Attribute, RF = MeanDecreaseGini) %>%
    mutate(RF = round(RF, 4))
  
  # Merge all three
  tbl <- bag_df %>%
    full_join(boost_df, by = "Attribute") %>%
    full_join(rf_df,    by = "Attribute") %>%
    arrange(desc(RF))
  
  return(tbl)
}

imp_AF <- make_importance_table(vi_bag_AF, vi_boost_AF, vi_rf_AF)
imp_MC <- make_importance_table(vi_bag_MC, vi_boost_MC, vi_rf_MC)
imp_UN <- make_importance_table(vi_bag_UN, vi_boost_UN, vi_rf_UN)

cat("\n--- Variable Importance: CArmedForces ---\n"); print(imp_AF, row.names = FALSE)
cat("\n--- Variable Importance: CMajComp ---\n");     print(imp_MC, row.names = FALSE)
cat("\n--- Variable Importance: CUnions ---\n");      print(imp_UN, row.names = FALSE)

# ============================================================
# Q10: Improved Bagging Model
# ============================================================

library(caret)
library(ipred)
library(pROC)

# --- Step 1: Feature Selection ---
# Remove the 7 low-importance attributes identified in Q8
low_importance <- c("ACTEnvOrg", "ACTHumanitarian", "ACTProfessional", 
                    "Sex", "ACTArtsEd", "ICQDetermination", "ICQUnselfishness")

# Create reduced predictor sets
predictors_reduced <- setdiff(predictors, low_importance)

# Rebuild training sets with reduced features
train_AF_reduced <- train_AF_adb[, c(predictors_reduced, "CArmedForces")]
train_MC_reduced <- train_MC_adb[, c(predictors_reduced, "CMajComp")]
train_UN_reduced <- train_UN_adb[, c(predictors_reduced, "CUnions")]

test_AF_reduced <- test_AF[, c(predictors_reduced, "CArmedForces")]
test_MC_reduced <- test_MC[, c(predictors_reduced, "CMajComp")]
test_UN_reduced <- test_UN[, c(predictors_reduced, "CUnions")]

# --- Step 2: Improved Bagging with ipred ---
# ipred uses deep, unpruned trees by default

set.seed(33739374)

# Increase nbagg from 10 to 100 
# Use control parameter to ensure deep trees
bag_improved_AF <- bagging(CArmedForces ~ ., 
                           data = train_AF_reduced,
                           nbagg = 500,
                           coob = TRUE)

bag_improved_MC <- bagging(CMajComp ~ ., 
                           data = train_MC_reduced,
                           nbagg = 500,
                           coob = TRUE)

bag_improved_UN <- bagging(CUnions ~ ., 
                           data = train_UN_reduced,
                           nbagg = 500,
                           coob = TRUE)

# --- Step 3: Predictions ---

# Class predictions
pred_bag_improved_AF <- predict(bag_improved_AF, newdata = test_AF_reduced)
pred_bag_improved_MC <- predict(bag_improved_MC, newdata = test_MC_reduced)
pred_bag_improved_UN <- predict(bag_improved_UN, newdata = test_UN_reduced)

# Probability predictions for ROC/AUC
pred_bag_improved_AF_prob <- predict(bag_improved_AF, newdata = test_AF_reduced, 
                                     type = "prob")[, "1"]
pred_bag_improved_MC_prob <- predict(bag_improved_MC, newdata = test_MC_reduced, 
                                     type = "prob")[, "1"]
pred_bag_improved_UN_prob <- predict(bag_improved_UN, newdata = test_UN_reduced, 
                                     type = "prob")[, "1"]

# --- Step 4: Evaluate Performance ---

# Use get_metrics function from Q5

# Calculate metrics
metrics_AF <- get_metrics(test_AF_reduced$CArmedForces, pred_bag_improved_AF)
metrics_MC <- get_metrics(test_MC_reduced$CMajComp,     pred_bag_improved_MC)
metrics_UN <- get_metrics(test_UN_reduced$CUnions,      pred_bag_improved_UN)

cat("\n=== Improved Bagging: CArmedForces ===\n")
print(metrics_AF$CM)
cat(sprintf("Accuracy: %.4f | Precision: %.4f | Recall: %.4f | F1: %.4f\n",
            metrics_AF$Accuracy, metrics_AF$Precision, metrics_AF$Recall, metrics_AF$F1))

cat("\n=== Improved Bagging: CMajComp ===\n")
print(metrics_MC$CM)
cat(sprintf("Accuracy: %.4f | Precision: %.4f | Recall: %.4f | F1: %.4f\n",
            metrics_MC$Accuracy, metrics_MC$Precision, metrics_MC$Recall, metrics_MC$F1))

cat("\n=== Improved Bagging: CUnions ===\n")
print(metrics_UN$CM)
cat(sprintf("Accuracy: %.4f | Precision: %.4f | Recall: %.4f | F1: %.4f\n",
            metrics_UN$Accuracy, metrics_UN$Precision, metrics_UN$Recall, metrics_UN$F1))

# --- Step 5: ROC Curves and AUC ---

# CArmedForces
roc_bag_improved_AF <- roc(to01(test_AF_reduced$CArmedForces), 
                           pred_bag_improved_AF_prob, quiet = TRUE)
auc_bag_improved_AF <- as.numeric(auc(roc_bag_improved_AF))

# CMajComp
roc_bag_improved_MC <- roc(to01(test_MC_reduced$CMajComp), 
                           pred_bag_improved_MC_prob, quiet = TRUE)
auc_bag_improved_MC <- as.numeric(auc(roc_bag_improved_MC))

# CUnions
roc_bag_improved_UN <- roc(to01(test_UN_reduced$CUnions), 
                           pred_bag_improved_UN_prob, quiet = TRUE)
auc_bag_improved_UN <- as.numeric(auc(roc_bag_improved_UN))

cat("\n=== AUC Scores ===\n")
cat(sprintf("CArmedForces: %.4f\n", auc_bag_improved_AF))
cat(sprintf("CMajComp:     %.4f\n", auc_bag_improved_MC))
cat(sprintf("CUnions:      %.4f\n", auc_bag_improved_UN))

# --- Step 6: ROC Comparison Plots (Original vs Improved Bagging) ---

plot_bagging_comparison <- function(y_true, prob_original, prob_improved, 
                                    title_text, auc_original, auc_improved) {
  # Create ROC objects
  roc_original <- roc(y_true, prob_original, quiet = TRUE)
  roc_improved <- roc(y_true, prob_improved, quiet = TRUE)
  
  # Plot original
  plot(roc_original,
       col = "red", lty = 1,
       main = title_text,
       print.auc = FALSE, legacy.axes = FALSE,
       xlim = c(0, 1),
       ylim = c(0, 1),
       xaxs = "i", yaxs = "i")
  
  # Add improved
  plot(roc_improved, col = "blue", lty = 1, add = TRUE)
  
  # Add diagonal reference line
  abline(a = 0, b = 1, lty = 3, col = "grey")
  
  # Legend with AUC values
  legend("topright",
         legend = c(sprintf("Original Bagging (AUC = %.3f)", auc_original),
                    sprintf("Improved Bagging (AUC = %.3f)", auc_improved)),
         col = c("red", "blue"), 
         lty = c(1, 1), 
         cex = 0.8)
}

# Create comparison plots for all three class variables
par(mfrow = c(1, 3))

plot_bagging_comparison(
  y_true = to01(test_AF_reduced$CArmedForces),
  prob_original = prob_bag_AF,
  prob_improved = pred_bag_improved_AF_prob,
  title_text = "ROC Comparison: CArmedForces",
  auc_original = 0.4586,
  auc_improved = auc_bag_improved_AF
)

plot_bagging_comparison(
  y_true = to01(test_MC_reduced$CMajComp),
  prob_original = prob_bag_MC,
  prob_improved = pred_bag_improved_MC_prob,
  title_text = "ROC Comparison: CMajComp",
  auc_original = 0.5583,
  auc_improved = auc_bag_improved_MC
)

plot_bagging_comparison(
  y_true = to01(test_UN_reduced$CUnions),
  prob_original = prob_bag_UN,
  prob_improved = pred_bag_improved_UN_prob,
  title_text = "ROC Comparison: CUnions",
  auc_original = 0.4357,
  auc_improved = auc_bag_improved_UN
)

par(mfrow = c(1, 1))
# --- Step 7: Summary Comparison Table ---

comparison <- data.frame(
  Model = c("Original Bagging", "Improved Bagging"),
  AF_F1 = c(0.7952, metrics_AF$F1),
  AF_AUC = c(0.4586, auc_bag_improved_AF),
  MC_F1 = c(0.5247, metrics_MC$F1),
  MC_AUC = c(0.5583, auc_bag_improved_MC),
  UN_F1 = c(0.1198, metrics_UN$F1),
  UN_AUC = c(0.4357, auc_bag_improved_UN)
)

cat("\n=== Performance Comparison ===\n")
print(comparison, row.names = FALSE)

# ============================================================
# Q11: Artificial Neural Network for Temporal Prediction
# ============================================================

library(neuralnet)
library(dplyr)

# --- Step 1: Identify Country with Most Observations ---

# Assuming your data has a 'Country' variable (check your actual column name)
# If it's Country_ISO, Country_Name, or similar, adjust accordingly
country_counts <- WD %>%
  group_by(Country) %>%  # Replace 'Country' with your actual column name
  summarise(n = n()) %>%
  arrange(desc(n))

print(country_counts)

# Select country with most observations
top_country <- country_counts$Country[1]
cat("\nCountry with most observations:", top_country, 
    "with", country_counts$n[1], "observations\n")

# --- Step 2: Filter Data for Top Country ---

WD_country <- WD %>%
  filter(Country == top_country) %>%  # Replace 'Country' with actual column
  filter(!is.na(Wave))  # Ensure Wave data exists for temporal comparison

# --- Step 3: Data Preprocessing for ANN ---

# Select class variable (using CArmedForces as example - choose based on Q8 insights)
# Use the class variable with best performance from earlier analysis
target_var <- "CArmedForces"

# Remove low-importance attributes identified in Q8
low_importance <- c("ACTEnvOrg", "ACTHumanitarian", "ACTProfessional", 
                    "Sex", "ACTArtsEd", "ICQDetermination", "ICQUnselfishness")

# Keep only high/moderate importance predictors
predictors_ann <- setdiff(predictors_reduced, low_importance)

# Create dataset with complete cases
ann_data <- WD_country %>%
  select(all_of(c(predictors_ann, target_var, "Wave"))) %>%
  na.omit()  # ANNs cannot handle missing values

# Convert target to numeric (0/1) if factor
if(is.factor(ann_data[[target_var]])) {
  ann_data[[target_var]] <- as.numeric(as.character(ann_data[[target_var]]))
}

# --- Step 4: Normalize Numeric Predictors (Critical for ANNs) ---

# Identify numeric columns (exclude binary 0/1 variables)
numeric_cols <- sapply(ann_data[, predictors_ann], function(x) {
  is.numeric(x) && length(unique(x)) > 2
})

# Min-max normalization to [0,1]
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

for(col in names(numeric_cols)[numeric_cols]) {
  ann_data[[col]] <- normalize(ann_data[[col]])
}

# --- Step 5: Split by Wave for Temporal Comparison ---

# Train on earlier waves, test on later wave(s)
waves <- sort(unique(ann_data$Wave))
cat("\nAvailable waves:", waves, "\n")

# Use first N-1 waves for training, last wave for testing
train_waves <- waves[-length(waves)]
test_wave <- waves[length(waves)]

train_ann <- ann_data %>% filter(Wave %in% train_waves)
test_ann <- ann_data %>% filter(Wave == test_wave)

cat(sprintf("\nTraining on waves %s (%d obs)\n", 
            paste(train_waves, collapse=", "), nrow(train_ann)))
cat(sprintf("Testing on wave %s (%d obs)\n", test_wave, nrow(test_ann)))

# --- Step 6: Build ANN Formula ---

# neuralnet requires explicit formula (cannot use ~ .)
formula_ann <- as.formula(paste(target_var, "~", 
                                paste(predictors_ann, collapse = " + ")))

# --- Step 7: Train ANN ---

set.seed(33739374)

ann_model <- neuralnet(
  formula_ann,
  data = train_ann[, c(predictors_ann, target_var)],
  hidden = c(5, 3),        # Two hidden layers: 5 neurons, then 3
  threshold = 0.01,        # Convergence threshold
  stepmax = 1e6,           # Max training iterations
  linear.output = FALSE,   # Use sigmoid activation (classification)
  err.fct = "ce",          # Cross-entropy error
  act.fct = "logistic"     # Logistic activation function
)

# Plot network architecture
plot(ann_model, rep = "best", 
     main = paste("ANN for", target_var, "in", top_country))

# --- Step 8: Make Predictions ---

# Predictions on training set
pred_train_prob <- compute(ann_model, train_ann[, predictors_ann])$net.result
pred_train <- ifelse(pred_train_prob > 0.5, 1, 0)

# Predictions on test set (latest wave)
pred_test_prob <- compute(ann_model, test_ann[, predictors_ann])$net.result
pred_test <- ifelse(pred_test_prob > 0.5, 1, 0)

# --- Step 9: Evaluate Performance ---

# Training performance
train_cm <- table(Actual = train_ann[[target_var]], Predicted = pred_train)
train_acc <- sum(diag(train_cm)) / sum(train_cm)

# Test performance (temporal validation)
test_cm <- table(Actual = test_ann[[target_var]], Predicted = pred_test)
test_acc <- sum(diag(test_cm)) / sum(test_cm)

cat("\n=== ANN Performance ===\n")
cat("\nTraining Confusion Matrix:\n")
print(train_cm)
cat(sprintf("Training Accuracy: %.4f\n", train_acc))

cat("\nTest Confusion Matrix (Wave %s):\n", test_wave)
print(test_cm)
cat(sprintf("Test Accuracy: %.4f\n", test_acc))

# Calculate F1 for test set
TP <- test_cm["1", "1"]
FP <- test_cm["0", "1"]
FN <- test_cm["1", "0"]
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)

cat(sprintf("Test F1-score: %.4f\n", f1))

# --- Step 10: ROC and AUC for Test Set ---

library(pROC)

roc_ann <- roc(test_ann[[target_var]], pred_test_prob[,1], quiet = TRUE)
auc_ann <- as.numeric(auc(roc_ann))

plot(roc_ann, main = paste("ANN ROC Curve -", target_var, "Wave", test_wave),
     legacy.axes = TRUE)
cat(sprintf("Test AUC: %.4f\n", auc_ann))


