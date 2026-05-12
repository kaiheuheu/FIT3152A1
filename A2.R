# ============================================================
# FIT3152 Assignment 2 – Kai Williams (33739374)
# ============================================================

# ---------- Setup & data creation ---------------------------

rm(list = ls())
set.seed(33739374) # Your Student ID is the random seed
WD = read.csv("WVSBinaryExtract.csv")
selected_cols = c(sample(3:49, 30),sample(50:63, 3))
WD = WD[c(1:2, selected_cols)]
WD = WD[sample(nrow(WD), 20000, replace = FALSE),]

summary(WD)

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
predictor_vars <- setdiff(names(WD), c("Country", "Wave", class_vars))
summary(WD[, predictor_vars])

# class balance visually
par(mfrow = c(1, length(class_vars)))
for (cv in class_vars) {
  barplot(prop.table(table(WD[[cv]])) * 100,
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
WD[WD < 0] <- NA

# convert to factors (for classifiers)
WD$CArmedForces <- as.factor(WD$CArmedForces)
WD$CMajComp     <- as.factor(WD$CMajComp)
WD$CUnions      <- as.factor(WD$CUnions)

# convert sex to factor (categorical predictor)
WD$Sex <- as.factor(WD$Sex)

# drop Country column
length(unique(WD$Country))
WD <- WD[, !names(WD) %in% c("Country")]

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
rf_AF <- randomForest(make_formula("CArmedForces"), data = train_AF, na.action = na.omit)
rf_MC <- randomForest(make_formula("CMajComp"),     data = train_MC, na.action = na.omit)
rf_UN <- randomForest(make_formula("CUnions"),      data = train_UN, na.action = na.omit)

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

# --- 1. Build per-target test sets (same idea as Q5) ---
test_AF <- WD.test[!is.na(WD.test$CArmedForces), c(predictors, "CArmedForces")]
test_MC <- WD.test[!is.na(WD.test$CMajComp),     c(predictors, "CMajComp")]
test_UN <- WD.test[!is.na(WD.test$CUnions),      c(predictors, "CUnions")]

# Helper: convert factor 0/1 to numeric 0/1
to01 <- function(x) as.numeric(as.character(x))

# ------------------------------------------------------------
# 2. Get predicted probabilities for class "1" (High confidence)
# ------------------------------------------------------------

## ----- CArmedForces -----

# Decision Tree
prob_dt_AF <- predict(dt_AF,  test_AF, type = "prob")[, "1"]

# Naive Bayes
prob_nb_AF <- predict(nb_AF,  test_AF, type = "raw")[, "1"]

# Bagging (adabag)
pred_bag_AF <- predict(bag_AF, newdata = test_AF)
prob_bag_AF <- pred_bag_AF$prob[, "1"]

# Boosting (adabag)
pred_boost_AF <- predict(boost_AF, newdata = test_AF)
prob_boost_AF <- pred_boost_AF$prob[, "1"]

# Random Forest
prob_rf_AF <- predict(rf_AF,  test_AF, type = "prob")[, "1"]  # [web:34][web:30]


## ----- CMajComp -----

prob_dt_MC <- predict(dt_MC, test_MC, type = "prob")[, "1"]
prob_nb_MC <- predict(nb_MC, test_MC, type = "raw")[, "1"]

pred_bag_MC <- predict(bag_MC, newdata = test_MC)
prob_bag_MC <- pred_bag_MC$prob[, "1"]

pred_boost_MC <- predict(boost_MC, newdata = test_MC)
prob_boost_MC <- pred_boost_MC$prob[, "1"]

prob_rf_MC <- predict(rf_MC, test_MC, type = "prob")[, "1"]    # [web:34][web:30]


## ----- CUnions -----

prob_dt_UN <- predict(dt_UN, test_UN, type = "prob")[, "1"]
prob_nb_UN <- predict(nb_UN, test_UN, type = "raw")[, "1"]

pred_bag_UN <- predict(bag_UN, newdata = test_UN)
prob_bag_UN <- pred_bag_UN$prob[, "1"]

pred_boost_UN <- predict(boost_UN, newdata = test_UN)
prob_boost_UN <- pred_boost_UN$prob[, "1"]

prob_rf_UN <- predict(rf_UN, test_UN, type = "prob")[, "1"]    # [web:34][web:30]

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
    roc_obj <- roc(y_true, probs_list[[name]], quiet = TRUE)   # [web:24][web:28]
    if (i == 1) {
      plot(roc_obj,
           col = cols[i], lty = ltys[i],
           main = title_text,
           print.auc = FALSE, legacy.axes = TRUE)
    } else {
      plot(roc_obj, col = cols[i], lty = ltys[i], add = TRUE)
    }
    aucs$AUC[aucs$Model == name] <- as.numeric(auc(roc_obj))   # [web:24][web:26]
    i <- i + 1
  }
  abline(a = 0, b = 1, lty = 3, col = "grey")
  legend("bottomright",
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

