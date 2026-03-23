# Q1: Descriptive analysis

rm(list = ls())
set.seed(33739374) # Your Student Number
VCData = read.csv(unz("WVSExtract.csv.zip", "WVSExtract.csv"))
VC = VCData[sample(1:nrow(VCData),100000, replace=FALSE),]
VC = VC[,c(1:3,sort(sample(4:50,25,replace=FALSE)),
           sort(sample(51:65,8,replace=FALSE)))]
write.csv(VC, "FIT3152A1Data_Kai.csv", row.names = FALSE)

# Extract Belarus data
VC_BLR <- VC[VC$Country == "BLR", ]

# Dimensions
dim(VC)
nrow(VC)
ncol(VC)

# Overall structure, variable classes, first values
str(VC)

# ----- Total and proportion of missing values overall -----
# Individual check
sum(VC$ILFam == -4, na.rm = TRUE)

# Replace all -ve values with NA
VC[VC < 0 & VC >= -5] <- NA

# Total and proportion of missing values overall
sum(is.na(VC))
mean(is.na(VC))

# # Missing by variable (for a table or figure)
# missing_by_var <- sapply(VC, function(x) sum(is.na(x)))
# missing_prop_by_var <- sapply(VC, function(x) mean(is.na(x)))
# 
# missing_summary <- data.frame(
#   variable = names(VC),
#   n_missing = missing_by_var,
#   prop_missing = missing_prop_by_var
# )
# 
# # View largest amounts of missingness
# missing_summary[order(-missing_summary$prop_missing), ]



# ----- Identify numeric variables -----
num_vars <- sapply(VC, is.numeric)

# Summary stats for numeric variables
summary(VC[, num_vars])

names(VC)

# Detailed stats (mean, sd, quantiles) in one object
num_summary <- data.frame(
  variable = names(VC)[num_vars],
  mean = sapply(VC[, num_vars, drop = FALSE], mean, na.rm = TRUE),
  sd = sapply(VC[, num_vars, drop = FALSE], sd, na.rm = TRUE),
  min = sapply(VC[, num_vars, drop = FALSE], min, na.rm = TRUE),
  q25 = sapply(VC[, num_vars, drop = FALSE], quantile, probs = 0.25, na.rm = TRUE),
  median = sapply(VC[, num_vars, drop = FALSE], median, na.rm = TRUE),
  q75 = sapply(VC[, num_vars, drop = FALSE], quantile, probs = 0.75, na.rm = TRUE),
  max = sapply(VC[, num_vars, drop = FALSE], max, na.rm = TRUE)
)

num_summary

library(ggplot2)

# Example: histogram of Age
ggplot(VC, aes(x = Age)) +
  geom_histogram(binwidth = 5, colour = "black", fill = "steelblue") +
  theme_minimal()

# Example: barplot for a confidence variable treated as numeric but showing counts
ggplot(VC, aes(x = factor(CGovernment))) +
  geom_bar(fill = "steelblue") +
  xlab("Confidence in Government (coded levels)") +
  theme_minimal()


