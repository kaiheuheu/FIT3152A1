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

# ----- Identify numeric variables -----
num_vars <- sapply(VC, is.numeric)

# Summary stats for numeric variables
summary(VC[, num_vars])

names(VC)

# Detailed stats (mean, sd, quantiles)
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

# Identify non-numeric variables (factor, character)
non_num_vars <- !num_vars

# Number of unique values
lapply(VC[, non_num_vars, drop = FALSE], function(x) {
  list(
    class = class(x),
    n_unique = length(unique(x))
  )
})

# Q2: Focus country vs all other countries as a group (independent of time)


