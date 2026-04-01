# Q1: Descriptive analysis

rm(list = ls())
set.seed(33739374) # Your Student Number
VCData = read.csv(unz("WVSExtract.csv.zip", "WVSExtract.csv"))
VC = VCData[sample(1:nrow(VCData),100000, replace=FALSE),]
VC = VC[,c(1:3,sort(sample(4:50,25,replace=FALSE)),
           sort(sample(51:65,8,replace=FALSE)))]
write.csv(VC, "FIT3152A1Data_Kai.csv", row.names = FALSE)

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

# Extract Belarus data and all other countries into 2 data frames
VC_BLR <- VC[VC$Country == "BLR", ]
VC_Others <- VC[VC$Country != "BLR", ]

# Sample sizes
nrow(VC_BLR)
nrow(VC_Others)

# Proportion of total data that is Belarus
nrow(VC_BLR) / nrow(VC)

# Quick summaries
summary(VC_BLR$Country)
summary(VC_Others$Country)

# Identify numeric variables
num_vars <- sapply(VC, is.numeric)
num_names <- names(VC)[num_vars]

# Function to get summary stats for a data frame
num_summary_fun <- function(df) {
  data.frame(
    variable = num_names,
    mean = sapply(df[, num_names, drop = FALSE], mean, na.rm = TRUE),
    median = sapply(df[, num_names, drop = FALSE], median, na.rm = TRUE),
    sd = sapply(df[, num_names, drop = FALSE], sd, na.rm = TRUE)
  )
}

BLR_num_summary    <- num_summary_fun(VC_BLR)
Others_num_summary <- num_summary_fun(VC_Others)

# Merge into one table with differences
num_compare <- merge(
  BLR_num_summary, Others_num_summary,
  by = "variable", suffixes = c("_BLR", "_Others")
)

# Add mean difference (Belarus - Others)
num_compare$mean_diff <- num_compare$mean_BLR - num_compare$mean_Others

# View the largest differences in means
num_compare[order(-abs(num_compare$mean_diff)), ]



