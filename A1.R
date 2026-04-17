# ============================================================
# FIT3152 Assignment 1 – Kai Williams (33739374)
# Focus country: Belarus (BLR)
# ============================================================

# ---------- Setup & data creation ---------------------------

rm(list = ls())
set.seed(33739374) # Your Student Number
VCData = read.csv(unz("WVSExtract.csv.zip", "WVSExtract.csv"))
VC = VCData[sample(1:nrow(VCData),100000, replace=FALSE),]
VC = VC[,c(1:3,sort(sample(4:50,25,replace=FALSE)),
           sort(sample(51:65,8,replace=FALSE)))]
write.csv(VC, "FIT3152A1Data_Kai.csv", row.names = FALSE)

# ============================================================
# Q1: Descriptive analysis
# ============================================================

# Dimensions + Structure
dim(VC)
str(VC)

# Replace all -ve values with NA
VC[VC < 0 & VC >= -5] <- NA

# Total and proportion of missing values overall
sum(is.na(VC))
mean(is.na(VC))

num_vars  <- sapply(VC, is.numeric)
num_names <- names(VC)[num_vars]

# Summary statistics table
num_summary <- data.frame(
  variable = num_names,
  mean   = sapply(VC[, num_names, drop = FALSE], mean,     na.rm = TRUE),
  sd     = sapply(VC[, num_names, drop = FALSE], sd,       na.rm = TRUE),
  min    = sapply(VC[, num_names, drop = FALSE], min,       na.rm = TRUE),
  q25    = sapply(VC[, num_names, drop = FALSE], quantile, probs = 0.25, na.rm = TRUE),
  median = sapply(VC[, num_names, drop = FALSE], median,   na.rm = TRUE),
  q75    = sapply(VC[, num_names, drop = FALSE], quantile, probs = 0.75, na.rm = TRUE),
  max    = sapply(VC[, num_names, drop = FALSE], max,       na.rm = TRUE)
)
num_summary

# Non-numeric columns
non_num_vars <- !num_vars
lapply(VC[, non_num_vars, drop = FALSE], function(x) {
  list(
    class = class(x),
    n_unique = length(unique(x))
  )
})

# ============================================================
# Q2: Focus country vs all other countries (no time split)
# ============================================================

VC_BLR <- VC[VC$Country == "BLR", ]
VC_Others <- VC[VC$Country != "BLR", ]

nrow(VC_BLR)
nrow(VC_Others)

# Proportion of total data that is Belarus
nrow(VC_BLR) / nrow(VC)

# ---- Identify predictor and confidence columns ----
conf_cols <- grep("^C", names(VC), value = TRUE)   # all C* variables
conf_cols <- setdiff(conf_cols, "Country")         # drop the country column

pred_cols  <- setdiff(num_names, c("Wave", "Year", conf_cols))

# ---- Q2a: Compare means BLR vs Others ----
options(scipen = 999)

num_summary_fun <- function(df) {
  data.frame(
    variable = num_names,
    mean   = sapply(df[, num_names, drop = FALSE], mean,   na.rm = TRUE),
    median = sapply(df[, num_names, drop = FALSE], median, na.rm = TRUE),
    sd     = sapply(df[, num_names, drop = FALSE], sd,     na.rm = TRUE)
  )
}

BLR_sum    <- num_summary_fun(VC_BLR)
Others_sum <- num_summary_fun(VC_Others)

num_compare <- merge(BLR_sum, Others_sum, by = "variable",
                     suffixes = c("_BLR", "_Others"))
num_compare$mean_diff <- num_compare$mean_BLR - num_compare$mean_Others
num_compare[order(-abs(num_compare$mean_diff)), ]

options(scipen = 1)

# t-tests for each numeric variable (BLR vs Others)
ttest_results <- lapply(num_names, function(v) {
  tt <- t.test(VC_BLR[[v]], VC_Others[[v]])
  data.frame(variable = v, t = tt$statistic, p = tt$p.value,
             mean_BLR = tt$estimate[1], mean_Others = tt$estimate[2])
})
ttest_df <- do.call(rbind, ttest_results)
ttest_df[order(ttest_df$p), ]

# ---- Q2a Plot: -log10(p) significance chart ----
library(ggplot2)

plot_df <- ttest_df[order(ttest_df$p), ]
plot_df$sig      <- plot_df$p < 0.05
plot_df$log10p   <- -log10(plot_df$p)
plot_df$variable <- factor(plot_df$variable, levels = rev(plot_df$variable))

ggplot(plot_df, aes(x = log10p, y = variable, fill = sig)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = -log10(0.05), linetype = "dashed",
             colour = "grey30", linewidth = 0.6) +
  scale_fill_manual(values = c("TRUE" = "#2C7A7A", "FALSE" = "#C0C0C0"),
                    labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05")) +
  labs(
    title    = "T-test: Belarus vs All Other Countries",
    subtitle = "Dashed line marks the p = 0.05 significance threshold",
    x        = expression(-log[10](p)),
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 9)
  )

# ---- Q2b: Linear regression – BLR ----
# For each confidence column, fit lm with all predictor columns
# and extract R-squared + top coefficients

lm_summary <- function(df, conf_cols, pred_cols, alpha = 0.05) {
  lapply(conf_cols, function(cv) {
    formula <- as.formula(paste(cv, "~", paste(pred_cols, collapse = " + ")))
    fit     <- lm(formula, data = df, na.action = na.omit)
    s       <- summary(fit)
    
    # coefficient table
    coefs <- coef(s)  # same as summary(fit)$coefficients
    pvals <- coefs[, "Pr(>|t|)"]
    
    # drop the intercept
    pvals <- pvals[setdiff(names(pvals), "(Intercept)")]
    
    # keep only predictors with p < alpha
    sig_pvals <- pvals[pvals < alpha]
    sig_pvals <- sort(sig_pvals, decreasing = FALSE)
    
    list(
      conf_var  = cv,
      r2        = s$r.squared,
      adj_r2    = s$adj.r.squared,
      sig_preds = sig_pvals   
    )
  })
}

blr_lm  <- lm_summary(VC_BLR,conf_cols, pred_cols)

# Compact R² table
blr_r2 <- data.frame(
  conf_var = sapply(blr_lm, `[[`, "conf_var"),
  R2       = round(sapply(blr_lm, `[[`, "r2"),     3),
  Adj_R2   = round(sapply(blr_lm, `[[`, "adj_r2"), 3)
)
blr_r2[order(-blr_r2$R2), ]

# Print top predictors for the highest R² confidence variable
best_blr <- blr_lm[[which.max(sapply(blr_lm, `[[`, "r2"))]]
cat("Best-predicted confidence var (BLR):", best_blr$conf_var,
    "  R2 =", best_blr$r2, "\n")
best_blr$sig_preds

for (cv in conf_cols) {
  cat("Trying:", cv, "\n")
  form <- as.formula(paste(cv, "~", paste(pred_cols, collapse = " + ")))
  print(try(summary(lm(form, data = VC_BLR, na.action = na.omit)), silent = TRUE))
}
