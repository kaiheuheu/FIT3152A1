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
# dim(VC)
# str(VC)

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

blr_lm <- lm_summary(VC_BLR,conf_cols, pred_cols)

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

# for (cv in conf_cols) {
#   cat("Trying:", cv, "\n")
#   form <- as.formula(paste(cv, "~", paste(pred_cols, collapse = " + ")))
#   print(summary(lm(form, data = VC_BLR, na.action = na.omit)))
# }

# ---- Q2c: Linear regression – Others ----
others_lm <- lm_summary(VC_Others, conf_cols, pred_cols, 0.05/length(pred_cols))
others_r2  <- data.frame(
  conf_var = sapply(others_lm, `[[`, "conf_var"),
  R2       = round(sapply(others_lm, `[[`, "r2"),     3),
  Adj_R2   = round(sapply(others_lm, `[[`, "adj_r2"), 3)
)
others_r2[order(-others_r2$R2), ]

# Print top predictors for the highest R² confidence variable
best_others <- others_lm[[which.max(sapply(others_lm, `[[`, "r2"))]]
cat("Best-predicted confidence var (Others):", best_others$conf_var,
    "  R2 =", best_others$r2, "\n")
best_others$sig_preds

# Comparison table BLR vs Others R²
r2_compare <- merge(blr_r2, others_r2, by = "conf_var", suffixes = c("_BLR", "_Others"))
r2_compare$R2_diff <- r2_compare$R2_BLR - r2_compare$R2_Others
r2_compare[order(-r2_compare$R2_BLR), ]

# ============================================================
# Q3: Focus country vs all other countries OVER TIME
# ============================================================

# Using Wave as time variable (more regular than Year)
waves <- sort(unique(VC$Wave))
years <- sort(unique(VC$Year))

# ---- Q3a: Mean responses per Wave --------------------------
# Compute wave-level means for all numeric variables, by group
wave_means <- function(df, group_label) {
  do.call(rbind, lapply(split(df, df$Wave), function(w) {
    row <- c(Wave = w$Wave[1], Group = group_label,
             sapply(w[, num_names, drop = FALSE], mean, na.rm = TRUE))
    as.data.frame(t(row), stringsAsFactors = FALSE)
  }))
}

# Replace string "NaN" with NA, then convert to numeric for all numeric columns
blr_waves    <- wave_means(VC_BLR,    "Belarus")
blr_waves[, num_names] <- lapply(blr_waves[, num_names], as.numeric)
others_waves <- wave_means(VC_Others, "Others")
others_waves[, num_names] <- lapply(others_waves[, num_names], as.numeric)
all_waves    <- rbind(blr_waves, others_waves)

# For each variable, count how many waves have non-NAN values
valid_vars <- sapply(num_names, function(v) {
  sum(is.na(blr_waves[[v]])) >= 2   # TRUE if at least 2 waves with data
})

vars_to_test  <- names(valid_vars)[!valid_vars]    
vars_excluded <- names(valid_vars)[valid_vars]

vars_to_test
vars_excluded

VC_BLR_kw <- VC_BLR[, setdiff(names(VC_BLR), vars_excluded)]

# Kruskal-Wallis test per variable for BLR across waves
kw_blr <- lapply(vars_to_test, function(v) {
  kt <- kruskal.test(VC_BLR_kw[[v]] ~ factor(VC_BLR$Wave))
  data.frame(variable = v, chi_sq = kt$statistic, p = kt$p.value)
})

kw_blr_df <- do.call(rbind, kw_blr)
kw_blr_df[order(kw_blr_df$p), ]

# Same test for Others
kw_others <- lapply(num_names, function(v) {
  kt <- kruskal.test(VC_Others[[v]] ~ factor(VC_Others$Wave))
  data.frame(variable = v, chi_sq = kt$statistic, p = kt$p.value)
})
kw_others_df <- do.call(rbind, kw_others)
kw_others_df[order(kw_others_df$p), ]

# --- Graphic Q3a: most interesting variable over time ----

# Variables to plot (Lowest 2 p-values for BLR and Others respectively)
vars_to_plot <- c("PolPetition", "PolScale", "ILReligion", "ICQIndependence")

# Label for each variable (for cleaner facet titles)
var_labels <- c(
  PolPetition      = "Signing Petitions (Belarus top)",
  PolScale         = "Political Scale (Belarus top)",
  ILReligion       = "Importance of Religion (Others top)",
  ICQIndependence  = "Child Quality: Independence (Others top)"
)

# Reshape to long format
plot_df <- all_waves[, c("Wave", "Group", vars_to_plot)]
plot_df <- pivot_longer(plot_df,
                        cols      = all_of(vars_to_plot),
                        names_to  = "variable",
                        values_to = "value")
plot_df$Wave     <- as.integer(plot_df$Wave)
plot_df$value    <- as.numeric(plot_df$value)
plot_df$variable <- factor(plot_df$variable,
                           levels = vars_to_plot,
                           labels = var_labels)

# Plot
ggplot(plot_df, aes(x = Wave, y = value, colour = Group, group = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = unique(plot_df$Wave)) +
  labs(
    title    = "Mean Response by Wave: Belarus vs Others",
    subtitle = "Top 2 most changed variables for each group",
    x        = "Wave",
    y        = "Mean Response",
    colour   = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

# ---- Q3b: R² per wave – how predictive power changes ----
# For each wave, fit lm for each confidence variable; record R²

r2_by_wave <- function(df, group_label, conf_cols, pred_cols, wave_means_df) {
  do.call(rbind, lapply(split(df, df$Wave), function(w) {
    wave_id <- w$Wave[1]
    
    # Find the row in wave_means_df corresponding to this wave and group
    wave_row <- wave_means_df[wave_means_df$Wave == wave_id &
                                wave_means_df$Group == group_label, ]
    
    # Keep only pred_cols that are non-NA in this wave's means
    valid_preds <- pred_cols[sapply(pred_cols, function(p) {
      p %in% names(wave_row) && !is.na(wave_row[[p]])
    })]
    
    do.call(rbind, lapply(conf_cols, function(cv) {
      # Also skip if response variable is all NA in this wave
      if (sum(!is.na(w[[cv]])) < length(valid_preds) + 2) {
        return(data.frame(Wave = wave_id, Group = group_label,
                          conf_var = cv, R2 = NA))
      }
      
      formula <- as.formula(paste(cv, "~", paste(valid_preds, collapse = " + ")))
      fit <- tryCatch(
        lm(formula, data = w, na.action = na.omit),
        error = function(e) NULL
      )
      
      if (is.null(fit)) return(data.frame(Wave = wave_id, Group = group_label,
                                          conf_var = cv, R2 = NA))
      
      data.frame(Wave     = wave_id,
                 Group    = group_label,
                 conf_var = cv,
                 R2       = summary(fit)$r.squared)
    }))
  }))
}

blr_r2w    <- r2_by_wave(VC_BLR,    "Belarus", conf_cols, pred_cols, blr_waves)
others_r2w <- r2_by_wave(VC_Others, "Others",  conf_cols, pred_cols, others_waves)
r2w_all    <- rbind(blr_r2w, others_r2w)


# Show overall trend
aggregate(R2 ~ Wave + Group, data = r2w_all, FUN = mean)

# Top 3 confidence variables with greatest R² variability across waves in BLR
r2_var_blr <- tapply(blr_r2w$R2, blr_r2w$conf_var, var, na.rm = TRUE)
top_conf_3  <- names(head(sort(r2_var_blr, decreasing = TRUE), 3))
top_conf_3

# Use all conf_vars, not just top_conf
plot_heat <- r2w_all
plot_heat$Wave <- as.factor(plot_heat$Wave)

ggplot(plot_heat, aes(x = Wave, y = conf_var, fill = R2)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(is.na(R2), "N/A", round(R2, 2))),
            size = 2.8) +
  scale_fill_gradient(low = "#f7f7f7", high = "#2166ac",
                      na.value = "grey90", limits = c(0, 0.31)) +
  facet_wrap(~ Group, ncol = 2) +
  labs(title    = "R² Heatmap: Model Fit Across Confidence Variables and Waves",
       subtitle = "Darker = higher R²; N/A = no data for that wave",
       x = "Wave", y = "Confidence Variable", fill = "R²") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 8),
        strip.text  = element_text(face = "bold"))

# Top predictors per wave for BLR using valid_preds
top_preds_by_wave <- function(df, group_label, conf_var, pred_cols, wave_means_df, n_top = 3) {
  do.call(rbind, lapply(split(df, df$Wave), function(w) {
    wave_id <- w$Wave[1]
    
    # Find the row in wave_means_df for this wave and group
    wave_row <- wave_means_df[wave_means_df$Wave == wave_id &
                                wave_means_df$Group == group_label, ]
    
    # Keep only pred_cols that are non-NA in this wave's means
    valid_preds <- pred_cols[sapply(pred_cols, function(p) {
      p %in% names(wave_row) && !is.na(wave_row[[p]])
    })]
    
    formula <- as.formula(paste(conf_var, "~", paste(valid_preds, collapse = " + ")))
    fit <- tryCatch(
      lm(formula, data = w, na.action = na.omit),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    
    s    <- summary(fit)
    coefs <- coef(s)[-1, , drop = FALSE]   # drop intercept
    
    # Sort by absolute t-value, take top n
    top <- head(sort(abs(coefs[, "t value"]), decreasing = TRUE), n_top)
    
    data.frame(
      Wave      = wave_id,
      Group     = group_label,
      conf_var  = conf_var,
      predictor = names(top),
      abs_t     = as.numeric(top)
    )
  }))
}

# Top predictors per wave for each of the top 3 confidence variables
blr_top_preds <- do.call(rbind, lapply(top_conf_3, function(cv) {
  top_preds_by_wave(VC_BLR, "Belarus", cv, pred_cols, blr_waves)
}))

others_top_preds <- do.call(rbind, lapply(top_conf_3, function(cv) {
  top_preds_by_wave(VC_Others, "Others", cv, pred_cols, others_waves)
}))

all_top_preds <- rbind(blr_top_preds, others_top_preds)
