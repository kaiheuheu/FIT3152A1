# ============================================================
# FIT3152 Assignment 3 – Kai Williams (33739374)
# ============================================================

# ---------- Setup & data creation ---------------------------

rm(list = ls())
.rs.restartR()
set.seed(33739374) # Your Student ID is the random seed
WD_full = read.csv("WVSBinaryExtract.csv")
selected_cols = c(sample(3:49, 30),sample(50:63, 3))
WD_full = WD_full[c(1:2, selected_cols)]
WD_full = WD_full[sample(nrow(WD_full), 20000, replace = FALSE),]

summary(WD_full)

# ============================================================
# Q1: Explore the Data
# ============================================================