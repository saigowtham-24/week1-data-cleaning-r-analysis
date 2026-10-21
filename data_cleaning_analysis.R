# Week 1 Task: Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic passenger training dataset from the CRAN titanic package
# Student: SONTYANA SAI GOWTHAM
# Submission date: 21/10/2026
#
# This script downloads/loads the public dataset, checks data quality,
# handles selected missing values, screens for outliers, transforms data,
# performs exploratory analysis, saves plots, and exports a cleaned CSV.
#
# IMPORTANT: Run this script in R or RStudio. Review the actual outputs before
# writing findings in the report. Do not report results that you have not verified.

# ----------------------------- 1. Setup ------------------------------------

required_packages <- c("titanic", "dplyr", "ggplot2", "tidyr", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(titanic)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

set.seed(123)

# Create folders for outputs
if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists(file.path("outputs", "plots"))) {
  dir.create(file.path("outputs", "plots"), recursive = TRUE)
}

# ------------------------ 2. Load the dataset -------------------------------

data("titanic_train", package = "titanic")
df_raw <- titanic_train

cat("\n--- Dataset dimensions ---\n")
print(dim(df_raw))

cat("\n--- Variable names ---\n")
print(names(df_raw))

cat("\n--- Structure ---\n")
str(df_raw)

cat("\n--- First six rows ---\n")
print(head(df_raw))

cat("\n--- Summary ---\n")
print(summary(df_raw))

# Save package/version information for reproducibility
writeLines(capture.output(sessionInfo()),
           con = file.path("outputs", "session_info.txt"))

# ----------------------- 3. Missingness audit -------------------------------

missing_count <- colSums(is.na(df_raw))
missing_summary <- data.frame(
  variable = names(df_raw),
  missing_n = as.integer(missing_count),
  missing_pct = round(100 * as.integer(missing_count) / nrow(df_raw), 2)
) |>
  arrange(desc(missing_n))

cat("\n--- Missing values before cleaning ---\n")
print(missing_summary)
write.csv(missing_summary, file.path("outputs", "missing_values_before.csv"),
          row.names = FALSE)

# ------------------------- 4. Duplicate checks ------------------------------

cat("\n--- Exact duplicate row count ---\n")
print(sum(duplicated(df_raw)))

# Check for values that should not be negative
range_checks <- data.frame(
  variable = c("Age", "Fare", "SibSp", "Parch"),
  negative_count = c(
    sum(df_raw$Age < 0, na.rm = TRUE),
    sum(df_raw$Fare < 0, na.rm = TRUE),
    sum(df_raw$SibSp < 0, na.rm = TRUE),
    sum(df_raw$Parch < 0, na.rm = TRUE)
  )
)
cat("\n--- Negative-value checks ---\n")
print(range_checks)

# ---------------------- 5. Handle missing values ----------------------------

df <- df_raw

# Preserve whether cabin information was present before dropping raw Cabin.
df$CabinKnown <- factor(
  ifelse(is.na(df$Cabin) | trimws(as.character(df$Cabin)) == "",
         "Unknown", "Known"),
  levels = c("Unknown", "Known")
)

# Median imputation for numeric variables.
age_median <- median(df$Age, na.rm = TRUE)
fare_median <- median(df$Fare, na.rm = TRUE)

df$Age[is.na(df$Age)] <- age_median
df$Fare[is.na(df$Fare)] <- fare_median

# Mode imputation for Embarked, if missing values are present.
get_mode <- function(x) {
  observed <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(observed) == 0) return(NA_character_)
  names(which.max(table(observed)))
}

embarked_mode <- get_mode(df$Embarked)
if (!is.na(embarked_mode)) {
  missing_embarked <- is.na(df$Embarked) |
    trimws(as.character(df$Embarked)) == ""
  df$Embarked[missing_embarked] <- embarked_mode
}

cat("\n--- Missing values after selected imputation ---\n")
missing_after <- data.frame(
  variable = names(df),
  missing_n = as.integer(colSums(is.na(df))),
  missing_pct = round(100 * as.integer(colSums(is.na(df))) / nrow(df), 2)
) |>
  arrange(desc(missing_n))
print(missing_after)
write.csv(missing_after, file.path("outputs", "missing_values_after.csv"),
          row.names = FALSE)

# ------------------------- 6. Outlier screening -----------------------------

iqr_outlier_report <- function(x, variable_name) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  iqr_value <- q[2] - q[1]
  lower_fence <- q[1] - 1.5 * iqr_value
  upper_fence <- q[2] + 1.5 * iqr_value
  flagged <- !is.na(x) & (x < lower_fence | x > upper_fence)

  data.frame(
    variable = variable_name,
    lower_fence = lower_fence,
    upper_fence = upper_fence,
    outlier_count = sum(flagged),
    outlier_pct = round(100 * sum(flagged) / sum(!is.na(x)), 2)
  )
}

outlier_summary <- bind_rows(
  iqr_outlier_report(df$Age, "Age"),
  iqr_outlier_report(df$Fare, "Fare")
)

cat("\n--- IQR outlier screening ---\n")
print(outlier_summary)
write.csv(outlier_summary, file.path("outputs", "outlier_summary.csv"),
          row.names = FALSE)

p_fare_box <- ggplot(df, aes(y = Fare)) +
  geom_boxplot() +
  labs(title = "Fare: IQR Outlier Screening", y = "Fare") +
  theme_minimal()

ggsave(file.path("outputs", "plots", "fare_boxplot.png"),
       p_fare_box, width = 7, height = 4.5, dpi = 300)

# ---------------------- 7. Encoding and scaling -----------------------------

df$Survived <- factor(df$Survived, levels = c(0, 1),
                      labels = c("No", "Yes"))
df$Pclass <- factor(df$Pclass, levels = c(1, 2, 3),
                    labels = c("First", "Second", "Third"))
df$Sex <- factor(df$Sex)
df$Embarked <- factor(df$Embarked)
df$CabinKnown <- factor(df$CabinKnown)

# Min-max scaling
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

df$Age_minmax <- minmax(df$Age)
df$Fare_minmax <- minmax(df$Fare)

# Z-score standardization
df$Age_z <- as.numeric(scale(df$Age))
df$Fare_z <- as.numeric(scale(df$Fare))

# Example dummy-variable matrix. Keep it separate from df to avoid
# unintentionally adding model.matrix's intercept to the exported table.
dummy_matrix <- model.matrix(~ Sex + Pclass + Embarked, data = df)[, -1, drop = FALSE]
write.csv(head(dummy_matrix, 10),
          file.path("outputs", "dummy_encoding_example_first10.csv"),
          row.names = FALSE)

# ------------------------ 8. Descriptive statistics -------------------------

numeric_variables <- c("Age", "Fare", "SibSp", "Parch")

numeric_stats <- data.frame(
  variable = numeric_variables,
  mean = sapply(df[numeric_variables], mean, na.rm = TRUE),
  median = sapply(df[numeric_variables], median, na.rm = TRUE),
  sd = sapply(df[numeric_variables], sd, na.rm = TRUE),
  min = sapply(df[numeric_variables], min, na.rm = TRUE),
  max = sapply(df[numeric_variables], max, na.rm = TRUE)
)

cat("\n--- Numerical descriptive statistics ---\n")
print(numeric_stats)
write.csv(numeric_stats, file.path("outputs", "numeric_descriptive_statistics.csv"),
          row.names = FALSE)

cat("\n--- Sex frequency ---\n")
print(table(df$Sex, useNA = "ifany"))

cat("\n--- Passenger class frequency ---\n")
print(table(df$Pclass, useNA = "ifany"))

cat("\n--- Survival proportions ---\n")
print(round(prop.table(table(df$Survived)), 4))

# ---------------------------- 9. Visualizations -----------------------------

p_age <- ggplot(df, aes(x = Age)) +
  geom_histogram(bins = 30, color = "white") +
  labs(title = "Distribution of Passenger Age", x = "Age", y = "Count") +
  theme_minimal()

p_fare <- ggplot(df, aes(x = Fare)) +
  geom_histogram(bins = 30, color = "white") +
  scale_x_continuous(trans = scales::log1p_trans()) +
  labs(title = "Distribution of Fare (log1p axis)",
       x = "Fare (log1p scale)", y = "Count") +
  theme_minimal()

p_survival <- ggplot(df, aes(x = Survived)) +
  geom_bar() +
  labs(title = "Passenger Survival Counts", x = "Survival", y = "Count") +
  theme_minimal()

p_sex_survival <- ggplot(df, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Proportion by Sex",
       x = "Sex", y = "Proportion") +
  theme_minimal()

p_class_survival <- ggplot(df, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Proportion by Passenger Class",
       x = "Passenger Class", y = "Proportion") +
  theme_minimal()

ggsave(file.path("outputs", "plots", "age_distribution.png"),
       p_age, width = 7, height = 4.5, dpi = 300)
ggsave(file.path("outputs", "plots", "fare_distribution.png"),
       p_fare, width = 7, height = 4.5, dpi = 300)
ggsave(file.path("outputs", "plots", "survival_counts.png"),
       p_survival, width = 7, height = 4.5, dpi = 300)
ggsave(file.path("outputs", "plots", "survival_by_sex.png"),
       p_sex_survival, width = 7, height = 4.5, dpi = 300)
ggsave(file.path("outputs", "plots", "survival_by_class.png"),
       p_class_survival, width = 7, height = 4.5, dpi = 300)

# -------------------------- 10. Correlation ---------------------------------

numeric_df <- df[, numeric_variables]
cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs", method = "pearson")

cat("\n--- Pearson correlation matrix ---\n")
print(round(cor_matrix, 3))
write.csv(round(cor_matrix, 3), file.path("outputs", "correlation_matrix.csv"))

cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("Variable1", "Variable2", "Correlation")

p_cor <- ggplot(cor_long, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(title = "Pearson Correlation Matrix", x = NULL, y = NULL) +
  theme_minimal()

ggsave(file.path("outputs", "plots", "correlation_heatmap.png"),
       p_cor, width = 6.5, height = 5.5, dpi = 300)

# ----------------------- 11. Analysis-ready table ---------------------------

analysis_df <- df |>
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked,
         CabinKnown, Age_minmax, Fare_minmax, Age_z, Fare_z)

cat("\n--- Cleaned analysis table structure ---\n")
str(analysis_df)

cat("\n--- Cleaned analysis table summary ---\n")
print(summary(analysis_df))

write.csv(analysis_df, file.path("outputs", "titanic_cleaned_analysis.csv"),
          row.names = FALSE)

# ------------------------------- 12. Finish ---------------------------------

cat("\nAnalysis complete.\n")
cat("Generated files are in the 'outputs' folder.\n")
cat("Review all outputs and plots before writing report conclusions.\n")
