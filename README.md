# Week 1: Data Cleaning and Preliminary Analysis with R

**Student:** SONTYANA SAI GOWTHAM  
**Submission date:** 21/10/2026

## Included files
- `titanic_cleaned_analysis.csv` — cleaned analysis table.
- `plots/` — generated data visualizations.
- `outputs/` — missingness audits, outlier report, descriptive statistics, correlation matrix, and the cleaned CSV.

## Dataset
The Titanic passenger training dataset (891 records) with fields including survival, passenger class, sex, age, family counts, fare, cabin, and embarkation port. The source table contains missing values in Age, Cabin, and Embarked.

## Cleaning steps
- Created `CabinKnown` to preserve whether cabin information was available.
- Filled missing Age and Fare values with their medians.
- Filled missing Embarked values with its most frequent observed category.
- Screened Age and Fare for IQR outliers; flagged values were not automatically deleted.
- Encoded categorical variables and created min-max and z-score scaled features.
- Produced descriptive statistics, correlations, and plots.

## Important execution note
The CSV and plots in this package were generated in this environment using a Python implementation of the same documented cleaning workflow because R/Rscript is not installed in the execution environment. They are real outputs from the Titanic data, but they are **not proof that the `.R` script itself ran successfully**. Run `data_cleaning_analysis.R` in RStudio before claiming R execution in your submission.
