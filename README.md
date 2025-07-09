# post_exposure_prophylaxis
Data Preprocessing and Statistical Analysis


# PEP (Post-Exposure Prophylaxis) Analysis Project

## 📌 Research Questions
1. What are the demographic and occupational characteristics of individuals receiving PEP?
2. What factors influence the time between injury event and PEP reporting?
3. Which variables are significantly associated with PEP administration?
4. How do different injury circumstances and locations affect PEP outcomes?

## 📊 Statistical Approach

### Descriptive Statistics
- **Categorical variables**: Reported as percentages and cumulative percentages
- **Continuous variables**: Reported as mean ± standard deviation (e.g., Age)
- **Recoded values**: Original categories converted to numerical codes with percentage distributions

### Analytical Methods
1. **Chi-Square Tests**  
   - Assess associations between PEP status and categorical predictors
   - Reported with χ² statistic, degrees of freedom, and p-values

2. **Logistic Regression**  
   - Models PEP status as binary outcome
   - Reports coefficients (B), standard errors, Wald statistics, odds ratios (Exp(B)), and 95% CIs
   - Model fit assessed via AIC

3. **Ordinal Logistic Regression**  
   - Used for ordered categorical outcomes (e.g., TEST DONE categories)
   - Reports McFadden's R² for model fit

4. **Survival Analysis**  
   - Kaplan-Meier curves for time-to-event analysis
   - Log-rank tests for group comparisons
   - Cox proportional hazards models for multivariate analysis

## 💻 Code Structure

### Data Preparation
```r
# Load and inspect data
library(readxl)
data <- read_excel("path/to/data.xlsx")

# Basic variable inspection
for(col in colnames(data)) {
  if(!col %in% c("Age","ID")) {
    print(table(data[[col]], useNA = "ifany"))
  }
}
```


## 🔍 Key Findings
Descriptive Statistics
Age: Mean ± SD = XX ± XX years

Gender: XX% female, XX% male

Occupation:

Clinical staff: XX%

Students: XX%

Other: XX%

Analytical Results
Significant predictors of PEP:

Occupation (χ²=XX, p<0.XX)

Injury location (HR=XX, 95% CI XX-XX)

Circumstances (HR=XX, 95% CI XX-XX)

Time to PEP:

Median time = XX days

Significant differences by occupation (log-rank p=0.XX)

## 📂 Output Files
data_mutated_factors.xlsx: Cleaned/recoded dataset

regression_models_results.xlsx: Model coefficients and fit statistics

Kaplan_Meier_*.jpg: Survival curve visualizations

## 📦 Dependencies
```r
install.packages(c("dplyr", "survival", "survminer", "readxl", "writexl", "stringr"))
```


