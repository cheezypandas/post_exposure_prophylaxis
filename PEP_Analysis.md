Categorical variables use %, other use mean etc... Copy in research questions, Percent and cumulative percent in a table Age: mean and sd, recoded values: percentages, new number (o - etc)


```r
# Load necessary libraries
library(dplyr)
library(stringr)

# Load the library
library(readxl)
# Import data
data <- read_excel("C:/Sandyn/SFGH_PEP/PEP_Data_SFGH_Rajeev_09_Dec_2024.xlsx") 
```

Basic statistics
```r
# Load the required library
library(readxl)

# Load the data
file_path <- "your_file.xlsx"  # Replace with the actual file path
data <- read_excel(file_path)

# Exclude columns Age and ID
exclude_columns <- c("Age", "ID")

# Loop through columns and print unique values & their counts
for (col_name in colnames(data)) {
  if (!(col_name %in% exclude_columns)) {
    cat("\n--- Column:", col_name, "---\n")
    
    # Get the unique values
    unique_values <- unique(data[[col_name]])
    cat("Unique Values:\n")
    print(unique_values)
    
    # Tally counts for each unique value
    cat("\nValue Counts:\n")
    print(table(data[[col_name]], useNA = "ifany"))
  }
}

```

# Extracting Counts For Individual Columns
```r
column_name <- "TIME BETWEEN EVENT AND REPORTING"

# Get unique values and their counts
value_counts <- table(data[[column_name]], useNA = "ifany")

# Print unique values with counts in a copy-paste friendly format
cat("Unique Value: Count\n")
for (i in names(value_counts)) {
  cat(paste0(i, ": ", value_counts[i], "\n"))
}


VARIABLE RECODING
! [1] "Identifier" 
*[2] "Gender"
! [3] "Age"
*[4] "Occupation"
! [5] "Referral Date"
*[6] "WHERE 1ST REPORTED"
*[7] "TIME BETWEEN EVENT AND REPORTING"    
*[8] "WHERE INJURY OCCURRED"
*[9] "CIRCUMSTANCES"
*[10] "SITE OF INJURY"
*[11] "PHYSICIAN REVIEW"
*[12] "TEST DONE"
*[13] "SOURCE TESTED"
*[14] "WEARING GLOVES"
*[15] "PEP"
*[16] "MEDICATIONS"
*[17] "SEROCONVERSION AFTER 6 MONTHS"       
*[18] "OFFICIAL REPORT FILED TO IPC"        
*[19] "OTHER REPORTED OCCUPATIONAL EXPOSURE"

*BINARY*
_Gender, PEP, Physician Review, WEARING GLOVES, SEROCONVERSION AFTER 6 MONTHS,_
_OFFICIAL REPORT FILED TO IPC, OTHER REPORTED OCCUPATIONAL EXPOSURE_


```r
# Load the dplyr package
library(dplyr)

# Assuming your dataframe is called data

data_mutated <- data %>%
  mutate(
    # Convert Gender to numeric (Female = 1, Male = 0)
    Gender = ifelse(Gender == "Female", 1, 0),
    
    # Convert PHYSICIAN REVIEW to numeric (Yes = 1, No = 0)
    `PHYSICIAN REVIEW` = ifelse(`PHYSICIAN REVIEW` == "Yes", 1, 0),
    
    # Convert PEP to numeric (Yes = 1, No = 0)
    PEP = case_when(
      str_to_lower(`PEP`) %in% c("yes", "y") ~ 0,
      str_to_lower(`PEP`) %in% c("no", "not", "not applicable") ~ 1,
      TRUE ~ 1 # not documented
    ),
    
    # Normalize WEARING GLOVES column
    `WEARING GLOVES` = case_when(
      str_to_lower(`WEARING GLOVES`) %in% c("yes", "y") ~ 0,
      str_to_lower(`WEARING GLOVES`) %in% c("no", "not", "not applicable") ~ 1,
      TRUE ~ 2
    ),
    
    # Normalize SEROCONVERSION AFTER 6 MONTHS (example logic; adjust as needed)
    `SEROCONVERSION AFTER 6 MONTHS` = case_when(
      str_to_lower(`SEROCONVERSION AFTER 6 MONTHS`) %in% c("yes", "y") ~ 1,
      str_to_lower(`SEROCONVERSION AFTER 6 MONTHS`) %in% c("no") ~ 0,
    ),
    
    # Normalize OFFICIAL REPORT FILED TO IPC
    `OFFICIAL REPORT FILED TO IPC` = case_when(
      str_to_lower(`OFFICIAL REPORT FILED TO IPC`) %in% c("yes", "y") ~ 0,
      str_to_lower(`OFFICIAL REPORT FILED TO IPC`) == "no" ~ 1,
      TRUE ~ 2
          ),
    
    # Normalize OTHER REPORTED OCCUPATIONAL EXPOSURE
    `OTHER REPORTED OCCUPATIONAL EXPOSURE` = case_when(
      str_to_lower(`OTHER REPORTED OCCUPATIONAL EXPOSURE`) %in% c("yes", "y") ~ 0,
      str_to_lower(`OTHER REPORTED OCCUPATIONAL EXPOSURE`) %in% c("no", "n") ~ 1,
      TRUE ~ 2
    )
  )


# View the mutated dataframe
head(data_mutated$Gender)
head(data_mutated$`PHYSICIAN REVIEW`)
head(data_mutated$`OTHER REPORTED OCCUPATIONAL EXPOSURE`)
head(data_mutated$PEP)
head(data_mutated$`WEARING GLOVES`)
head(data_mutated$`SEROCONVERSION AFTER 6 MONTHS`)
head(data_mutated$`OFFICIAL REPORT FILED TO IPC`)
head(data_mutated$`OTHER REPORTED OCCUPATIONAL EXPOSURE`)

```
*Triple & Quadripple*
_TEST DONE_
```r
data_mutated <- data_mutated %>%
  mutate(
    # Normalize TEST DONE column: remove spaces and replace special characters
    `TEST DONE` = gsub("[+&]", "and", gsub(" ", "", `TEST DONE`))
  ) %>%
  mutate(
    # Assign numerical values directly to TEST DONE based on the groups
    `TEST DONE` = case_when(
      # Group 0: Anything with CBC, LFT, or RFT (regardless of HIV, HepB, HepC, or VDRL)
      str_detect(str_to_lower(`TEST DONE`), "cbc") |
      str_detect(str_to_lower(`TEST DONE`), "lft") |
      str_detect(str_to_lower(`TEST DONE`), "rft") ~ 0,

      # Group 1: Tests with HIV, HepB, HepC, or C but NOT containing VDRL
      (str_detect(str_to_lower(`TEST DONE`), "hiv") &
       str_detect(str_to_lower(`TEST DONE`), "hepb") &
       str_detect(str_to_lower(`TEST DONE`), "hepc") |
       str_detect(str_to_lower(`TEST DONE`), "c")) &
      !str_detect(str_to_lower(`TEST DONE`), "vdrl") ~ 1,

      # Group 2: Tests with HIV, HepB, HepC, or C and containing VDRL
      (str_detect(str_to_lower(`TEST DONE`), "hiv") &
       str_detect(str_to_lower(`TEST DONE`), "hepb") &
       str_detect(str_to_lower(`TEST DONE`), "hepc") |
       str_detect(str_to_lower(`TEST DONE`), "c")) &
      str_detect(str_to_lower(`TEST DONE`), "vdrl") ~ 2,

      # Default to Group 2 if no match (fallback case for anything else)
      TRUE ~ 2
    )
  ) %>%
  mutate(`TEST DONE` = as.numeric(`TEST DONE`))  # Convert TEST DONE to numeric

# View the results for debugging
print(data_mutated$`TEST DONE`)


```

_Where Injury Occured_
```r
# Grouping WHERE INJURY OCCURRED based on specified keywords
# Grouping WHERE INJURY OCCURRED based on specified keywords
data_mutated <- data_mutated %>%
  mutate(
    # Normalize WHERE INJURY OCCURRED column: remove spaces and make lowercase
    `WHERE INJURY OCCURRED` = str_replace_all(str_to_lower(`WHERE INJURY OCCURRED`), " ", ""),
    
    # Assign groups based on keywords
    `WHERE INJURY OCCURRED` = case_when(
      str_detect(`WHERE INJURY OCCURRED`, "ward") ~ 0, # "Ward"
      str_detect(`WHERE INJURY OCCURRED`, "a&e") ~ 1, # "A&E"
      str_detect(`WHERE INJURY OCCURRED`, "surgical|urology|paed|orthop|ent|operatingt") ~ 2, # "Specialty Unit"
      str_detect(`WHERE INJURY OCCURRED`, "unit") ~ 3, # "Other Unit"
      TRUE ~ 4 # "Other"
    )
  )

# View unique results for validation
#print(unique(data_mutated$`WHERE INJURY OCCURRED`))


# View the updated column for verification
print(data_mutated$`WHERE INJURY OCCURRED`)
 
```
_Circumstances_
```r
# Use regex with str_detect to categorize the CIRCUMSTANCES column
data_mutated$CIRCUMSTANCES <- case_when(
  str_detect(str_to_lower(data_mutated$CIRCUMSTANCES), "discard|disposing|throw|disposal") ~ 0, #"Discarding",
  str_detect(str_to_lower(data_mutated$CIRCUMSTANCES), "removing|retrieving|extracting|retrieval|recapping") ~ 1, #"Removing",
  str_detect(str_to_lower(data_mutated$CIRCUMSTANCES), "performing|suturing|surgical|incision|drainage") ~ 2, #"Performing a Procedure",
  str_detect(str_to_lower(data_mutated$CIRCUMSTANCES), "administering|inserting|giving|applying|medication|blood") ~ 2, #"Performing a Procedure",
  str_detect(str_to_lower(data_mutated$CIRCUMSTANCES), "inject|administration|blood|medication|sub-cut|injection") ~ 2, #"Performing a Procedure",
  TRUE ~ 3 #"Other"
)

# Check the results
print(data_mutated$CIRCUMSTANCES)  # View the first few rows of the new 'category' column

```

_Site Of Injury_
```r
# Categorize SITE OF INJURY based on specific finger and left/right hand using regex
data_mutated$`SITE OF INJURY` <- case_when(
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "thumb") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "left") ~ 1, #"Left Thumb",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "thumb") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "right") ~ 2, #"Right Thumb",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "little|pinkie") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "left") ~ 3, #"Left Little Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "little|pinkie") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "right") ~ 4, #"Right Little Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "index") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "left") ~ 5, #"Left Index Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "index") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "right") ~ 6, #"Right Index Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "ring") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "left") ~ 7, #"Left Ring Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "ring") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "right") ~ 8, #"Right Ring Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "middle|third") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "left") ~ 9, #"Left Middle Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "middle|third") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "right") ~ 10, #"Right Middle Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "fourth") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "left") ~ 11, #"Left Fourth Finger",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "fourth") & str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "right") ~ 12, #"Right Fourth Finger",
  
  # Handle general cases where "left" or "right" appears without specific finger names
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "left") ~ 13, #"Left Hand (unspecified)",
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "right") ~ 14, #"Right Hand (unspecified)",
  
  # Handle unspecified or not documented cases
  str_detect(str_to_lower(data_mutated$`SITE OF INJURY`), "not documented") ~ 15, #"Not Documented",
  
  # Catch-all for any other entries
  TRUE ~ 16 #"Other"
)

# Show the results to verify categorization
head(data_mutated$`SITE OF INJURY`)  # View the first few rows of the new 'site_category' column

```

_Source Tested_
```r
# Create a new category based on SOURCE TESTED responses
library(dplyr)
library(stringr)

# Ensure the column exists
data_mutated <- data_mutated %>%
  mutate(`SOURCE TESTED` = case_when(
    str_to_lower(`SOURCE TESTED`) %in% c("yes", "YES") ~ 0,  # Handling Yes
    str_to_lower(`SOURCE TESTED`) == "not documented" ~ 1,   # Not Documented
    str_to_lower(`SOURCE TESTED`) == "unknown" ~ 2,          # Unknown
    str_to_lower(`SOURCE TESTED`) == "no" ~ 3,               # No
    str_to_lower(`SOURCE TESTED`) == "not necessary" ~ 3,    # Not Necessary
    TRUE ~ 4                                                # Other
  ))

# Convert to factor if needed
data_mutated$`SOURCE TESTED` <- factor(data_mutated$`SOURCE TESTED`)


# View the result to verify the categorization
head(data_mutated$`SOURCE TESTED`)

```

_WHERE WHERE 1ST REPORTED_
```r
# Categorize WHERE 1ST REPORTED into the specified groups
data_mutated$`WHERE 1ST REPORTED` <- case_when(
  str_detect(str_to_lower(data_mutated$`WHERE 1ST REPORTED`), "a&e|hospital|sfg|mt\\. hope|point fortin|augustus long") ~ 0, #"A&E or Hospital",
  str_detect(str_to_lower(data_mutated$`WHERE 1ST REPORTED`), "health centre|health facility|facilities") ~ 1, #"Health Centres",
  str_detect(str_to_lower(data_mutated$`WHERE 1ST REPORTED`), "ipc|infectious disease unit") ~ 2, #"IPC Unit",
  TRUE ~ 3, #"Other"
)

# View the categorized data for verification
head(data_mutated$`WHERE 1ST REPORTED`)


```

_TIME BETWEEN EVENT AND REPORTING_
```r
# Categorize TIME BETWEEN EVENT AND REPORTING
data_mutated$`TIME BETWEEN EVENT AND REPORTING` <- case_when(
  str_detect(str_to_lower(data_mutated$`TIME BETWEEN EVENT AND REPORTING`), "immediately") ~ 0, #"Immediately",
  str_detect(str_to_lower(data_mutated$`TIME BETWEEN EVENT AND REPORTING`), "10 minutes|10 mins|15 mins|15 minutes|5 mins") ~ 1, #"45 or less",
  str_detect(str_to_lower(data_mutated$`TIME BETWEEN EVENT AND REPORTING`), "not documented") ~ 2, #"Not documented",
  TRUE ~ 3, #"Other"
)


# View the categorized data for verification
head(data_mutated$`TIME BETWEEN EVENT AND REPORTING`)


```

_Medication_
```r
data_mutated$MEDICATIONS <- case_when(
  str_detect(data_mutated$`MEDICATIONS`, regex("Combivir & Kaletra", ignore_case = TRUE)) ~ 0, #"Combivir & Kaletra",
  str_detect(data_mutated$`MEDICATIONS`, regex("Combivir", ignore_case = TRUE)) ~ 1, #"Combivir",
  str_detect(data_mutated$`MEDICATIONS`, regex("nil|refused|declined", ignore_case = TRUE)) ~ 2, #"Nil added to other",
  TRUE ~ 2, #"Other"
)

head(data_mutated$MEDICATIONS)


```

_Occupation_

```r
# 0 clinical staff, 1 student /trainee, 2 other
data_mutated$Occupation <- case_when(
  str_detect(str_to_lower(data_mutated$`Occupation`), "atn") ~ 0, #"ATN",
  str_detect(str_to_lower(data_mutated$`Occupation`), "daily paid labourer") ~ 2, #"Daily Paid Labourer",
  str_detect(str_to_lower(data_mutated$`Occupation`), "ena") ~ 0, #"ENA",
  str_detect(str_to_lower(data_mutated$`Occupation`), "house officer") ~ 2, #"House Officer",
  str_detect(str_to_lower(data_mutated$`Occupation`), "medical intern") ~ 1, #"Medical Intern",
  str_detect(str_to_lower(data_mutated$`Occupation`), "medical officer") ~ 0, #"Medical Officer",
  str_detect(str_to_lower(data_mutated$`Occupation`), "phlebotomist") ~ 0, #"Phlebotomist",
  str_detect(str_to_lower(data_mutated$`Occupation`), "registered nurse") ~ 0, #"Registered Nurse",
  str_detect(str_to_lower(data_mutated$`Occupation`), "student nurse") ~ 1, #"Student Nurse",
  str_detect(str_to_lower(data_mutated$`Occupation`), "wards maid|wardsmaid") ~ 0, #"Wards Maid",
  TRUE ~ 2 #"Other"
)

head(data_mutated$`Occupation`)


```

```r

head(data_mutated$`TEST DONE`)
head(data_mutated$`WHERE INJURY OCCURRED`)
head(data_mutated$`CIRCUMSTANCES`)
head(data_mutated$`SITE OF INJURY`)
head(data_mutated$`SOURCE TESTED`)
head(data_mutated$`WHERE 1ST REPORTED`)

```


```r
# install.packages("writexl")
library(writexl)

# Save data_mutated to an Excel file
write_xlsx(as.data.frame(data_mutated), "C:/Sandyn/SFGH_PEP/data_mutated_factors.xlsx")

```

# Chi squared tests
```r

# Load necessary libraries
library(readxl)

# Load dataset
data_mutated <- read_excel("C:/Sandyn/SFGH_PEP/data_mutated_factors.xlsx")

# Ensure PEP is a factor, recode levels to be interpretable
data_mutated$PEP <- factor(data_mutated$PEP, levels = c(0, 1), labels = c("Yes", "No"))

# List of categorical variables to analyze
categorical_vars <- c(
  "Gender", "Occupation", "WHERE INJURY OCCURRED", "CIRCUMSTANCES", 
  "SITE OF INJURY", "PHYSICIAN REVIEW", "TEST DONE", 
  "WEARING GLOVES", "MEDICATIONS", "OFFICIAL REPORT FILED TO IPC", 
  "OTHER REPORTED OCCUPATIONAL EXPOSURE"
)

# Function for cross-tabulation and Chi-Square test
cross_tabulation <- function(data, var, target) {
  # Filter for non-missing values
  filtered_data <- data[!is.na(data[[var]]) & !is.na(data[[target]]), ]
  
  # Create cross-tabulation
  tab <- table(filtered_data[[target]], filtered_data[[var]])
  
  # Calculate percentages
  tab_percent <- prop.table(tab, margin = 1) * 100
  
  # Perform Chi-Square test
  chi_sq <- if (all(dim(tab) > 1)) {
    chisq.test(tab)
  } else {
    list(statistic = NA, p.value = NA, parameter = NA)
  }
  
  # Return results
  list(
    variable = var,
    table = tab,
    percentages = tab_percent,
    chi_square_stat = chi_sq$statistic,
    df = chi_sq$parameter,
    p_value = chi_sq$p.value
  )
}

# Analyze each categorical variable
results <- lapply(categorical_vars, function(var) {
  cross_tabulation(data_mutated, var, "PEP")
})

# Display results
for (res in results) {
  cat("\n========================================\n")
  cat("Variable:", res$variable, "\n")
  cat("\nCross-Tabulation (Counts):\n")
  print(res$table)
  cat("\nCross-Tabulation (Percentages):\n")
  print(round(res$percentages, 2))
  cat("\nChi-Square Statistic:", res$chi_square_stat, "\n")
  if (!is.na(res$df)) {
    cat("df:", res$df, "\n")
  } else {
    cat("df: Not applicable (1xN or Nx1 table)\n")
  }
  cat("p-value:", res$p_value, "\n")
}

```

_chi sqared 2_
```r
# Load necessary library
library(dplyr)

# Ensure categorical variables are factors
data_mutated <- data_mutated %>%
  mutate(
    PEP = factor(PEP),
    `SEROCONVERSION AFTER 6 MONTHS` = factor(`SEROCONVERSION AFTER 6 MONTHS`),
    `SOURCE TESTED` = factor(`SOURCE TESTED`),
    `WHERE 1ST REPORTED` = factor(`WHERE 1ST REPORTED`),
    `TIME BETWEEN EVENT AND REPORTING` = factor(`TIME BETWEEN EVENT AND REPORTING`)
  )

# Function to perform Chi-Square Test
perform_chisq_test <- function(var1, var2, data) {
  table_var <- table(data[[var1]], data[[var2]])  # Create contingency table
  chisq_result <- chisq.test(table_var)  # Perform Chi-Square Test
  
  # Print results
  print(paste("Chi-Square Test between", var1, "and", var2))
  print(chisq_result)
  print("Expected Counts:")
  print(chisq_result$expected)
  cat("\n--------------------\n")
}

# Run Chi-Square tests for PEP against the selected variables
perform_chisq_test("PEP", "SEROCONVERSION AFTER 6 MONTHS", data_mutated)
perform_chisq_test("PEP", "SOURCE TESTED", data_mutated)
perform_chisq_test("PEP", "WHERE 1ST REPORTED", data_mutated)
perform_chisq_test("PEP", "TIME BETWEEN EVENT AND REPORTING", data_mutated)


```




``` r
# Load necessary libraries
library(dplyr)
library(MASS)
library(writexl)
library(car)  # For checking multicollinearity
library(brglm2)  # For fitting models with bias reduction

# Assuming your data is in a dataframe called 'data'
# Check if 'data' is a dataframe
if (!is.data.frame(data)) {
  stop("The object 'data' is not a dataframe. Please load your data correctly.")
}

# Convert categorical variables to factors
data_mutated <- data %>%
  mutate(
    `WHERE INJURY OCCURRED` = as.factor(`WHERE INJURY OCCURRED`),
    `CIRCUMSTANCES` = as.factor(`CIRCUMSTANCES`),
    `TEST DONE` = as.factor(`TEST DONE`),
    `WEARING GLOVES` = as.factor(`WEARING GLOVES`),
    PEP = as.factor(PEP)
  )

# Identify and remove aliased coefficients
alias_info <- alias(glm(PEP ~ `WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE` + `WEARING GLOVES`, data = data_mutated, family = binomial))
aliased_predictors <- names(alias_info$Complete)
data_mutated <- data_mutated %>% select(-all_of(aliased_predictors))

# Check for multicollinearity
vif_values <- vif(glm(PEP ~ `WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE` + `WEARING GLOVES`, data = data_mutated, family = binomial))
print(vif_values)

# Main Effects Only Model with bias reduction
main_effects_model <- brglm2::brglm(
  PEP ~ `WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE` + `WEARING GLOVES`, 
  data = data_mutated, 
  family = binomial
)

# Model with Interactions with bias reduction
interaction_model <- brglm2::brglm(
  PEP ~ (`WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE` + `WEARING GLOVES`)^2, 
  data = data_mutated, 
  family = binomial
)

# Simplified Interaction Model with bias reduction
simplified_interaction_model <- brglm2::brglm(
  PEP ~ `WHERE INJURY OCCURRED` * `CIRCUMSTANCES` + `TEST DONE` * `WEARING GLOVES`, 
  data = data_mutated, 
  family = binomial
)

# Function to organize results into a data frame
organize_results <- function(model, model_name) {
  coef_table <- summary(model)$coefficients
  exp_coefficients <- exp(coef_table[, "Estimate"])
  conf_int <- confint(model)
  z_values <- coef_table[, "Estimate"] / coef_table[, "Std. Error"]
  df_values <- rep(1, length(z_values))  # Degrees of freedom for each predictor
  p_values <- coef_table[, "Pr(>|z|)"]
  model_AIC <- AIC(model)
  
  results <- data.frame(
    Model = rep(model_name, length(exp_coefficients)),
    Predictor = rownames(coef_table),
    B = coef_table[, "Estimate"],
    S.E. = coef_table[, "Std. Error"],
    Wald = z_values^2,  # Wald statistic as square of the z-value
    df = df_values,
    p = p_values,
    Exp.B. = exp_coefficients,
    X95.C.I..Lower = conf_int[, 1],
    X95.C.I..Upper = conf_int[, 2],
    AIC = model_AIC
  )
  
  return(results)
}

# Organize results for each model
main_effects_results <- organize_results(main_effects_model, "Main Effects Only Model")
interaction_results <- organize_results(interaction_model, "Model with Interactions")
simplified_interaction_results <- organize_results(simplified_interaction_model, "Simplified Interaction Model")

# Combine results into one data frame
all_results <- bind_rows(main_effects_results, interaction_results, simplified_interaction_results)

# Write results to an Excel file
write_xlsx(all_results, "regression_models_results.xlsx")

```


_Display Beta, Wald, s.e, df, p, CI, AIC and R^2_
# Logistic Regression - Correct PEP
```r

# Load necessary libraries
library(dplyr)
library(MASS)
library(writexl)
library(car)  # For checking multicollinearity
# Main Effects Only Model (no interactions)
main_effects_model <- glm(
  PEP ~ `WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE` + `WEARING GLOVES`, 
  data = data_mutated, 
  family = binomial
)

# Interactions Model (including all 2-way interactions)
interaction_model <- glm(
  PEP ~ (`WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE`)^2, 
  data = data_mutated, 
  family = binomial
)

# Full Model (Main Effects + 2-way interactions + 3-way interaction)
full_interaction_model <- glm(
  PEP ~ (`WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE`)^2 + 
    `WHERE INJURY OCCURRED` * `CIRCUMSTANCES` * `TEST DONE`,
  data = data_mutated,
  family = binomial
)


# Function to organize results into a data frame
organize_results <- function(model, model_name) {
  coef_table <- summary(model)$coefficients
  exp_coefficients <- exp(coef_table[, "Estimate"])
  conf_int <- confint(model)
  z_values <- coef_table[, "Estimate"] / coef_table[, "Std. Error"]
  df_values <- rep(1, length(z_values))  # Degrees of freedom for each predictor
  p_values <- coef_table[, "Pr(>|z|)"]
  model_AIC <- AIC(model)
  
  results <- data.frame(
    Model = rep(model_name, length(exp_coefficients)),
    Predictor = rownames(coef_table),
    B = coef_table[, "Estimate"],
    S.E. = coef_table[, "Std. Error"],
    Wald = z_values^2,  # Wald statistic as square of the z-value
    df = df_values,
    p = p_values,
    Exp.B. = exp_coefficients,
    X95.C.I..Lower = conf_int[, 1],
    X95.C.I..Upper = conf_int[, 2],
    AIC = model_AIC
  )
  
  return(results)
}

# Organize results for each model
main_effects_results <- organize_results(main_effects_model, "Main Effects Only Model")
interaction_results <- organize_results(interaction_model, "Model with Interactions")
simplified_interaction_results <- organize_results(simplified_interaction_model, "Simplified Interaction Model")

# Combine results into one data frame
all_results <- bind_rows(main_effects_results, interaction_results, simplified_interaction_results)

# Write results to an Excel file
#write_xlsx(all_results, "C:/Sandyn/SFGH_PEP/regression_models_results.xlsx")

```

# Logistic Regression: Test.Done
```r
# Load necessary libraries
library(dplyr)
library(MASS)
library(writexl)

# Assuming your data is in a dataframe called 'data_mutated'
# Check if 'data_mutated' is a dataframe
if (!is.data.frame(data_mutated)) {
  stop("The object 'data_mutated' is not a dataframe. Please load your data correctly.")
}

# Convert categorical variables to factors
data_mutated <- data_mutated %>%
  mutate(
    `WHERE INJURY OCCURRED` = as.factor(`WHERE INJURY OCCURRED`),
    `CIRCUMSTANCES` = as.factor(`CIRCUMSTANCES`),
    `TEST DONE` = as.factor(`TEST DONE`),
    `WEARING GLOVES` = as.factor(`WEARING GLOVES`),
    PEP = as.factor(PEP)
  )

# Fit the ordinal logistic regression model
ordinal_model <- polr(`TEST DONE` ~ `WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `WEARING GLOVES`, 
                      data = data_mutated, method = "logistic")

# Print the summary of the model
summary_model <- summary(ordinal_model)
print(summary_model)

# Extract coefficients, standard errors, and other statistics
coef_table <- summary_model$coefficients

# Compute p-values and Exp(B) for odds ratios
z_values <- coef_table[, "Value"] / coef_table[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z_values)))
exp_b <- exp(coef_table[, "Value"])

# Compute AIC
AIC_value <- AIC(ordinal_model)

# Compute McFadden's R²
null_model <- polr(`TEST DONE` ~ 1, data = data_mutated, method = "logistic")
logLik_full <- logLik(ordinal_model)
logLik_null <- logLik(null_model)
mcfadden_r2 <- 1 - (logLik_full / logLik_null)

# Create the results table
result_table <- data.frame(
  Model = rep("Ordinal Logistic Regression", nrow(coef_table)),
  Predictor = rownames(coef_table),
  B = coef_table[, "Value"],
  S.E. = coef_table[, "Std. Error"],
  Wald = z_values^2,  # Wald statistic is z^2
  df = rep(1, nrow(coef_table)),  # df for each predictor is typically 1
  p = p_values,
  Exp.B = exp_b,
  AIC = rep(AIC_value, nrow(coef_table))  # Repeat AIC value for all predictors
)

# Save the results table as an Excel file
write_xlsx(result_table, "C:/Sandyn/SFGH_PEP/ordinal_model_summary.xlsx")

# Print the result table
print(result_table)

# Print additional results
print(paste("AIC:", AIC_value))
print(paste("McFadden R²:", mcfadden_r2))

```


_Saving Table: PEP_
```r
# Fit the models
main_effects_model <- glm(
  PEP ~ WHERE.INJURY.OCCURRED + CIRCUMSTANCES + TEST.DONE, 
  data = data_mutated, 
  family = binomial
)

interaction_model <- glm(
  PEP ~ (WHERE.INJURY.OCCURRED + CIRCUMSTANCES + TEST.DONE)^2, 
  data = data_mutated, 
  family = binomial
)

full_interaction_model <- glm(
  PEP ~ (WHERE.INJURY.OCCURRED + CIRCUMSTANCES + TEST.DONE)^2 + 
    WHERE.INJURY.OCCURRED * CIRCUMSTANCES * TEST.DONE,
  data = data_mutated,
  family = binomial
)

# Extract and combine results
final_results_table <- rbind(
  extract_model_info(main_effects_model, "Main Effects Only"),
  extract_model_info(interaction_model, "Two-Way Interactions"),
  extract_model_info(full_interaction_model, "Full Model (Main + 2-way + 3-way Interactions)")
)

# Save to Excel
write_xlsx(final_results_table, "C:/Sandyn/SFGH_PEP/results_PEP.xlsx")

```


_Saving Table: Test Done_
```r
# Load necessary libraries
library(dplyr)
library(MASS)
library(writexl)

# Assuming your data is in a dataframe called 'data_mutated'
# Check if 'data_mutated' is a dataframe
if (!is.data.frame(data_mutated)) {
  stop("The object 'data_mutated' is not a dataframe. Please load your data correctly.")
}

# Convert categorical variables to factors
data_mutated <- data_mutated %>%
  mutate(
    `WHERE INJURY OCCURRED` = as.factor(`WHERE INJURY OCCURRED`),
    `CIRCUMSTANCES` = as.factor(`CIRCUMSTANCES`),
    `TEST DONE` = as.factor(`TEST DONE`),
    `WEARING GLOVES` = as.factor(`WEARING GLOVES`),
    PEP = as.factor(PEP),
    `SOURCE TESTED` = as.factor(`SOURCE TESTED`)
  )

# Fit the ordinal logistic regression model
ordinal_model <- polr(`SOURCE TESTED` ~ `WHERE INJURY OCCURRED` + `CIRCUMSTANCES` + `TEST DONE`, 
                      data = data_mutated, method = "logistic")

# Print the summary of the model
summary_model <- summary(ordinal_model)
print(summary_model)

# Extract coefficients, standard errors, and other statistics
coef_table <- summary_model$coefficients

# Compute p-values and Exp(B) for odds ratios
z_values <- coef_table[, "Value"] / coef_table[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z_values)))
exp_b <- exp(coef_table[, "Value"])

# Compute AIC
AIC_value <- AIC(ordinal_model)

# Compute McFadden's R²
null_model <- polr(`SOURCE TESTED` ~ 1, data = data_mutated, method = "logistic")
logLik_full <- logLik(ordinal_model)
logLik_null <- logLik(null_model)
mcfadden_r2 <- 1 - (logLik_full / logLik_null)

# Create the results table
result_table <- data.frame(
  Model = rep("Ordinal Logistic Regression", nrow(coef_table)),
  Predictor = rownames(coef_table),
  B = coef_table[, "Value"],
  S.E. = coef_table[, "Std. Error"],
  Wald = z_values^2,  # Wald statistic is z^2
  df = rep(1, nrow(coef_table)),  # df for each predictor is typically 1
  p = p_values,
  Exp.B = exp_b,
  AIC = rep(AIC_value, nrow(coef_table))  # Repeat AIC value for all predictors
)

# Save the results table as an Excel file
write_xlsx(result_table, "ordinal_model_summary.xlsx")

# Print the result table
print(result_table)

# Print additional results
print(paste("AIC:", AIC_value))
print(paste("McFadden R²:", mcfadden_r2))


```




_Saving Table: TEST.DONE_
```r
# Fit the models
main_effects_model <- glm(
  TEST.DONE ~ WHERE.INJURY.OCCURRED + CIRCUMSTANCES + SOURCE.TESTED, 
  data = data_mutated, 
  family = binomial
)

interaction_model <- glm(
  TEST.DONE ~ (WHERE.INJURY.OCCURRED + CIRCUMSTANCES + SOURCE.TESTED)^2, 
  data = data_mutated, 
  family = binomial
)

full_interaction_model <- glm(
  TEST.DONE ~ (WHERE.INJURY.OCCURRED + CIRCUMSTANCES + SOURCE.TESTED)^2 + 
    WHERE.INJURY.OCCURRED * CIRCUMSTANCES * SOURCE.TESTED,
  data = data_mutated,
  family = binomial
)

# Extract and combine results
final_results_table <- rbind(
  extract_model_info(main_effects_model, "Main Effects Only"),
  extract_model_info(interaction_model, "Two-Way Interactions"),
  extract_model_info(full_interaction_model, "Full Model (Main + 2-way + 3-way Interactions)")
)

# Save to Excel
write_xlsx(final_results_table, "C:/Sandyn/SFGH_PEP/results_TEST_DONE.xlsx")

```



# Kaplan Meier Surivial Analysis
_KM Individual Interactions: Age_
```r
# Load necessary libraries
library(survival)
library(survminer)

# Ensure 'TIME BETWEEN EVENT AND REPORTING' is numeric and remove NAs or non-numeric entries
data_mutated$`TIME BETWEEN EVENT AND REPORTING` <- as.numeric(as.character(data_mutated$`TIME BETWEEN EVENT AND REPORTING`))

# Convert PEP to numeric (1 for Yes, 0 for No)
data_mutated$PEP <- ifelse(data_mutated$PEP == "Yes", 1, 0)

# Create Age Groups in the dataset
data_mutated$Age_Group <- cut(
  data_mutated$Age,
  breaks = c(18, 24, 34, 44, 54, Inf),  # Define breakpoints
  labels = c("18-24", "25-34", "35-44", "45-54", "55+"),  # Labels for groups
  right = FALSE  # Interval is [min, max)
)

# Filter valid data (remove rows with NA in time or PEP columns)
data_valid <- data_mutated[!is.na(data_mutated$`TIME BETWEEN EVENT AND REPORTING`) & 
                           !is.na(data_mutated$PEP) &
                           !is.na(data_mutated$Age_Group), ]

# Check for missing values in critical columns
cat("Missing values in 'TIME BETWEEN EVENT AND REPORTING': ", sum(is.na(data_valid$`TIME BETWEEN EVENT AND REPORTING`)), "\n")
cat("Missing values in 'PEP': ", sum(is.na(data_valid$PEP)), "\n")
cat("Missing values in 'Age_Group': ", sum(is.na(data_valid$Age_Group)), "\n")

# Check the distribution of Age_Group
cat("Distribution of Age_Group:\n")
print(table(data_valid$Age_Group))

# Ensure there are no empty age groups
age_group_counts <- table(data_valid$Age_Group)
empty_groups <- names(age_group_counts[age_group_counts == 0])
if (length(empty_groups) > 0) {
  cat("Empty Age Groups: ", paste(empty_groups, collapse = ", "), "\n")
}

# Fit Kaplan-Meier survival model for Age Groups
km_fit_age <- survfit(Surv(`TIME BETWEEN EVENT AND REPORTING`, PEP) ~ Age_Group, data = data_valid)

# Check the Kaplan-Meier fit summary
cat("Kaplan-Meier Fit Summary:\n")
print(summary(km_fit_age))

# Plot the Kaplan-Meier survival curves
ggsurvplot(
  km_fit_age,
  data = data_valid,
  risk.table = TRUE,  # Include risk table
  pval = TRUE,        # Include p-value for log-rank test
  xlab = "Time to PEP (days)",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Age Group",
  palette = "Dark2"
)

```

_KM Individual Interactions: Gender_
```r
# Filter valid data
data_valid_gender <- data_mutated[!is.na(data_mutated$`TIME BETWEEN EVENT AND REPORTING`) &
                                  !is.na(data_mutated$PEP) &
                                  !is.na(data_mutated$Gender), ]

# Convert PEP to numeric (1 for Yes, 0 for No)
data_mutated$PEP <- ifelse(data_mutated$PEP == "Yes", 1, 0)

# Fit Kaplan-Meier survival model for Gender
km_fit_gender <- survfit(Surv(`TIME BETWEEN EVENT AND REPORTING`, PEP) ~ Gender, data = data_valid_gender)

# Plot Kaplan-Meier survival curves for Gender
ggsurvplot(
  km_fit_gender,
  data = data_valid_gender,
  risk.table = TRUE,
  pval = TRUE,
  xlab = "Time to PEP (days)",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Gender",
  palette = "Dark2"
)

```

_KM Individual Interactions: Occupation_

```r
# Create the data frame using the raw data you provided
PEP <- c(0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

Occupation <- factor(c("Clinician", "Other", "Clinician", "Clinician", "Support Staff", 
                       "Support Staff", "Support Staff", "Support Staff", "Clinician", "Support Staff", 
                       "Other", "Support Staff", "Support Staff", "Support Staff", "Other", 
                       "Clinician", "Other", "Other", "Other", "Clinician", "Clinician", 
                       "Other", "Support Staff", "Clinician", "Support Staff", "Clinician", 
                       "Other", "Other", "Support Staff", "Support Staff", "Support Staff", 
                       "Support Staff", "Clinician", "Other", "Support Staff", "Support Staff", 
                       "Clinician", "Support Staff", "Support Staff", "Clinician", "Other", 
                       "Support Staff", "Support Staff", "Clinician", "Other", "Other", 
                       "Clinician", "Support Staff", "Clinician", "Support Staff", "Other", 
                       "Clinician", "Other", "Support Staff", "Support Staff", "Other", 
                       "Support Staff", "Clinician", "Other", "Clinician", "Clinician", 
                       "Other", "Clinician", "Support Staff", "Other", "Other", "Clinician", 
                       "Support Staff", "Other", "Support Staff", "Support Staff", "Other", 
                       "Support Staff", "Clinician", "Clinician", "Other", "Support Staff", 
                       "Support Staff", "Other", "Clinician", "Support Staff", "Support Staff", 
                       "Clinician", "Clinician", "Other", "Other", "Support Staff", "Clinician", 
                       "Other", "Support Staff", "Other", "Other", "Support Staff", "Other", 
                       "Other", "Support Staff", "Clinician", "Support Staff", "Other", "Clinician", 
                       "Other", "Clinician", "Support Staff", "Other", "Support Staff", "Other", 
                       "Support Staff", "Clinician", "Support Staff", "Other", "Support Staff", 
                       "Support Staff", "Clinician", "Support Staff", "Clinician", "Support Staff", 
                       "Other", "Other", "Support Staff", "Clinician", "Other", "Support Staff", 
                       "Clinician", "Other", "Support Staff", "Clinician", "Support Staff", "Clinician", 
                       "Other", "Clinician", "Support Staff", "Clinician", "Clinician", "Support Staff", 
                       "Support Staff", "Other", "Support Staff", "Other", "Support Staff", "Clinician", 
                       "Support Staff", "Clinician", "Clinician", "Other", "Support Staff", "Support Staff", 
                       "Clinician", "Clinician", "Other", "Other", "Support Staff", "Clinician", "Other", 
                       "Support Staff", "Support Staff", "Support Staff", "Clinician", "Clinician", 
                       "Clinician", "Support Staff", "Clinician", "Support Staff", "Support Staff", 
                       "Support Staff", "Support Staff", "Clinician", "Support Staff", "Support Staff", 
                       "Other", "Other", "Support Staff", "Clinician", "Support Staff", "Support Staff", 
                       "Other", "Clinician", "Clinician", "Support Staff", "Clinician", "Clinician", 
                       "Other", "Support Staff", "Other", "Clinician", "Support Staff", "Clinician", 
                       "Support Staff", "Support Staff", "Clinician", "Clinician", "Support Staff", 
                       "Other", "Clinician", "Other", "Other", "Support Staff", "Clinician", 
                       "Clinician", "Clinician", "Clinician", "Clinician", "Other", "Support Staff", 
                       "Clinician", "Support Staff", "Support Staff", "Clinician", "Other"))

TIME <- c(3, 3, 2, 2, 2, 2, 2, 1, 3, 3, 1, 2, 3, 2, 2, 2, 2, 1, 2, 3, 3, 2, 1, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 
          2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 1, 1, 2, 2, 1, 3, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 0, 0, 
          2, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 0, 1, 1, 2, 2, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 1, 3, 
          2, 2, 1, 2, 0, 2, 3, 1, 2, 3, 2, 2, 2, 3, 0, 0, 2, 3, 2, 1, 2, 3, 2, 0, 0, 2, 2, 2, 2, 0, 2, 2, 2, 2, 
          0, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 0, 2, 2, 3, 2, 1, 0, 2, 3, 0, 0, 0)

# Combine the data into a data frame
raw_data <- data.frame(PEP, Occupation, TIME)

```

```r

# Convert PEP to numeric (1 for Yes, 0 for No)
data_mutated$$PEP <- ifelse(data_mutated$PEP == "Yes", 1, 0)

# Check the distribution of Occupation
cat("Distribution of Occupation:\n")
print(table(data_valid$Occupation))

# Ensure there are no empty age groups
Occupation_counts <- table(data_valid$Occupation)
empty_groups <- names(Occupation_counts[Occupation_counts == 0])
if (length(empty_groups) > 0) {
  cat("Empty Age Groups: ", paste(empty_groups, collapse = ", "), "\n")
}


# Fit Kaplan-Meier survival model for Occupation
km_fit_Occupation <- survfit(Surv(`TIME BETWEEN EVENT AND REPORTING`, PEP) ~ Occupation, data = data_mutated)

# Check the Kaplan-Meier fit summary
cat("Kaplan-Meier Fit Summary:\n")
print(summary(km_fit_Occupation))

# Plot Kaplan-Meier survival curves for Occupation
ggsurvplot(
  km_fit_Occupation,
  data = data_valid_Occupation,
  risk.table = TRUE,
  pval = TRUE,
  xlab = "Time to PEP (days)",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Occupation",
  palette = "Dark2"
)



```

_KM Individual Interactions: Where Injury Occured_
```r
# Ensure column names are valid for non-standard evaluation
colnames(data_valid_injury) <- make.names(colnames(data_valid_injury))

# Convert PEP to numeric (1 for Yes, 0 for No)
data_mutated$PEP <- ifelse(data_mutated$PEP == "Yes", 1, 0)

# Verify that the dataset contains valid data
print(head(data_valid_injury))

# Refit Kaplan-Meier survival model using modified column names
km_fit_injury <- survfit(Surv(TIME.BETWEEN.EVENT.AND.REPORTING, PEP) ~ WHERE.INJURY.OCCURRED, 
                         data = data_valid_injury)

# Check for success
print(summary(km_fit_injury))

# Define a custom palette for the number of levels in 'WHERE INJURY OCCURRED'
custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(unique(data_valid_injury$WHERE.INJURY.OCCURRED)))

# Plot Kaplan-Meier survival curves
ggsurvplot(
  km_fit_injury,
  data = data_valid_injury,
  risk.table = TRUE,
  pval = TRUE,
  xlab = "Time to PEP (days)",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Where Injury Occurred",
  palette = custom_palette  # Use custom palette
)

```

_KM Individual Interactions: Circumstances_
```r
# Load necessary libraries
library(survival)
library(survminer)

# Ensure column names are valid for non-standard evaluation
colnames(data_mutated) <- make.names(colnames(data_mutated))

# Convert PEP to numeric (1 for Yes, 0 for No)
data_mutated$PEP <- ifelse(data_mutated$PEP == "Yes", 1, 0)

# Ensure 'TIME.BETWEEN.EVENT.AND.REPORTING' is numeric
data_mutated$TIME.BETWEEN.EVENT.AND.REPORTING <- as.numeric(data_mutated$TIME.BETWEEN.EVENT.AND.REPORTING)

# Filter valid data (remove rows with NA in relevant columns)
data_valid_circumstances <- data_mutated[
  !is.na(data_mutated$TIME.BETWEEN.EVENT.AND.REPORTING) &
    !is.na(data_mutated$PEP) &
    !is.na(data_mutated$CIRCUMSTANCES), 
]

# Ensure 'CIRCUMSTANCES' is treated as a factor
data_valid_circumstances$CIRCUMSTANCES <- as.factor(data_valid_circumstances$CIRCUMSTANCES)

# Check levels and distribution in 'CIRCUMSTANCES'
cat("Levels in CIRCUMSTANCES:\n")
print(levels(data_valid_circumstances$CIRCUMSTANCES))
cat("Distribution of CIRCUMSTANCES:\n")
print(table(data_valid_circumstances$CIRCUMSTANCES))

# Check if any group in 'CIRCUMSTANCES' has no events
for (level in levels(data_valid_circumstances$CIRCUMSTANCES)) {
  cat(paste("CIRCUMSTANCES =", level, ": Events =", 
            sum(data_valid_circumstances$PEP[data_valid_circumstances$CIRCUMSTANCES == level]), 
            "\n"))
}

# Fit Kaplan-Meier survival model for CIRCUMSTANCES
km_fit_circumstances <- survfit(
  Surv(TIME.BETWEEN.EVENT.AND.REPORTING, PEP) ~ CIRCUMSTANCES, 
  data = data_valid_circumstances
)

# Check the Kaplan-Meier fit summary
cat("Kaplan-Meier Fit Summary:\n")
print(summary(km_fit_circumstances))

# Define a custom palette for the number of levels in 'CIRCUMSTANCES'
custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(levels(data_valid_circumstances$CIRCUMSTANCES)))

# Plot Kaplan-Meier survival curves
ggsurvplot(
  km_fit_circumstances,
  data = data_valid_circumstances,
  risk.table = TRUE,  # Include risk table
  pval = TRUE,        # Include p-value for log-rank test
  xlab = "Time to PEP (days)",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Circumstances",
  palette = custom_palette  # Use custom palette
)

```

_Saving Image_
```r
# Save the plot as a JPEG file
jpeg("Kaplan_Meier_Survival_Curve.jpg", width = 800, height = 600)

# Recreate the Kaplan-Meier plot
plot(
  km_fit, 
  col = rainbow(length(levels(data_mutated$Group))), 
  main = "Kaplan-Meier Survival Curve by Combined Group", 
  xlab = "Time to PEP (days)", 
  ylab = "Survival Probability",
  lwd = 2
)
legend(
  "topright", 
  legend = levels(data_mutated$Group), 
  col = rainbow(length(levels(data_mutated$Group))), 
  lwd = 2
)

# Close the device to save the file
dev.off()

```


# Cox PH Hazards
_Circumstances, Where Injury Occured_
```r
# Load necessary libraries
library(survival)
library(survminer)

# Ensure column names are valid for non-standard evaluation
colnames(data_mutated) <- make.names(colnames(data_mutated))

# Filter valid data (remove rows with missing values in relevant columns)
data_valid <- data_mutated[!is.na(data_mutated$TIME.BETWEEN.EVENT.AND.REPORTING) &
                           !is.na(data_mutated$PEP), ]

# Ensure significant variables are treated appropriately
data_valid$WHERE.INJURY.OCCURRED <- as.factor(data_valid$WHERE.INJURY.OCCURRED)
data_valid$CIRCUMSTANCES <- as.factor(data_valid$CIRCUMSTANCES)

# Fit Cox Proportional Hazards model with significant variables
cox_model_significant <- coxph(
  Surv(TIME.BETWEEN.EVENT.AND.REPORTING, PEP) ~ CIRCUMSTANCES + WHERE.INJURY.OCCURRED,
  data = data_valid
)

# Summary of the model
summary(cox_model_significant)

# Check proportional hazards assumption
cox_zph_significant <- cox.zph(cox_model_significant)
print(cox_zph_significant)

# Plot Schoenfeld residuals to assess proportional hazards assumption
ggcoxzph(cox_zph_significant)

# Visualize the hazard ratios (forest plot)
ggforest(cox_model_significant, data = data_valid)

```

# Ensure the response variable (PEP) is a factor
# data_mutated$PEP <- as.factor(data_mutated$PEP)

# Normality Test for all continuous variables
# Continuous variables: Age, TIME BETWEEN EVENT AND REPORTING, OTHER REPORTED OCCUPATIONAL EXPOSURE, etc.
shapiro.test(data_mutated$Age)
shapiro.test(data_mutated$`TIME BETWEEN EVENT AND REPORTING`)
shapiro.test(data_mutated$`TIME BETWEEN EVENT AND REPORTING`)


_Most Significant: Circumstances, Age, Where Injury Occured_
```r

# Load necessary libraries
library(survival)
library(survminer)

# Ensure column names are valid for non-standard evaluation
colnames(data_mutated) <- make.names(colnames(data_mutated))

# Filter valid data (remove rows with missing values in relevant columns)
data_valid <- data_mutated[!is.na(data_mutated$TIME.BETWEEN.EVENT.AND.REPORTING) &
                           !is.na(data_mutated$PEP), ]

# Ensure significant variables are treated appropriately
data_valid$WHERE.INJURY.OCCURRED <- as.factor(data_valid$WHERE.INJURY.OCCURRED)
data_valid$CIRCUMSTANCES <- as.factor(data_valid$CIRCUMSTANCES)

# Fit Cox Proportional Hazards model with significant variables
cox_model_significant <- coxph(
  Surv(TIME.BETWEEN.EVENT.AND.REPORTING, PEP) ~ CIRCUMSTANCES + WHERE.INJURY.OCCURRED + Age,
  data = data_valid
)

# Summary of the model
summary(cox_model_significant)

# Check proportional hazards assumption
cox_zph_significant <- cox.zph(cox_model_significant)
print(cox_zph_significant)

# Plot Schoenfeld residuals to assess proportional hazards assumption
ggcoxzph(cox_zph_significant)

# Visualize the hazard ratios (forest plot)
ggforest(cox_model_significant, data = data_valid)

```
