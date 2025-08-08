# Full Name: Souad Atigi
# Date: 08/08/2025

#### 1. Upload the data

# Upload the dataset
patient_data <- read.csv("/Users/macos/Downloads/patient_info.csv")

# Verify data loaded correctly
print(dim(patient_data))

# Show the head of data (first rows)
print(head(patient_data))

# Show the structure of the data
str(patient_data)

# Show the summary statistics
print(summary(patient_data))

#### 2. Set Project Directory

# Check current working directory
getwd()

# Create project subfolders
dir.create("data")     # For storing raw or cleaned data files
dir.create("script")   # For saving R scripts  
dir.create("results")  # For saving analysis outputs
dir.create("plots")    # For saving plots

# Verify folders created
print(list.files())

#### 3. Data Manipulation

# Extract the entire age column and show first 5 values
age_values <- patient_data$age
print("First 5 values from age column:")
print(age_values[1:5])

# Extract the entire gender column (the 3rd one) and show first 5 values
gender_column <- patient_data[, 3]  # 3rd column
print("First 5 values from gender column:")
print(gender_column[1:5])

# Extract the entire first row (all data for patient 1)
first_patient <- patient_data[1, ]  # Row 1, all columns
print("All data for first patient:")
print(first_patient)

# Extract the diagnosis of the 2nd patient
patient2_diagnosis <- patient_data[2, 4]  # Row 2, Column 4
print(paste("Patient 2 diagnosis:", patient2_diagnosis))

# Select the first 3 patients
first_three <- patient_data[1:3, ] # rows from 1 to 3, all columns
print("First 3 patients:")
print(first_three)

# Select all the patients with age > 50
older_patients <- patient_data[patient_data$age > 50, ]
print("Patients older than 50:")
print(nrow(older_patients))
print(older_patients)

#### 4. Remove rows and columns from data

# Remove the bmi column 
no_bmi <- patient_data[, !names(patient_data) %in% "bmi"]
print("Dataset with BMI column removed:")
print(head(no_bmi))

# Remove multiple columns: bmi and smoker columns
no_bmi_smoker <- patient_data[, !names(patient_data) %in% c("bmi", "smoker")]
print("Dataset with BMI and smoker columns removed:")
print(head(no_bmi_smoker))

# Remove the first patient from the data
no_first_patient <- patient_data[-1, ]  # Remove row 1
print("Dataset with first patient removed:")
print(head(no_first_patient))

# Remove first two patients:  1 & 2
no_first_two <- patient_data[-c(1,2), ]  # Remove rows 1 and 2
print("Dataset with first two patients removed:")
print(head(no_first_two))

# Remove all the young patients from the data (all under 30)
no_young <- patient_data[patient_data$age >= 30, ]  # Keep only age +30
print("Dataset with patients under 30 removed:")
print(head(no_young))


#### 5. Select multiple columns

# Select only the patient id, age and gender (select columns by name)
demographics <- patient_data[, c("patient_id", "age", "gender")]
print("Demographics only (by column name):")
print(head(demographics))

# Select only patient_id, gender and bmi (select columns by position)
cols_1_3_5 <- patient_data[, c(1, 3, 5)]  # Columns 1, 3, and 5
print("Columns 1, 3, and 5 (by position):")
print(head(cols_1_3_5))

# Select the first 4 columns from the data
first_four <- patient_data[, 1:4]  # Columns 1 through 4
print("First 4 columns (by range):")
print(head(first_four))

# Create clinical subset using subset function
clinical_info <- subset(patient_data, select = c("diagnosis", "bmi", "smoker"))
print("Clinical information (using subset):")
print(head(clinical_info))

# Remove the first column: patient_id from the data
no_id <- patient_data[, -1]  # Remove first column (patient_id)
print("Dataset without patient ID:")
print(head(no_id))


#### 6. Data Type Operations and Conversions 

# Check original data types
print(sapply(patient_data, class))

# Create working copy
clean_data <- patient_data

# Convert patient_id from factor to character (IDs should be character, not factor)
clean_data$patient_id <- as.character(clean_data$patient_id)

# Show factor levels for existing factors
print(levels(clean_data$gender))

# Check what diagnosis values we actually have
print(unique(clean_data$diagnosis))

# Reorder levels based on actual data (Normal first, then Cancer)
clean_data$diagnosis <- factor(clean_data$diagnosis,
                               levels = c("Normal", "Cancer"))
print(levels(clean_data$diagnosis))

# Show smoker levels
print(levels(clean_data$smoker))

# Create binary smoking status (1 for "Yes", 0 for "No")
clean_data$smoking_binary <- ifelse(clean_data$smoker == "Yes", 1, 0)
clean_data$smoking_binary <- as.factor(clean_data$smoking_binary)
print(head(clean_data))

# Create binary diagnosis status (0 = Normal, 1 = Cancer)
clean_data$diagnosis_binary <- ifelse(clean_data$diagnosis == "Cancer", 1, 0)
clean_data$diagnosis_binary <- as.factor(clean_data$diagnosis_binary)
print(head(clean_data))

# Create binary gender status (0 = Male, 1 = Female)
clean_data$gender_binary <- ifelse(clean_data$gender == "Female", 1, 0)
clean_data$gender_binary <- as.factor(clean_data$gender_binary)
print(head(clean_data))

# Convert age to categories
clean_data$age_group <- cut(clean_data$age,
                            breaks = c(0, 30, 50, 70, 100),
                            labels = c("Young", "Adult", "Middle_age", "Senior"))
print(table(clean_data$age_group))

# Final cleaned data structure
str(clean_data)

# Show the head of the cleaned data
print(head(clean_data))


#### 7. Create Visualizations

# Create the table objects first
gender_counts <- table(clean_data$gender)
diagnosis_counts <- table(clean_data$diagnosis)
smoking_counts <- table(clean_data$smoker)
age_group_counts <- table(clean_data$age_group)

# Plot 1: Age distribution
png("plots/age_histogram.png", width = 600, height = 400)
hist(clean_data$age, 
     main = "Distribution of Patient Ages", 
     xlab = "Age", 
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
dev.off()

# Plot 2: Gender distribution
png("plots/gender_barplot.png", width = 600, height = 400)
gender_counts <- table(clean_data$gender)
barplot(gender_counts, 
        main = "Distribution of Gender", 
        xlab = "Gender", 
        ylab = "Count",
        col = c("pink", "lightblue"))
dev.off()

# Plot 3: Diagnosis distribution
png("plots/diagnosis_barplot.png", width = 600, height = 400)
diagnosis_counts <- table(clean_data$diagnosis)
barplot(diagnosis_counts, 
        main = "Distribution of Diagnosis", 
        xlab = "Diagnosis", 
        ylab = "Count",
        col = c("lightgreen", "red"))
dev.off()

# Plot 4: BMI vs Age scatter plot
png("plots/bmi_age_scatter.png", width = 600, height = 400)
plot(clean_data$age, clean_data$bmi,
     main = "BMI vs Age Relationship",
     xlab = "Age (years)",
     ylab = "BMI",
     col = "blue",
     pch = 16)
dev.off()

# Plot 5: Smoking status pie chart
png("plots/smoking_piechart.png", width = 600, height = 400)
smoking_counts <- table(clean_data$smoker)
pie(smoking_counts, 
    main = "Smoking Status Distribution",
    col = c("lightcoral", "lightgray"),
    labels = paste(names(smoking_counts), "\n", smoking_counts, "patients"))
dev.off()

# Plot 6: Age groups distribution
png("plots/age_groups_barplot.png", width = 600, height = 400)
age_group_counts <- table(clean_data$age_group)
barplot(age_group_counts, 
        main = "Distribution of Age Groups", 
        xlab = "Age Group", 
        ylab = "Count",
        col = rainbow(length(age_group_counts)))
dev.off()


#### 8. Exporting Results

# Save cleaned dataset as CSV 
write.csv(clean_data, file = "data/patient_info_clean.csv", row.names = FALSE)

# Save demographic subset
write.csv(demographics, file = "results/demographics.csv", row.names = FALSE)

# Save clinical subset
write.csv(clinical_info, file = "results/clinical_data.csv", row.names = FALSE)

# Create and save summary statistics
summary_stats <- data.frame(
  Variable = c("Total Patients", "Average Age", "Age Range", "Male Count", "Female Count", 
               "Cancer Cases", "Normal Cases", "Smokers", "Non-smokers", "Average BMI"),
  Value = c(nrow(clean_data),
            round(mean(clean_data$age), 1),
            paste(min(clean_data$age), "-", max(clean_data$age)),
            sum(clean_data$gender == "Male"),
            sum(clean_data$gender == "Female"),
            sum(clean_data$diagnosis == "Cancer"),
            sum(clean_data$diagnosis == "Normal"),
            sum(clean_data$smoker == "Yes"),
            sum(clean_data$smoker == "No"),
            round(mean(clean_data$bmi), 1))
)

write.csv(summary_stats, file = "results/summary_statistics.csv", row.names = FALSE)

# Save specific R objects
save(clean_data, demographics, clinical_info, summary_stats, file = "results/analysis_objects.RData")

# Save entire workspace
save.image(file = "results/complete_workspace.RData")