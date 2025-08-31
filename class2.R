# Assignment 2 
# Souad Atigi

# Create data folder if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Create results folder if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Download the files to data folder
base_url <- "https://raw.githubusercontent.com/AI-Biotechnology-Bioinformatics/AI_and_Omics_Research_Internship_2025/main/"

# Download the files
for (file in c("DEGs_Data_1.csv", "DEGs_Data_2.csv")) {
    download.file(paste0(base_url, file), 
                  destfile = file.path("data", file),
                  method = "curl", quiet = TRUE)
    cat("Downloaded:", file, "\n")
}

# Define the classify_gene function
classify_gene <- function(logFC, padj) {
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

# Set folder paths
input_dir <- "data"
output_dir <- "results"

# List of files to process
files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")

# Empty list to store results
result_list <- list()

# Process each file in a for-loop
for (file_name in files_to_process) {
  
  cat("\n----------------------------------\n")
  cat("Processing:", file_name, "\n")
  cat("----------------------------------\n")
  
  # Read the file from data folder
  input_path <- file.path(input_dir, file_name)
  
  # Check if file exists before reading
  if (!file.exists(input_path)) {
    cat("File not found:", input_path, "\n")
    cat("Skipping to next file...\n")
    next
  }
  
  data <- read.csv(input_path, header = TRUE)
  cat("Loaded", nrow(data), "genes\n")
  
  if ("Gene_Id" %in% names(data) && !"gene_id" %in% names(data)) {
    names(data)[names(data) == "Gene_Id"] <- "gene_id"
    cat("Renamed Gene_Id to gene_id\n")
  }
  
  # Check and report missing values before replacement
  na_count <- sum(is.na(data$padj))
  if (na_count > 0) {
    cat("Found", na_count, "missing padj values - replacing with 1\n")
    data$padj[is.na(data$padj)] <- 1
  }
  
  # Check for missing logFC values
  na_logFC <- sum(is.na(data$logFC))
  if (na_logFC > 0) {
    cat("Warning:", na_logFC, "missing logFC values found\n")
  }
  
  # Add status column using the function
  data$status <- character(nrow(data))
  for (i in 1:nrow(data)) {
    data$status[i] <- classify_gene(data$logFC[i], data$padj[i])
  }
  
  # Print summary using table()
  cat("\nGene Classification Summary:\n")
  status_table <- table(data$status)
  print(status_table)
  
  # Generate summary counts
  upregulated <- sum(data$status == "Upregulated")
  downregulated <- sum(data$status == "Downregulated")
  not_significant <- sum(data$status == "Not_Significant")
  total_significant <- upregulated + downregulated
  
  cat("\nSummary Counts:\n")
  cat("- Upregulated genes:", upregulated, "\n")
  cat("- Downregulated genes:", downregulated, "\n")
  cat("- Not Significant genes:", not_significant, "\n")
  cat("- Total Significant genes:", total_significant, "\n")
  
  # Calculate percentages
  total_genes <- nrow(data)
  cat("\nPercentages:\n")
  cat("- Upregulated:", round(upregulated/total_genes * 100, 1), "%\n")
  cat("- Downregulated:", round(downregulated/total_genes * 100, 1), "%\n")
  cat("- Significant:", round(total_significant/total_genes * 100, 1), "%\n")
  
  # Store in list
  result_list[[file_name]] <- data
  
  # Save processed results to results folder
  output_path <- file.path(output_dir, paste0("Processed_", file_name))
  write.csv(data, output_path, row.names = FALSE)
}

# Final summary
if (length(result_list) > 0) {
  cat("\n==================================\n")
  cat("Files processed:", length(result_list), "\n")
  cat("Results saved in:", output_dir, "folder\n")
  
  # Combined summary across all files
  if (length(result_list) > 1) {
    cat("\nCombined Summary (All Files):\n")
    all_status <- unlist(lapply(result_list, function(x) x$status))
    print(table(all_status))
  }
}

# Save R workspace
workspace_filename <- "Souad_Atigi_Class_2_Assignment.RData"
save.image(file = workspace_filename)
