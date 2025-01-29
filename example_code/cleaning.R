# Load required libraries
library(openxlsx)
library(esc)
library(dplyr)
library(robvis)

# Read datasets
Dataset <- read.xlsx('Dataset_version_latest.xlsx')
es_df <- read.xlsx('Dataset_version_latest.xlsx', sheet = 'Combined')

# Select relevant columns for risk of bias data
rob_data <- unique(es_df[c('Study.Identifier', 'Domain.1', 'Domain.2', 'Domain.3', 
                           'Domain.4', 'Domain.5', 'Overall')])

# Generate ROB2 risk of bias plots
pdf('Figures/rob_percent.pdf', width = 10, height = 3)
rob_summary(rob_data, tool = "ROB2")
dev.off()

pdf('Figures/rob_table.pdf', width = 6.5, height = 10)
rob_traffic_light(rob_data, tool = "ROB2", psize = 7)
dev.off()

# Prepare data for effect size calculations
Dataset$Caregiver.intervention.type <- relevel(Dataset$Caregiver.intervention.type, 'No intervention')

# Define columns for data and information
data_cols <- c("Pre_mean", "Recruitment_N", "Pre_N", "Pre_SD", "Post_mean", "Post_N", "Post_SD",
               "Attrition", "Attrition.percent", "Patient.female_percent", "Patient.mean_age", 
               "Caregiver.female_percent", "Caregiver.mean_age")

info_cols <- colnames(Dataset)[!colnames(Dataset) %in% data_cols]

# Split the dataset into control and experimental groups
meta_df.ctrl <- Dataset[Dataset$Caregiver.intervention.type == 'No intervention', ]
meta_df.expt <- Dataset[Dataset$Caregiver.intervention.type != 'No intervention', ]

# Initialize an empty list to store effect size data
es_df <- list()

# Loop through experimental groups to calculate effect sizes
for (i in 1:nrow(meta_df.expt)) {
  item <- meta_df.expt[i, ]
  item_info <- item[info_cols]
  item_data <- item[data_cols]
  
  # Rename experimental data columns
  colnames(item_data) <- c("Expt_Pre_mean", "Expt_Recruitment_N", "Expt_Pre_N", "Expt_Pre_SD", 
                           "Expt_Post_mean", "Expt_Post_N", "Expt_Post_SD", "Expt_Attrition", 
                           "Expt_Attrition.percent", "Expt_Patient.female_percent", "Expt_Patient.mean_age", 
                           "Expt_Caregiver.female_percent", "Expt_Caregiver.mean_age")
  
  # Subset control data by study and effect measure
  item_data_ctrl <- meta_df.ctrl[meta_df.ctrl$Study.Identifier == item_info$Study.Identifier, ]
  item_data_ctrl <- item_data_ctrl[item_data_ctrl$Effect.measure == item_info$Effect.measure, ]
  
  # Additional selection steps by measurement method, type, and other factors
  for (col in c("Measurement.method", "Measurement.type", "Duration", "Measurement.point")) {
    item_data_ctrl <- item_data_ctrl[item_data_ctrl[[col]] == item_info[[col]], ]
  }
  
  # Remove duplicates and check selection uniqueness
  item_data_ctrl <- unique(item_data_ctrl)
  if (nrow(item_data_ctrl) > 1) {
    print(item$Study.Identifier)
    print(item$Effect.measure)
    print(item_data_ctrl)
  }
  
  # Select required columns and rename them for control group data
  item_data_ctrl <- item_data_ctrl[data_cols]
  colnames(item_data_ctrl) <- c("Ctrl_Pre_mean", "Ctrl_Recruitment_N", "Ctrl_Pre_N", "Ctrl_Pre_SD", 
                                "Ctrl_Post_mean", "Ctrl_Post_N", "Ctrl_Post_SD", "Ctrl_Attrition", 
                                "Ctrl_Attrition.percent", "Ctrl_Patient.female_percent", "Ctrl_Patient.mean_age", 
                                "Ctrl_Caregiver.female_percent", "Ctrl_Caregiver.mean_age")
  
  # Combine experimental and control data
  data_item <- cbind(item_info, item_data, item_data_ctrl)
  es_df <- rbind(es_df, data_item)
}

# Calculate effect sizes for different measurement types
# SMD calculation for standardized mean differences
es_df.smd <- es_df[es_df$Measurement.type == 'SMD', ]
es_df.smd$mean.effect <- hedges_g(es_df.smd$Expt_Post_mean, es_df.smd$Expt_Post_N + es_df.smd$Ctrl_Post_N)
es_df.smd$variance <- es_df.smd$Expt_Post_SD^2
es_df.smd$standard_error <- es_df.smd$Expt_Post_SD / sqrt(es_df.smd$Expt_Post_N + es_df.smd$Ctrl_Post_N)

# Regression effect size
es_df.regression <- es_df[es_df$Measurement.type == 'Regression', ]
es_df.regression.SMD <- data.frame(esc_B(es_df.regression$Expt_Post_mean, es_df.regression$Expt_Post_SD, 
                                         es_df.regression$Expt_Post_N, es_df.regression$Ctrl_Post_N, 
                                         es.type = 'g', study = es_df.regression$Study.Identifier))
es_df.regression$mean.effect <- es_df.regression.SMD$es
es_df.regression$variance <- es_df.regression.SMD$var
es_df.regression$standard_error <- es_df.regression.SMD$se

# MD calculation for mean differences
es_df.md <- es_df[es_df$Measurement.type == 'MD', ]
reserved_cols <- c('Ctrl_Post_mean', 'Ctrl_Post_SD', 'Ctrl_Post_N', 'Expt_Post_mean', 'Expt_Post_SD', 
                   'Expt_Post_N', 'Study.Identifier')
es_df.md.data <- es_df.md[reserved_cols]

row_MD <- function(row_data) {
  transformed <- esc_mean_sd(
    grp1m = as.numeric(row_data[1]), grp1sd = as.numeric(row_data[2]), grp1n = as.numeric(row_data[3]), 
    grp2m = as.numeric(row_data[4]), grp2sd = as.numeric(row_data[5]), grp2n = as.numeric(row_data[6]), 
    es.type = 'g', study = row_data[7])
  df <- data.frame(transformed)
  rownames(df) <- NULL
  return(df)
}

# Apply row_MD function to MD data
es_df.md.SMD <- do.call('rbind', apply(es_df.md.data, 1, row_MD))
es_df.md$mean.effect <- -es_df.md.SMD$es
es_df.md$variance <- es_df.md.SMD$var
es_df.md$standard_error <- es_df.md.SMD$se

# Calculate pooled SD and SMD
pooled_SD <- sqrt(( (es_df.md$Expt_Post_N - 1) * es_df.md$Expt_Post_SD^2 + 
                      (es_df.md$Ctrl_Post_N - 1) * es_df.md$Ctrl_Post_SD^2 + 
                      (es_df.md$Expt_Post_N * es_df.md$Ctrl_Post_N) * 
                      (es_df.md$Expt_Post_mean^2 + es_df.md$Ctrl_Post_mean^2 - 
                         2 * es_df.md$Expt_Post_mean * es_df.md$Ctrl_Post_mean) ) / 
                    (es_df.md$Expt_Post_N + es_df.md$Ctrl_Post_N - 1))

SMD <- (es_df.md$Expt_Post_mean - es_df.md$Ctrl_Post_mean) / pooled_SD

# Raw effect size calculations
es_df.raw <- es_df[es_df$Measurement.type == 'Raw', ]
es_df.raw.data <- es_df.raw[reserved_cols]

row_SMD <- function(row_data) {
  transformed <- esc_mean_gain(
    pre1mean = as.numeric(row_data[1]), pre1sd = as.numeric(row_data[2]), post1mean = as.numeric(row_data[3]), 
    post1sd = as.numeric(row_data[4]), grp1n = as.numeric(row_data[5]), 
    pre2mean = as.numeric(row_data[6]), pre2sd = as.numeric(row_data[7]), post2mean = as.numeric(row_data[8]), 
    post2sd = as.numeric(row_data[9]), grp2n = as.numeric(row_data[10]), 
    es.type = 'g', study = row_data[11])
  df <- data.frame(transformed)
  rownames(df) <- NULL
  return(df)
}

# Apply row_SMD function to Raw data
es_df.raw.SMD <- do.call('rbind', apply(es_df.raw.data, 1, row_SMD))
es_df.raw$mean.effect <- es_df.raw.SMD$es
es_df.raw$variance <- es_df.raw.SMD$var
es_df.raw$standard_error <- es_df.raw.SMD$se

# Combine all effect size data into one dataframe
colnames(es_df.smd) <- colnames(es_df.raw)
colnames(es_df.regression) <- colnames(es_df.raw)
colnames(es_df.md) <- colnames(es_df.raw)

es_df.g <- rbind(es_df.raw, es_df.smd, es_df.regression, es_df.md)

# Calculate total recruitment, attrition, and other metrics
es_df.g$Recruitment_total_N <- es_df.g$Expt_Recruitment_N + es_df.g$Ctrl_Recruitment_N
es_df.g$Post_total_N <- es_df.g$Expt_Post_N + es_df.g$Ctrl_Post_N
es_df.g$Total_attrition <- es_df.g$Recruitment_total_N - es_df.g$Post_total_N
es_df.g$Total_attrition.percent <- 100 * (es_df.g$Total_attrition / es_df.g$Recruitment_total_N)

# Calculate average percentages and mean age for patients and caregivers
es_df.g$Patient.female_percent <- (es_df.g$Ctrl_Patient.female_percent + es_df.g$Expt_Patient.female_percent) / 2
es_df.g$Patient.mean_age <- (es_df.g$Ctrl_Patient.mean_age + es_df.g$Expt_Patient.mean_age) / 2
es_df.g$Caregiver.female_percent <- (es_df.g$Ctrl_Caregiver.female_percent + es_df.g$Expt_Caregiver.female_percent) / 2
es_df.g$Caregiver.mean_age <- (es_df.g$Ctrl_Caregiver.mean_age + es_df.g$Expt_Caregiver.mean_age) / 2

# Output the final dataset to a new Excel file
write.xlsx(es_df.g, 'Dataset_effect_size_calculated.xlsx', overwrite = TRUE)
