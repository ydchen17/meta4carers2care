# Load required libraries
library(openxlsx)
library(meta)
library(esc)
library(dmetar)
library(metafor)
library(dplyr)
library(ggpubr)
library(metaforest)

# Read the dataset
data <- read.xlsx('Dataset_effect_size_calculated_immediate.xlsx')

# Extract data for caregiver burden effect measure
burden <- data[data$Effect.measure == 'Caregiver burden', ]

# Test for normality of caregiver burden data using Shapiro-Wilk test
shapiro_result <- shapiro.test(burden$mean.effect)
print(shapiro_result)

# Create metagen model for caregiver burden
burden_meta <- metagen(TE = burden$mean.effect,
                       seTE = burden$standard_error,
                       studlab = burden$Study.Identifier,
                       data = burden,
                       sm = "SMD",
                       comb.fixed = FALSE,
                       comb.random = TRUE,
                       text.common = 'Fixed-effects model',
                       text.random = 'Random-effects model')

# Perform outlier detection and influence analysis
outliers <- find.outliers(burden_meta)
inf_analysis <- InfluenceAnalysis(burden_meta, return.separate.plots = TRUE)

# Save influence plots
pdf('Figures/burden_infan.pdf', width = 15, height = 5)
plot(inf_analysis, "i2")
dev.off()

pdf('Figures/burden_baujat.pdf', width = 5, height = 5)
plot(inf_analysis, "baujat")
dev.off()

# Leave-one-out analysis
burden_leave1out <- metainf(burden_meta)
write.csv(data.frame(burden_leave1out), 'Figures/burden_leave1out.csv')

# Create forest plot for caregiver burden
pdf('Figures/burden_forest.pdf', width = 12.5, height = 10)
forest(burden_meta,
       prediction = TRUE,
       prediction.subgroup = TRUE,
       sortvar = burden$mean.effect,
       leftcols = c('Study.Identifier', 'Country', 'Caregiver.intervention.type.abbr', 'Overall', 
                    'Ctrl_Post_N', 'Expt_Post_N', 'Total_attrition.percent'),
       leftlabs = c('Study Identifier', 'Country', 'Type', 'Risk of bias', 
                    expression(N[Control]), expression(N[Intervention]), 'Attrition%'),
       overall = TRUE, 
       overall.hetstat = TRUE, 
       xlim = c(-7, 7))
dev.off()

# Remove outliers and create new model
burden_no_outlier <- burden[burden$Study.Identifier != "Shata 2017", ]
burden_meta_no_outlier <- metagen(TE = burden_no_outlier$mean.effect,
                                  seTE = burden_no_outlier$standard_error,
                                  studlab = burden_no_outlier$Study.Identifier,
                                  data = burden_no_outlier,
                                  sm = "SMD",
                                  comb.fixed = FALSE,
                                  comb.random = TRUE)

# Create forest plot after removing outlier
pdf('Figures/burden_forest(remove_outlier).pdf', width = 12.5, height = 10)
forest(burden_meta_no_outlier, prediction = TRUE, prediction.subgroup = TRUE, xlim = c(-7, 7))
dev.off()

# MetaForest model
burden_meta_forest <- MetaForest(mean.effect ~ Country + Caregiver.intervention.type + 
                                   Total_attrition.percent + Frequency + Overall, 
                                 data = burden, vi = "variance", 
                                 whichweights = "random", ntree = 20000)
plot(burden_meta_forest)

# Funnel plot
pdf('Figures/burden_funnel.pdf', width = 10, height = 7)
col_contour <- c("gray75", "gray85", "gray95")
metabias(burden_meta)
funnel.meta(burden_meta, xlim = c(-3.2, 1), contour = c(0.9, 0.95, 0.99), col.contour = col_contour, 
            studlab = TRUE, pos.studlab = '2', cex.studlab = 0.7)
legend(x = -3.2, y = 0.01, legend = c("p < 0.1", "p < 0.05", "p < 0.01"), fill = col_contour)
legend(x = 0, y = 0.9, bty = "n", legend = "Egger's test, p = 0.8080")
dev.off()

# Subgroup analysis 
subgroup_country <- update(burden_meta, subgroup = Country, tau.common = TRUE)
subgroup_stats <- data.frame(do.call(cbind, subgroup_country[c('k.w', 'TE.random.w', 'lower.random.w', 'upper.random.w',
                                                               'lower.predict.w', 'upper.predict.w', 'pval.random.w',
                                                               'I2.w', 'lower.I2.w', 'upper.I2.w')]))
write.csv(subgroup_stats, 'Figures/burden_country_subgroup_analysis.csv', row.names = FALSE)

# Meta-regression analysis 
burden_metareg <- metareg(burden_meta, ~ Year)  # Corrected variable name
print(burden_metareg)
bubble(burden_metareg,
       studlab = TRUE, pos.studlab = 4,
       xlim = c(2008, 2022), ylim = c(-2.5, 1),
       xlab = "Covariate Year")
burden_metareg_p <- permutest(burden_metareg)  # Corrected variable name
burden_metareg_p
