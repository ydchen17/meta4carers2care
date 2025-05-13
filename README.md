# Effectiveness of Interventions to Support Carers of People With Dementia in Low- and Middle-Income Countries: A Systematic Review and Meta-Analysis

This repository contains code and example data supporting the study:

> Chen, Frank; Hu, Zhiwei; Li, Quan; Zheng, Xuan; Li, Meizhi; Salcher-Konrad, Maximilian; Comas-Herrera, Adelina; Knapp, Martin; Shi, Cheng; The STRiDE Consortium (2025). "[Effectiveness of Interventions to Support Carers of People With Dementia in Low- and Middle-Income Countries: A Systematic Review and Meta-Analysis](https://pubmed.ncbi.nlm.nih.gov/40015952/)". *International Journal of Geriatric Psychiatry*, 40(3), e70054.

## Key Findings

1. Despite the significant increase in dementia research in low‐ and middle‐income countries (LMICs), evidence from high‐quality randomised controlled trials (RCTs) remained limited.
2. Existing interventions in LMICs target one or more of three main objectives: improving knowledge about dementia, reducing care dependency, and enhancing carers' mental health.
3. The interventions to support carers, in general, yielded statistically significant, medium‐to‐large effects on improving carers' perceived burden, depression, and other health indicators, with dyadic interventions for both people with dementia and carers being more effective.
4. To scale up supportive interventions for a broader population of carers, high‐quality RCTs and culturally tailored approaches are urgently required to bolster the evidence base and enhance their effectiveness.

## Repository Content

### 📁 File Description 

* **`cleaning.R`**: Scripts for risk-of-bias visualisation and data transformation.
* **`analysis.R`**: Scripts for conducting meta-analysis, influence diagnostics, subgroup analysis, meta-regression, and MetaForest analysis.
* **`example-data.xlsx`**: A template Excel datasheet demonstrating the data structure used in the study.([GitHub][1])

### 📦 R Packages Utilised

The analysis leverages several R packages, including:

* [meta](https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/): For conducting meta-analyses.
* [esc](https://strengejacke.github.io/esc/): For converting effect sizes.
* [dmetar](https://dmetar.protectlab.org): For influence diagnostics and leave-one-out analyses.
* [MetaForest](https://cran.r-project.org/web/packages/metaforest/index.html): For exploring heterogeneity using machine learning approaches.

These packages should be installed in your R environment to run the provided scripts effectively.

### 📊 Data Access

The repository includes only example data. For access to the full dataset, please contact the STRiDE Consortium at:

📧 [stride.dementia@lse.ac.uk](mailto:stride.dementia@lse.ac.uk)

## Disclaimer

The code and data are provided "as-is" without warranty of any kind. Users are responsible for ensuring their appropriateness for specific applications. You may also contact me at [ydchen.lse@gmail.com](mailto:ydchen.lse@gmail.com) and I will try to help.
