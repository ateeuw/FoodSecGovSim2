# Main script sourcing all other scripts

# Author: Aleid Sunniva Teeuwen
# Date: 10.04.2022
# Project: FoodSecGovSim2
# Publication: A systematic review of simulation studies that assess the impact of food security governance measures

rm(list = ls()) #start with a clean environment

# Install dependencies
list.of.packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "countrycode", "maps", "xlsx", "rJava")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

source("./Data_processing.R") 


# This script pre-processes the raw data exported from atlas ti so that, for instance, countries are grouped into regions and governance measures are grouped into governance tools
# Input: 
# -- ./Data/all_quotes_simpl.xlsx: The raw data exported from atlas ti
# Output: 
# -- ./Output/quotes_wide.csv & ./Output/quotes_long.csv: Two tables (long format and wide format) connecting variables and variable categories to Documents 
# -- ./Output/all_variables.xlsx: An overview with a sheet of summary statistics for each variable

rm(list = ls()) #start with a clean environment

# Install dependencies
list.of.packages <- c("dplyr", "tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

source("./Data_nesting.R")


# This script selects and organizes the data so that they co-occuring codes are aligned correctly and can be visualised accordingly
# Input: 
# -- ./Output/quotes_wide.csv
# Output: 
# -- ./Output/nested_data_all.csv: Data ready for visualisation
# -- ./Output/nested_data_wide.csv: Data ready for visualisation


source("./PRISMAflow")

# This script is used to make the PRISMA flow diagram (Fig. 1)
# Input: 
# -- ./Data/20220311_full reference list revised.xlsx
# Figures:
# -- ./Figures/abstract_exclusion_venn_diagramm.png
# -- ./Figures/fulltext_exclusion_venn_diagramm.png
# Output:
# -- ./Output/overview_for_prisma.xlsx

source("./Visualisation.R")

# This script is used to visualise the results 
# Input: 
# -- ./Output/nested_data_all.csv
# -- ./Output/nested_data_wide.csv
# Figures:
# -- ./Figures/per-model_type_domain_scale_bubble.png (Fig. 2)
# -- ./Figures/spatially_socially_disaggregated_pool.png (Fig. 4 + Appendix Figs. 9 & 10)
# Output:
# -- ./Output/VCdata_for_fig3.csv (Fig. 3)

source("./Appendix.R")

# This script is used to create additional information and visuals for appendix
# Input: 
# -- ./Output/nested_data_all.csv
# -- ./Output/quotes_long.csv
# Figures:
# -- ./Figures/papers_year_table.png (Table A5)
# -- ./Figures/cellular automatadomains.png (Figure A6)
# -- ./Figures/ABMdomains.png (Figure A6)
# -- ./Figures/system dynamics modeldomains.png (Figure A6)
# -- ./Figures/optimisationdomains.png (Figure A6)
# -- ./Figures/PE modeldomains.png (Figure A6)
# -- ./Figures/CGE modeldomains.png (Figure A6)
# -- ./Figures/microsimulation modeldomains.png (Figure A6)
# -- ./Figures/statistical econometricdomains.png (Figure A6)
# -- ./Figures/mathematical otherdomains.png (Figure A6)
# -- ./Figures/per-model_type_domain_co-occurence_pooled.png (FIgure A7)
# -- ./Figures/per-model_type_region_bubble.png (Figure A8)
# Output:

