# This script is used to create additional information and visuals for appendix

# Author: Aleid Sunniva Teeuwen
# Date: 10.04.2021
# Project: FoodSecGovSim2
# Publication: A systematic review of simulation studies that assess the impact of food security governance measures

rm(list = ls()) #start with a clean environment


# Libraries
print("loading libraries")
library(colorspace)
library(ggplot2)
library(ggpubr)
library(egg)
library(formattable)
library(kableExtra)
library(VennDiagram)

# Functions
source("./Functions/level1_count.R")

# Data
print("loading data")
data <- read.csv("./Output/nested_data_all.csv", sep = ";")
load("./Output/nested_names_all.rda")
colnames(data) <- c(columnnames_wide)

datawide <- read.csv("./Output/nested_data_wide.csv", sep = ";")
load("./Output/nested_names_wide.rda")
#data <- data[,-1]
colnames(datawide) <- c(columnnames_wide)

quotes_long <- read.csv(file = "./Output/quotes_long.csv")

# Figure with publications per year (Table A5)
n_studies <- 110
year <- quotes_long[quotes_long$code_group == "papers - year",]
year <- year[!is.na(year$code_group),]
year_sum <- level1_count(sheet = year)

y_range <- min(as.numeric(as.character(year_sum$name))):max(as.numeric(as.character(year_sum$name)))

year_sum$name <- factor(year_sum$name, levels = y_range)
year_sum <- year_sum[order(year_sum$name),]
colnames(year_sum) <- c("publication year", "number of studies")
year_sum$proportion <- round(year_sum$number/n_studies, 2)

colnames(year_sum)[2] <- "number of studies"
year_sum$`number of studies` <- color_bar("lightgreen")(year_sum$`number of studies`)

ft_year <- year_sum %>%  
  group_by(`number of studies`) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")


ft_year %>% as_image(width = 22, file = "./Figures/papers_year_table.png")

# Model domains per model type (FIgure A6)

myCol <- c('#88CCEE', '#44AA99', '#332288', '#DDCC77')

# Domain
mod <- data[,colnames(data) %in% c("model_type", "domain", "Document"),]
mod <- mod %>% distinct()
mod <- na.omit(mod)
head(mod)

for(i in unique(mod$model_type)){
  
  imod <- mod[mod$model_type == i,]
  ilist <- list()
  j <- 0
  inames <- c()
  iCol <- c()
  econpapers <- imod$Document[which(imod$domain == "economic domain")]
  if(length(econpapers) > 0){
    ilist[[j + 1]] <- econpapers
    j <- j + 1
    inames <- c(inames, "econ")
    iCol <- c(iCol, myCol[1])}
  biopapers <- imod$Document[which(imod$domain == "biophysical domain")]
  if(length(biopapers) > 0){
    ilist[[j + 1]] <- biopapers
    j <- j + 1
    inames <- c(inames, "biophys")
    iCol <- c(iCol, myCol[2])}
  socpapers <- imod$Document[which(imod$domain == "social domain")]
  if(length(socpapers) > 0){
    ilist[[j + 1]] <- socpapers
    j <- j + 1
    inames <- c(inames, "social")
    iCol <- c(iCol, myCol[3])}
  logpapers <- imod$Document[which(imod$domain == "logistic domain")]
  if(length(logpapers) > 0){
    ilist[[j + 1]] <- logpapers
    j <- j + 1
    inames <- c(inames, "logist")
    iCol <- c(iCol, myCol[4])}
  
  venn.diagram(
    x = ilist,
    category.names = inames,
    filename = paste0('./Figures/', gsub("/", "", i), 'domains.png'),
    output=TRUE,
    
    # Output features
    imagetype="png" ,
    height = 780 , 
    width = 880 , 
    resolution = 300,
    compression = "lzw",
    margin = 0.1,
    
    # Circles
    lwd = 1,
    lty = 1,
    fill = iCol,
    
    # Numbers
    cex = 1,
    #fontface = "bold",
    fontfamily = "sans",
    main = i,
    
    # Set names
    cat.cex = 1,
    main.cex = 1.5,
    cat.fontface = "bold",
    cat.default.pos = "outer",
    # cat.pos = c(-27, 27, 135),
    #cat.dist = rep(0.055, j),
    cat.fontfamily = "sans"# ,
    # rotation = 1
  )
}

# Figure showing which model types are coupled to each other (Fig. A7)

typ <- mod[,c(1,3)]
typ <- typ %>% distinct()

typ$model_type <- factor(typ$model_type, levels = rev(c("cellular automata", "ABM", "system dynamics model", "optimisation", 
                                                        "PE model", "CGE model", "microsimulation model","statistical/econometric", "mathematical other")))


V <- crossprod(table(typ))
diag(V) <- 0
V <- as.data.frame(V)
V$modeltype2 <- row.names(V)

x <- 2
colours4mtyp <- c('#999933', '#44AA99', '#117733', '#882255', '#DDCC77', '#332288',   '#CC6677', '#88CCEE', "darkgrey")

dat_cooc <- gather(data = V, model_type, cooccurence, c(`mathematical other`:`cellular automata`))
dat_cooc$cooccurence <- as.numeric(as.character(dat_cooc$cooccurence))

dat_cooc$model_type <- factor(dat_cooc$model_type, levels = rev(c("cellular automata", "ABM", "system dynamics model", "optimisation", 
                                                                  "PE model", "CGE model", "microsimulation model","statistical/econometric", "mathematical other")))

dat_cooc$modeltype2 <- factor(dat_cooc$modeltype2, levels = rev(c("cellular automata", "ABM", "system dynamics model", "optimisation", 
                                                                  "PE model", "CGE model", "microsimulation model","statistical/econometric", "mathematical other")))


png(filename = paste0("./Figures/per-model_type_domain_co-occurence_pooled.png"), width = 1800, height = 1800)
ggplot(dat_cooc, aes(y = modeltype2, x = model_type, col = model_type, label = cooccurence)) +
  geom_point(size = dat_cooc$cooccurence*15+5, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = dat_cooc$cooccurence*3+10) +
  xlab("Model type") +
  ylab("Co-occuring model type") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 90, colour = darken(colours4mtyp, amount = 0.2)), 
        axis.text.y = element_text(size = rel(2.5*x), colour = darken(colours4mtyp, amount = 0.2)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(name = "", values = colours4mtyp) +
  coord_flip()
dev.off()



