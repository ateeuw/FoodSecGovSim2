# Here I visualise the data 

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

# Data
print("loading data")
data <- read.csv("./Output/nested_data_all.csv", sep = ";")
load("./Output/nested_names_all.rda")
colnames(data) <- c(columnnames_wide)

datawide <- read.csv("./Output/nested_data_wide.csv", sep = ";")
load("./Output/nested_names_wide.rda")
#data <- data[,-1]
colnames(datawide) <- c(columnnames_wide)

# Figure of the domains and scales of different model types (Fig. 2)

# Domain
mod <- data[,colnames(data) %in% c("model_type", "domain", "Document"),]
mod <- mod %>% distinct()
mod <- na.omit(mod)
head(mod)
modsum <- mod %>% group_by(model_type, domain) %>% summarise(n = n())

modsum$domain <- factor(modsum$domain, levels = c("economic domain", "biophysical domain", "social domain", "logistic domain"))

x <- 2
colours4mtyp <- c('#88CCEE', '#44AA99', '#332288', '#DDCC77', 'grey')
darken(colours4mtyp, amount = 0.2)
lighten(colours4mtyp, amount = 0.2)

head(modsum)
modsum$group <- ""

typsum <- mod[,c(1,3)]
typsum <- typsum %>% distinct()
typsum <- typsum %>% group_by(model_type) %>% summarise(n = n())

typsum$model_type <- factor(typsum$model_type, 
                            levels = c("cellular automata", "ABM", "system dynamics model", "optimisation", "statistical/econometric",
                                       "PE model", "CGE model", "microsimulation model", "mathematical other"))
typsum

domsum <- mod[,c(1,2)]
domsum <- domsum %>% distinct()
domsum <- domsum %>% group_by(domain) %>% summarise(n = n())

domsum$domain <- factor(domsum$domain, levels = c("economic domain", "biophysical domain", "social domain", "logistic domain"))


typsum$domain <- "total"
domsum$model_type <- "total"

typsum <- typsum[,c(1,3,2)]
domsum <- domsum[,c(3,1,2)]

typsum$group <- "total"
domsum$group <- "total"
modsum <- rbind(as.data.frame(modsum), as.data.frame(typsum))
modsum <- rbind(modsum, as.data.frame(domsum))

modsum$group[which(modsum$model_type %in% c("cellular automata", "ABM", 'system dynamics model'))] <- "dynamic by default"
modsum$group[which(modsum$model_type %in% c("statistical/econometric"))] <- "mixed"
modsum$group[which(modsum$model_type %in% c("optimisation", "PE model", "CGE model"))] <- "static by default"
modsum$group[which(modsum$model_type %in% c("microsimulation model", "mathematical other"))] <- "mixed"

modsum$bubble <- modsum$n*3+10
modsum$bubble[modsum$group == "total"] <- modsum$n[modsum$group == "total"]/1.2 + 25
modsum$bubble2 <- modsum$bubble
modsum$bubble2[modsum$domain == "total"] <- 1

modsum$bubble3 <- modsum$bubble2
modsum$bubble3 <- modsum$bubble3*1.5

modsum$model_type <- factor(modsum$model_type, levels = rev(c("total", "cellular automata", "ABM", "system dynamics model", "optimisation", 
                                                              
                                                              
                                                              "PE model", "CGE model", "microsimulation model","statistical/econometric", "mathematical other")))
colours4mtyp <- c('#88CCEE', '#44AA99', '#332288', '#DDCC77', 'white')

d0 <- ggplot(modsum[modsum$group ==  "total",], 
             aes(y = model_type, x = domain, col = domain, label = n, size = bubble)) +
  geom_point(size = modsum$bubble3[modsum$group ==  "total"], col = "white", alpha = 0.9) +
  geom_text(fontface = "bold", size = modsum$bubble[modsum$group ==  "total"]/2) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0, size = rel(2.5*x), angle = 90), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(name = "", values = colours4mtyp) + 
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(position = "top", 
                   drop=FALSE, labels=c("economic domain" = "economic", "biophysical domain" = "biophysical",
                                        "social domain" = "social", "logistic domain" = "logistic", 
                                        "total" = "total")) 

d0

d1 <- ggplot(modsum[modsum$group ==  "dynamic by default",], 
             aes(y = model_type, x = domain, col = domain, label = n, size = bubble)) +
  geom_point(size = modsum$bubble3[modsum$group ==  "dynamic by default"], alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = modsum$bubble[modsum$group ==  "dynamic by default"]/2) +
  ggtitle("A") +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 270, 
                                   colour = darken(colours4mtyp, amount = 0.3)), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.position = "none",
        title = element_text(size = rel(2.5*x))) + 
  scale_colour_manual(name = "", values = colours4mtyp) + 
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(position = "top", 
                   labels=c("economic domain" = "economic", "biophysical domain" = "biophysical",
                            "social domain" = "social", "logistic domain" = "logistic", 
                            "total" = "total")) 

d1

d3 <- ggplot(modsum[modsum$group ==  "static by default",], aes(y = model_type, x = domain, col = domain, label = n)) +
  geom_point(size = modsum$bubble3[modsum$group ==  "static by default"], alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = modsum$bubble[modsum$group ==  "static by default"]/2) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 270), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(name = "", values = colours4mtyp) + 
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(position = "top", 
                   drop=FALSE, labels=c("economic domain" = "economic", "biophysical domain" = "biophysical",
                                        "social domain" = "social", "logistic domain" = "logistic", "total" = "total")) 
d3

d4 <- ggplot(modsum[modsum$group ==  "mixed",], aes(y = model_type, x = domain, col = domain, label = n)) +
  geom_point(size = modsum$bubble3[modsum$group ==  "mixed"], alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = modsum$bubble[modsum$group ==  "mixed"]/2) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 270, 
                                   colour = darken(colours4mtyp, amount = 0.3)), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(drop=FALSE, name = "", values = colours4mtyp) + 
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(position = "top", 
                   drop=FALSE, labels=c("economic domain" = "economic", "biophysical domain" = "biophysical",
                                        "social domain" = "social", "logistic domain" = "logistic", "total" = "total")) 
d4
# Scale
mod <- data[,colnames(data) %in% c("model_type", "spatial & temporal - ref quan scale", "Document"),]
mod <- mod %>% distinct()
mod <- na.omit(mod)

head(mod)
colnames(mod)[2] <- "scale"
mod$scale <- as.factor(mod$scale)

modsum <- mod %>% group_by(model_type, scale) %>% summarise(n = n())

head(modsum)
modsum$group <- ""

typsum
colnames(typsum)[2] <- "scale"
sclsum <- mod[,c(1,2)] 
sclsum <- sclsum %>% distinct() 
sclsum <- sclsum %>% group_by(scale) %>% summarise(n = n())
head(sclsum)
sclsum$model_type <- "total"

sclsum <- sclsum[,c(3,1,2)]
sclsum$group <- "total"
modsum <- rbind(as.data.frame(modsum), as.data.frame(typsum))

modsum$group[which(modsum$model_type %in% c("cellular automata", "ABM", 'system dynamics model'))] <- "dynamic by default"
modsum$group[which(modsum$model_type %in% c("statistical/econometric"))] <- "mixed"
modsum$group[which(modsum$model_type %in% c("optimisation", "PE model", "CGE model"))] <- "static by default"
modsum$group[which(modsum$model_type %in% c("microsimulation model", "mathematical other"))] <- "mixed"

modsum <- rbind(modsum, as.data.frame(sclsum))

modsum$bubble <- modsum$n*3+10
modsum$bubble[modsum$group == "total"] <- modsum$n[modsum$group == "total"]/1.2 + 25
modsum$bubble2 <- modsum$bubble
modsum$bubble2[modsum$scale == "total"] <- 1

modsum$bubble3 <- modsum$bubble2
modsum$bubble3 <- modsum$bubble3*1.5

modsum$model_type <- factor(modsum$model_type, levels = rev(c("total", "cellular automata", "ABM", "system dynamics model", "optimisation", 
                                                              "PE model", "CGE model", "microsimulation model","statistical/econometric", "mathematical other")))

colours4mtyp <- c('#332288', '#88CCEE', '#44AA99', '#117733',  '#999933', '#DDCC77', 'white')
colours4mtypd = darken(colours4mtyp, amount = 0.3)

s0 <- ggplot(modsum[modsum$group ==  "total",], 
             aes(y = model_type, x = scale, col = scale, label = n, size = bubble)) +
  geom_point(size = modsum$bubble2[modsum$group ==  "total"], col = "white", alpha = 0.7) +
  geom_text(fontface = "bold", size = modsum$bubble[modsum$group ==  "total"]/2) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0, size = rel(2.5*x), angle = 270), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(name = "", values = colours4mtypd) + 
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", 
                            "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", 
                            "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(drop = FALSE, position = "top", 
                   labels = c("1" = expression(phantom(x) <="village"), 
                              "2" = expression(phantom(x) <="municipality"), 
                              "3" = expression(phantom(x) <="province"),
                              "4" = expression(phantom(x) <="country"), 
                              "5" = expression(phantom(x) <="continent"), 
                              "6" = expression(phantom(x) > "continent"))) 

s0

s1 <- ggplot(modsum[modsum$group ==  "dynamic by default",], 
             aes(y = model_type, x = scale, col = scale, label = n, size = bubble)) +
  geom_point(size = modsum$bubble3[modsum$group ==  "dynamic by default"],alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = modsum$bubble[modsum$group ==  "dynamic by default"]/2) +
  ggtitle("B") +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 270, 
                                   colour = darken(colours4mtyp, amount = 0.3)),  
        axis.text.y = element_text(size = rel(2.5*x)), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(2.5*x))) + 
  scale_colour_manual(drop = FALSE, name = "", values = colours4mtyp) + 
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(position = "top", drop = FALSE,
                   labels = c("1" = expression(phantom(x) <="village"), 
                              "2" = expression(phantom(x) <="municipality"), 
                              "3" = expression(phantom(x) <="province"),
                              "4" = expression(phantom(x) <="country"), 
                              "5" = expression(phantom(x) <="continent"), 
                              "6" = expression(phantom(x) >"continent")))
s1

s3 <- ggplot(modsum[modsum$group ==  "static by default",], 
             aes(y = model_type, x = scale, col = scale, label = n)) +
  geom_point(size = modsum$bubble3[modsum$group ==  "static by default"], alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = modsum$bubble[modsum$group ==  "static by default"]/2) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 270), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(name = "", values = colours4mtyp) + 
  #scale_alpha_manual(values=c(rep(0.7, 6), 0.00001), guide=F) +
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(position = "top", 
                   labels = c("1" = expression(phantom(x) <="village"), 
                              "2" = expression(phantom(x) <="municipality"), 
                              "3" = expression(phantom(x) <="province"),
                              "4" = expression(phantom(x) <="country"), 
                              "5" = expression(phantom(x) <="continent"), 
                              "6" = expression(phantom(x) > "continent")))
s3

s4 <- ggplot(modsum[modsum$group ==  "mixed",], aes(y = model_type, x = scale, col = scale, label = n)) +
  geom_point(size = modsum$bubble3[modsum$group ==  "mixed"], alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = modsum$bubble[modsum$group ==  "mixed"]/2) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 270, 
                                   colour = darken(colours4mtyp, amount = 0.3)), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_colour_manual(drop=FALSE, name = "", values = colours4mtyp) + 
  scale_y_discrete(labels=c("cellular automata" = "cellular\nautomata", "ABM" = "agent-based\nmodel", "system dynamics model" = "system\ndynamics\nmodel", "optimisation" = "optimisation\n model", "statistical/econometric" = "econometric \n model",
                            "PE model" = "PE model", "CGE model" = "CGE model", "microsimulation model" = "microsimulation\n model", "mathematical other" = "other \n models", "total" = "total")) +
  scale_x_discrete(position = "top", drop = FALSE,
                   labels = c("1" = expression(phantom(x) <="village"), 
                              "2" = expression(phantom(x) <="municipality"), 
                              "3" = expression(phantom(x) <="province"),
                              "4" = expression(phantom(x) <="country"), 
                              "5" = expression(phantom(x) <="continent"), 
                              "6" = expression(phantom(x) > "continent")))
s4

png(filename = paste0("./Figures/per-model_type_domain_scale_bubble.png"), width = 900*1.5*2, height = 1500*1.5)
ggarrange(d1 + theme(panel.border = element_rect(colour = "black", fill=NA)), 
          s1 + theme(panel.border = element_rect(colour = "black", fill=NA)),  
          d3 + theme(axis.text.x = element_blank(),
                     panel.border = element_rect(colour = "black", fill=NA),
                     #axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()), 
          s3 + theme(axis.text.x = element_blank(),
                     panel.border = element_rect(colour = "black", fill=NA),
                     #axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()), 
          d4 + theme(axis.text.x = element_blank(),
                     panel.border = element_rect(colour = "black", fill=NA),
                     #axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()),
          s4 + theme(axis.text.x = element_blank(),
                     panel.border = element_rect(colour = "black", fill=NA),
                     #axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()), 
          d0 + theme(axis.text.x = element_blank(),
                     axis.line = element_line(colour = "black"),
                     #axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()),
          s0 + theme(axis.text.x = element_blank(),
                     axis.line = element_line(colour = "black"),
                     #axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()), 
          ncol = 2)
dev.off()


#Data for value chain echelon figure (Fig. 3)
colnames(datawide)[c(30:35,1)]

ech <- datawide[,c(30:35,1)]
ech <- ech %>% distinct()

colnames(ech)
ech$all <- ""

for(i in 1:nrow(ech)){
  if(ech$`food - production*`[i] == "yes")
    ech$all[i] <- paste(ech$all[i], "1", sep = "-")
  if(ech$`food - distribution & transport`[i] == "yes")
    ech$all[i] <- paste(ech$all[i], "2", sep = "-")
  if(ech$`food - storage and processing`[i] == "yes")
    ech$all[i] <- paste(ech$all[i], "3", sep = "-")
  if(ech$`food - trade & wholesale`[i] == "yes")
    ech$all[i] <- paste(ech$all[i], "4", sep = "-")
  if(ech$`food - retail`[i] == "yes")
    ech$all[i] <- paste(ech$all[i], "5", sep = "-")
  if(ech$`food - consumption`[i] == "yes")
    ech$all[i] <- paste(ech$all[i], "6", sep = "-")
}

echsum <- ech %>% group_by(all) %>% summarise(n = n())

echsum$radius <- sqrt(echsum$n/pi)
echsum$diameter <- round(echsum$radius*2, 1)*1.5

write.csv(echsum, "./Output/VCdata_for_fig3.csv")

# Socially and spatially disaggregated governance and data (Fig. 4)

# combine aspatial data
print(" transform aspatial data into long format")
head(data$aspatial_data)
#data <- data %>% gather(aspatial_data, applicable, model_type, `modelling - data quan spatial`)
#data$aspatial_data <- gsub(pattern = "modelling - ", replacement = "", x = data$aspatial_data)
#data$aspatial_data <- paste(data$aspatial_data, "data")
#data <- data[data$applicable=="yes",]

data$NATO4 <- as.character(data$NATO)
data$NATO4[data$NATO %in% c("Packaged self-serve messages", "Propaganda", "Group-targeted messages", "Bespoke messages")] <- "Nodality"
data$NATO4[data$NATO %in% c("Open compacts", "Open permits", "Standard constraints", "Group-targeted constraints", "Conditional tokens", "Enablements")] <- "Authority"
data$NATO4[data$NATO %in% c("Bounties", "Bearer-directed payments", "Conduits", "Contracts", "Transfers")] <- "Treasure"
data$NATO4[data$NATO %in% c("At-large treatment", "At-large processing", "At-large storage and custody", "Group treatnent", "Group-targeted transportation and distribution", "Processing", "Storage and custody", "Transport and distribution")] <- "Organisation"

data$AGI <- ""
data$AGI[data$NATO %in% c("Packaged self-serve messages", "Propaganda", "Open compacts", "Open permits", "Standard constraints", "Bounties", "Bearer-directed payments", "At-large treatment", "At-large processing", "At-large storage and custody")] <- "At-large"
data$AGI[data$NATO %in% c("Group-targeted messages", "Group-targeted constraints", "Conduits", "Group treatnent", "Group-targeted transportation and distribution")] <- "Group"
data$AGI[data$NATO %in% c("Bespoke messages", "Conditional tokens", "Enablements", "Contracts", "Transfers", "Processing", "Storage and custody", "Transport and distribution")] <- "Individual"

colnames(data)
socdat <- data[,c(1,20, 28, 32)]
socdat <- socdat %>% distinct()

socsum <- socdat %>% group_by(model_type, aspatial_data, AGI) %>% summarise(n = n())

for(i in unique(socdat$Document)){
  rows <- socdat[socdat$Document==i,]
  if("disaggregated quantitative data" %in% rows[,3] || "disaggregated qualitative data"  %in% rows[,3]){
    socdat[socdat$Document==i,3] <- "disaggregated"
  }else{
    socdat[socdat$Document==i,3] <- "aggregated"
  }
}

socdat <- socdat %>% distinct()

socsum <- socdat %>% group_by(model_type, aspatial_data, AGI) %>% summarise(n = n())
socsum$bubble <- socsum$n*8 + 10
socsum$AGI <- factor(socsum$AGI, levels = rev(sort(unique(socsum$AGI))))
socsum$bubcol <- "green"
socsum$bubcol[socsum$aspatial_data == "aggregated" & socsum$AGI %in% c("Group", "Individual")] <- "red"
socsum$bubcol[socsum$AGI == "At-large"] <- "sea"

x <- 1.5
png(filename = paste0("./Figures/socially_disaggregated_mtyp.png"), width = 900*1.5, height = 900*1.5)
ggplot(data = socsum, aes(y = AGI, x = aspatial_data, col = bubcol,label = n)) +
  geom_point(size = socsum$bubble, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = socsum$bubble/2) +
  ylab("Social targeting of governance measures") +
  xlab("Type of social data") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2.5*x), angle = 90), 
        axis.text.y = element_text(size = rel(2.5*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x)),
        strip.text.x = element_text(size = rel(1.5*x))) +
  scale_colour_manual(drop=FALSE, name = "", values = c( '#117733', "coral", '#44AA99')) + 
  scale_y_discrete(labels=c("At-large" = "At-large", "Group" = "Group", "Individual" = "Individual")) +
  facet_wrap(~model_type)
dev.off()  

sidat <- socdat[,c(1,3,4)]
sidat <- sidat %>% distinct()
socsum <- sidat %>% group_by(aspatial_data, AGI) %>% summarise(n = n())
socsum$bubble <- socsum$n*5 + 10
socsum$AGI <- factor(socsum$AGI, levels = rev(sort(unique(socsum$AGI))))
socsum$bubcol <- "green"
socsum$bubcol[socsum$aspatial_data == "aggregated" & socsum$AGI %in% c("Group", "Individual")] <- "red"
socsum$bubcol[socsum$AGI == "At-large"] <- "sea"

actsocsum <- socsum

x <- 2
png(filename = paste0("./Figures/socially_disaggregated_pool.png"), width = 900*1.5, height = 900*1.5)
ggplot(data = socsum, aes(y = AGI, x = aspatial_data, col = bubcol,label = n)) +
  geom_point(size = socsum$bubble, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = socsum$bubble/2) +  xlab("") +
  ylab("Social targeting of governance measures") +
  xlab("Type of social data") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(2*x), angle = 90), 
        axis.text.y = element_text(size = rel(2*x)), 
        axis.title = element_text(size=rel(0.8*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) +
  scale_colour_manual(drop=FALSE, name = "", values = c( '#117733', "coral", '#44AA99')) + 
  scale_y_discrete(labels=c("At-large" = "At-large", "Group" = "Group", "Individual" = "Individual")) 
dev.off()  

dissum <- socdat[,c(1,3)]
dissum <- dissum %>% distinct()
dissum <- dissum %>% group_by(aspatial_data) %>% summarise(n = n())
head(actsocsum)
head(dissum)
dissum$AGI <- "total"
dissum$bubble <- NA
dissum$bubcol <- "white"

head(actsocsum)
head(dissum)
dissum <- dissum[,c(1,3:5,2)]

actsocsum <- rbind(as.data.frame(actsocsum), as.data.frame(dissum))

dissum <- socdat[,c(1,4)]
dissum <- dissum %>% distinct()
dissum <- dissum %>% group_by(AGI) %>% summarise(n = n())
dissum$aspatial_data <- "total"
dissum$bubble <- NA
dissum$bubcol <- "white"

actsocsum <- rbind(as.data.frame(actsocsum), as.data.frame(dissum))


# combine spatial data
print(" transform aspatial data into long format")
head(data$`modelling - data quan spatial`)

colnames(data)
socdat <- data[,c(1,3,20,13)]
socdat <- socdat %>% distinct()

for(i in unique(socdat$Document)){
  rows <- socdat[socdat$Document==i,]
  if(2 %in% rows[,2]){
    socdat[socdat$Document==i,2] <- 2
  }else if(1 %in% rows[,2]){
    socdat[socdat$Document==i,2] <- 1
  }else{
    socdat[socdat$Document==i,2] <- 0
  }
}
socdat <- socdat %>% distinct()

socsum <- socdat %>% group_by(model_type, `modelling - data quan spatial`, `per measure - spatially targeted?`) %>% summarise(n = n())
socsum$bubble <- socsum$n*5 + 10

socsum$bubcol <- "green"
socsum$bubcol[socsum$`modelling - data quan spatial` == 0 & socsum$`per measure - spatially targeted?` == "yes"] <- "red"
socsum$bubcol[socsum$`per measure - spatially targeted?` == "no"] <- "sea"
socsum$`modelling - data quan spatial` <- as.factor(socsum$`modelling - data quan spatial`)

png(filename = paste0("./Figures/spatially_disaggregated_mtyp.png"), width = 900*1.5, height = 900*1.5)
ggplot(data = socsum, aes(y = `per measure - spatially targeted?`, x = `modelling - data quan spatial`, col = bubcol,label = n)) +
  geom_point(size = socsum$bubble, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = socsum$bubble/2) +  
  xlab("Type of spatial data") +
  ylab("Spatial targeting of governance measures") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.5*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.5*x)), 
        axis.title = element_text(size=rel(0.9*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x)),
        strip.text.x = element_text(size = rel(1.5*x))) +
  scale_colour_manual(drop=FALSE, name = "", values = c('#117733', "coral",  '#44AA99')) + 
  scale_x_discrete(labels=c("0" = "No\nspatial\ndata", "1" = "Discrete\nspatial\ndata", "2" = "Continuous\nspatial\ndata")) +
  facet_wrap(~model_type)
dev.off()  

sidat <- socdat[,c(1,2,4)]
sidat <- sidat %>% distinct()
socsum <- sidat %>% group_by(`modelling - data quan spatial`, `per measure - spatially targeted?`) %>% summarise(n = n())
socsum$`modelling - data quan spatial` <- as.factor(socsum$`modelling - data quan spatial`)
socsum$bubcol <- "green"
socsum$bubcol[socsum$`modelling - data quan spatial` == 0 & socsum$`per measure - spatially targeted?` == "yes"] <- "red"
socsum$bubcol[socsum$`per measure - spatially targeted?` == "no"] <- "sea"

dissum <- sidat[,c(1,2)]
dissum <- dissum %>% distinct()
dissum <- dissum %>% group_by(`modelling - data quan spatial`) %>% summarise(n = n())
head(socsum)
head(dissum)
dissum$`per measure - spatially targeted?` <- "total"
dissum$bubcol <- "white"

head(socsum)
head(dissum)
dissum <- dissum[,c(1,3:4,2)]

socsum <- rbind(as.data.frame(socsum), as.data.frame(dissum))

dissum <- socdat[,c(1,4)]
dissum <- dissum %>% distinct()
dissum <- dissum %>% group_by(`per measure - spatially targeted?`) %>% summarise(n = n())
dissum$`modelling - data quan spatial` <- "total"
socsum$`modelling - data quan spatial` <- as.character(socsum$`modelling - data quan spatial`)
dissum$bubcol <- "white"
dissum <- dissum[,c(3,1,2,4)]

socsum <- rbind(as.data.frame(socsum), as.data.frame(dissum))


socsum$bubble <- socsum$n*3 + 10
head(socsum)



png(filename = paste0("./Figures/spatially_disaggregated_pool.png"), width = 900*1.5, height = 900*1.5)
spatpool <- ggplot(data = socsum, aes(y = `per measure - spatially targeted?`, x = `modelling - data quan spatial`, col = bubcol,label = n)) +
  geom_point(size = socsum$bubble, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = socsum$bubble/2) +
  ylab("Spatial targeting of governance measures") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.5*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.5*x)), 
        axis.title = element_text(size=rel(0.9*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x)),
        strip.text.x = element_text(size = rel(1.5*x))) +
  scale_colour_manual(drop=FALSE, name = "", values = c('#117733', "coral",  '#44AA99', "white")) + 
  scale_x_discrete(labels=c("0" = "No\nspatial\ndata", "1" = "Discrete\nspatial\ndata", "2" = "Continuous\nspatial\ndata"))
spatpool
dev.off()  

# Nice figure for manuscript
socsum$`per measure - spatially targeted?` <- factor(socsum$`per measure - spatially targeted?`, levels = c("yes", "nothing here", "no", "total"))
socsum$bubble <- socsum$n*2 + 10
socsum$bubble2 <- socsum$bubble
socsum$bubble2[socsum$`modelling - data quan spatial` == "total" | socsum$`per measure - spatially targeted?` == "total"] <- 1

spatpool <- ggplot(data = socsum, aes(y = `per measure - spatially targeted?`, x = `modelling - data quan spatial`, col = bubcol,label = n)) +
  geom_point(size = socsum$bubble2, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = socsum$bubble/4 + 10) +  
  ggtitle("B") +
  ylab("") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = 42, angle = 90, colour = c("coral", '#117733', '#117733', 'black')), 
        axis.text.y = element_text(size = 42, colour = c('#117733', 'yellow', '#44AA99', 'black')), 
        legend.position = "none",
        title = element_text(size = 62)) +
  scale_colour_manual(drop=FALSE, name = "", values = c('#117733', "coral",  '#44AA99', 'white')) + 
  scale_y_discrete(drop = FALSE, labels=c("total" = 'total', "no" = "Not\nspatially targeted\ngovernance", "nothing here" = " ","yes" = "Spatially targeted\n governance")) +
  scale_x_discrete(drop = FALSE, labels=c("0" = "No\nspatial\ndata", "1" = "Discrete\nspatial\ndata", "2" = "Continuous\nspatial\ndata", 'total' = 'total'))
spatpool

actsocsum$aspatial_data <- factor(actsocsum$aspatial_data, levels = c("aggregated", "nothing here", "disaggregated", "total"))
actsocsum$bubble <- actsocsum$n*2 + 10
actsocsum$bubble2 <- actsocsum$bubble
actsocsum$bubble2[actsocsum$aspatial_data == "total" | actsocsum$AGI == "total"] <- 1

socpool <- ggplot(data = actsocsum, aes(y = AGI, x = aspatial_data, col = bubcol,label = n)) +
  geom_point(size = actsocsum$bubble2, alpha = 0.7) +
  geom_text(col = "black", fontface = "bold", size = actsocsum$bubble/4 + 10) +
  ggtitle("A") +
  ylab("") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = 42, angle = 90, colour = c("coral", 'white', '#117733', "black")), 
        axis.text.y = element_text(size = 42, colour = c('#117733', '#117733', '#44AA99', "black")), 
        legend.position = "none",
        title = element_text(size = 62)) +
  scale_colour_manual(drop=FALSE, name = "", values = c( '#117733', "coral", '#44AA99', "white")) + 
  scale_x_discrete(drop = FALSE, labels=c("aggregated" = "Aggregated\nsocial data", "nothing here" = " ", "disaggregated" = "Disaggregated\nsocial data", "total" = "total")) +
  scale_y_discrete(labels=c("total" = "total", "At-large" = "Not\nsocially targeted\ngovernance", "Group" = "Governance\ntargeted\ntowards groups", "Individual" = "Governance\ntargeted\ntowards individuals")) 
socpool

png(filename = paste0("./Figures/spatially_socially_disaggregated_pool.png"), width = 1700*1.5, height = 900*1.5)
ggarrange(socpool, 
          spatpool, 
          nrow = 1)
dev.off()
