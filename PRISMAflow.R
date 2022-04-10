# overview of papers

rm(list = ls())

###################### load libraries ###################### 
library("readxl") #for reading excel files
library("stringr")
library("VennDiagram")
library("xlsx") # for writing excel files
###################### load libraries ###################### 

dataloc <- "./data/20220311_full reference list revised.xlsx"

###################### load data ###################### 
papers <- read_excel(dataloc, sheet = "-duplicates")
bpapers <- read_excel(dataloc, sheet = "all")
###################### load data ###################### 

scopus <- bpapers[!is.na(bpapers$Scopus),]
print(paste("scopus:", nrow(scopus)))

wos <- bpapers[!is.na(bpapers$`Web of Science`),]
print(paste("WOS:", nrow(wos)))

print(paste("total:", nrow(bpapers)))

dup <- nrow(bpapers) - nrow(papers)
print(paste("duplicates:", dup))
dup

ndup <- dup

too_old <- which(papers$Year<2000)
youngpapers <- papers[-too_old,]
#too_young <- which(papers$Year>2020)

###################### exclusion reasons ######################  

# ineligible for abstract
simple <- papers

ineligible <- papers[papers$Eligible_abstract == 0,]

unique(ineligible$Reason)

ineligible$Reason <- gsub("pathways", "pathway", ineligible$Reason)
ineligible$Reason <- gsub("in Chinese", "in chinese", ineligible$Reason)
ineligible$Reason <- gsub("policy not aimed at FS", "not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("Policy not aimed at FS", "not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("not aimed at FS", "policy not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("RCPs", "representative concentration pathway", ineligible$Reason)
ineligible$Reason <- gsub("LCA", "life cycle assessment", ineligible$Reason)
ineligible$Reason <- gsub("FS", "food security", ineligible$Reason)
ineligible$Reason <- gsub("conferece", "conference", ineligible$Reason)
ineligible$Reason <- gsub("no policy inclusion", "no policy", ineligible$Reason)
ineligible$Reason <- gsub("policy", "governance measure", ineligible$Reason)
ineligible$Reason <- gsub("governance measure goals not measures", "governance goal not measure", ineligible$Reason)

all_reasons <- c()

for(i in 1:nrow(ineligible)){
  reasons <- strsplit(ineligible$Reason[i], ";")[[1]]
  for(j in 1:length(reasons)){
    if(reasons[j] %in% all_reasons){
      next
    }else{
      all_reasons <- c(all_reasons, reasons[j])
    }
  }
}

all_reasons <- unique(str_trim(all_reasons))
sort(all_reasons)

policy_simpl <- c("governance measure = implied", "not governance measure, representative concentration pathway", 
                  "governance measure = outcome", "no governance measure", 
                  "not governance measure, farm management", "not governance measure, store management", 
                  "not governance measure, water management", "not governance measure, socioeconomic pathway",
                  "governance goal not measure", "governance measure not described clearly", 
                  "socioeconomic pathway, not governance measure")
FS_simpl <- c("about obesity", "governance measure not aimed at food security", "about health", "about malnutrition", 
              "food security impact not included", "about food safety")
foodsystem_simpl <- c("no food system", "about bio-energy", "about fish", "ecology", "bio-medical", "hunting")
model_simpl <- c("meta-analysis", "opinion paper", "no simulation model", "vision paper", "position paper", "literature review",
                 "framework", "life cycle assessment")
other <- c("historical")
not_eng <- c("in spanish", "in chinese", "in Chinese")

ineligible$simpl <- ""

govpapers <- c()
FSpapers <- c()
foodpapers <- c()
modelpapers <- c()
otherpapers <- c()
notavpapers <- c()
oldpapers <- c()
confpapers <- c()
nengpapers <- c()
bookpapers <- c()
retpapers <- c()

for(i in 1:nrow(ineligible)){
  reasons <- str_trim(strsplit(ineligible$Reason[i], ";")[[1]])
  for(j in 1:length(reasons)){
    if(reasons[j] %in% policy_simpl){
      govpapers <- c(govpapers, ineligible$ID[i])
    }else if(reasons[j] %in% FS_simpl){
      FSpapers <- c(FSpapers, ineligible$ID[i])
    }else if(reasons[j] %in% foodsystem_simpl){
      foodpapers <- c(foodpapers, ineligible$ID[i])
    }else if(reasons[j] %in% model_simpl){
      modelpapers <- c(modelpapers, ineligible$ID[i])
    }else if(reasons[j] %in% other){
      otherpapers <- c(otherpapers, ineligible$ID[i])
    }else if(reasons[j] == "not retractable"){
      notavpapers <- c(notavpapers, ineligible$ID[i])
    }else if(reasons[j] == "too old"){
      oldpapers <- c(oldpapers, ineligible$ID[i])
    }else if(reasons[j] == "conference"){
      confpapers <- c(confpapers, ineligible$ID[i])
    }else if(reasons[j] %in% not_eng){
      nengpapers <- c(nengpapers, ineligible$ID[i])
    }else if(reasons[j] == "book"){
      bookpapers <- c(bookpapers, ineligible$ID[i])
    }else if(reasons[j] == "retracted article"){
      retpapers <- c(retpapers, ineligible$ID[i])
    }
  }
}

govpapers <- unique(govpapers)
FSpapers <- unique(FSpapers)
foodpapers <- unique(foodpapers)
modelpapers <- unique(modelpapers)
otherpapers <-unique(otherpapers)
notavpapers <- unique(notavpapers)
oldpapers <- unique(oldpapers)
confpapers <- unique(confpapers)
nengpapers <- unique(nengpapers)

print("papers discarded before screening")
print(paste("publication date:", length(oldpapers)))
simple[simple$ID %in% oldpapers,]$Reason <- "too old"
confpapers <- confpapers[!(confpapers %in% c(oldpapers))]
print(paste("conference:", length(confpapers)))
simple[simple$ID %in% confpapers,]$Reason <- "conference"
bookpapers <- bookpapers[!(bookpapers %in% c(oldpapers, confpapers))]
print(paste("books:", length(bookpapers)))
simple[simple$ID %in% bookpapers,]$Reason <- "book"
simple[simple$ID %in% nengpapers,]$Reason <- "language"
print(paste("language:", length(nengpapers)))
retpapers <- retpapers[!(retpapers %in% c(oldpapers, confpapers, bookpapers, nengpapers))]
print(paste("retracted:", length(retpapers)))
simple[simple$ID %in% retpapers,]$Reason <- "retracted"

print(paste("total #papers rejected prior to screening:",
            ndup + length(oldpapers) + length(confpapers) + length(bookpapers) + length(nengpapers) + length(retpapers)))

screened <- papers[!(papers$ID %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers)),]
print(paste("abstracts screened:", nrow(screened)))


print("papers after abstract screening")
govpapers <- govpapers[!(govpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("governance:", length(govpapers)))
FSpapers <- FSpapers[!(FSpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("food security:", length(FSpapers)))
foodpapers <- foodpapers[!(foodpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("food system:", length(foodpapers)))
modelpapers <- modelpapers[!(modelpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("simulation model:", length(modelpapers)))
otherpapers <- otherpapers[!(otherpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("historical:", length(modelpapers)))
notavpapers <- notavpapers[!(notavpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("not available:", length(notavpapers)))

multpapers <- table(c(govpapers, FSpapers, foodpapers, modelpapers, otherpapers))
multpapers <- multpapers[(multpapers) > 1]
length(multpapers)
simple[simple$ID %in% names(multpapers),]$Reason <- "multiple abstract"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% govpapers,]$Reason <- "governance abstract"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% FSpapers,]$Reason <- "food security abstract"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% foodpapers,]$Reason <- "food system abstract"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% modelpapers,]$Reason <- "modelling abstract"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% otherpapers,]$Reason <- "historical abstract"

myCol <- c("yellow", "deeppink", 
           "green", 
           "blue", "turquoise1")

venn.diagram(
  x = list(govpapers, FSpapers, 
           foodpapers, 
           modelpapers, otherpapers),
  category.names = c("Governance" , "Food security" , 
                     "Food system", 
                     "Simulation model", "Historical"),
  filename = './Figures/abstract_exclusion_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 780 , 
  width = 880 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 1,
  lty = 1,
  fill = myCol,
  
  # Numbers
  cex = .5,
  #fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.4,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.085, 
               0.075, 
               0.075, 0.085),
  cat.fontfamily = "sans"# ,
  # rotation = 1
)

print("papers after fulltext screening")
ineligible <- papers[papers$Eligible_abstract == 1 & papers$Eligible_fulltext == 0,]

unique(ineligible$Reason)

ineligible$Reason <- gsub("pathways", "pathway", ineligible$Reason)
ineligible$Reason <- gsub("in Chinese", "in chinese", ineligible$Reason)
ineligible$Reason <- gsub("policy not aimed at FS", "not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("Policy not aimed at FS", "not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("not aimed at FS", "policy not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("RCPs", "representative concentration pathway", ineligible$Reason)
ineligible$Reason <- gsub("LCA", "life cycle assessment", ineligible$Reason)
ineligible$Reason <- gsub("FS", "food security", ineligible$Reason)
ineligible$Reason <- gsub("conferece", "conference", ineligible$Reason)
ineligible$Reason <- gsub("no policy inclusion", "no policy", ineligible$Reason)
ineligible$Reason <- gsub("policy", "governance measure", ineligible$Reason)
ineligible$Reason <- gsub("governance measure goals not measures", "governance goal not measure", ineligible$Reason)

all_reasons <- c()

for(i in 1:nrow(ineligible)){
  reasons <- strsplit(ineligible$Reason[i], ";")[[1]]
  for(j in 1:length(reasons)){
    if(reasons[j] %in% all_reasons){
      next
    }else{
      all_reasons <- c(all_reasons, reasons[j])
    }
  }
}

all_reasons <- unique(str_trim(all_reasons))
sort(all_reasons)

policy_simpl <- c("governance measure = implied", "not governance measure, representative concentration pathway", 
                  "governance measure = outcome", "no governance measure", 
                  "not governance measure, farm management", "not governance measure, store management", 
                  "not governance measure, water management", "not governance measure, socioeconomic pathway",
                  "governance goal not measure", "governance measure not described clearly", 
                  "socioeconomic pathway, not governance measure")
FS_simpl <- c("about obesity", "governance measure not aimed at food security", "about health", "about malnutrition", 
              "food security impact not included", "about food safety")
foodsystem_simpl <- c("no food system", "about bio-energy", "about fish", "ecology", "bio-medical", "hunting")
model_simpl <- c("meta-analysis", "opinion paper", "no simulation model", "vision paper", "position paper", "literature review",
                 "framework", "life cycle assessment")
other <- c("historical")
not_eng <- c("in spanish", "in chinese")

ineligible$simpl <- ""

govpapers <- c()
FSpapers <- c()
foodpapers <- c()
modelpapers <- c()
otherpapers <- c()
notavpapers <- c()
oldpapers <- c()
confpapers <- c()
nengpapers <- c()
bookpapers <- c()
retpapers <- c()

for(i in 1:nrow(ineligible)){
  reasons <- str_trim(strsplit(ineligible$Reason[i], ";")[[1]])
  for(j in 1:length(reasons)){
    if(reasons[j] %in% policy_simpl){
      govpapers <- c(govpapers, ineligible$ID[i])
    }else if(reasons[j] %in% FS_simpl){
      FSpapers <- c(FSpapers, ineligible$ID[i])
    }else if(reasons[j] %in% foodsystem_simpl){
      foodpapers <- c(foodpapers, ineligible$ID[i])
    }else if(reasons[j] %in% model_simpl){
      modelpapers <- c(modelpapers, ineligible$ID[i])
    }else if(reasons[j] %in% other){
      otherpapers <- c(otherpapers, ineligible$ID[i])
    }else if(reasons[j] == "not retractable"){
      notavpapers <- c(notavpapers, ineligible$ID[i])
    }else if(reasons[j] == "too old"){
      oldpapers <- c(oldpapers, ineligible$ID[i])
    }else if(reasons[j] == "conference"){
      confpapers <- c(confpapers, ineligible$ID[i])
    }else if(reasons[j] %in% not_eng){
      nengpapers <- c(nengpapers, ineligible$ID[i])
    }else if(reasons[j] == "book"){
      bookpapers <- c(bookpapers, ineligible$ID[i])
    }else if(reasons[j] == "retracted article"){
      retpapers <- c(retpapers, ineligible$ID[i])
    }
  }
}

govpapers <- unique(govpapers)
FSpapers <- unique(FSpapers)
foodpapers <- unique(foodpapers)
modelpapers <- unique(modelpapers)
otherpapers <-unique(otherpapers)
notavpapers <- unique(notavpapers)
oldpapers <- unique(oldpapers)
confpapers <- unique(confpapers)
nengpapers <- unique(nengpapers)

print("papers discarded before screening")
print(paste("publication date:", length(oldpapers)))
confpapers <- confpapers[!(confpapers %in% c(oldpapers))]
print(paste("conference:", length(confpapers)))
bookpapers <- bookpapers[!(bookpapers %in% c(oldpapers, confpapers))]
print(paste("books:", length(bookpapers)))
nengpapers <- nengpapers[!(notavpapers %in% c(oldpapers, confpapers, bookpapers))]
print(paste("language:", length(nengpapers)))
retpapers <- retpapers[!(retpapers %in% c(oldpapers, confpapers, bookpapers, nengpapers))]
print(paste("retracted:", length(retpapers)))

print(paste("total #papers rejected prior to screening:",
            ndup + length(oldpapers) + length(confpapers) + length(bookpapers) + length(nengpapers) + length(retpapers)))

screened <- ineligible[!(ineligible$ID %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers)),]
print(paste("fulltext screened:", nrow(screened)))

print("papers after abstract screening")
govpapers <- govpapers[!(govpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("governance:", length(govpapers)))
FSpapers <- FSpapers[!(FSpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("food security:", length(FSpapers)))
foodpapers <- foodpapers[!(foodpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("food system:", length(foodpapers)))
modelpapers <- modelpapers[!(modelpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("simulation model:", length(modelpapers)))
otherpapers <- otherpapers[!(otherpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("historical:", length(modelpapers)))
notavpapers <- notavpapers[!(notavpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("not available:", length(notavpapers)))

multpapers <- table(c(govpapers, FSpapers, foodpapers, modelpapers, otherpapers))
multpapers <- multpapers[(multpapers) > 1]
length(multpapers)
simple[simple$ID %in% names(multpapers),]$Reason <- "multiple fulltext"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% govpapers,]$Reason <- "governance fulltext"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% FSpapers,]$Reason <- "food security fulltext"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% foodpapers,]$Reason <- "food system fulltext"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% modelpapers,]$Reason <- "modelling fulltext"
simple[!(simple$ID %in% names(multpapers)) & simple$ID %in% otherpapers,]$Reason <- "historical fulltext"

screened <- papers$ID[!(papers$ID %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
print(paste("fulltext screened:", length(screened)))

myCol <- c("yellow", "deeppink", 
           #"green", 
           "blue", "turquoise1")

venn.diagram(
  x = list(govpapers, FSpapers, 
           #foodpapers, 
           modelpapers, otherpapers),
  category.names = c("Governance" , "Food security" , 
                     #"Food system", 
                     "Simulation model", "Historical"),
  filename = './Figures/fulltext_exclusion_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 780 , 
  width = 880 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 1,
  lty = 1,
  fill = myCol,
  
  # Numbers
  cex = .5,
  #fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.4,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.085, 
               #0.075, 
               0.075, 0.085),
  cat.fontfamily = "sans"# ,
  # rotation = 1
)

eligible <- papers[papers$Eligible_abstract == 1 & papers$Eligible_fulltext == 1,]
length(unique(eligible$ID) ) 

which(papers[papers$Eligible_abstract == 0 & papers$Eligible_fulltext == 0,]$ID %in% eligible$ID)

write.xlsx(simple, file = "./Output/overview_for_prisma.xlsx")
print("Simplified exclusion reasons (NAs were not excluded):")
table(simple$Reason)

