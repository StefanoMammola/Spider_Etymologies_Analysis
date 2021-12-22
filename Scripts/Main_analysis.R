## ------------------------------------------------------------------------
## 'Spiders etymologies'
## ------------------------------------------------------------------------

# 

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)

# Loading R packages ------------------------------------------------------
library("arakno")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("stringr")
library("xlsx")
library("tidyverse")

# Functions ---------------------------------------------------------------

## Function for calculating the standard error
std <- function(x) sd(x)/sqrt(length(x))

# Custom function to predict logistic regressions
logisticline <- function(z,model) {
  eta <- model$coefficients[1] + model$coefficients[2]*z ;
  1 / (1 + exp(-eta))
}

logisticline_min <- function(z,model) {
  eta <- model$coefficients[1] + model$coefficients[2]*z - 1.96*summary(model)$coefficients[2] ;
  1 / (1 + exp(-eta))
}

logisticline_max <- function(z,model) {
  eta <- model$coefficients[1] + model$coefficients[2]*z + 1.96*summary(model)$coefficients[2] ;
  1 / (1 + exp(-eta))
}

## Ggplot custom theme

theme_year <- theme(
  legend.position = c(0.3, 0.7),
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size=10),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  panel.grid = element_blank(),
  plot.caption = element_text(size = 10, color = "gray50"),
  plot.title = element_text(face="bold", size=12)
  
)

# Setting the working directory -------------------------------------------

setwd("/Users/stefanomammola/Desktop/Mammola et al. Spider_etymology/") # <---- change me :)

# Loading the database ----------------------------------------------------

db <- read.csv(file = "db_etymology.csv", header = TRUE, sep = "\t", as.is = FALSE)
head(data)
str(data)

# Calculating number of characters for each name --------------------------
Ncar_Gen   <- sapply(as.vector(db$genus),nchar) #genus
Ncar_Sp    <- sapply(as.vector(db$species),nchar) #species
Ncar_GenSp <- sapply(as.vector(paste(db$genus,db$species,sep='')),nchar) #genus + species
Letter <- substr(as.vector(db$species),1, 1)

par(mfrow=c(1,3))
hist(Ncar_Gen,main="gen")
hist(Ncar_Sp,main="sp")
hist(Ncar_GenSp,main="gen sp")

# Storing the data
db <- data.frame(db,Ncar_Gen,Ncar_Sp,Ncar_GenSp,GenSp = paste(db$genus,db$species,sep=' '))

# Geography of species ----------------------------------------------------

library("arakno")
library("dplyr")
library("countrycode")

# ISO code wsc
ISO <- force(wscmap)
colnames(ISO)[1] <- "distribution"

# ISO code vs country
data_cont <- force(codelist)
data_cont <- data_cont %>% select(continent,code=genc3c)
data_cont <- na.omit(data_cont)

Cont <- unique(data_cont$continent)

# Match
db$distribution <- as.character(db$distribution)

distribution_string <- lapply(1:nrow(db), function (x) tolower(strsplit(db$distribution[x], 
                       c("\\, | and | to |from |\\?|\\/|probably|possibly|introduced"))[[1]]) )

list_distr <- lapply(1:length(distribution_string), 
                     function (x) colSums(ISO[ISO$distribution %in% distribution_string[[x]] , ][,2:ncol(ISO)]))

distribution <- do.call("rbind",list_distr)

continent_list <- list()

for(i in 1:nrow(distribution))  {
  
  distr_i <- distribution[i, ]
  continent <- unique(data_cont[data_cont$code %in% names(distr_i[distr_i > 0]),]$continent) 
  
  continent <- factor(continent, levels = Cont)
  
  continent_list[[i]] <- table(continent)
  }

final_distribution <- data.frame(code = db$code, genus = db$genus, species = db$species, distribution = db$distribution, do.call("rbind",continent_list))
write.csv(final_distribution, "distribution.csv")

# General statistics ------------------------------------------------------

#number of species
length(unique(db$GenSp))

#number of subspecies 
nrow(db)-length(unique(db$GenSp))

#number of unique species etymologies
length(unique(db$species))

#Yearly range of the database
range(db$year)

#Most prolific authors
authors <- do.call("c",str_split(db$author, c(", "))) #separate author by comma
authors <- do.call("c",str_split(authors, c(" & "))) #separate author by &
authors <- data.frame(sort(table(authors), decreasing = TRUE))

hist(authors$Freq, breaks = 50)

#xlsx::write.xlsx(authors,"Authors.xlsx")

#Type of check
table(db$Source)

#Contributors
table(db$Contributor)

#Etymology counts

nrow(db) - nrow(db[db$N_meanings>0,]) #no etymology

sum_ety <- db[db$N_meanings>0,] %>% 
               dplyr::select(size,
                             shape,
                             colour,
                             behaviour,
                             ecology,
                             geography,
                             scientists,
                             otherPeople,
                             modernCulture,
                             pastCulture,
                             others)
sum_ety[is.na(sum_ety)] <- 0

(sum_ety <- apply(sum_ety, 2, sum))

# Random sampling ---------------------------------------------------------

total = 8*50

names <- c("Stefano",
           "Diego",
           "Nathanel",
           "Atysha",
           "Dylan",
           "Julien",
           "Christophe",
           "Stephen")

db_inferred <- db[db$Source == "Inferred",] ; droplevels(db_inferred)
index <- sample(db_inferred$code) [1 : total]

db_sampled   <- db_inferred[db_inferred$code %in% index,]
db_sampled <- data.frame(Assignment = sort(rep(names, 50)), db_sampled)

#xlsx::write.xlsx(db_sampled,"sampled_cross_validation.xlsx")

## ------------------------------------------------------------------------
# 'Curiosity box #1'

# What are the most frequent species names?

# most frequent names
Bar_plot <- data.frame(sort(table(db$species))) ; colnames(Bar_plot) <- c("sp","N")

top30 <- Bar_plot[Bar_plot$N>30,] ; rm(Bar_plot)

col <- droplevels(top30$sp)
levels(col) <- c("blueviolet",rep("grey30",2),"blueviolet",rep("grey30",2),
                 rep("grey30",2),rep("blueviolet",2),rep("grey30",2),
                 "grey30","blueviolet",rep("grey30",3),"blueviolet",
                 rep("grey30",3),"blueviolet",rep("grey30",2),
                 rep("grey30",6),
                 rep("grey30",7),
                 rep("blueviolet",2))
 
(plot_CountSP <- ggplot(top30, aes(x= sp, y=N))+
    geom_bar(stat="identity", colour = "black", fill= col)+
    labs(title="Most frequent spider names (N > 30 occurrences across the World Spider Catalog)", x=NULL, y = "Count")+
    coord_flip()+
    theme_bw()+
    theme_year+
    theme(axis.text.y= element_text(size = 10,face = "italic")))

## ------------------------------------------------------------------------
# 'Curiosity box #2'

# What are the longest and shortest species name?

# Genus + species
db[db$Ncar_GenSp == range(Ncar_GenSp)[2],]$GenSp #Longest binomial name: "Chilobrachys jonitriantisvansickleae" (35 char)
db[db$Ncar_GenSp == range(Ncar_GenSp)[1],]$GenSp #Shortest binomial name: "Gea eff" (6 char)

# Only species
db[db$Ncar_Sp == range(Ncar_Sp)[2],]$species #Longest specific epithet: "santaritadopassaquatrensis" (26 char)
db[db$Ncar_Sp == 2,]$species #Shortest specific epithet: ab an ef fo la kh mi no oz oz wa wu yi zu

## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
# 'Curiosity box #3'

# What is the distribution of etymologies by letter

(plot_Letter <- ggplot(data.frame(table(Letter)),aes(x= Letter, y=Freq))+
    geom_bar(stat="identity", colour = "black")+
    labs(title=NULL, x=NULL, y = "Count")+
    theme_bw()+
    theme_year+
    theme(axis.text.y= element_text(size = 10,face = "italic")))

## ------------------------------------------------------------------------

# Summarizing character data by year
db_year_chr <- db %>% 
  group_by(year) %>% 
  summarise(Ncar_GenSp_mean = mean(Ncar_GenSp), 
            Ncar_GenSp_se = std(Ncar_GenSp),
            Ncar_Sp_mean = mean(Ncar_Sp), 
            Ncar_Sp_se = std(Ncar_Sp)) 

bar1 <- data.frame(Ncar_GenSp)
(plot_char1 <- ggplot(bar1,aes(x=Ncar_GenSp))+
    labs(title = "(a)",
         x = "N° of characters (Genus + species)", 
         y = "Frequency")+
    geom_bar()+
    theme_bw() + theme_year) ; rm(bar1)

bar2 <- data.frame(Ncar_Sp)
(plot_char2 <- ggplot(bar2,aes(x=Ncar_Sp))+
    labs(title = "(b)",
         x = "N° of characters (Species)", 
         y = "Frequency")+
  geom_bar()+
  theme_bw() + theme_year) ; rm(bar2)


(plot_char3 <- ggplot(db_year_chr[db_year_chr$year<2020,], aes(x=year, y=Ncar_Sp_mean)) + 
    
    geom_line(linetype = 1,alpha=1,col="black") + 
    
    geom_vline(aes(xintercept = 1900),linetype = 1, color = "gray70", size = 0.2) +
    
    scale_x_continuous(breaks = c(1757,1800,1850,1900,1950,2000,2019),
                       labels=as.character(c(1757,1800,1850,1900,1950,2000,2019)))+ 
    
    labs(title = "(c)",
         x = NULL, 
         y = "N° of characters (species)\n[Annual average]")+
    
    theme_bw()+
    theme_year
)

pdf("Figure_XX.pdf",width = 8, height = 5, paper = 'special')

lay_char <- rbind(c(1,2),c(3,3))

gridExtra::grid.arrange(plot_char1,plot_char2,plot_char3, layout_matrix = lay_char)

dev.off()


## ------------------------------------------------------------------------
# 'Curiosity box #4'

# How many Arbitrary?

arb <- startsWith(as.character(db$Notes), "Arbitrary combination of letters")
table(arb)

## ------------------------------------------------------------------------

###########################################################################
# Temporal patterns -------------------------------------------------------
###########################################################################

db2 <- db[db$N_meanings>0,]

db2 <- db2 %>% dplyr::select(year,
                      size,
                      shape,
                      colour,
                      behaviour,
                      ecology,
                      geography,
                      scientists,
                      otherPeople,
                      modernCulture,
                      pastCulture,
                      others) %>% data.frame

db2 <- data.frame(year = db2$year,
                  morpho = rowSums(db2[,2:4]),
                  ecol = rowSums(db2[,5:6]), 
                  geo = db2[,7],
                  people = rowSums(db2[,8:9]),
                  culture = rowSums(db2[,10:11]),
                  other = db2[,12])

db2[,2:7] <- apply(db2[,2:7], 2, function (x) ifelse(x > 1, 1 , x)) %>% data.frame
db2[is.na(db2)] <- 0

# Database absolute values
db_year <- apply(db2[,2:7], 2, function (x) tapply(x, as.factor(db2$year), sum)) %>% data.frame

db_year_plot <- data.frame(Year  = as.numeric(rep(rownames(db_year), 6 )),
                       Value = c(db_year$morpho,
                                 db_year$ecol,
                                 db_year$geo,
                                 db_year$people,
                                 db_year$culture,
                                 db_year$other),
                       Type = c(rep("Morphology",nrow(db_year)),
                                rep("Ecology & Behavior",nrow(db_year)),
                                rep("Geography",nrow(db_year)),
                                rep("People",nrow(db_year)),
                                rep("Modern & past culture",nrow(db_year)),
                                rep("Others",nrow(db_year))),
                       Tot = rep(rowSums(db_year),6)
                       )

db_year_plot$Type <- factor(db_year_plot$Type, 
                            levels = c("Morphology",
                                      "Ecology & Behavior",
                                      "Geography",
                                      "People",
                                      "Modern & past culture",
                                      "Others"))

# Database temporal trends
db_year       <- db_year %>% rownames_to_column("year")
db_year$year  <- as.numeric(db_year$year)
db_model      <- data.frame(db_year, 
                            tot = rowSums(db_year[,2:7]))

#Plot parameters
y2 <- seq(from = min(db_model$year), to = max(db_model$year), 1) #temporal series of interest
COL <- c("black", "darkgreen", "blue", "purple", "orange", "cyan4")
levels(db_year_plot$Type) <- c(paste0("Morphology [n= ", sum(db_model$morpho),"]"),
                               paste0("Ecology & Behavior [n= ", sum(db_model$ecol),"]"),
                               paste0("Geography [n= ", sum(db_model$geo),"]"),
                               paste0("People [n= ", sum(db_model$people),"]"),
                               paste0("Modern & past culture [n= ", sum(db_model$culture),"]"),
                               paste0("Others [n= ", sum(db_model$other),"]"))

(plot_trend1 <- ggplot(db_year_plot) +
    
    geom_line(aes(x=Year, y= Value, color=Type),size=.5,linetype = 1) + 
    
    scale_color_manual(values= COL)+
    labs(x = NULL, 
         y = "Total number of etymologies",
         title = "A")+
    #subtitle =  "Absolute values")+
    
    scale_x_continuous(breaks = c(seq(from=min(db_year2$Year),to=max(db_year2$Year),by=30)))+ 
    
    theme_bw()+
    
    theme(
      legend.position = c(0.2, 0.8),
      legend.title = element_blank(),
      legend.text = element_text(size=10),
      plot.title = element_text(color="black", size=14, face="bold"),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      panel.grid = element_blank(),
      plot.caption = element_text(size = 10, color = "gray30")))

(plot_trend2 <- ggplot() +
    labs(x = NULL, 
         y = "Relative proportion of etymologies",
         title = "B")+

    geom_smooth(data = db_model, aes(y = morpho/tot, x = year),
                method = "gam", formula = y ~ s(x), col= COL[1], fill = COL[1])+
    
    geom_smooth(data = db_model, aes(y = ecol/tot, x = year),
                method = "gam", formula = y ~ s(x), col= COL[2], fill = COL[2])+
    
    geom_smooth(data = db_model, aes(y = geo/tot, x = year),
                method = "gam", formula = y ~ s(x), col= COL[3], fill = COL[3])+
    
    geom_smooth(data = db_model, aes(y = people/tot, x = year),
                method = "gam", formula = y ~ s(x), col= COL[4], fill = COL[4])+
    
    geom_smooth(data = db_model, aes(y = culture/tot, x = year),
                method = "gam", formula = y ~ s(x), col= COL[5], fill = COL[5])+
    
    geom_smooth(data = db_model, aes(y = other/tot, x = year),
                method = "gam", formula = y ~ s(x), col= COL[6], fill = COL[6])+
    
    theme_bw()+
    theme(
      plot.title = element_text(color="black", size=14, face="bold"),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      panel.grid = element_blank(),
      plot.caption = element_text(size = 10, color = "gray30"))
  
)

# (plot_trend2 <- ggplot() +
#     labs(x = NULL, 
#          y = "Relative proportion of etymologies",
#          title = "B")+
#     
#     #trend lines
#     geom_line(aes(y = logisticline(y2,model[[1]]), x = y2), colour = COL[1],linetype="solid",size=1.1,alpha=1)+
#     geom_line(aes(y = logisticline(y2,model[[2]]), x = y2), colour = COL[2],linetype="solid",size=1.1,alpha=1)+
#     geom_line(aes(y = logisticline(y2,model[[3]]), x = y2), colour = COL[3],linetype="solid",size=1.1,alpha=1)+
#     geom_line(aes(y = logisticline(y2,model[[4]]), x = y2), colour = COL[4],linetype="solid",size=1.1,alpha=1)+
#     geom_line(aes(y = logisticline(y2,model[[5]]), x = y2), colour = COL[5],linetype="solid",size=1.1,alpha=1)+
#     geom_line(aes(y = logisticline(y2,model[[6]]), x = y2), colour = COL[6],linetype="solid",size=1.1,alpha=1)+
#     
#     #confidence intervals
#     geom_ribbon(aes(ymax = logisticline_max(y2, model[[01]]),
#                     ymin = logisticline_min(y2, model[[01]]),x = y2),alpha = 0.5,fill=COL[1])+
#     geom_ribbon(aes(ymax = logisticline_max(y2, model[[02]]),
#                     ymin = logisticline_min(y2, model[[02]]),x = y2),alpha = 0.5,fill=COL[2])+
#     geom_ribbon(aes(ymax = logisticline_max(y2, model[[03]]),
#                     ymin = logisticline_min(y2, model[[03]]),x = y2),alpha = 0.5,fill=COL[3])+
#     geom_ribbon(aes(ymax = logisticline_max(y2, model[[04]]),
#                     ymin = logisticline_min(y2, model[[04]]),x = y2),alpha = 0.5,fill=COL[4])+
#     geom_ribbon(aes(ymax = logisticline_max(y2, model[[05]]),
#                     ymin = logisticline_min(y2, model[[05]]),x = y2),alpha = 0.5,fill=COL[5])+
#     geom_ribbon(aes(ymax = logisticline_max(y2, model[[06]]),
#                     ymin = logisticline_min(y2, model[[06]]),x = y2),alpha = 0.5,fill=COL[6])+
#     
#     scale_x_continuous(breaks = c(seq(from=min(db_year2$Year),to=max(db_year2$Year),by=30)))+ 
#     
#     # #Text
#     # annotate(geom="text", hjust = 0,vjust = 0.3,
#     #          x= 2021, y= logisticline_max(y2, model[[01]])[159], 
#     #          label = paste0("Morphology [n= ", sum(db_model$morpho),"]"),
#     #          color=COL[1],alpha=1)+
#     # 
#     # annotate(geom="text", hjust = 0,vjust = 0,
#     #          x= 2021, y= logisticline_max(y2, model[[02]])[159]-0.01, 
#     #          label = paste0("Ecology [n= ", sum(db_model$ecol),"]"),
#     #          color=COL[2],alpha=1)+
#     # 
#     # annotate(geom="text", hjust = 0,vjust = 0,
#     #          x= 2021, y= logisticline_max(y2, model[[03]])[159], 
#     #          label = paste0("Geography [n= ", sum(db_model$geo),"]"),
#     #          color=COL[3],alpha=1)+
#     # 
#     # annotate(geom="text", hjust = 0,vjust = 0,
#     #          x= 2021, y= logisticline_max(y2, model[[04]])[159], 
#     #          label = paste0("People [n= ", sum(db_model$people),"]"),
#     #          color=COL[4],alpha=1)+
#     # 
#     # annotate(geom="text", hjust = 0,vjust = 0,
#     #          x= 2021, y= logisticline_max(y2, model[[05]])[159]-0.01, 
#     #          label = paste0("Culture [n= ", sum(db_model$culture),"]"),
#     #          color=COL[5],alpha=1)+
#     # 
#     # annotate(geom="text", hjust = 0,vjust = 0,
#     #          x= 2021, y= logisticline_max(y2, model[[06]])[159]+0.01, 
#     #          label = paste0("Others [n= ", sum(db_model$other),"]"),
#     #          color=COL[6],alpha=1)+
#     # 
#     # coord_cartesian(xlim = c(min(y2), max(y2)), # This focuses the x-axis on the range of interest
#     #                 clip = 'off') +   # This keeps the labels from disappearing
#     # 
#     theme_bw()+
#     theme(
#       plot.title = element_text(color="black", size=14, face="bold"),
#       axis.title = element_text(size = 12),
#       axis.text.x = element_text(size = 11),
#       axis.text.y = element_text(size = 11),
#       panel.grid = element_blank(),
#       plot.caption = element_text(size = 10, color = "gray30")))
# )


pdf("Figure_temporal_trends.pdf",width = 14, height = 5, paper = 'special')

gridExtra::grid.arrange(plot_trend1,plot_trend2, nrow = 1, ncol = 2)

dev.off()

