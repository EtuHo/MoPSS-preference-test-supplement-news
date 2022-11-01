# Place Preference simsalRbim analysis Figure 1
# 28.10.2022

rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggpubr)
library(gnm)
library(prefmod)
library(reshape2)
library(rlang)
library(stringr)
library(forestmangr)
library(viridis)
library(ggrepel)

library(simsalRbim)

####################################################################################################
#------------------------------- MoPSS results 46 h all 12 mice ------------------------------------
#Data preparation
setwd("/home/birk/Ute_MoPPSprojekt/")

#read active and inactive data
data_active = read.delim("./MOPPS data prepared for simsalRbim_active time.txt", header= T, sep = "\t", dec= ".")
data_inactive = read.delim("./MOPPS data prepared for simsalRbim_inactive time.txt", header= T, sep = "\t", dec= ".")

#combine active and inactive stay durations and calculate overall percentage of stay duration
data_all <- bind_cols(data_active , data_inactive)
data_all <- mutate(data_all, (quantityA = quantityA...5 + quantityA...11)/2 , (quantityB = quantityB...6 + quantityB...12)/2)#neue Spalte mit addiererten Werten /2 erstellen
data_all <- select(data_all, compiled_studies...1, subjectID...2, optionA...3, optionB...4, "(quantityA = quantityA...5 + quantityA...11)/2", "(quantityB = quantityB...6 + quantityB...12)/2") 
data_all <- data_all %>% rename(compiled_studies      = compiled_studies...1,
                                  subjectID             = subjectID...2,
                                  optionA               = optionA...3, 
                                  optionB               = optionB...4,
                                  quantityA             = "(quantityA = quantityA...5 + quantityA...11)/2",
                                  quantityB             = "(quantityB = quantityB...6 + quantityB...12)/2")                    

##### structural enrichments #######################################################################
data_structural = filter(data_all, compiled_studies == "structural")

dat        <- data_structural
simOpt     <- "second_plane"
GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt, )
worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.1,0.3) )

all_46h_struc   <- bimeval(ydata     = predat, 
                      worth     = worth$worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.1,0.3))

CEerror = round(mean(all_46h_struc$errors$CE),2)
all_46h_struc = all_46h_struc$p
all_46h_struc <- all_46h_struc + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_46h_struc <- all_46h_struc + theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_46h_struc <- all_46h_struc + labs(title="46h",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5))
all_46h_struc


##### active enrichments "foraging"  ###############################################################
data_active = filter(data_all, compiled_studies == "active")

dat        <- data_active
simOpt     <- "latticeball"
GT         <- c("flappuzzle", "treatball",  "slidingpuzzle", "tube_stones" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.6))

all_46h_act   <- bimeval(ydata     = predat, 
                      worth     = worth$worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0,0.6))

CEerror = round(mean(all_46h_act$errors$CE),2)
all_46h_act = all_46h_act$p
all_46h_act <- all_46h_act + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_46h_act <- all_46h_act +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_46h_act <- all_46h_act + labs(title = "46h",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_46h_act


##### housing enrichments  #########################################################################
data_housing = filter(data_all, compiled_studies == "housing")

dat        <- data_housing
simOpt     <- "floorhouse"
GT         <- c("paperhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4))

all_46h_hous   <- bimeval(ydata     = predat, 
                      worth     = worth$worth,
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.0,0.35))

CEerror = round(mean(all_46h_hous$errors$CE),2)
all_46h_hous = all_46h_hous$p
all_46h_hous <- all_46h_hous + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_46h_hous <- all_46h_hous +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_46h_hous <- all_46h_hous + labs(title = "46h",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_46h_hous


####################################################################################################
#------------------------------- MoPSS results active time all mice --------------------------------

data = read.delim("./MOPPS data prepared for simsalRbim_active time.txt", header= T, sep = "\t", dec= ".")

##### structural enrichments #######################################################################
data_structural = filter(data, compiled_studies == "structural")
data_structural = select(data_structural, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_structural
simOpt     <- "second_plane"
GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt, )
worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.1,0.3) )

all_active_struc   <- bimeval(ydata     = predat, 
                      worth     = worth$worth,
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.1,0.3))

CEerror = round(mean(all_active_struc$errors$CE),2)
all_active_struc = all_active_struc$p
all_active_struc <- all_active_struc + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_active_struc <- all_active_struc +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_active_struc <- all_active_struc + labs(title = "active time",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_active_struc <- all_active_struc + ylab("")#removes title of yaxis
all_active_struc

##### active enrichments  ##########################################################################
data_active = filter(data, compiled_studies == "active")
data_active = select(data_active, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_active
simOpt     <- "latticeball"
GT         <- c("flappuzzle", "treatball",  "slidingpuzzle", "tube_stones" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.6))

all_active_act   <- bimeval(ydata     = predat, 
                      worth     = worth$worth,
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0,0.6))

CEerror = round(mean(all_active_act$errors$CE),2)
all_active_act = all_active_act$p
all_active_act <- all_active_act + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_active_act <- all_active_act +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_active_act <- all_active_act + labs(title = "active time",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_active_act <- all_active_act + ylab("")#removes title of yaxis
all_active_act


##### housing enrichments  #########################################################################
data_housing = filter(data, compiled_studies == "housing")
data_housing = select(data_housing, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_housing
simOpt     <- "floorhouse"
GT         <- c("paperhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4))

all_active_hous   <- bimeval(ydata     = predat, 
                      worth     = worth$worth,
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.0,0.35))

CEerror = round(mean(all_active_hous$errors$CE),2)
all_active_hous = all_active_hous$p
all_active_hous <- all_active_hous + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_active_hous <- all_active_hous +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_active_hous <- all_active_hous + labs(title = "active time",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_active_hous <- all_active_hous + ylab("")#removes title of yaxis
all_active_hous


####################################################################################################
#------------------------------- MoPSS results inactive time all mice ------------------------------

data = read.delim("./MOPPS data prepared for simsalRbim_inactive time.txt", header= T, sep = "\t", dec= ".")

##### structural enrichments #######################################################################
data_structural = filter(data, compiled_studies == "structural")
data_structural = select(data_structural, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_structural
simOpt     <- "rope"
GT         <- c("mouseswing", "clip_papertube",  "second_plane", "clip_plastictube" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.1,0.3))

all_inactive_struc   <- bimeval(ydata     = predat, 
                      worth     = worth$worth,
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.1,0.3))

CEerror = round(mean(all_inactive_struc$errors$CE),2)
all_inactive_struc = all_inactive_struc$p
all_inactive_struc <- all_inactive_struc + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_inactive_struc <- all_inactive_struc +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_inactive_struc <- all_inactive_struc + labs(title = "inactive time",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_inactive_struc <- all_inactive_struc + ylab("")
all_inactive_struc

##### active enrichments ###########################################################################
data_active = filter(data, compiled_studies == "active")
data_active = select(data_active, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_active
simOpt     <- "flappuzzle"
GT         <- c("latticeball", "treatball",  "slidingpuzzle", "tube_stones" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.6))

all_inactive_act   <- bimeval(ydata     = predat, 
                      worth     = worth$worth,
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0,0.6))

CEerror = round(mean(all_inactive_act$errors$CE),2)
all_inactive_act = all_inactive_act$p
all_inactive_act <- all_inactive_act + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_inactive_act <- all_inactive_act +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_inactive_act <- all_inactive_act + labs(title = "inactive time",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_inactive_act <- all_inactive_act + ylab("")
all_inactive_act


##### housing enrichments ##########################################################################
data_housing = filter(data, compiled_studies == "housing")
data_housing = select(data_housing, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_housing
simOpt     <- "floorhouse"
GT         <- c("paperhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     =c(0.18,0.22) )

all_inactive_hous   <- bimeval(ydata     = predat, 
                      worth     = worth$worth,
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.0,0.35))

CEerror = round(mean(all_inactive_hous$errors$CE),2)
all_inactive_hous = all_inactive_hous$p
all_inactive_hous <- all_inactive_hous + theme(plot.title = element_text(hjust = 0.5))#centers the title
all_inactive_hous <- all_inactive_hous +  theme(legend.position  = "none", legend.justification = c("left"))#removes legend for bubbles and puts them under the plot
all_inactive_hous <- all_inactive_hous + labs(title = "inactive time",subtitle=paste0("total CE = ",CEerror,"%")) + #plots the title
  theme(plot.title = element_text(hjust = 0.5) )
all_inactive_hous <- all_inactive_hous + ylab("")
all_inactive_hous


str(all_inactive_hous)


############################################################# Plots #################################################################################
#Abbildung all mice: foraging, structural, housing enrichments in 46 h active, inactive


tiff("Ranking_1.tiff", units="px", width=3500, height=2726, res = 200, compression = "lzw")
ggarrange(all_46h_act, all_active_act, all_inactive_act,
                 all_46h_struc, all_active_struc, all_inactive_struc,
                 all_46h_hous, all_active_hous, all_inactive_hous,
                 labels = c("A foraging", "", "","B structural", "", "","C housing", "", ""),
                 ncol = 3, nrow = 3)
dev.off()


