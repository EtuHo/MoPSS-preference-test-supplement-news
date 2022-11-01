# Place Preference simsalRbim analysis Figure 2
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


#save tables
temp = data_active
temp$group = c(rep(1,120),rep(2,120),rep(3,120))
temp = temp[, c("compiled_studies","subjectID","group","optionA","optionB","quantityA","quantityB")]
temp$compiled_studies[temp$compiled_studies == "active"] = "foraging"
write.table(temp,"./placePrefData_S2_Table_active.csv",sep=",",row.names=FALSE,col.names=TRUE)

temp = data_inactive
temp$group = c(rep(1,120),rep(2,120),rep(3,120))
temp = temp[, c("compiled_studies","subjectID","group","optionA","optionB","quantityA","quantityB")]
temp$compiled_studies[temp$compiled_studies == "active"] = "foraging"
write.table(temp,"./placePrefData_S3_Table_inactive.csv",sep=",",row.names=FALSE,col.names=TRUE)

temp = data_all
temp$group = c(rep(1,120),rep(2,120),rep(3,120))
temp = temp[, c("compiled_studies","subjectID","group","optionA","optionB","quantityA","quantityB")]
temp$compiled_studies[temp$compiled_studies == "active"] = "foraging"
write.table(temp,"./placePrefData_S1_Table_all.csv",sep=",",row.names=FALSE,col.names=TRUE)


#------------------------------------ MoPSS results grouped by cage (a 4 mice each) ----------------
group1 <- filter(data_all, subjectID == c("mouse1","mouse2","mouse3","mouse4"))
group2 <- filter(data_all, subjectID == c("mouse5","mouse6","mouse7","mouse8"))
group3 <- filter(data_all, subjectID == c("mouse9","mouse10","mouse11","mouse12"))


####################################################################################################
#----------- neue Bimworth2 Funktion um Plot extrahieren und bearbeiten zu k?nnen ------------------
bimworth2 <- function (ydata = NULL, GT = NULL, simOpt = NULL, randOP = FALSE,
                       intrans = FALSE, showPlot = FALSE, ylim = c(0, 0.8), size = 5,
                       verbose = FALSE)
{
  worth <- NULL
  optionList <- c(GT, simOpt)
  if (randOP == TRUE) {
    nosim <- ydata[ydata$sim == FALSE, ]
    simdat <- ydata[ydata$sim == TRUE, ]
    simdat$result <- sample(c(0, 1, -1), replace = TRUE,
                            length(simdat$result))
    ydata <- rbind(nosim, simdat)
  }
  else {
  }
  if (intrans == TRUE) {
    I <- bimintrans(dat = ydata, idcolumn = "subjectID",
                    I2 = "optionB", I1 = "optionA", response = "result")
  }
  else {
  }
  modData <- dcast(ydata, formula = subjectID ~ test, fun.aggregate =
                     sum,
                   value.var = "result")
  modData$subjectID <- NULL
  modelY <- llbt.design(modData, nitems = length(optionList),
                        objnames = optionList)
  formula <- as.formula(paste("y~",
                              paste(optionList[1:(length(optionList) -
                                                    1)], collapse = "+")))
  h1Y <- gnm(formula, data = modelY, family = poisson)
  if (showPlot == "coef") {
    f <- function(d) {
      fit <- do.call("gnm", list(formula, data = d,
                                 family = poisson))
      stats::confint(fit)
    }
    CI <- as.data.frame(f(as.data.frame(modelY)))
    rownames(CI)[1] <- simOpt
    hworY <- as.matrix(coef(h1Y))
    colnames(hworY) <- "estimate"
    row.names(hworY)[1] <- simOpt
    new_df <- cbind(hworY[row.names(CI), ], CI)
    colnames(new_df) <- c("estimate", "lwr",
                          "upr")
    new_df$item <- rownames(new_df)
    new_df$item <- factor(new_df$item, levels =
                            new_df$item[order(new_df$estimate)])
    if (verbose == TRUE) {
      print(new_df)
    }
    else {
    }
    p <- ggplot(new_df, aes(x = item, y = estimate)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
      labs(title = "Item comparisons (no simulation)",
           subtitle = "Model coefficients with 95% confidence
intervals") +
      ylab("Estimate") + xlab("") + labs(colour = "Item") +
      theme_bw()
    p <- p + coord_flip()
    print(p)
  }
  else {
  }
  if (verbose == TRUE) {
    print(summary(h1Y))
  }
  else {
  }
  hworY <- llbt.worth(h1Y, outmat = "worth")
  if (showPlot == "worth") {
    hcl <- NULL
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    n = dim(hworY)[1]
    cols = gg_color_hue(n)
    p <- ggplot(data.frame(hworY), aes(x = rep(1, dim(hworY)[1]),
                                       y = worth)) + geom_line() + geom_point(color = "black",
                                                                              shape = 21, size = size, fill = cols, stroke = 1.2) +
      labs(title = "Preferences") + ylab("Worth value") +
      xlab("Items") + ylim(ylim) + theme_bw() +
      scale_x_discrete(limits = factor(1))
    p <- p + theme(legend.position = "none")
    p <- p + geom_label_repel(aes(label =
                                    rownames(data.frame(hworY))),
                              size = 4, box.padding = unit(1.2, "lines"),
                              point.padding = unit(1.2, "lines"), show.legend = FALSE)
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.line = element_line(colour = "black"),
                   strip.background = element_rect(fill = "white",
                                                   colour = "black", size = 0.8), strip.text =
                     element_text(size = 12),
                   axis.text.x = element_text(size = 12), axis.title.x =
                     element_text(size = 13),
                   axis.text.y = element_text(size = 13), axis.title.y =
                     element_text(size = 14))
    p <- p + theme(axis.text.x = element_blank())
    print(p)
  }
  else {
  }
  if (intrans == TRUE) {
    return(list(worth = hworY, I = I))
  }
  else {
    return(list(worth = hworY, p = p))
  }
}


####################################################################################################
#------------------------------------ preference ranking group1 ------------------------------------
data_structural = filter(group1, compiled_studies == "structural")

dat        <- data_structural
simOpt     <- "second_plane"
GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt, )
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4) )

g1_46h_struc <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 1") 

g1_46h_struc


##### active enrichments ###########################################################################
data_active = filter(group1, compiled_studies == "active")

dat        <- data_active
simOpt     <- "latticeball"
GT         <- c("flappuzzle", "treatball",  "slidingpuzzle", "tube_stones" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.4))

g1_46h_act <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 1") 

g1_46h_act


##### housing enrichments ##########################################################################
data_housing = filter(group1, compiled_studies == "housing")

dat        <- data_housing
simOpt     <- "floorhouse"
GT         <- c("paperhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4))

g1_46h_hous <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 1") 

g1_46h_hous


####################################################################################################
#-------------------------preference ranking group2-------------------------------------------------
data_structural = filter(group2, compiled_studies == "structural")

dat        <- data_structural
simOpt     <- "second_plane"
GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt, )
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4) )

g2_46h_struc <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 2") +
  ylab("")

g2_46h_struc


##### active enrichments ###########################################################################
data_active = filter(group2, compiled_studies == "active")

dat        <- data_active
simOpt     <- "latticeball"
GT         <- c("flappuzzle", "treatball",  "slidingpuzzle", "tube_stones" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.4))

g2_46h_act <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 2")+
  ylab("") 

g2_46h_act


##### housing enrichments ##########################################################################
data_housing = filter(group2, compiled_studies == "housing")

dat        <- data_housing
simOpt     <- "floorhouse"
GT         <- c("paperhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4))

g2_46h_hous <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 2")+
  ylab("") 

g2_46h_hous


####################################################################################################
#-----------------------------preference ranking group3---------------------------------------------
data_structural = filter(group3, compiled_studies == "structural")

dat        <- data_structural
simOpt     <- "second_plane"
GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt, )
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4) )

g3_46h_struc <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 3") +
  ylab("")

g3_46h_struc


##### active enrichments ###########################################################################
data_active = filter(group3, compiled_studies == "active")

dat        <- data_active
simOpt     <- "latticeball"
GT         <- c("flappuzzle", "treatball",  "slidingpuzzle", "tube_stones" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.4))

g3_46h_act <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 3") +
  ylab("")

g3_46h_act


##### housing enrichments ##########################################################################
data_housing = filter(group3, compiled_studies == "housing")

dat        <- data_housing
simOpt     <- "floorhouse"
GT         <- c("paperhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )
predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
worth      <- bimworth2(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4))

g3_46h_hous <- 
  worth$p +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Group 3") +
  ylab("")

g3_46h_hous


############################################################# Plots zusammenf?gen ##################
#Abbildung all mice: foraging, structural, housing enrichments in 46 h active, inactive

tiff("Ranking_2.tiff", units="px", width=3500, height=2726, res = 200, compression = "lzw")
ggarrange(g1_46h_act, g2_46h_act , g3_46h_act,
          g1_46h_struc , g2_46h_struc , g3_46h_struc,
          g1_46h_hous , g2_46h_hous , g3_46h_hous,
          labels = c("A foraging", "", "","B structural", "", "","C housing", "", ""), ncol = 3, nrow = 3)
dev.off()





