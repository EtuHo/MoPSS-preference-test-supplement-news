# Place Preference Plots und Statistik
# 27.10.2022

rm(list=ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("/home/purzel/Homeoffice/Ute/preftest/")


####################################################################################################
### INACTIVE time analyse ##########################################################################
####################################################################################################

#load data
newplacePreference_invI = read.table("./placePrefData_inactive_time.csv", sep=",")

################# Daten umformen ###################################################################
#placePreferenc-inv array in data.frame umwandeln, Zeile 1 gibt Versuchstage an (1-60) und die Spalten 1-12 die Aufenthaltsdauer in % der Tiere
placePreference_inv.df <- as.data.frame(newplacePreference_invI)

#Zeile 1 umbenennen in Mausnummern
placePreference_inv.df <- placePreference_inv.df %>% 
  rename(
    mouse1 = V1,
    mouse2 = V2,
    mouse3 = V3,
    mouse4 = V4,
    mouse5 = V5,
    mouse6 = V6,
    mouse7 = V7,
    mouse8 = V8,
    mouse9 = V9,
    mouse10 = V10,
    mouse11 = V11,
    mouse12 = V12
  )
#transformieren der Tabelle (Spalten zu Zeilen)
place_preference_transformed <-data.frame(t(placePreference_inv.df))

#summiert Preferenzen von Versuchstag X1+X2, X3+X4, usw. und teilt durch 2 um Gesamtpreferenz der 2 Versuchstage 1-30 zu erhalten f?r o.g. 1. Enrichment
place_preference_30daysR <- transmute(place_preference_transformed,
                                      V01= (X1 + X2)/2,
                                      V02= (X3 + X4)/2, 
                                      V03= (X5 + X6)/2, 
                                      V04= (X7 + X8)/2,
                                      V05= (X9 + X10)/2,
                                      V06= (X11 + X12)/2,
                                      V07= (X13 + X14)/2,
                                      V08= (X15 + X16)/2,
                                      V09= (X17 + X18)/2,
                                      V10= (X19 + X20)/2,
                                      V11= (X21 + X22)/2,
                                      V12= (X23 + X24)/2,
                                      V13= (X25 + X26)/2,
                                      V14= (X27 + X28)/2,
                                      V15= (X29 + X30)/2,
                                      V16= (X31 + X32)/2,
                                      V17= (X33 + X34)/2,
                                      V18= (X35 + X36)/2,
                                      V19= (X37 + X38)/2,
                                      V20= (X39 + X40)/2,
                                      V21= (X41 + X42)/2,
                                      V22= (X43 + X44)/2,
                                      V23= (X45 + X46)/2,
                                      V24= (X47 + X48)/2,
                                      V25= (X49 + X50)/2,
                                      V26= (X51 + X52)/2,
                                      V27= (X53 + X54)/2,
                                      V28= (X55 + X56)/2,
                                      V29= (X57 + X58)/2,
                                      V30= (X59 + X60)/2    )

#erstellt Tabelle mit Pr?ferenzen der linken (2.Seiten)
place_preference_30daysL <- 100-(place_preference_30daysR)

#erstellt Tabelle mit beiden Seiten L+R ?ber 30 Versuchstage
place_preference_L_R_30days <- bind_rows(
  mutate(place_preference_30daysL, side = "L"),
  mutate(place_preference_30daysR, side = "R")
) 

#erstellt Tabelle mit Spalten day (Versuchstag), side, preference f?r leichteres plotten
place_preference_L_R_30days_transformed <- place_preference_L_R_30days %>% gather(day, preference, V01:V30)

#speichert place preference_L_30 days Tabelle f?r ranking Auswertung, weiter mit R skript data preparation in preference ranking
write.table(place_preference_30daysL, "place_preference_30daysL_inactive time.txt", sep = "\t", row.names = T)


#################### Grafiken erstellen ############################################################
#Boxplot erstellen
#boxplot inactive time alles gruppiert
ggplot(place_preference_L_R_30days_transformed, aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  xlab("day")+
  ylab("stay duration in %") +
  ggtitle("MOPSS enrichment preferences inactive time")

#boxplot inactive time alles small multiple
ggplot(place_preference_L_R_30days_transformed, aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h") +
  xlab("day")+
  ylab("stay duration in %") +
  ggtitle("MOPSS enrichment preferences inactive time")

#boxplot inactive time Versuchtage 1-10 structural enrichment small multiple
ggplot(subset(place_preference_L_R_30days_transformed, day %in% c("V01","V02","V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10")), aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h", ncol=5, 
             labeller = as_labeller( c("V01" = "Seil vs. Mausschaukel",
                                       "V02" = "Clip+Pappr?hre vs. Seil",
                                       "V03" = "Mausschaukel vs. Clip+Pappr?hre", 
                                       "V04" = "Clip+Pappr?hre vs. 2.Ebene", 
                                       "V05" = "2. Ebene vs. Seil", 
                                       "V06" = "Clip+Plastikr?hre vs. Seil", 
                                       "V07" = "Clip+Plastikr?hre vs. 2.Ebene", 
                                       "V08" = "Clip+Plastikr?hre vs. Clip+Pappr?hre", 
                                       "V09" = "Clip+Plastikr?hre vs. Mausschaukel", 
                                       "V10" = "2.Ebene vs. Mausschaukel")     )) +
  xlab("day")+
  ylab("stay duration in %") +
  ggtitle("MOPSS structural enrichment preferences inactive time")


ggplot(subset(place_preference_L_R_30days_transformed, day %in% c("V01","V02","V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10")), aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h", ncol=5, 
             labeller = as_labeller( c("V01" = "rope vs. mouse swing",
                                       "V02" = "clip+paper tube vs. rope",
                                       "V03" = "mouse swing vs. clip+paper tube", 
                                       "V04" = "clip+paper tube vs. 2nd plane", 
                                       "V05" = "2nd plane vs. rope", 
                                       "V06" = "clip+plastic tube vs. rope", 
                                       "V07" = "clip+plastic tube vs. 2nd plane", 
                                       "V08" = "clip+plastic tube vs. clip+paper tube", 
                                       "V09" = "clip+plastic tube vs. mouse swing", 
                                       "V10" = "2nd plane vs. mouse swing")     )) +
  xlab("day")+
  ylab("stay duration in %") +
  ggtitle("MOPSS structural enrichment preferences inactive time")

#boxplot inactive time Versuchtage 11-20 active enrichment small multiple
ggplot(subset(place_preference_L_R_30days_transformed, day %in% c("V11","V12","V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20")), aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h", ncol=5, 
             labeller = as_labeller( c("V11" = "treat ball vs. lattice ball",
                                       "V12" = "tube+stones vs. flap puzzle",
                                       "V13" = "flap puzzle vs. treat ball", 
                                       "V14" = "tube+stones vs. lattice ball", 
                                       "V15" = "tube+stones vs. sliding puzzle", 
                                       "V16" = "tube+stones vs. treat ball", 
                                       "V17" = "lattice ball vs. sliding puzzle", 
                                       "V18" = "treat ball vs. sliding puzzle", 
                                       "V19" = "flap puzzle vs. sliding puzzle", 
                                       "V20" = "lattice ball vs. flap puzzle")     )) +
  xlab("day")+
  ylab("stay duration in %") +
  ggtitle("MOPSS active enrichment preferences inactive time")

#boxplot inactive time Versuchtage 21-30 housing enrichment small multiple
ggplot(subset(place_preference_L_R_30days_transformed, day %in% c("V21","V22","V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30")), aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h", ncol=5, 
             labeller = as_labeller( c("V21" = "floor house vs. wooden angle",
                                       "V22" = "wooden angle with hole vs. house ball",
                                       "V23" = "wooden angle with hole vs. wooden angle", 
                                       "V24" = "wooden angle vs. paper house", 
                                       "V25" = "paper house vs. floor house", 
                                       "V26" = "floor house vs. wooden angle with hole", 
                                       "V27" = "paper house vs. wooden angle with hole", 
                                       "V28" = "house ball vs. floor house", 
                                       "V29" = "wooden angle vs. house ball", 
                                       "V30" = "house ball vs. paper house")     )) +
  xlab("day")+
  ylab("stay duration in %") +
  ggtitle("MOPSS housing enrichment preferences inactive time")



################# Statistical Analysis #############################################################

# 2 verbundene Stichproben
#erstellt Tabelle mit Differenzwerten (L-R) der Pr?ferenzen
place_preference_30days_diff <- place_preference_30daysL - place_preference_30daysR

#Test auf Normalverteilung der Differenzen: shapiro wilk test = p- Wert gr??er 0.05 = Normalverteilung angenommen

#f?hrt f?r jede Spalte in place_preference_diff shapiro test durch
shapiro_test <- lapply(place_preference_30days_diff, shapiro.test)

#wandelt Testergebnis in dataframe um und transponiert dataframe
shapiro_test_results <- t(as.data.frame(sapply( shapiro_test , `[` , c("statistic", "p.value")  )))
shapiro_test_results <- as.data.frame(shapiro_test_results)

#f?gt Spalte mit Versuchstag hinzu
shapiro_test_results <- mutate(shapiro_test_results, day= 1:30) 

#normalverteilte Differenzen (Versuche)+ t-Test f?r Differenzen mit mu=0 d.h. Mittelwertdifferenzen unterscheiden sich von 0 (bei 50%-50% Pr?ferenz w?re Differenz O) (t.test)
shapiro_test_results_norm <- filter(shapiro_test_results, p.value > 0.05)
#ergibt (Tage 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 20 21 22 23 24 25 26 27 28 29 30)
#NEU     Tage 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 20 21 22 23 24 25 26 27 28 29 30

#nicht-normalverteilte Differenzen (Versuche) + Vorzeichen- Test f?r Differenzen (binom.test) d.h. sind pos. bzw. neg. Werte der Diff. zuf?llig
shapiro_test_results_notnorm <- filter(shapiro_test_results, p.value < 0.05)
# ergibt (Tage 1 19)
#NEU      Tage 1 19

#create new table holding all p-values for the experiments
p_values_all = array(dim=c(30))


for(i in c(2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29,30))
{
  #save differences of place preference in new variable
  diff_pp = place_preference_30days_diff[,i]
  
  #perform t-test on those differences
  ttest_diff_pp = t.test(diff_pp, mu=0)
  
  #extract p-value from t-test results (removing label -> [[1]])
  p_ttest_diff_pp = ttest_diff_pp[3][[1]]
  
  #save p-value of the day into new table, at the position corresponding to the experiment day
  p_values_all[i] = p_ttest_diff_pp  
}


for(i in c(1,19))
{
  #save differences of place preference in new variable
  diff_pp = place_preference_30days_diff[,i]
  
  #perform t-test on those differences
  binomtest_diff_pp = binom.test( x=sum( diff_pp > 0 ) , n=12 , p=6/12)#sum( place_preference_30days_diff[,2] > 0 )#positive values in column
  
  #extract p-value from t-test results (removing label -> [[1]])
  p_binomtest_diff_pp = binomtest_diff_pp[3][[1]]
  
  #save p-value of the day into new table, at the position corresponding to the experiment day
  p_values_all[i] = p_binomtest_diff_pp  
}


#convert to dataframe
p_values_all = as.data.frame(p_values_all)

#add columns to dataframe p_values_all with rounded p and day
#rounding of p_values_all dataframe
p_value_round = c( round(p_values_all$p_values_all, digits=3) )    
p_values_all = mutate(p_values_all, p_value = p_value_round, day = c("V01","V02","V03","V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22", "V23","V24","V25","V26","V27","V28","V29","V30"))
p_values_all = mutate(p_values_all, stat_test = c("binom.test ", "t.test ", "t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","binom.test ", "t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test ","t.test "))


#function to add p-values from dataframe to boxplot
#boxplot+ geom_text(data= p_values_all,  aes(y=50, x=1), hjust=1.5, vjust=-5.5, size=3, label= paste0("p = ", p_values_all$p_value),  inherit.aes = FALSE )
#boxplot of all enrichments with p-value
ggplot(place_preference_L_R_30days_transformed, aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h",
             labeller = as_labeller( c("V01" = "rope vs. mouse swing",
                                       "V02" = "clip+paper tube vs. rope",
                                       "V03" = "mouse swing vs. clip+paper tube", 
                                       "V04" = "clip+paper tube vs. 2nd plane", 
                                       "V05" = "2nd plane vs. rope", 
                                       "V06" = "clip+plastic tube vs. rope", 
                                       "V07" = "clip+plastic tube vs. 2nd plane", 
                                       "V08" = "clip+plastic tube vs. clip+paper tube", 
                                       "V09" = "clip+plastic tube vs. mouse swing", 
                                       "V10" = "2nd plane vs. mouse swing",
                                       "V11" = "treat ball vs. lattice ball",
                                       "V12" = "tube+stones vs. flap puzzle",
                                       "V13" = "flap puzzle vs. treat ball", 
                                       "V14" = "tube+stones vs. lattice ball", 
                                       "V15" = "tube+stones vs. sliding puzzle", 
                                       "V16" = "tube+stones vs. treat ball", 
                                       "V17" = "lattice ball vs. sliding puzzle", 
                                       "V18" = "treat ball vs. sliding puzzle", 
                                       "V19" = "flap puzzle vs. sliding puzzle", 
                                       "V20" = "lattice ball vs. flap puzzle",
                                       "V21" = "floor house vs. wooden angle",
                                       "V22" = "wooden angle with hole vs. house ball",
                                       "V23" = "wooden angle with hole vs. wooden angle", 
                                       "V24" = "wooden angle vs. paper house", 
                                       "V25" = "paper house vs. floor house", 
                                       "V26" = "floor house vs. wooden angle with hole", 
                                       "V27" = "paper house vs. wooden angle with hole", 
                                       "V28" = "house ball vs. floor house", 
                                       "V29" = "wooden angle vs. house ball", 
                                       "V30" = "house ball vs. paper house" )     )) +
  xlab("day")+
  ylab("stay duration in %") +
  ggtitle("MOPSS enrichment preferences inactive time")+
  geom_text(data= p_values_all,  aes(y=50, x=1), hjust=2.2, vjust=-6, size=3, label= paste0("p = ", p_values_all$p_value),  inherit.aes = FALSE )


################# Plotting with p values for categories ############################################
#structural
structural <- subset(place_preference_L_R_30days_transformed, day %in% c("V01","V02","V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10"))
p_values_structural <- subset(p_values_all, day %in% c("V01","V02","V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10") )

ggplot(structural, aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h", ncol=5, 
             labeller = as_labeller( c("V01" = "rope vs. mouse swing",
                                       "V02" = "clip+paper tube vs. rope",
                                       "V03" = "mouse swing vs. clip+paper tube", 
                                       "V04" = "clip+paper tube vs. 2nd plane", 
                                       "V05" = "2nd plane vs. rope", 
                                       "V06" = "clip+plastic tube vs. rope", 
                                       "V07" = "clip+plastic tube vs. 2nd plane", 
                                       "V08" = "clip+plastic tube vs. clip+paper tube", 
                                       "V09" = "clip+plastic tube vs. mouse swing", 
                                       "V10" = "2nd plane vs. mouse swing")     )) +
  xlab("comparison")+
  ylab("stay duration in %") +
  ggtitle("MoPSS structural enrichment preferences inactive time")+
  theme(legend.position = "none")+
  scale_fill_grey(start=0.9, end=0.6)+
  ylim(0,100)+
  geom_text(data= p_values_structural,  aes(y=100, x=1), hjust=0.9, vjust=0, size=3, label= paste0(p_values_structural$stat_test,(paste0("p = ", p_values_structural$p_value))),  inherit.aes = FALSE )


#active
active <- subset(place_preference_L_R_30days_transformed, day %in% c("V11","V12","V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20"))
p_values_active <- subset(p_values_all, day %in% c("V11","V12","V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20") )

ggplot(active, aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h", ncol=5, 
             labeller = as_labeller( c("V11" = "treat ball vs. lattice ball",
                                       "V12" = "tube+stones vs. flap puzzle",
                                       "V13" = "flap puzzle vs. treat ball", 
                                       "V14" = "tube+stones vs. lattice ball", 
                                       "V15" = "tube+stones vs. sliding puzzle", 
                                       "V16" = "tube+stones vs. treat ball", 
                                       "V17" = "lattice ball vs. sliding puzzle", 
                                       "V18" = "treat ball vs. sliding puzzle", 
                                       "V19" = "flap puzzle vs. sliding puzzle", 
                                       "V20" = "lattice ball vs. flap puzzle")     )) +
  xlab("comparison")+
  ylab("stay duration in %") +
  ggtitle("MoPSS active enrichment preferences inactive time")+
  theme(legend.position = "none")+
  scale_fill_grey(start=0.9, end=0.6)+
  ylim(0,100)+
  geom_text(data= p_values_active,  aes(y=100, x=1), hjust=0.9, vjust=0, size=3, label= paste0(p_values_active$stat_test,(paste0("p = ", p_values_active$p_value))),  inherit.aes = FALSE )

#housing
housing <- subset(place_preference_L_R_30days_transformed, day %in% c("V21","V22","V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30"))
p_values_housing <- subset(p_values_all, day %in% c("V21","V22","V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30"))


ggplot(subset(place_preference_L_R_30days_transformed, day %in% c("V21","V22","V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30")), aes( x=day, y=preference, fill=side))+
  geom_boxplot() + 
  facet_wrap(~day, scale="free", dir="h", ncol=5, 
             labeller = as_labeller( c("V21" = "floor house vs. wooden angle",
                                       "V22" = "wooden angle with hole vs. house ball",
                                       "V23" = "wooden angle with hole vs. wooden angle", 
                                       "V24" = "wooden angle vs. paper house", 
                                       "V25" = "paper house vs. floor house", 
                                       "V26" = "floor house vs. wooden angle with hole", 
                                       "V27" = "paper house vs. wooden angle with hole", 
                                       "V28" = "house ball vs. floor house", 
                                       "V29" = "wooden angle vs. house ball", 
                                       "V30" = "house ball vs. paper house")     )) +
  xlab("comparison")+
  ylab("stay duration in %") +
  #ggtitle("MoPSS housing enrichment preferences inactive time")+
  theme(legend.position = "none")+
  scale_fill_grey(start=0.9, end=0.6)+
  ylim(0,100)+
  geom_text(data= p_values_housing,  aes(y=100, x=1), hjust=0.9, vjust=0, size=3, label= paste0(p_values_housing$stat_test,(paste0("p = ", p_values_housing$p_value))),  inherit.aes = FALSE )






