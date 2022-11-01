# Place Preference coincidence Plot
# 28.10.2022

rm(list=ls())

library(ggplot2)
library(gridExtra)


####################################################################################################
##### plot coincidence #############################################################################
####################################################################################################

### Coincidence plots ##############################################################################
#load data
allmousepresence = read.table("./placePrefData_mousepresence.csv", sep=",")

VTageGesamt = nrow(allmousepresence)/9

mousepres1T = allmousepresence[1:60+60*0,] * 100
mousepres2T = allmousepresence[1:60+60*1,] * 100
mousepres3T = allmousepresence[1:60+60*2,] * 100
mousepres1A = allmousepresence[1:60+60*3,] * 100
mousepres2A = allmousepresence[1:60+60*4,] * 100
mousepres3A = allmousepresence[1:60+60*5,] * 100
mousepres1I = allmousepresence[1:60+60*6,] * 100
mousepres2I = allmousepresence[1:60+60*7,] * 100
mousepres3I = allmousepresence[1:60+60*8,] * 100

#plot all three cages day by day
#colors = c("m1-m2"="darkred","m1-m3"="darkgreen","m1-m4" ="darkblue","m2-m3"="magenta","m2-m4"="black","m3-m4"="blue")
#select dataset and conert to percentages
{ #plot at once
  p1 = ggplot(data = as.data.frame(mousepres1A), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres1A[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres1A[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres1A[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres1A[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres1A[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres1A[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    theme(legend.position="none") +
    ggtitle("active time group 1")
  
  p2 = ggplot(data = as.data.frame(mousepres2A), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres2A[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres2A[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres2A[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres2A[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres2A[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres2A[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    theme(legend.position="none") +
    ggtitle("active time group 2")
  
  p3 = ggplot(data = as.data.frame(mousepres3A), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres3A[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres3A[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres3A[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres3A[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres3A[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres3A[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    theme(legend.position="none") +
    ggtitle("active time group 3")
  
  p4 = ggplot(data = as.data.frame(mousepres1I), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres1I[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres1I[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres1I[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres1I[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres1I[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres1I[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    theme(legend.position="none") +
    ggtitle("inactive time group 1")
  
  p5 = ggplot(data = as.data.frame(mousepres2I), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres2I[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres2I[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres2I[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres2I[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres2I[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres2I[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    theme(legend.position="none") +
    ggtitle("inactive time group 2")
  
  p6 = ggplot(data = as.data.frame(mousepres3I), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres3I[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres3I[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres3I[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres3I[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres3I[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres3I[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    theme(legend.position="none") +
    ggtitle("inactive time group 3")
  
  p7 = ggplot(data = as.data.frame(mousepres1T), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres1T[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres1T[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres1T[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres1T[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres1T[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres1T[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    labs(color="interaction") +
    ggtitle("total time group 1")
  
  p8 = ggplot(data = as.data.frame(mousepres2T), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres2T[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres2T[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres2T[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres2T[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres2T[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres2T[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    labs(color="interaction") +
    ggtitle("total time group 2")
  
  p9 = ggplot(data = as.data.frame(mousepres3T), aes(x = c(1:VTageGesamt))) +
    geom_line(size = 1, aes(y=mousepres3T[,1], color="m1-m2")) +
    geom_line(size = 1, aes(y=mousepres3T[,2], color="m1-m3")) +
    geom_line(size = 1, aes(y=mousepres3T[,3], color="m1-m4")) +
    geom_line(size = 1, aes(y=mousepres3T[,4], color="m2-m3")) +
    geom_line(size = 1, aes(y=mousepres3T[,5], color="m2-m4")) +
    geom_line(size = 1, aes(y=mousepres3T[,6], color="m3-m4")) +
    ylim(20,100) +
    scale_x_continuous(breaks = c(1,(1:(VTageGesamt/5))*5)) + 
    xlab("day") +
    ylab("% time spent together") +
    labs(color="interaction") +
    ggtitle("total time group 3")
}

#might take long, might time out if whole script is run at once
grid.arrange(p1, p4, p7, p2, p5, p8, p3, p6, p9, ncol=3)

#save to disc
tiff("coincidence_wide.tiff", units="px", width=4000, height=1700, res = 150, compression = "lzw")
grid.arrange(p1, p4, p7, p2, p5, p8, p3, p6, p9, ncol=3)
dev.off()
