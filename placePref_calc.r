# Place Preference Aufenthaltsbestimmung
# 27.10.2022


rm(list=ls())


###### MoPPs 2-4 Daten #######
setwd("/home/birk/Ute_MoPPSprojekt/")
#Read data from file
data_m2 = read.table("./RFIDLOG_m2b.txt", sep = ",", stringsAsFactors=FALSE, strip.white=TRUE)
data_m3 = read.table("./RFIDLOG_m3.txt", sep = ",", stringsAsFactors=FALSE, strip.white=TRUE)
data_m4 = read.table("./RFIDLOG_m4.txt", sep = ",", stringsAsFactors=FALSE, strip.white=TRUE)

#remove unwanted entries: Exits and Button Presses
data_m2 = data_m2[data_m2[,5] != "X",] #drop exits
data_m3 = data_m3[data_m3[,5] != "X",] #drop exits
data_m4 = data_m4[data_m4[,5] != "X",] #drop exits

data_m2 = data_m2[data_m2[,1] != "B1",] #drop button presses
data_m3 = data_m3[data_m3[,1] != "B1",] #drop button presses
data_m4 = data_m4[data_m4[,1] != "B1",] #drop button presses

data_m2 = data_m2[data_m2[,1] != "B2",] #drop button presses
data_m3 = data_m3[data_m3[,1] != "B2",] #drop button presses
data_m4 = data_m4[data_m4[,1] != "B2",] #drop button presses

#allowed mice
m2_mice = c("972_270000392543","972_270000382754","972_270000392970","972_270000392552")
m3_mice = c("972_270000392701","972_270000382748","972_270000392751","972_270000382965")
m4_mice = c("972_270000391015","972_270000391487","972_270000382179","972_270000383308")

#check number of mice and remove unwanted tag-reads
paste("Reads MoPPs2 dropped:", nrow(data_m2) - nrow(data_m2[data_m2[,3] %in% m2_mice,]))
data_m2[!(data_m2[,3] %in% m2_mice),] #print discarded lines
paste("Reads MoPPs3 dropped:", nrow(data_m3) - nrow(data_m3[data_m3[,3] %in% m3_mice,]))
data_m3[!(data_m3[,3] %in% m3_mice),] #print discarded lines
paste("Reads MoPPs4 dropped:", nrow(data_m4) - nrow(data_m4[data_m4[,3] %in% m4_mice,]))
data_m4[!(data_m4[,3] %in% m4_mice),] #print discarded lines

data_m2 = data_m2[data_m2[,3] %in% m2_mice,]
data_m3 = data_m3[data_m3[,3] %in% m3_mice,]
data_m4 = data_m4[data_m4[,3] %in% m4_mice,]

#Start: 1568970000 -> "2019-09-20 09:00:00 UTC"
#as.POSIXct(1568970000,origin="1970-01-01",tz="UTC")

#Versuchsstart
VStart = 1568970000 #Startzeit offiziell (11)
#VStart = 1568973600 #12
VDauer = 82800 #23h
#VDauer = 21600 #6h

#Versuchstage
#0  1  2  3  4  5  6
#fr sa so mo di mi do
VTage = c(0,3,4,5) #Preftest Tage
#VTage = c(0,1,2,3,4,5,6)
VWochen = 15

#Berechne Gesamtzahl Versuchstage
VTageGesamt = length(VTage) * 15
starttimes = array(dim=c(VTageGesamt)) #enthält die unixtime wann jeder Versuchstag gestartet hat (benötigt für Aufenthaltsdauer)

#3 Listen erstellen die jeweils die einzelnen Tage als separate listen enthalten
m2_slices = list()
m3_slices = list()
m4_slices = list()

#definiere versuchstage (mehrfache des Wochenvektors)
l = 1
for(w in (0:(VWochen - 1))){
  #Zeitfenster je Versuchstag berechnen
  for(i in (VTage + 7 * w)){
    starttimes[l] = VStart + 86400 * i #Liste mit allen Startzeiten für jeden Versuchstag erstellen
    
    m2_slices[[l]] = data_m2[  (data_m2[,2] >= VStart + 86400 * i) & (data_m2[,2] < ((VStart + 86400 * i) + VDauer))  ,]
    m3_slices[[l]] = data_m3[  (data_m3[,2] >= VStart + 86400 * i) & (data_m3[,2] < ((VStart + 86400 * i) + VDauer))  ,]
    m4_slices[[l]] = data_m4[  (data_m4[,2] >= VStart + 86400 * i) & (data_m4[,2] < ((VStart + 86400 * i) + VDauer))  ,]
    l = l + 1
  }
}

#----- sanity checks -----

#test if all days have data
for(i in VTageGesamt:1){
  if(nrow(m2_slices[[i]]) == 0 | nrow(m3_slices[[i]]) == 0 | nrow(m4_slices[[i]]) == 0){
    #remove empty days and adjust VTageGesamt
    print(paste("Day:",i,"Missing Data!"))
    m2_slices[[i]] = NULL
    m3_slices[[i]] = NULL
    m4_slices[[i]] = NULL
    starttimes = starttimes[-i]
    VTageGesamt = VTageGesamt - 1
  }
}

####################################################################################################
####################################################################################################
####################################################################################################

#----- SETUP -----
#seconds a change may take
max_good_transition = 15
max_true_transition = 3

#Master table place preference
placePreference = array(dim=c(VTageGesamt,12))
placePreferenceB = array(dim=c(VTageGesamt,12))

transitions_list_m2 = list()
transitions_list_m3 = list()
transitions_list_m4 = list()

for(e in 1:3){
  errors = array(dim=c(7,6),rep(0,42))
  
  error1 = 0
  error2 = 0
  error3 = 0
  error4 = 0
  errorCD = 0
  
  #iterate through all days of the experiment
  for(f in 1:VTageGesamt){
    
    if(e == 1){observed = m2_slices[[f]]}
    if(e == 2){observed = m3_slices[[f]]}
    if(e == 3){observed = m4_slices[[f]]}
    
    #returns list with all unique mice tags
    mice = unique(observed[,3])
    
    #analyse for all mice
    for(g in 1:length(mice)){
      #print current progress
      cat('\r',paste0("cage: ",e," day: ",f," mouse: ",g,"   "))
      
      #returns list with only the selected mouse and it's crossing times
      mouse = observed[observed[,3] == mice[g],]
      
      ##############################################################################################
      #return directions from point to point
      diffs = array(dim=c(nrow(mouse),8))
      
      #generate a phantom transition that helps determine bad transitions in the start
      phantomTrans = array(dim=c(2,ncol(diffs)))
      phantomTrans[1,1] = 0
      phantomTrans[2,1] = mouse[1,2]
      phantomTrans[,7] = 0
      
      if(mouse[1,1] == "R2"){
        phantomTrans[1,6] = "R1"
        phantomTrans[2,6] = "R2"
      } else{
        phantomTrans[1,6] = "R2"
        phantomTrans[2,6] = "R1"
      }
      diffs = rbind(phantomTrans,diffs)
      
      #populate columns with: human readable time, event-index, reader, event-time
      for(i in 1:(nrow(mouse))){
        diffs[i+2,6] = mouse[i,1]
        diffs[i+2,7] = mouse[i,2]
        diffs[i+2,8] = mouse[i,4]
      }
      for(i in 1:(nrow(diffs))){
        diffs[i,5] = i
        diffs[i,4] = as.character(as.POSIXlt(as.numeric(diffs[i,7]), origin="1970-01-01"))
      }
      
      #add comment on what type of transition/event happened
      for(i in 1:(nrow(mouse)-1)){
        k = i+2
        diffs[k,1] = as.numeric(mouse[i+1,2]) - as.numeric(mouse[i,2])
        
        if((mouse[i,1] == "R1") && (mouse[i+1,1] == "R2")){diffs[k,2] = "transR >>"}
        if((mouse[i,1] == "R2") && (mouse[i+1,1] == "R1")){diffs[k,2] = "transL <<"}
        if((mouse[i,1] == "R1") && (mouse[i+1,1] == "R1")){diffs[k,2] = "pokeL ><"}
        if((mouse[i,1] == "R2") && (mouse[i+1,1] == "R2")){diffs[k,2] = "pokeR <>"}
      }
      
      ##############################################################################################
      #calculate high-certainty transitions based on time between reads from one to the next reader
      for(i in 1:(nrow(diffs)-1)){
        if(as.numeric(diffs[i,1]) <= max_true_transition){ #sekunden die die Maus zum passieren hat siehe reader-abstand
          if( (diffs[i,6] == "R1") && (diffs[i+1,6] == "R2") ){
            diffs[i+1,3] = "switch RL" #the reader where the mouse arrived after the switch
            if(is.na(diffs[i,3])){
              diffs[i,3] = "-" #the reader before the switch, but don't write if already occupied by a swith --> [R1 R2 R1 R2] overwrites...
            }
          } else if( (diffs[i,6] == "R2") && (diffs[i+1,6] == "R1") ){
            diffs[i+1,3] = "switch LR"
            if(is.na(diffs[i,3])){
              diffs[i,3] = "-" #the reader before the switch, but don't write if already occupied by a swith --> [R1 R2 R1 R2] overwrites...
            }
          }
        }
      }
      
      #extract all rows where a switch is happening
      transitions = diffs[substr(diffs[,3],1,6) == "switch",]
      transitions = transitions[rowSums(is.na(transitions)) != ncol(transitions),] #remove rows only containing NA
      
      
      ##############################################################################################
      #find transitions that are "impossible" i.e. went into cage 1 when already in cage 1
      if(NCOL(transitions) > 1){ #if we only have 0-1 transition(+1 for the phantom transition) we can't tell
        for(i in 1:(nrow(transitions)-1)){
          #if a switch happens into the same cage
          if(transitions[i,3] == transitions[i+1,3]){
            #get position of events in diffs
            indexStart = as.numeric(transitions[i,5])
            indexStop = as.numeric(transitions[i+1,5])    
            
            #get number of events between transitions. 2 events are a regular transition: R2 [R1 R2] R1
            events = indexStop-indexStart 
            
            #Find error where mouse was completely undetected during a transition
            if(events == 2){
              print("Error Class C/D!")
              print(diffs[as.numeric(transitions[i,5]),])
              errorCD = errorCD + 1
            }
            
            #Find and eliminate errors where one gate was missed (=3 events) during a transition: R2 [R1 R2 R2] R1
            if(events == 3){
              if(transitions[i,3] == "switch RL"){
                diffs[indexStart+1,3] = "switch LR*"
              } else{
                diffs[indexStart+1,3] = "switch RL*"
              }
              error1 = error1 + 1
            }
            
            #try to resolve errors with many intermediate events: R2 [R1 ... R2] R1
            if(events >= 4){
              #get number of transitions in timeframe
              transitionEvents = 0
              transitionIndex = c()
              k=0
              for(j in (indexStart):(indexStop-2)){
                if(diffs[j,6] != diffs[j+1,6]){
                  transitionEvents = transitionEvents + 1
                  transitionIndex[(k = k+1)] = diffs[j+1,5]
                }
              }
              
              #if it is only one transition we can be quite certain it is true:  R2 [R1 ... R1 R2 ... R2] R1 
              #(future: increasing accuracy by time-analysis)
              if(transitionEvents == 1){
                if(transitions[i,3] == "switch RL"){
                  diffs[as.numeric(transitionIndex[1]),3] = "switch LR**"
                } else{
                  diffs[as.numeric(transitionIndex[1]),3] = "switch RL**"
                }
                error2 = error2 + 1
              }
            }
          }
        }
      }
      ##############################################################################################
      #find transitions that are possible, but may be a result of two missed reads i.e. many reads between transitions or took too long
      if(NCOL(transitions) > 1){ #if we only have 0/1 transition(+1 for the phantom transition) we can't tell
        for(i in 1:(nrow(transitions)-1)){
          #transition into different cage
          if(transitions[i,3] != transitions[i+1,3]){
            #get position of events in diffs
            indexStart = as.numeric(transitions[i,5])
            indexStop = as.numeric(transitions[i+1,5])  
            
            #only consider transitions where more than necessary read-events happened (additional trans/pokes): R2 [R1 ... R1] R2
            if((indexStop - indexStart) > 2){
              #get number of transitions in timeframe
              transitionEvents = 0
              transitionIndex = c()
              k=0
              for(j in (indexStart):(indexStop-2)){
                if(diffs[j,6] != diffs[j+1,6]){
                  transitionEvents = transitionEvents + 1
                  transitionIndex[(k = k+1)] = diffs[j+1,5]
                }
              }
              
              #only consider transitions if the read-events aren't exclusively pokes: R2 [R1 ... R2 ... R1] R2
              if(transitionEvents > 0){
                #if there are only two read-events in between, assume those were the ones where the mouse passed back and forth
                #R2 [R1 R2 R1|2 R1] R2
                if((indexStop - (indexStart+2)) == 2){
                  if(transitions[i,3] == "switch LR"){
                    diffs[indexStart+1,3] = "switch RL***"
                    diffs[indexStart+2,3] = "switch LR***"
                  } else{
                    diffs[indexStart+1,3] = "switch LR***"
                    diffs[indexStart+2,3] = "switch RL***"
                  }
                  error3 = error3 + 1
                }
              }
            }
          }
        }     
      }
      ##############################################################################################
      #recalculate transitions for last step
      transitions = diffs[substr(diffs[,3],1,6) == "switch",]
      transitions = transitions[rowSums(is.na(transitions)) != ncol(transitions),] #remove rows only containing NA
      
      ##############################################################################################
      #find transitions, both possible and impossible with a high amount of intermediary reads that aren't pokes
      if(NCOL(transitions) > 1){ #if we only have 0/1 transition(+1 for the phantom transition) we can't tell
        for(i in 1:(nrow(transitions)-1)){
          #get position of events in diffs[]
          indexStart = as.numeric(transitions[i,5])
          indexStop = as.numeric(transitions[i+1,5])
          
          #only look at events where more than 2 events happend (not just transitions)
          if((indexStop - indexStart) > 2){
            #get number of transitions in timeframe
            transitionEvents = 0
            transitionIndex = c()
            k=0
            for(j in (indexStart):(indexStop-2)){
              if(diffs[j,6] != diffs[j+1,6]){
                transitionEvents = transitionEvents + 1
                transitionIndex[(k = k+1)] = diffs[j+1,5]
              }
            }
            
            #if there is more than 1 additional transition, make an educated guess and assume even slower transitions are correct
            if(transitionEvents > 1){
              #evalute the time it took for the long transitions
              #create labeled matrix containing good and bad transitions
              transArray = array(dim=c(length(transitionIndex),2))
              transArray[,1] = transitionIndex
              
              #evaluate transition times
              for(j in 1:nrow(transArray)){
                if(as.numeric(diffs[as.numeric(transArray[j,1])-1,1]) < max_good_transition){ #<- max transition time
                  transArray[j,2] = "good"
                } else{
                  transArray[j,2] = "bad"
                }
              }
              
              #write switch label
              if(transitions[i,3] == "switch LR"){
                diffs[as.numeric(transArray[c(TRUE,FALSE),1]),3] = "switch RL****"
                diffs[as.numeric(transArray[!c(TRUE,FALSE),1]),3] = "switch LR****"
              } else{
                diffs[as.numeric(transArray[c(TRUE,FALSE),1]),3] = "switch LR****"
                diffs[as.numeric(transArray[!c(TRUE,FALSE),1]),3] = "switch RL****"
              }
              
              #add "g" and "b" labels to switch label indicating if it was a good or bad time
              goodIndex = as.numeric(transArray[(transArray[,2] == "good"),1])
              badIndex = as.numeric(transArray[(transArray[,2] == "bad"),1])
              diffs[goodIndex,3] = paste(diffs[goodIndex,3],"g",sep="")
              
              diffs[badIndex,3] = paste(diffs[badIndex,3],"b",sep="")
              
              error4 = error4 + nrow(transArray)
            }
          }
        }
      }
      
      ##############################################################################################
      #save data into 4 arrays for manual verification
      if(g == 1){mouse1 = diffs}
      if(g == 2){mouse2 = diffs}
      if(g == 3){mouse3 = diffs}
      if(g == 4){mouse4 = diffs}
      
      ##############################################################################################
      #unresolved transitions & error counter
      transitions = diffs[substr(diffs[,3],1,6) == "switch",] #extract all rows where a switch is happening
      transitions = transitions[rowSums(is.na(transitions)) != ncol(transitions),] #remove rows only containing NA
      
      unresolved = 0
      if(NCOL(transitions) > 1){ #if we only have 0/1 transition(+1 for the phantom transition) we can't tell
        for(i in 1:(nrow(transitions)-1)){
          if(substr(transitions[i,3],1,9) == substr(transitions[i+1,3],1,9)){ #if a switch happens into the same cage
            print("unresolved transition:")
            print(transitions[i,c(4,5)])
            unresolved = unresolved + 1
          }
        }
      }
      
      #create table containing all errors for all mice and sum them together for all mice for a single MoPPs
      errors[1,g] = errors[1,g] + error1
      errors[2,g] = errors[2,g] + error2
      errors[3,g] = errors[3,g] + error3
      errors[4,g] = errors[4,g] + error4
      errors[5,g] = errors[5,g] + unresolved
      errors[6,g] = errors[6,g] + nrow(diffs)-2
      errors[7,g] = errors[7,g] + sum((substr(diffs[3:nrow(diffs),3],1,6) == "switch"), na.rm = TRUE) #returns # of transitions
      
      #reset error counters
      error1 = 0
      error2 = 0
      error3 = 0
      error4 = 0
      unresolved = 0
      
      #create list with transitions
      if( e == 1){transitions_list_m2[[g+f*4-4]] = diffs}
      if( e == 2){transitions_list_m3[[g+f*4-4]] = diffs}
      if( e == 3){transitions_list_m4[[g+f*4-4]] = diffs}
    } #end of mice loop g
  } #end of loop going over all experiment days f
  
  #sum all errors, events and transitions for the whole dataset
  for(i in 1:7){errors[i,5] = sum(errors[i,1:4])}
  for(i in 1:7){errors[i,6] = round((errors[i,5]/errors[6,5]*100),3)}
  
  rownames(errors) = c("Error*","Error**","Error***","Error****","Error C/D","Events","Transitions")
  colnames(errors) = c("mouse1","mouse2","mouse3","mouse4","TOTAL","PERC.")
  
  if(e == 1){m2_errors = errors}
  if(e == 2){m3_errors = errors}
  if(e == 3){m4_errors = errors}
  
}# end of e loop, (different MoPPs')

####################################################################################################
#print errors
m2_errors
m3_errors
m4_errors

####################################################################################################
##### AUSWERTUNG ###################################################################################
####################################################################################################

### Calculate the times each mouse stayed in each cage ###

presMainList = array(dim=c(VTageGesamt,8,3)) #day, mouseside, cage

#for manual evaluation
timeframe = 1
cage = 2
d = 45
#iterate through all timeframes
for(timeframe in 1:2){
  #vom histogram
  #inactive       12:30h -    20h     , 3:30h -      8h
  #abschnitte = c(3600 * 1.5, 3600 * 9, 3600 * 16.5, 3600 * 21)
  #active         11h -     12:30h    , 20h -     3:30h      , 8h -       10h
  #abschnitte = c(3600 * 0, 3600 * 1.5, 3600 * 9, 3600 * 16.5, 3600 * 21, 3600 * 23)
  ###### define NEW/DIFFERENT time-frame for mice staying in same cage #####
  if(timeframe == 1){
    #all day 11:00 - 10:00 (23h)
    VDauer2 = 3600 * 23    #max 23h
    VDauertotal = VDauer2
    Startoffset = 3600 * 0 #startzeit 11Uhr
  }
  if(timeframe == 2){
    #active time 20:00 - 8:00 (12h)
    VDauer2 = 3600 * 12    #max 23h
    VDaueractive = VDauer2
    Startoffset = 3600 * 9 #startzeit
  }
  
  presmatrixlistC1S0 = list() #presence matrix, cage 1 side 0
  presmatrixlistC1S1 = list()
  presmatrixlistC2S0 = list()
  presmatrixlistC2S1 = list()
  presmatrixlistC3S0 = list()
  presmatrixlistC3S1 = list()
  
  #iterate through all cages
  for(cage in 1:3){

    if(cage == 1) temp_transitions_list = transitions_list_m2
    if(cage == 2) temp_transitions_list = transitions_list_m3
    if(cage == 3) temp_transitions_list = transitions_list_m4
    
    #iterate through all experiment days
    for(d in 1:VTageGesamt){
      cat('\r',paste0("presence time calc timeframe: ",timeframe," cage: ",cage," day: ",d,"   "))
      
      #create list with transitions for each mouse
      o1 = temp_transitions_list[[1+d*4-4]][substring(temp_transitions_list[[1+d*4-4]][,3],1,6) == "switch",]
      o2 = temp_transitions_list[[2+d*4-4]][substring(temp_transitions_list[[2+d*4-4]][,3],1,6) == "switch",]
      o3 = temp_transitions_list[[3+d*4-4]][substring(temp_transitions_list[[3+d*4-4]][,3],1,6) == "switch",]
      o4 = temp_transitions_list[[4+d*4-4]][substring(temp_transitions_list[[4+d*4-4]][,3],1,6) == "switch",]
      
      #temp = temp_transitions_list[[1+d*4-4]]
      
      #remove all NAs
      o1 = o1[!is.na(o1[,4]),]
      o2 = o2[!is.na(o2[,4]),]
      o3 = o3[!is.na(o3[,4]),]
      o4 = o4[!is.na(o4[,4]),]
      
      #remove all phantom transitions
      o1 = o1[!(o1[,7] == 0),]
      o2 = o2[!(o2[,7] == 0),]
      o3 = o3[!(o3[,7] == 0),]
      o4 = o4[!(o4[,7] == 0),]
      
      #use column 2 to to add in which cage mouse resides when the switch is done. RIGHT=0 LEFT=1
      o1[substring(o1[,3],1,9) == "switch RL",2] = 1
      o1[substring(o1[,3],1,9) == "switch LR",2] = 0
      o2[substring(o2[,3],1,9) == "switch RL",2] = 1
      o2[substring(o2[,3],1,9) == "switch LR",2] = 0
      o3[substring(o3[,3],1,9) == "switch RL",2] = 1
      o3[substring(o3[,3],1,9) == "switch LR",2] = 0
      o4[substring(o4[,3],1,9) == "switch RL",2] = 1
      o4[substring(o4[,3],1,9) == "switch LR",2] = 0
      
      #use column 1 for mouse names
      o1[,1] = 1
      o2[,1] = 2
      o3[,1] = 3
      o4[,1] = 4
      
      # #remove non-perfect transitions
      # o1 = o1[substring(o1[,3],10) == "",]
      # o2 = o2[substring(o2[,3],10) == "",]
      # o3 = o3[substring(o3[,3],10) == "",]
      # o4 = o4[substring(o4[,3],10) == "",]

      #timeframe adjustments
      o1 = o1[ (o1[,7] >= (starttimes[d] + Startoffset)) & (o1[,7] < (starttimes[d] + Startoffset + VDauer2)) ,]
      o2 = o2[ (o2[,7] >= (starttimes[d] + Startoffset)) & (o2[,7] < (starttimes[d] + Startoffset + VDauer2)) ,]
      o3 = o3[ (o3[,7] >= (starttimes[d] + Startoffset)) & (o3[,7] < (starttimes[d] + Startoffset + VDauer2)) ,]
      o4 = o4[ (o4[,7] >= (starttimes[d] + Startoffset)) & (o4[,7] < (starttimes[d] + Startoffset + VDauer2)) ,]
      
      #remove changes into the same cage, this happens when we missed a transition with no indication when it might have happened (error C/D)
      #however since the mouse must have switched, removing these transitions creates a tradeoff between missed vs educated guess of presence
      # droptrans = c()
      # for(i in 1:(nrow(o1)-1)){ if(o1[i,2] == o1[i+1,2]){ droptrans = c(droptrans,i+1) }}
      # if(length(droptrans) != 0) o1 = o1[-droptrans,]
      # droptrans = c()
      # for(i in 1:(nrow(o2)-1)){ if(o2[i,2] == o2[i+1,2]){ droptrans = c(droptrans,i+1) }}
      # if(length(droptrans) != 0) o2 = o2[-droptrans,]
      # droptrans = c()
      # for(i in 1:(nrow(o3)-1)){ if(o3[i,2] == o3[i+1,2]){ droptrans = c(droptrans,i+1) }}
      # if(length(droptrans) != 0) o3 = o3[-droptrans,]
      # droptrans = c()
      # for(i in 1:(nrow(o4)-1)){ if(o4[i,2] == o4[i+1,2]){ droptrans = c(droptrans,i+1) }}
      # if(length(droptrans) != 0) o4 = o4[-droptrans,]
      # 
      # #check if changes into same cage still occur
      # for(i in 1:(nrow(o1)-1)){ if(o1[i,2] == o1[i+1,2]) print(paste("mouse1",i,timeframe,cage,d))}
      # for(i in 1:(nrow(o2)-1)){ if(o2[i,2] == o2[i+1,2]) print(paste("mouse2",i,timeframe,cage,d))}
      # for(i in 1:(nrow(o3)-1)){ if(o3[i,2] == o3[i+1,2]) print(paste("mouse3",i,timeframe,cage,d))}
      # for(i in 1:(nrow(o4)-1)){ if(o4[i,2] == o4[i+1,2]) print(paste("mouse4",i,timeframe,cage,d))}

      #mouse by mouse presence
      #mouse1
      prestimeo1s0 = array(dim=c(1,2)) #presence times, mouse 1 side 0
      prestimeo1s1 = array(dim=c(1,2))
      for(i in 1:(nrow(o1)-1)) { #copy start and end times of mouse being in side 0/1 to new arrays
        if((o1[i,2] == 0) & (o1[i+1,2] == 1)) {prestimeo1s0 = rbind(prestimeo1s0, c(as.numeric(o1[i,7]),as.numeric(o1[i+1,7])))}
        if((o1[i,2] == 1) & (o1[i+1,2] == 0)) {prestimeo1s1 = rbind(prestimeo1s1, c(as.numeric(o1[i,7]),as.numeric(o1[i+1,7])))}}
      prestimeo1s0 = prestimeo1s0[2:nrow(prestimeo1s0),] #remove initialisation row
      prestimeo1s1 = prestimeo1s1[2:nrow(prestimeo1s1),] #remove initialisation row
      if(as.numeric(o1[1,2]) == 1){prestimeo1s0 = rbind(c(starttimes[d]+Startoffset,as.numeric(o1[1,7])),prestimeo1s0)} #if mouse starts in cage 1, add time from beginning of recording to first change to array
      if(as.numeric(o1[nrow(o1),2]) == 0){prestimeo1s0 = rbind(prestimeo1s0,c(as.numeric(o1[nrow(o1),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 1, add time from last transition to end to array
      if(as.numeric(o1[1,2]) == 0){prestimeo1s1 = rbind(c(starttimes[d]+Startoffset,as.numeric(o1[1,7])),prestimeo1s1)} #if mouse starts in cage 0, add time from beginning of recording to first change to array
      if(as.numeric(o1[nrow(o1),2]) == 1){prestimeo1s1 = rbind(prestimeo1s1,c(as.numeric(o1[nrow(o1),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 0, add time from last transition to end to array
     
      #mouse2
      prestimeo2s0 = array(dim=c(1,2))
      prestimeo2s1 = array(dim=c(1,2))
      for(i in 1:(nrow(o2)-1)) { #copy start and end times of mouse being in side 0/1
        if((o2[i,2] == 0) & (o2[i+1,2] == 1)) {prestimeo2s0 = rbind(prestimeo2s0, c(as.numeric(o2[i,7]),as.numeric(o2[i+1,7])))}
        if((o2[i,2] == 1) & (o2[i+1,2] == 0)) {prestimeo2s1 = rbind(prestimeo2s1, c(as.numeric(o2[i,7]),as.numeric(o2[i+1,7])))}}
      prestimeo2s0 = prestimeo2s0[2:nrow(prestimeo2s0),] #remove initialisation row
      prestimeo2s1 = prestimeo2s1[2:nrow(prestimeo2s1),] #remove initialisation row
      if(as.numeric(o2[1,2]) == 1){prestimeo2s0 = rbind(c(starttimes[d]+Startoffset,as.numeric(o2[1,7])),prestimeo2s0)} #if mouse starts in cage 1, add time from beginning of recording to first change to array
      if(as.numeric(o2[nrow(o2),2]) == 0){prestimeo2s0 = rbind(prestimeo2s0,c(as.numeric(o2[nrow(o2),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 1, add time from last transition to end to array
      if(as.numeric(o2[1,2]) == 0){prestimeo2s1 = rbind(c(starttimes[d]+Startoffset,as.numeric(o2[1,7])),prestimeo2s1)} #if mouse starts in cage 0, add time from beginning of recording to first change to array
      if(as.numeric(o2[nrow(o2),2]) == 1){prestimeo2s1 = rbind(prestimeo2s1,c(as.numeric(o2[nrow(o2),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 0, add time from last transition to end to array
      
      #mouse3
      prestimeo3s0 = array(dim=c(1,2))
      prestimeo3s1 = array(dim=c(1,2))
      for(i in 1:(nrow(o3)-1)) { #copy start and end times of mouse being in side 0/1
        if((o3[i,2] == 0) & (o3[i+1,2] == 1)) {prestimeo3s0 = rbind(prestimeo3s0, c(as.numeric(o3[i,7]),as.numeric(o3[i+1,7])))}
        if((o3[i,2] == 1) & (o3[i+1,2] == 0)) {prestimeo3s1 = rbind(prestimeo3s1, c(as.numeric(o3[i,7]),as.numeric(o3[i+1,7])))}}
      prestimeo3s0 = prestimeo3s0[2:nrow(prestimeo3s0),] #remove initialisation row
      prestimeo3s1 = prestimeo3s1[2:nrow(prestimeo3s1),] #remove initialisation row
      if(as.numeric(o3[1,2]) == 1){prestimeo3s0 = rbind(c(starttimes[d]+Startoffset,as.numeric(o3[1,7])),prestimeo3s0)} #if mouse starts in cage 1, add time from beginning of recording to first change to array
      if(as.numeric(o3[nrow(o3),2]) == 0){prestimeo3s0 = rbind(prestimeo3s0,c(as.numeric(o3[nrow(o3),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 1, add time from last transition to end to array
      if(as.numeric(o3[1,2]) == 0){prestimeo3s1 = rbind(c(starttimes[d]+Startoffset,as.numeric(o3[1,7])),prestimeo3s1)} #if mouse starts in cage 0, add time from beginning of recording to first change to array
      if(as.numeric(o3[nrow(o3),2]) == 1){prestimeo3s1 = rbind(prestimeo3s1,c(as.numeric(o3[nrow(o3),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 0, add time from last transition to end to array
      
      #mouse4
      prestimeo4s0 = array(dim=c(1,2))
      prestimeo4s1 = array(dim=c(1,2))
      for(i in 1:(nrow(o4)-1)) { #copy start and end times of mouse being in side 0/1
        if((o4[i,2] == 0) & (o4[i+1,2] == 1)) {prestimeo4s0 = rbind(prestimeo4s0, c(as.numeric(o4[i,7]),as.numeric(o4[i+1,7])))}
        if((o4[i,2] == 1) & (o4[i+1,2] == 0)) {prestimeo4s1 = rbind(prestimeo4s1, c(as.numeric(o4[i,7]),as.numeric(o4[i+1,7])))}}
      prestimeo4s0 = prestimeo4s0[2:nrow(prestimeo4s0),] #remove initialisation row
      prestimeo4s1 = prestimeo4s1[2:nrow(prestimeo4s1),] #remove initialisation row
      if(as.numeric(o4[1,2]) == 1){prestimeo4s0 = rbind(c(starttimes[d]+Startoffset,as.numeric(o4[1,7])),prestimeo4s0)} #if mouse starts in cage 1, add time from beginning of recording to first change to array
      if(as.numeric(o4[nrow(o4),2]) == 0){prestimeo4s0 = rbind(prestimeo4s0,c(as.numeric(o4[nrow(o4),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 1, add time from last transition to end to array
      if(as.numeric(o4[1,2]) == 0){prestimeo4s1 = rbind(c(starttimes[d]+Startoffset,as.numeric(o4[1,7])),prestimeo4s1)} #if mouse starts in cage 0, add time from beginning of recording to first change to array
      if(as.numeric(o4[nrow(o4),2]) == 1){prestimeo4s1 = rbind(prestimeo4s1,c(as.numeric(o4[nrow(o4),7]),starttimes[d]+Startoffset+VDauer2))}#if mouse finishes in cage 0, add time from last transition to end to array
      
      #Calculate overlap in presence times
      prestimes0 = list()
      prestimes0[[1]] = prestimeo1s0
      prestimes0[[2]] = prestimeo2s0
      prestimes0[[3]] = prestimeo3s0
      prestimes0[[4]] = prestimeo4s0
      prestimes1 = list()
      prestimes1[[1]] = prestimeo1s1
      prestimes1[[2]] = prestimeo2s1
      prestimes1[[3]] = prestimeo3s1
      prestimes1[[4]] = prestimeo4s1
      
      #get dwelling times #mouse 1 side 0, mouse 1 side 1 etc.
      #if(timeframe == 1){ #only total time for now
      for(s in 1:4){
        presMainList[d,(s*2)-1,cage] = sum(prestimes0[[s]][,2] - prestimes0[[s]][,1])
        presMainList[d,s*2,cage] = sum(prestimes1[[s]][,2] - prestimes1[[s]][,1])
      }
    
      #sanity check presence times for negative Values
      for(s in 1:4){
        if(min(prestimes0[[s]][,2] - prestimes0[[s]][,1]) < 0){
          print(paste("Presence time error! Tf:",timeframe,"cage0:",cage,"day:",d,"mouse:",s))
        }
        if(min(prestimes1[[s]][,2] - prestimes1[[s]][,1]) < 0){
          print(paste("Presence time error! Tf:",timeframe,"cage1:",cage,"day:",d,"mouse:",s))
        }
      }
      
      #sanity check presence time of total time
      for(s in 1:4){
        a = sum(prestimes0[[s]][,2] - prestimes0[[s]][,1]) +  sum(prestimes1[[s]][,2] - prestimes1[[s]][,1])
        if(a > VDauer2){print(paste(timeframe,cage,d,s,a)) }
      }
      #}
      
      #plot presence in two cages visually
      #plot(1569315673,xlim=c(starttimes[d] + Startoffset,starttimes[d] + Startoffset + VDauer2),ylim=c(0,11))
      #rect(prestimeo1s0[,1], 2, prestimeo1s0[,2], 10,border = NA,col = "red")
      #rect(prestimeo1s1[,1], 0, prestimeo1s1[,2], 8,border = NA,col = "black")
      
      #create matrix with presence times (in seconds of each mouse with each mouse)
      presmatrixs0 = array(dim=c(4,4))
      presmatrixs1 = array(dim=c(4,4))
      for(a in 1:3){
        for(b in (a+1):4){
          #side 0
          overlapsums0 = 0
          for(i in 1:nrow(prestimes0[[a]])){
            overlaps = prestimes0[[b]][,1] <= prestimes0[[a]][i,2] & prestimes0[[b]][,2] >= prestimes0[[a]][i,1]
            overlaps = which(overlaps, arr.ind = TRUE)
            
            #sum overlap for all overlapping segments
            for(j in overlaps){
              overlapsums0 = overlapsums0 + min(prestimes0[[b]][j,2],prestimes0[[a]][i,2]) - max(prestimes0[[b]][j,1],prestimes0[[a]][i,1])
            }
          }
          #side 1
          overlapsums1 = 0
          for(i in 1:nrow(prestimes1[[a]])){
            overlaps = prestimes1[[b]][,1] <= prestimes1[[a]][i,2] & prestimes1[[b]][,2] >= prestimes1[[a]][i,1]
            overlaps = which(overlaps, arr.ind = TRUE)
            
            #sum overlap for all overlapping segments
            for(j in overlaps){
              overlapsums1 = overlapsums1 + min(prestimes1[[b]][j,2],prestimes1[[a]][i,2]) - max(prestimes1[[b]][j,1],prestimes1[[a]][i,1])
            }
          }
          
          presmatrixs0[a,b] = overlapsums0
          presmatrixs0[b,a] = overlapsums0
          presmatrixs1[a,b] = overlapsums1
          presmatrixs1[b,a] = overlapsums1
        }
      }
      
      #create 3 pairs of lists for each cage separate
      if(cage == 1){
        presmatrixlistC1S0[[d]] = presmatrixs0 #matrixlist for side 0
        presmatrixlistC1S1[[d]] = presmatrixs1 #matrixlist for side 1
      }
      if(cage == 2){
        presmatrixlistC2S0[[d]] = presmatrixs0 #matrixlist for side 0
        presmatrixlistC2S1[[d]] = presmatrixs1 #matrixlist for side 1
      }
      if(cage == 3){
        presmatrixlistC3S0[[d]] = presmatrixs0 #matrixlist for side 0
        presmatrixlistC3S1[[d]] = presmatrixs1 #matrixlist for side 1
      }
    }
  }
  
  ##day by day time of each mouse spent with each other mouse
  #create new array for easier plotting
  mousepres1 = array(dim=c(VTageGesamt,6))
  colnames(mousepres1) = c("m1-m2","m1-m3","m1-m4","m2-m3","m2-m4","m3-m4")
  k = 1
  for(a in 1:3){
    for(b in (a+1):4){
      for(i in 1:VTageGesamt){mousepres1[i,k] = presmatrixlistC1S0[[i]][a,b] + presmatrixlistC1S1[[i]][a,b]}
      k = k+1
    }}
  mousepres2 = array(dim=c(VTageGesamt,6))
  colnames(mousepres2) = c("m1-m2","m1-m3","m1-m4","m2-m3","m2-m4","m3-m4")
  k = 1
  for(a in 1:3){
    for(b in (a+1):4){
      for(i in 1:VTageGesamt){mousepres2[i,k] = presmatrixlistC2S0[[i]][a,b] + presmatrixlistC2S1[[i]][a,b]}
      k = k+1
    }}
  mousepres3 = array(dim=c(VTageGesamt,6))
  colnames(mousepres3) = c("m1-m2","m1-m3","m1-m4","m2-m3","m2-m4","m3-m4")
  k = 1
  for(a in 1:3){
    for(b in (a+1):4){
      for(i in 1:VTageGesamt){mousepres3[i,k] = presmatrixlistC3S0[[i]][a,b] + presmatrixlistC3S1[[i]][a,b]}
      k = k+1
    }}
  
  if(timeframe == 1){
    #total time
    mousepres1T = mousepres1
    mousepres2T = mousepres2
    mousepres3T = mousepres3
    presMainListT = presMainList #place preference total time
  }
  if(timeframe == 2){
    #active time
    mousepres1A = mousepres1
    mousepres2A = mousepres2
    mousepres3A = mousepres3
    presMainListA = presMainList #place preference active time
  }
} #end of timeframe loop

#compute inactive time
mousepres1I = mousepres1T - mousepres1A
mousepres2I = mousepres2T - mousepres2A
mousepres3I = mousepres3T - mousepres3A
presMainListI = presMainListT - presMainListA

#convert stay durations to percentages
mousepres1A = mousepres1A/VDaueractive
mousepres2A = mousepres2A/VDaueractive
mousepres3A = mousepres3A/VDaueractive
mousepres1I = mousepres1I/(VDauertotal - VDaueractive)
mousepres2I = mousepres2I/(VDauertotal - VDaueractive)
mousepres3I = mousepres3I/(VDauertotal - VDaueractive)
mousepres1T = mousepres1T/VDauertotal
mousepres2T = mousepres2T/VDauertotal
mousepres3T = mousepres3T/VDauertotal

####################################################################################################
##### AUSWERTUNG ###################################################################################
####################################################################################################

#sanity check dwelling times, there will be dys with shorter than allowed times due to some missed reads
#presMainList - day, mouseside, cage
#total time
for(cage in 1:3){
  for(mouse in 1:4){
    for(day in 1:VTageGesamt){
      if(presMainListT[day,mouse*2-1,cage] + presMainListT[day,mouse*2,cage] != VDauertotal){
        print(paste("missing secs:",VDauertotal-(presMainListT[day,mouse*2-1,cage] + presMainListT[day,mouse*2,cage]),"cage",cage,"day",day,"mouse",mouse))
      }}}}

#active time
for(cage in 1:3){
  for(mouse in 1:4){
    for(day in 1:VTageGesamt){
      if(presMainListA[day,mouse*2-1,cage] + presMainListA[day,mouse*2,cage] != VDaueractive){
        print(paste("missing secs:",VDaueractive-(presMainListA[day,mouse*2-1,cage] + presMainListA[day,mouse*2,cage]),"cage",cage,"day",day,"mouse",mouse))
      }}}}

#inactive time
for(cage in 1:3){
  for(mouse in 1:4){
    for(day in 1:VTageGesamt){
      if(presMainListI[day,mouse*2-1,cage] + presMainListI[day,mouse*2,cage] != (VDauertotal - VDaueractive)){
        print(paste("missing secs:",(VDauertotal - VDaueractive)-(presMainListI[day,mouse*2-1,cage] + presMainListI[day,mouse*2,cage]),"cage",cage,"day",day,"mouse",mouse))
      }}}}

####################################################################################################
#create percentages from stay durations, measured from the observed time of the mouse (excludes missing time) for side 0/right
presMainListpercT = array(dim=c(VTageGesamt,8,3))
presMainListpercA = array(dim=c(VTageGesamt,8,3))
presMainListpercI = array(dim=c(VTageGesamt,8,3))

#get percentages of stay duration adjusted to available data
for(cage in 1:3){
  for(day in 1:VTageGesamt){
    for(mouse in 1:4){
      t = presMainListT[day,mouse*2-1,cage] + presMainListT[day,mouse*2,cage]
      presMainListpercT[day,mouse*2-1,cage] = presMainListT[day,mouse*2-1,cage]/t
      presMainListpercT[day,mouse*2,cage]   = presMainListT[day,mouse*2,cage]/t
      
      t = presMainListA[day,mouse*2-1,cage] + presMainListA[day,mouse*2,cage]
      presMainListpercA[day,mouse*2-1,cage] = presMainListA[day,mouse*2-1,cage]/t
      presMainListpercA[day,mouse*2,cage]   = presMainListA[day,mouse*2,cage]/t
      
      t = presMainListI[day,mouse*2-1,cage] + presMainListI[day,mouse*2,cage]
      presMainListpercI[day,mouse*2-1,cage] = presMainListI[day,mouse*2-1,cage]/t
      presMainListpercI[day,mouse*2,cage]   = presMainListI[day,mouse*2,cage]/t
    }}}
#check if we reach 100% for all mice, no ouptut if ok
if(sum(presMainListpercT[,c((1:4)*2-1),1:3] + presMainListpercT[,c((1:4)*2),1:3] != 1) > 0) print("Presence time error!")
if(sum(presMainListpercA[,c((1:4)*2-1),1:3] + presMainListpercA[,c((1:4)*2),1:3] != 1) > 0) print("Presence time error!")
if(sum(presMainListpercI[,c((1:4)*2-1),1:3] + presMainListpercI[,c((1:4)*2),1:3] != 1) > 0) print("Presence time error!")

#create table with the stay durations for side 0/right
newplacePreferenceT = array(dim=c(VTageGesamt,12))
newplacePreferenceA = array(dim=c(VTageGesamt,12))
newplacePreferenceI = array(dim=c(VTageGesamt,12))
for(day in 1:VTageGesamt){
  for(cage in 0:2){
    for(mouse in 1:4){
      newplacePreferenceT[day,mouse+4*cage] = presMainListpercT[day,mouse*2-1,cage+1]
      newplacePreferenceA[day,mouse+4*cage] = presMainListpercA[day,mouse*2-1,cage+1]
      newplacePreferenceI[day,mouse+4*cage] = presMainListpercI[day,mouse*2-1,cage+1]
    }}}

####################################################################################################
#calucalte side preferences
#create a simpler table, averaging all mice for each day
shortlistT = array(dim=(c(VTageGesamt,3)))
shortlistA = array(dim=(c(VTageGesamt,3)))
shortlistI = array(dim=(c(VTageGesamt,3)))
colnames(shortlistT) = c("MoPSS 2","MoPSS 3","MoPSS 4")
colnames(shortlistA) = c("MoPSS 2","MoPSS 3","MoPSS 4")
colnames(shortlistI) = c("MoPSS 2","MoPSS 3","MoPSS 4")
#create rownames
rnam = c()
for(i in 1:VTageGesamt){rnam[i] = paste("day-",i,sep="")}
rownames(shortlistT) = rnam
rownames(shortlistA) = rnam
rownames(shortlistI) = rnam

#calculate average stay duration for all mice per MoPPs per day
for(i in 1:VTageGesamt){
  shortlistT[i,1] = sum(newplacePreferenceT[i,1:4])/4
  shortlistT[i,2] = sum(newplacePreferenceT[i,5:8])/4
  shortlistT[i,3] = sum(newplacePreferenceT[i,9:12])/4
  shortlistA[i,1] = sum(newplacePreferenceA[i,1:4])/4
  shortlistA[i,2] = sum(newplacePreferenceA[i,5:8])/4
  shortlistA[i,3] = sum(newplacePreferenceA[i,9:12])/4
  shortlistI[i,1] = sum(newplacePreferenceI[i,1:4])/4
  shortlistI[i,2] = sum(newplacePreferenceI[i,5:8])/4
  shortlistI[i,3] = sum(newplacePreferenceI[i,9:12])/4
}

#Seitenpräferenz average pro käfig
a = sum(shortlistT[,1])/VTageGesamt #Seitenpräferenz für MoPPs 2
b = sum(shortlistT[,2])/VTageGesamt
c = sum(shortlistT[,3])/VTageGesamt
(a+b+c)/3 #seitenpräferenz über alle daten total time
d = sum(shortlistA[,1])/VTageGesamt #Seitenpräferenz für MoPPs 2
e = sum(shortlistA[,2])/VTageGesamt
f = sum(shortlistA[,3])/VTageGesamt
(d+e+f)/3 #seitenpräferenz über alle daten active time
g = sum(shortlistI[,1])/VTageGesamt #Seitenpräferenz für MoPPs 2
h = sum(shortlistI[,2])/VTageGesamt
i = sum(shortlistI[,3])/VTageGesamt
(g+h+i)/3 #seitenpräferenz über alle daten active time

#Seitenpräferenz pro Maus
for(i in 1:12) print(sum(newplacePreferenceT[,i])/VTageGesamt)
for(i in 1:12) print(sum(newplacePreferenceA[,i])/VTageGesamt)
for(i in 1:12) print(sum(newplacePreferenceI[,i])/VTageGesamt)

#print shortlist containing average stay duration of mice for each MoPSS per day in the right/0 side
shortlistT
shortlistA
shortlistI

####################################################################################################
#create table containing enrichments, first enrichment=1, on the right side
#an enrichment marked with a 1 was first presented on the right side, and on the next day on the left side
{
  enrichments = array(dim=c(30,3))
  
  enrichments[1,] = c(1,1,0) #1 Mausschaukel 0 Seil #In this example mausschaukel was presented right for cage 1+2, and left for cage 3
  enrichments[2,] = c(0,1,1) #Seil Pappröhre
  enrichments[3,] = c(1,0,1) #Pappröhre Mausschaukel 
  enrichments[4,] = c(1,1,1) #Zweite Ebene Pappröhre
  enrichments[5,] = c(1,0,0) #Seil Zweite ebene
  enrichments[6,] = c(0,0,0) #Seil Plastikröhre
  enrichments[7,] = c(1,0,1) #Zweite Ebene Plastikröhre
  enrichments[8,] = c(0,0,1) #Pappröhre Plastikröhre
  enrichments[9,] = c(1,0,1) #Mausschaukel Plastikröhre
  enrichments[10,] = c(0,1,0)#Mausschaukel Zweite ebene
  
  enrichments[11,] = c(0,1,1) #Gitterball Futterball
  enrichments[12,] = c(1,1,1) #Klappenrätsel Röhre
  enrichments[13,] = c(1,1,1) #Futterball Klappenrätsel
  enrichments[14,] = c(1,1,1) #Gitterball Röhre
  enrichments[15,] = c(1,0,0) #Schieberätsel Röhre
  enrichments[16,] = c(1,1,1) #Futterball Röhre
  enrichments[17,] = c(1,0,1) #Schieberätsel Gitterball
  enrichments[18,] = c(1,0,0) #Schieberätsel Futterball
  enrichments[19,] = c(1,0,0) #Schieberätsel Klappenrätsel
  enrichments[20,] = c(1,1,1) #Klappenrätsel Gitterball
  
  enrichments[21,] = c(1,0,0) #Holzwinkel Etagenhaus
  enrichments[22,] = c(1,1,1) #Hausball Holzwinkel-Loch
  enrichments[23,] = c(1,1,0) #Holzwinkel Holzwinkel-Loch
  enrichments[24,] = c(1,1,0) #Papphäuschen Holzwinkel
  enrichments[25,] = c(1,1,1) #Etagenhaus Papphäuschen
  enrichments[26,] = c(1,1,1) #Holzwinkel-Loch Etagenhaus
  enrichments[27,] = c(1,1,0) #Holzwinkel-Loch Papphäuschen
  enrichments[28,] = c(1,0,0) #Etagenhaus Hausball
  enrichments[29,] = c(1,1,1) #Hausball Holzwinkel
  enrichments[30,] = c(1,0,0) #Papphäuschen Hausball
  
  #neues array das alle enrichmentpositionen enthält, formatiert passend zu placePreference
  #every second day the enrichments change sides
  enrichments_raw = array(dim=c(60,12))
  for(i in 1:30){
    enrichments_raw[i*2-1,] = c(rep(enrichments[i,1],4),rep(enrichments[i,2],4),rep(enrichments[i,3],4))
    enrichments_raw[i*2,] = 1 - enrichments_raw[i*2-1,] 
  }
}
####################################################################################################
#create table containing only one enrichment preference for all values by inverting all entires where the enrichment was on the opposite side

#invert enrichments so both days contain only presence times for first/right/1 enrichment (see table above)
#presence times where the enrichment was on the left side get inverted,
#now all presence times are the one where the mouse stayed with the first listed enrichment
newplacePreference_invT = abs(newplacePreferenceT - !enrichments_raw) * 100 #for percentages
newplacePreference_invA = abs(newplacePreferenceA - !enrichments_raw) * 100 #for percentages
newplacePreference_invI = abs(newplacePreferenceI - !enrichments_raw) * 100 #for percentages

### data export ####################################################################################

#save place preferences as csv
write.table(newplacePreference_invT,"./placePrefData_total_time.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(newplacePreference_invA,"./placePrefData_active_time.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(newplacePreference_invI,"./placePrefData_inactive_time.csv",sep=",",row.names=FALSE,col.names=FALSE)

newplacePreference_invT #<-- place preference array total time
newplacePreference_invA #<-- place preference array active time
newplacePreference_invI #<-- place preference array inactive time

#save concidences as one table, total/active/inactive
allmousepresence = rbind(mousepres1T,mousepres2T,mousepres3T,mousepres1A,mousepres2A,mousepres3A,mousepres1I,mousepres2I,mousepres3I)
write.table(allmousepresence,"./placePrefData_mousepresence.csv",sep=",",row.names=FALSE,col.names=FALSE)










####################################################################################################
### follow and influence calcualtion  ##############################################################
####################################################################################################
#iterate through the three cages
for(cage in 1:3){
  if(cage == 1) temp_transitions_list = transitions_list_m2
  if(cage == 2) temp_transitions_list = transitions_list_m3
  if(cage == 3) temp_transitions_list = transitions_list_m4  
  
  #initialize stuff
  o1_trans = 0
  o2_trans = 0
  o3_trans = 0
  o4_trans = 0
  
  m12_bp = c()
  m13_bp = c()
  m14_bp = c()
  m21_bp = c()
  m23_bp = c()
  m24_bp = c()
  m31_bp = c()
  m32_bp = c()
  m34_bp = c()
  m41_bp = c()
  m42_bp = c()
  m43_bp = c()
  
  mousefollowmatrix = array(0,dim=c(4,5))
  
  all_mousepresencec2 = list()
  all_mousepresencec3 = list()
  all_mousepresencec4 = list()
  
  for(day in 1:VTageGesamt){ #iterate through 60 experiment days
    #some feedback as it can take quite long
    cat('\r',paste0("cage: ",cage," day: ",day,"   "))

    #create list with transitions for each mouse
    o1 = temp_transitions_list[[1+day*4-4]][substring(temp_transitions_list[[1+day*4-4]][,3],1,6) == "switch",]
    o2 = temp_transitions_list[[2+day*4-4]][substring(temp_transitions_list[[2+day*4-4]][,3],1,6) == "switch",]
    o3 = temp_transitions_list[[3+day*4-4]][substring(temp_transitions_list[[3+day*4-4]][,3],1,6) == "switch",]
    o4 = temp_transitions_list[[4+day*4-4]][substring(temp_transitions_list[[4+day*4-4]][,3],1,6) == "switch",]
    
    #remove all NAs
    o1 = o1[!is.na(o1[,4]),]
    o2 = o2[!is.na(o2[,4]),]
    o3 = o3[!is.na(o3[,4]),]
    o4 = o4[!is.na(o4[,4]),]
    
    #remove all phantom transitions
    o1 = o1[!(o1[,7] == 0),]
    o2 = o2[!(o2[,7] == 0),]
    o3 = o3[!(o3[,7] == 0),]
    o4 = o4[!(o4[,7] == 0),]
    
    #use column to to add in which cage mouse resides
    o1[substring(o1[,3],1,9) == "switch RL",2] = 1
    o1[substring(o1[,3],1,9) == "switch LR",2] = 0
    o2[substring(o2[,3],1,9) == "switch RL",2] = 1
    o2[substring(o2[,3],1,9) == "switch LR",2] = 0
    o3[substring(o3[,3],1,9) == "switch RL",2] = 1
    o3[substring(o3[,3],1,9) == "switch LR",2] = 0
    o4[substring(o4[,3],1,9) == "switch RL",2] = 1
    o4[substring(o4[,3],1,9) == "switch LR",2] = 0
    
    #use column 1 for mouse names
    o1[,1] = 1
    o2[,1] = 2
    o3[,1] = 3
    o4[,1] = 4
    
    #count transitions
    o1_trans = o1_trans + nrow(o1)
    o2_trans = o2_trans + nrow(o2)
    o3_trans = o3_trans + nrow(o3)
    o4_trans = o4_trans + nrow(o4)
    
    allmice = rbind(o1,o2,o3,o4)
    allmice = allmice[order(allmice[,8]),]
    
    #get following behavior
    for(j in 1:nrow(allmice)){
      #get all transitions within X ms of the current transition
      withinlimit = (as.numeric(allmice[,8]) >= as.numeric(allmice[j,8])) & (as.numeric(allmice[,8]) < (as.numeric(allmice[j,8]) + 1000)) & (allmice[,2] == allmice[j,2])
      
      if(sum(withinlimit) > 1){
        limitmice = allmice[withinlimit,]
        
        #add the time of only the next following mouse to the boxplot time table
        if(limitmice[1,1] == 1 & limitmice[2,1] == 2) m12_bp = c(m12_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 1 & limitmice[2,1] == 3) m13_bp = c(m13_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 1 & limitmice[2,1] == 4) m14_bp = c(m14_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))        
        if(limitmice[1,1] == 2 & limitmice[2,1] == 1) m21_bp = c(m21_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 2 & limitmice[2,1] == 3) m23_bp = c(m23_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 2 & limitmice[2,1] == 4) m24_bp = c(m24_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))        
        if(limitmice[1,1] == 3 & limitmice[2,1] == 1) m31_bp = c(m31_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 3 & limitmice[2,1] == 2) m32_bp = c(m32_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 3 & limitmice[2,1] == 4) m34_bp = c(m34_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))        
        if(limitmice[1,1] == 4 & limitmice[2,1] == 1) m41_bp = c(m41_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 4 & limitmice[2,1] == 2) m42_bp = c(m42_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))
        if(limitmice[1,1] == 4 & limitmice[2,1] == 3) m43_bp = c(m43_bp,as.numeric(limitmice[2,8]) - as.numeric(limitmice[1,8]))        
        
        for(k in 2:2){ #nrow(limitmice)) #count all following mice as followers
          mousefollowmatrix[as.numeric(limitmice[1,1]),as.numeric(limitmice[k,1])] = mousefollowmatrix[as.numeric(limitmice[1,1]),as.numeric(limitmice[k,1])] + 1
        }
      }
    }
  }
  
  if(cage == 1){
    cage2_followmatrix = mousefollowmatrix
    cage2_followmatrix[1,5] = o1_trans
    cage2_followmatrix[2,5] = o2_trans
    cage2_followmatrix[3,5] = o3_trans
    cage2_followmatrix[4,5] = o4_trans
    cage2_m12_bp = m12_bp
    cage2_m13_bp = m13_bp
    cage2_m14_bp = m14_bp
    cage2_m21_bp = m21_bp
    cage2_m23_bp = m23_bp
    cage2_m24_bp = m24_bp
    cage2_m31_bp = m31_bp
    cage2_m32_bp = m32_bp
    cage2_m34_bp = m34_bp
    cage2_m41_bp = m41_bp
    cage2_m42_bp = m42_bp
    cage2_m43_bp = m43_bp
  }
  if(cage == 2){
    cage3_followmatrix = mousefollowmatrix
    cage3_followmatrix[1,5] = o1_trans
    cage3_followmatrix[2,5] = o2_trans
    cage3_followmatrix[3,5] = o3_trans
    cage3_followmatrix[4,5] = o4_trans
    cage3_m12_bp = m12_bp
    cage3_m13_bp = m13_bp
    cage3_m14_bp = m14_bp
    cage3_m21_bp = m21_bp
    cage3_m23_bp = m23_bp
    cage3_m24_bp = m24_bp
    cage3_m31_bp = m31_bp
    cage3_m32_bp = m32_bp
    cage3_m34_bp = m34_bp
    cage3_m41_bp = m41_bp
    cage3_m42_bp = m42_bp
    cage3_m43_bp = m43_bp
  }
  if(cage == 3){
    cage4_followmatrix = mousefollowmatrix
    cage4_followmatrix[1,5] = o1_trans
    cage4_followmatrix[2,5] = o2_trans
    cage4_followmatrix[3,5] = o3_trans
    cage4_followmatrix[4,5] = o4_trans
    cage4_m12_bp = m12_bp
    cage4_m13_bp = m13_bp
    cage4_m14_bp = m14_bp
    cage4_m21_bp = m21_bp
    cage4_m23_bp = m23_bp
    cage4_m24_bp = m24_bp
    cage4_m31_bp = m31_bp
    cage4_m32_bp = m32_bp
    cage4_m34_bp = m34_bp
    cage4_m41_bp = m41_bp
    cage4_m42_bp = m42_bp
    cage4_m43_bp = m43_bp
  }
}

##calculate followrate and influencerate as a relation between # of transitions and follow/influence events
c2_influenceRate = array(0,dim=c(4))
c3_influenceRate = array(0,dim=c(4))
c4_influenceRate = array(0,dim=c(4))
c2_followRate = array(0,dim=c(4))
c3_followRate = array(0,dim=c(4))
c4_followRate = array(0,dim=c(4))
for(i in 1:4){
  c2_influenceRate[i] = sum(cage2_followmatrix[i,1:4])/cage2_followmatrix[i,5]
  c3_influenceRate[i] = sum(cage3_followmatrix[i,1:4])/cage3_followmatrix[i,5]
  c4_influenceRate[i] = sum(cage4_followmatrix[i,1:4])/cage4_followmatrix[i,5]
  c2_followRate[i] = sum(cage2_followmatrix[,i])/cage2_followmatrix[i,5]
  c3_followRate[i] = sum(cage3_followmatrix[,i])/cage3_followmatrix[i,5]
  c4_followRate[i] = sum(cage4_followmatrix[,i])/cage4_followmatrix[i,5]
}
  
#plot activity against influence and followrate
ggplot() +
  geom_point(aes(c2_influenceRate,cage2_followmatrix[,5]),color="red",size=2) +
  geom_point(aes(c3_influenceRate,cage3_followmatrix[,5]),color="black",size=2) +
  geom_point(aes(c4_influenceRate,cage4_followmatrix[,5]),color="blue",size=2) +
  geom_point(aes(c2_followRate,cage2_followmatrix[,5]),color="red",shape=4,size=2,stroke=2) +
  geom_point(aes(c3_followRate,cage3_followmatrix[,5]),color="black",shape=4,size=2,stroke=2) +
  geom_point(aes(c4_followRate,cage4_followmatrix[,5]),color="blue",shape=4,size=2,stroke=2) +
  geom_path(aes(c(c2_influenceRate,c2_followRate),c(rep(cage2_followmatrix[,5],2))),group = rep(c(1,2,3,4),2),color="red") +
  geom_path(aes(c(c3_influenceRate,c3_followRate),c(rep(cage3_followmatrix[,5],2))),group = rep(c(1,2,3,4),2),color="black") +
  geom_path(aes(c(c4_influenceRate,c4_followRate),c(rep(cage4_followmatrix[,5],2))),group = rep(c(1,2,3,4),2),color="blue")

  
#followevents and influenceevents as percentages
fi_percent = array(0,dim=c(12,7))
colnames(fi_percent) = c("Group","Mouse","Transitions","FollowE","Follow%","InfluenceE","Influence%")
fi_percent[,1] = c(1,1,1,1,2,2,2,2,3,3,3,3)
fi_percent[,2] = rep(c(1,2,3,4),3)
for(i in 1:4){
  fi_percent[i,3] = cage2_followmatrix[i,5]
  fi_percent[i,4] = sum(cage2_followmatrix[1:4,i])
  fi_percent[i,5] = (sum(cage2_followmatrix[1:4,i])/cage2_followmatrix[i,5])*100
  fi_percent[i,6] = sum(cage2_followmatrix[i,1:4])
  fi_percent[i,7] = (sum(cage2_followmatrix[i,1:4])/cage2_followmatrix[i,5])*100
  
  fi_percent[i+4,3] = cage3_followmatrix[i,5]
  fi_percent[i+4,4] = sum(cage3_followmatrix[1:4,i])
  fi_percent[i+4,5] = (sum(cage3_followmatrix[1:4,i])/cage3_followmatrix[i,5])*100
  fi_percent[i+4,6] = sum(cage3_followmatrix[i,1:4])
  fi_percent[i+4,7] = (sum(cage3_followmatrix[i,1:4])/cage3_followmatrix[i,5])*100
  
  fi_percent[i+8,3] = cage4_followmatrix[i,5]
  fi_percent[i+8,4] = sum(cage4_followmatrix[1:4,i])
  fi_percent[i+8,5] = (sum(cage4_followmatrix[1:4,i])/cage4_followmatrix[i,5])*100
  fi_percent[i+8,6] = sum(cage4_followmatrix[i,1:4])
  fi_percent[i+8,7] = (sum(cage4_followmatrix[i,1:4])/cage4_followmatrix[i,5])*100
}
mean(fi_percent[,5]) #mean follow event %
mean(fi_percent[,7]) #mean influence event %
fi_percent

#plot influencerate against followrate
ggplot() +
  geom_point(aes(c2_followRate,c2_influenceRate),color="red",size=2) + 
  geom_point(aes(c3_followRate,c3_influenceRate),color="black",size=2) + 
  geom_point(aes(c4_followRate,c4_influenceRate),color="blue",size=2) +
  geom_abline()


