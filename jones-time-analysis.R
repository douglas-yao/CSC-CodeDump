library(dplyr)
library(ggplot2)

phys.data <- read.csv('SurgTimes-2018.csv')

###### Save pain doc ID numbers; Melnik and Fernandez are excluded ######
    painIDs <- c(945,769,960,971,778,962,709,961)
    CSIDs <- c(945,769,709)
    MACIDs <- c(960,971,778,961)
    painIDs.noJones <- c(769,960,971,778,962,709,961)
    CSIDs.noJones <- c(769,709)
#########################################################################

###### SUBSETS (Continued after manipulation)! ######
    # Subset pain docs, extract columns we want
    pain <- phys.data[phys.data$PhysID %in% painIDs,]
    pain <- pain[c("PhysID","PtIDVst","DOS","SchedTime","PtArr","PrimProc",
                   "PreOpStart","PreOpMin","ORStart","ORMin","SrgyStart",
                   "SrgyMin","RcvyStart","RcvyMin")]
#########################################################################
    
###### Manipulation Time! ######
    ### Time manipulations
        # Convert times to appropriate type
        pain$PreOpStart <- strptime(pain$PreOpStart, "%I:%M")
        pain$ORStart <- strptime(pain$ORStart, "%I:%M")
        pain$SrgyStart <- strptime(pain$SrgyStart, "%I:%M")
        pain$RcvyStart <- strptime(pain$RcvyStart, "%I:%M")
    
        # Convert times to minutes after midnight
        pain$PreOpStart2 <- pain$PreOpStart$hour*60 + pain$PreOpStart$min
        pain$ORStart2 <- pain$ORStart$hour*60 + pain$ORStart$min
        pain$SrgyStart2 <- pain$SrgyStart$hour*60 + pain$SrgyStart$min
        pain$RcvyStart2 <- pain$RcvyStart$hour*60 + pain$RcvyStart$min
        
        # Differentiate AM minutes from PM minutes
        pain$PreOpStartMin2[pain$PreOpStartMin2 < 360] <- (pain$PreOpStartMin2[pain$PreOpStartMin2 < 360] + 720)
        pain$ORStart2[pain$ORStart2 < 360] <- (pain$ORStart2[pain$ORStart2 < 360] + 720)
        pain$SrgyStart2[pain$SrgyStart2 < 360] <- (pain$SrgyStart2[pain$SrgyStart2 < 360] + 720)
        pain$RcvyStartMin2[pain$RcvyStartMin2 < 360] <- (pain$RcvyStartMin2[pain$RcvyStartMin2 < 360] + 720)
    
    # Sort by DOS and start minute
    pain <- pain[with(pain, order(PhysID, DOS, ORStart2)),]
    
    ### Calculation for dressing time
        # Calculate for surgery end time in minutes after midnight
        pain$Dressing <- pain$ORStart2 + pain$SrgyMin
    ### Calculation for turnover time 
        StartMin <- pain$ORStart2
        RcvyStart3 <- pain$RcvyStart2
        
        # Create new out-of-OR time vector of end-time of previous surgery, append NA onto start of new dressing time vector
        RcvyStart3 <- append(NA, pain$RcvyStart2, 1)
        
        # Delete last element of dressing time vector
        index <- length(RcvyStart3)-1
        RcvyStart3 <- RcvyStart3[1:index]
        
        # Append new dressing time vector to kim.data2 dataframe
        pain$RcvyStart3 <- RcvyStart3
        
        # Create new vector of:
        #     Time difference between OR in-the-room time and previous out-of-OR time.
        #     Equivalent to time between cases.
        pain$Out2In <- pain$ORStart2 - pain$RcvyStart3
        
        # NA for time differences under 0 minutes
        pain$Out2In[pain$Out2In < 0] <- NA
        
        # NA for time differences (turnover times) over 20 minutes
        pain$Out2In[pain$Out2In > 20] <- NA
    
    ### Calculation for in-the-OR time to cut time
        pain$In2Cut <- pain$SrgyStart2 - pain$ORStart2
        
        # NA for time differences under 0 minutes (probably data entry error)
        pain$In2Cut[pain$In2Cut < 0] <- NA
        
        # NA for time differences over 30 min
        pain$In2Cut[pain$In2Cut > 30] <- NA
        
# Get rid of NA values
pain <- na.omit(pain)

###### SUBSETS (Continued)! ######
    # Subset docs who use IVCS
    CS <- pain[pain$PhysID %in% CSIDs,]
    
    # Subset docs who use MAC
    MAC <- pain[pain$PhysID %in% MACIDs,]
    
    # Subset all docs but Jones
    pain.noJones <- pain[pain$PhysID %in% painIDs.noJones,]
    
    # Save all non-Jones, conscious sedation docs (Yung and Chen)
    CS.noJones <- pain[pain$PhysID %in% CSIDs.noJones,]
    
    # Subset Jones 
    Jones <- pain[pain$PhysID==945,]
    
    # Subset Yung
    Yung <- pain[pain$PhysID==709,]
    
    # Subset Chen
    Chen <- pain[pain$PhysID==769,]
    
    # Subset Stoller
    Stoller <- pain[pain$PhysID==961,]
    
    # Subset Derby
    Derby <- pain[pain$PhysID==960,]
#########################################################################