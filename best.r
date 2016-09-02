best <- function(state, outcome){
    
    ################################################################
    #   Make the outcome string compatible with the column Name so #
    #   one can fetch the data from the column                     #
    ################################################################
    
    s1 <- strsplit(outcome," ")     #split the input string by words
    s1 <- unlist(s1)                #removing the class as list
    
        
        #FUNCTION that takes in a word and changes the first letter to upper case #
        
    chngupper <-function(s){
        s<-strsplit(s,"")
        s<-unlist(s)                #making s as a character vector
        s[1]<-toupper(s[1])         #the fist letter is changed to Upper case
        s<-paste(s,collapse="")     #bringing it all together as one string 
        
    }
    
    for (i in 1:length(s1)){        #calls chngupper for eac word in outcome string
        s1[i]<-chngupper(s1[i])
        
    }
    
    s2<-character()                 #sews together the modified input string
    for (i in 1:length(s1)){
        s2<-paste(s2,".",s1[i],sep="")
    }
    
    stroutcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from",s2,sep="")   #final string as per the column  name to be fetch
    
    ###############################################################
    #VERIFY IF STATE AND OUTCOME ARE VALID, OTHERWISE THROW ERROR #
    ###############################################################
    my_file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    if(!state %in% my_file$State)
        stop("invalid state")
    else if(!stroutcome %in% c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        stop("invalid outcome")
    
    ######################################################################################################
    # 1.select data from file only for the input state
    # 2.remove outcome=NA valued rows(suppress warning(s) just for this operation)
    # 3.order the file by outcome
    # 4.return only the top(least outcome/rate of mortality) row's Hospital.Name from ordered file
    #################################################################################################
    
    my_file <- my_file[my_file$State==state,]
    options(warn=-1)
    my_file[[stroutcome]] <- as.numeric(my_file[[stroutcome]])
    options(warn=0)
    my_file <- my_file[!is.na(my_file[[stroutcome]]),]  #rows with stroutcome=NA removed
    
    my_file<-my_file[order(my_file[[stroutcome]]),] #ordered my_file
    my_file[1,]$Hospital.Name
    
}
