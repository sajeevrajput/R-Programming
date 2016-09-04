rankall <- function(outcome,num="best"){
    
     #LOAD DATASET
    my_file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    ######################################################################################################
    # COLUMN NAME BUILDER                                                                                #  
    ######################################################################################################
    
    outcome<- strsplit(outcome," ")
    outcome<-unlist(outcome)
    
    
    #FUNCTION TO CHANGE FIRST LETTER TO UPPERCASE
    
    changeUpper <- function(s){
        s<-strsplit(s,"")
        s<-unlist(s)
        s[1]<-toupper(s[1])
        s<-paste(s,collapse="")
    }
    
    for (i in 1:length(outcome)){
        outcome[i]<-changeUpper(outcome[i])
    }
    temp <- character()
    for (i in 1:length(outcome)){
        temp <-paste(temp,".",outcome[i],sep="")
    }
    
    outcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from",temp,sep="")   #final string as per the column  name to be fetch
    
    
    ###############################################################
    #VERIFY IF STATE AND OUTCOME ARE VALID, OTHERWISE THROW ERROR #
    ###############################################################
    
    
    if(!outcome %in% c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        stop("invalid outcome")
    ########################################################################################################
     my_file<-my_file[,c("Hospital.Name","State",outcome)]      #keeping only relevant columns
     
     options(warn=-1)
     my_file[[outcome]]<-as.numeric(my_file[[outcome]])         #convert into numeric values       
     options(warn=0)
     
     my_file <- my_file[!is.na(my_file[[outcome]]),]            #removing outcome=NA valued rows 
     #return(my_file)
    
    if(num=="best") num<- 1
    
    res<-data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)  #empty data fram to store the result
    idx<-1                                                                          #index for loop
    
    for (i in sort(unique(my_file$State))){                                         #loops over the state to find the hospitals in order of outcome
        
        temp_file<-my_file[my_file$State==i,]                                       #selects the data from the state
        
        temp_file<-temp_file[order(temp_file[[outcome]],temp_file$Hospital.Name),]  #order temp list by outcome and then Hospital.Name
        
        
        if (num=="worst") res[idx,]<- c(temp_file[dim(temp_file)[1],]$Hospital.Name,i) #for num=worst
        else res[idx,]<-c(temp_file[num,]$Hospital.Name,i)                              #for num=<rest>
        idx<-idx+1
        
    }
   res
    
}