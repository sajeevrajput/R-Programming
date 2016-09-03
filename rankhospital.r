rankhospital <- function(state, outcome, num=best){

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
    
    
    if(!state %in% my_file$State)
        stop("invalid state")
    else if(!outcome %in% c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        stop("invalid outcome")
    ########################################################################################################
    
    
    my_filestate<-my_file[,c("Hospital.Name","State",outcome)]                              #Keeping only required columns
    
    my_filestate<- my_filestate[my_filestate$State==state,]                                 #dataset for the input State only e.g only for TX
    
    options(warn=-1)
    my_filestate[[outcome]]<-as.numeric(my_filestate[[outcome]])                            #converting outcome col to be numeric
    options(warn=0)
    
    my_filestate <- my_filestate[!is.na(my_filestate[[outcome]]),]                          #removing outcome==NA rows
    
    res_data <- my_filestate[order(my_filestate[[outcome]],my_filestate$Hospital.Name),]    #order by outcome, then by hospital
    
    
    if(num=="best") return(res_data[1,]$Hospital.Name)
    else if(num=="worst") return(res_data[dim(res_data)[1],]$Hospital.Name)
    else return(res_data[num,]$Hospital.Name)

}