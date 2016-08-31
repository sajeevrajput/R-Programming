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
    
    str1<-paste("Hospital.30.Day.Death..Mortality..Rates.from",s2,sep="")   #final string as per the column  name to be fetch
    

}