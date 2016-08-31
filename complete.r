complete <- function(directory, id = 1:332){
	res_df <- data.frame(numeric(),numeric())
	#print(res_df)
	for (i in id){
	
		if(nchar(i)==1){ mod_i <- paste("00",i,sep="")}
		else if(nchar(i)==2){ mod_i <- paste("0",i,sep="")}
		
		df1<-read.csv(paste(directory,"/",mod_i,".csv",sep=""))
		
		comp_df1 <- df1[complete.cases(df1),]
		
		res_df <- rbind(res_df,c(i,nrow(comp_df1)))
		#print(names(res_df))	
	}
	names(res_df)<-c("id","nobs")
	res_df
	
}