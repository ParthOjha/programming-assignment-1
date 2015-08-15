complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
	
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        datafiles<-c()
	datafiles<-c(datafiles,paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep=""))
	
	i<-1
	#nobs is the no of complete cases in each id 
	nobs<-c()
	while(i <= length(id))
	{
		data<-read.csv(datafiles[i])
		nobs<-c(nobs,nrow(data[complete.cases(data),]))
		data<-c()
		i<-i+1
	}
        
	## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
	
	data_frame<-data.frame(id,nobs)
	return(data_frame)
}