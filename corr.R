corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
	
	id=1:332
	datafiles<-c()
	datafiles<-c(datafiles,paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep=""))
	
	#vect is a numeric vertor which stores correlation between sulfate and nitrate	
	vect<-numeric()
	i<-1
	#nobs is the no of complete cases in each id 
	nobs<-numeric()
	while(i <= length(id))
	{
		data<-read.csv(datafiles[i])
		data<-data[complete.cases(data),]
		nobs<-nrow(data)
		if(nobs>threshold)
		{
			c<-cor(data$sulfate,data$nitrate)
			vect<-c(vect,c)
		}	
		i<-i+1
	}
	return(vect)
}
