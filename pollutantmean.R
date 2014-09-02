pollutantmean <- function(directory, pollutant, id = 1:332) {

        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        l <<- NULL
        for (i in id) {
           ll  <- paste(directory,"/", sprintf("%03d",i),".csv",sep="")
           l   <<-c(l,ll)
        }

        len <- length(id)
		
        mydata <<- NULL
        for (i in 1:len) {
           x <- read.csv(l[[i]])
           mydata <<- rbind(mydata ,x)
        }


        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        if (!((pollutant=="sulfate")||(pollutant=="nitrate"))){
          print("pollutant must be either sulfate or nitrate")
          return()
        }

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

        c <- mydata[[pollutant]]
        m <- mean(c , na.rm=T)
        print(m)
}