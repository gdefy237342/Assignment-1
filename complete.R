complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        files_list <- list.files(directory, full.names=TRUE)    # creates a list of files in directory
        dat <- data.frame()                                     # creates an empty data frame to read a CSV file
        resultdf <- data.frame()                                # creates an empty data frame (for result)
        for (i in id) {                                         # for each of the monitor ID numbers
                dat <- read.csv(files_list[i])                  # read the CSV file for the monitor ID
                nobs <- sum(complete.cases(dat))                # calculate the number of complete cases
                resultdf <- rbind(resultdf,c(i,nobs))           # rbind to result data frame
        }       
        colnames(resultdf) <- c("id", "nobs")                   # changedFilesge column names for cleanup
        resultdf                                                # change column names for cleanup
}