pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        files_list <- list.files(directory, full.names=TRUE)    # creates a list of files
        dat <- data.frame()                                     # creates and empty data frame
        for (i in id) {                                         # loop through the files, binding together
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        if (pollutant == "sulfate") {                           # selecting columne that matches the pollutant
                mean(dat[, "sulfate"], na.rm = TRUE)            # identifies the mean whilst stripping out the NAs
        } else {
                mean(dat[, "nitrate"], na.rm = TRUE)
        }                                    
}