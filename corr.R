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

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations

        # Create vector of the number of completely observed cases in each data file.
        cob <- complete(directory, 1:332)                       # Check all monitor stations, using complete function
        
        # Subset vector where number of completely observed cases is greather than threshold.
        cobthreshold <- subset(cob$id, cob$nobs > threshold)    # Subset cob vector based on complete observations exceed threshold
        
        corr <- numeric()                                       # creates an empty numeric vector for result 
        if (length(cobthreshold)>0) {                           # Check to see if any monitors meet the threshold requirement
        # For each station that has exceeeded the threshold, calculate correlation between sulfate and nitrate
                files_list <- list.files(directory, full.names=TRUE)    # Create a list of files
                for (i in cobthreshold) {                               # For each monitor that has exceeded the threshold
                        intercorr <- read.csv(files_list[i])            # Read in the monitor observations from the single .CSV file
                        x <- intercorr$sulfate                          # Subset sulfate obs into vector for correlation operation
                        y <- intercorr$nitrate                          # Subset nitrate obs into vector for correlation operation
                        z <- cor(x, y, use="pairwise.complete.obs")     # Calculate correlation of sulfate and nitrate obs, accounting for NAs
                        corr <- append(corr,z)                          # Add correlation result to vector of past results
                }
        }
        corr                                                    # Return a numeric vector of correlations
}

