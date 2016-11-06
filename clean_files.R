# clean up data to prepare for simulation





# split the solar data into two individual data frames
# 1: solarMonth -- month + monthly capacity factor
# 2: solarDay -- hour * utilization of solar by hour of the day (value/total)
# and delete the original solar data
splitSolarData <- function(data){

    # solar supply needs to be split into two dfs
    data$solarMonth <- data$solarSupply[,1:3]
    data$solarDay <- data$solarSupply[,5:7]
    # https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-can-I-set-components-of-a-list-to-NULL_003f
    # deletes an element of a list 
    data$solarSupply <- NULL
    data
}


# ensure we have the right class/types for each df
# str(all_data)
# by manual inspection, all seems to be right

# remove incomplete cases

trimNAs <- function(data) {
    
    print("Initial object size, pre trimNAs is:")
    print(object.size(data),units='auto')
    
    # trim the NA values from the data
    
    # store the names for replacement
    temp_names <- names(data)
    
    # iterate through all list items
    temp <- lapply(1:length(data), function(x){
        # return only complete cases of the tbl_df object
        data[[x]][complete.cases(data[[x]]),]
    })
    
    # add the names back in
    names(temp) <- temp_names
    print("The input file is now:")
    print(object.size(temp),units='auto')
    temp
}

# test trimNAs function
#temp <- trimNAs(all_data)
