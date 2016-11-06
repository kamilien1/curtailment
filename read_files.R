# read in files function


# function list
# 
# loadAll - load all selected worksheets in a file
# vars: 
# - files - a list of file paths
# - load_file_index - the index of the files variable that we want to read
# - worksheets - a data frame of all worksheets that should be read

# trimColumns - trim columns of a list of tbl_df 
# vars:
# - dataList - a list of tbl_df objects
# - worksheets - a data frame of all worksheets with # of cols to keep

# readInteger - user input to ask which file to use 
# vars:
# files - a list of files with absolute filepaths 


# clean the data
# get a list of data types for that tab
# cast the data into these data types
# standardize names if need be

# subset data
# subset based on various criteria



# load in libraries
library(readxl) # excel, vignette https://cran.r-project.org/web/packages/readxl/readxl.pdf
library(dplyr)  # data manipulation 


# http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value
# use assign("var",value) to split the output into different parts
getSpreadsheetData <- function(dir = 'data/') {
    
    
    
    
    #the double operator <<- globally assigns these variables
    
    # list all files
    
    
    files <<- list.files(dir, full.name=T)
    
    # get the index for reading data
    # assume that you are in the current project directory
    # the input data should be stored in the 'data/' directory
    # review the files in that directory and 
    # note: if a file begins with the tilde (~) ignore that file, it means
    # it is a local temporary save and should be ignored
    load_file_index <<- readInteger(files)

    # get the names of the worksheets
    # sanity check to see what is in the files
     worksheet_names <<- excel_sheets(files[load_file_index])
     print("The names of the worksheet you selected are:")
     print(worksheet_names)
    
    # get the data summary worksheet
    # note: if there is no data summary sheet, these functions will not work
    dataSummary <<- read_excel(files[load_file_index],sheet="dataSummary")
    
    dataSummary <<- dataSummary[complete.cases(dataSummary),]
    
    # make a list of sheets to read, exclude the 3rd column (a description column)
    
    sheets_to_read <<- 
        filter( select(dataSummary, -3), read == "T")
    
    # have a quick look at what to read
    print("The data summary table shows:")
    print(sheets_to_read)
    
    
}



# example code to run this function
# dir = 'data/'
# getSpreadsheetData(dir)


# read in all files somehow
# store them
# process them later


# get the size of the data frame
# https://stat.ethz.ch/R-manual/R-devel/library/utils/html/object.size.html 


### function list
# read_all_sheets - initial read, or clean read
# read_one_sheet - one sheet of a workbook, when data is updated



# read an integer, used to read files in
# http://www.rexamples.com/4/Reading%20user%20input
readInteger <- function(files)
{ 
    print("List of files to choose from are:")
    print(files)
    n <- readline(prompt="Enter the file index to read:")
    
    # if not an integer, try again
    if(!grepl("^[0-9]+$",n))
    {
        print(paste("Sorry, index",n,"is not an integer. Try again."))
        return(readInteger(files))
    }
    
    print(paste("Returning index",n,"which corresponds to file",files[as.integer(n)]))
    return(as.integer(n))
}

# test
#print(readInteger(files))


# a fresh load in of all the data
# loadAll loads all data into a list of tbl_df objects
# variables:
# - files: a list of files with the absolute path to the file location
# - load_file_index: an integer index value used to choose the file to read
# - worksheets: a data frame with column 1 providing the exact string match

# to a worksheet name in files
loadAll <-  function(files, load_file_index = 4, worksheets){
    
    
    temp <- lapply(1:dim(worksheets)[1],
                   function(x){
                       read_excel(files[load_file_index],
                                  # first column of worksheets table
                                  # has the name of the sheet
                                  sheet=as.character(worksheets[x,1]))
                       
                   })
    
    
    
    # first column is used for names
    names(temp) <- as.character(unlist(worksheets[,1]))
    temp
}
 
# test loadAll()
# all_data <- loadAll(files, load_file_index, sheets_to_read)
# names(all_data)
# str(all_data)


# load a single worksheet from a list of files with a load_file_index
# use case: we are updating some data and want to update a single
# data frame input
# files - list of files to read
# load_file_index - the index of the file to read
# worksheet_to_load - the name of the worksheet to reload, if blank or wrong
# it must be corrected by the user via text input
# dataList - the list of data to replace

loadOne <- function(files, load_file_index, worksheet_to_load = NA, dataList) {
    
    if(is.na(worksheet_to_load) | !(worksheet_to_load %in% sheets_to_read$`Data Table` )){
        print(paste("Worksheet:",worksheet_to_load,"not found"))
        print(sheets_to_read$`Data Table`)
        worksheet_to_load <- readline(prompt = 'Type the name of the worksheet, with the right case: ')
        
        if (!(worksheet_to_load %in% sheets_to_read$`Data Table`)){
            return(loadOne())
        }
    }
    # read in the sheet
    print(paste("Reading in single worksheet",worksheet_to_load))
    temp <- list(read_excel(files[load_file_index], sheet = worksheet_to_load))
    
    # trim the columns using trimColumns()
    temp <- trimColumns(temp, filter(sheets_to_read,`Data Table`==worksheet_to_load))
    
    # add the names back in
    names(temp) <- worksheet_to_load
    

    
    # add this to the existing data frame
    
    # if solar, split solar
    if (worksheet_to_load == "solarSupply"){
        
        # split the solar data before adding it back
        temp <- splitSolarData(temp)
        print(temp)
        
    }
    
    # trim any NA values
    # note: this must be done after checking for solarSupply
    temp <- trimNAs(temp)
    
    # http://stackoverflow.com/questions/9561053/assign-values-to-a-list-element-in-r
    # clever way to update the list based on x elements in temp
    
    # update the data list
    dataListNames <- names(dataList)
    dataList <- lapply(names(dataList), function(x){

    # if the data is found in temp, update it from there
    # otherwise keep it the same
    if(x %in% names(temp)){
        # print(paste(x,'in names temp',names(temp)))
        temp[[x]]
    } else {
        
        # print(paste(x,'not in names temp'))
        # the only odd ball is for solarMonth, solarDay above value is printed twice
        dataList[[x]]
    }
        
    })
    
    names(dataList) <- dataListNames
    dataList
}

# test code
# all_data <- loadOne(files, load_file_index, 'solarSupply', all_data)








# trimColumns
# takes in the master list of data tables
# spits out a trimmed version where there are the right # of columns
# variables:
# - dataList: a list object of the data tables
# - worksheets: a data frame that includes the data to trim columns
# and also includes the names of each list element to rename
# there may be instances where a worksheet contains two data frames
# focus only on cutting columns out here
trimColumns <- function(dataList, worksheets) {
    
    
    
    temp <- lapply(1:dim(worksheets)[1],
                   function(x){
                       # access the native object with [[]] in the list
                       # return the 1:# of data columns to trim it
                       dataList[[x]][1:worksheets$dataColumns[x]]
                   })
    
    
    # rename the names of each list item that were lost
    names(temp) <- as.character(unlist(worksheets[,1]))
    temp
    
}
    
# test trimColumns
# all_data <- trimColumns(all_data,sheets_to_read)
# names(all_data)
# str(all_data)


