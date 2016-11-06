# now all the data has been loaded and cleaned
# we must prepare it for the final step before simulation
# each type of data needs to be in its final format, ready for simluation
# we'll create a large data frame to perform row-wise operations
#
# Demand
# - min, max profiles
# - generate hourly time series from start to end year
# - 



# charts
# load gradient chart: x - 24 hrs, y - day, fill = energy intensity
# http://esd.mit.edu/people/dissertations/cheng.pdf page 163



# supply
# historical supply
# - solar
# - wind
# - hydro? 
# forecasted supply
# - coal
# - nat gas

# time series
# supply - wind, solar, hydro? 
# demand - bulk demand
# - - min profile
# - - max profile
# - - averaged profile





# start and stop dates
# create an hourly counter of these dates
# year, month, day, hour

# POSIXlt is the class required to store
# information in y, m, d, and hour format
# start_year, end_year are easy
# tz is optional, leave blank to default to GMT to avoid snafu's with computers
# for now, it will be a start and end year, not a specific range of dates
# note: leap years are handled properly here
generateHourlyDates <- function(start_year, end_year, tz = "GMT"){

    start <- as.POSIXlt(paste(start_year,"-01-01 00:00",sep=''),tz=tz)
    end <- as.POSIXlt(paste(end_year,"-12-31 23:00",sep=''), tz = tz)
    # make a seq from start to end date by hour
    temp <- seq(from=start, to=end, by='hour')
    # return a data frame of year, month, day, and hour for the date range
    data.frame(year=year(temp),month = month(temp), day=day(temp), hour=hour(temp))
}
# test it out
dateRange <- generateHourlyDates(2012,2020)


# get grid subset model
getModel <- function(df, models, name, dfName){
    
    # get 
    if (name %in% unique(models)){
        # return the subset, use unique() in case there are duplicates by mistake
        # NOTE: there should be no duplicate rows anywhere
        unique(df[models==name,])
    } else {
        # print not found
        print(paste("Warning, model",name,"not found in subset",dfName))
    }
    
}
# test it out
#test <- getModel(allData$chpDates, allData$chpDates$model, "jjt", "CHP Dates")
#test




# check the grid model

checkModels <- function(df, gridModelName, checkList) {
    
    
    # go through each item in the checkList
    # NOTE: the data frame must have a field labeled "model", otherwise
    # there will be an error
    hasSubset <- lapply(checkList, function(x){
        temp <- getModel(df[[x]], df[[x]]$model, gridModelName, x)
        ifelse(is.data.frame(temp), TRUE, FALSE)
    })
    # unlist to create a boolean vector
    hasSubset <- unlist(hasSubset)
    # return missing dataframe subset names
    checkList[!hasSubset]
    

}
# test the function
# don't look at the first df in allData, which is a descriptive data frame
checkList <- names(allData)[-1]
missingData <- checkModels(allData, 'jjt', checkList)
missingData

# get the subset of missing data
subset <- allData[missingData]
# decide on a default to use 

# choose the model to use in each data frame where the model
# was missing
# returns a list of the subset models of the missing model data frames
# NOTE: each df must have a column named "model"
chooseModel <- function(dfList, dfNames, modelChoice = NULL){
    
    # go through each model in missingData
    # if there is no model, return an empty data frame or NA
    # if there is one model only, use that one model
    # if there is 2 or more models, prompt the user to choose
    
    
    
    temp <-
     lapply(dfNames, function(x){
        models <- unique(dfList[[x]]$model)
        
        if(length(models) < 1){
            # return empty data frame
            print(paste("WARNING: data frame", x,
                        "does not have any models. Please manually fix this."))
            data.frame()
            
        } else{
            if(length(models) == 1){
                
                # get the only model and return it
                getModel(dfList[[x]], dfList[[x]]$model, models[1], models[1])
            } else{
                # if the model choice is left blank or its not found
                if(is.null(modelChoice) | !(modelChoice %in% models) ){
                    # allow the user to choose the model
                    print(paste("Please choose your model for",x))
                    temp <- readInteger(models)
                    getModel(dfList[[x]], dfList[[x]]$model, models[temp], models[temp])    
                } else{
                    # get the model from modelChoice 
                    getModel(dfList[[x]], dfList[[x]]$model, modelChoice, modelChoice)
                }
                
            }
        }
        
    })
    names(temp) <- dfNames
    temp
}
# test it out 
checkList <- names(allData)[-1]
jjtModel <- chooseModel(allData, checkList, modelChoice = 'jjt')




# create a one year hourly vector of kwh/kw values 
# for a solar profile
# inputs:
# day - a data frame with model hour, and pct_daily_usage columns
# month - a data frame with model, month, and monthly_cf columns
# outputs:
# vector of 8760 values, representing kwh/kw
dayMonthToAnnualProfile <- function(day, month){
    
    # merge day and month
    temp <- merge(month, day)
    # capacity factor for the month * total power/24 hrs times 24 hours = 
    # total power used in that hour
    temp$kwh_kw <- temp$monthly_cf*temp$pct_daily_usage*24
    temp <- temp[,c('month','hour','kwh_kw')]
    
    # 2014 is a non-leap year year, so it has 8760 rows
    one_year <- seq(from=as.POSIXlt('2014-01-01 00:00'),to=as.POSIXlt('2014-12-31 23:00'),by='hour')
    one_year <- data.frame(one_year)
    one_year$month <- month(one_year$one_year)
    one_year$hour <- hour(one_year$one_year)
    one_year <- one_year[,c('month','hour')]
    
    # left join one year on temp, so create 8760
    temp <- left_join(one_year,temp, by = c("month","hour"))
    
    # return vector of kwh/kw values
    temp$kwh_kw
}
# test it out
solarAnnualHourly <- dayMonthToAnnualProfile(allData$solarDay, allData$solarMonth)

# duplicate values with a start and stop index
# use case: duplicate feb 28 to feb 29 in leap year
# ex: vec = 1 2 3 4 5 6
# start = 2, pad = 2
# result = 1 2 3 4 3 4 5 6
padValues <- function(vec, start, padSize, reps = 2){
    
    # start after the start spot
    # rep it reps times
    # splice it into the start space
    c(vec[1:start], rep(vec[(start+1):(start+padSize)],reps),vec[(start+1+padSize):length(vec)])
}

# test it out
# 31 days jan, 27 days into feb, 24 hours start index
# copy 24 hours into feb 29
leap <- padValues(solarAnnualHourly, (31+27)*24, 24)
length(leap)

# pad all years of data
# go through each year
# if has leap year (feb 29)
# pad that day


# add data to date range
# use case:
# solar, wind annual profile needs to be elongated to the start and end
# year, and deal with leap year issue
addDataToDateRange <- function(dataVector, years){
    
    # get the leap years and call it leaps
    leaps <- years[leap_year(years)]
    
    temp <- lapply(years, function(x,y){
        
        # if year in leap year, pad it
        if(x %in% y){
            padValues(dataVector, (31+27)*24,24)
            
        # otherwise simply return the original data vector
        } else {
            dataVector
        }
    }, y = leaps)
    
    # return the data as a vector, not a list
    as.vector(unlist(temp))
}
# test it out with solar and wind
solarAll <- addDataToDateRange(solarAnnualHourly, unique(dateRange$year))
windAll <- addDataToDateRange(allData$windHourly$kWh_kW, unique(dateRange$year))

# build demand profile


# hd, md, ad = hourly, monthly, annual df
# properly formatted before entered into function
# all df's must be subset AND strip the model column
# hourly profile also has a seasons column to pay attention to
setAnnualProfile <- function(df, hd, md, ad){
    
    # join annual by year, month by month, and hourly by 
    # hour and seasonal profile
    temp <- left_join(df, ad, by ='year' )
    temp <- left_join(temp, md, by = 'month' )
    temp <- left_join(temp, hd, by = c('hour','season'))    
    
    temp
    
}


# set up the annual demand profile of a supply data source
# that has 1 year of annual demand data
# need to scale it by its growth estimate
# returns a vector to be applied TO a data frame column
setAnnualSupplyProfile <- function(dateDF, supply, supplyProfile, supplyType, joinBy = 'year'){
    
    # filter out the supply type from the supply data frame
    # it should have column name supply_type 
    temp <- filter(supply, supply_type == supplyType)
    # get rid of columns not being used
    temp <- select(temp, -c(model,supply_type))
    # join by year by default
    temp <- left_join(dateDF, temp, by = joinBy)
    
    # assign the icap * supply value
    # it is in MW, turn it into GW 
    temp$tempCol <- temp$iCap_mw*supplyProfile/1000
    
    # change the name
    # note: This is useless here, except can be recycled
    # later when adding different columns
    newColName <- paste(supplyType,"GWh",sep='')
    index <- names(temp) == "tempCol"
    names(temp)[index] = newColName
    
    
    # return the column as a data frame
    temp <- data.frame(temp[,newColName])
    names(temp) = newColName
    temp
    
    
}
# test it out with solar data
solarAll <- addDataToDateRange(solarAnnualHourly, unique(dateRange$year))
temp <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast, solarAll, "solar")
temp <- cbind(dateRange, temp)




# test it out
# see below setHiLoDemandProfile for test

setHiLoDemandProfile <- function(df){
    # create the high load profile
    # note that column names must be the same as vars used below
    temp <- mutate(df, loadHi = d_relative_peak_month*pct_load*max_gw_demand)
    
    
    # create the low load profile
    # note that column names must be the same as vars used below
    # get the min load by season, which dictates the profile to build
    minLoadSeason <- temp %>% group_by(season) %>% dplyr::summarize(minLoad = min(pct_load))

    
    # get min load
    # need to scale the min demand value upward by value/min(load_for_season)
    temp <- left_join(temp, minLoadSeason, by = 'season')
    
    # get the min peak for the month, which we will also use to scale UPWARD
    minLoadMonth <- min(df$d_relative_peak_month)
    
    
    temp <- mutate(temp, loadLo = 
                      # the hourly load divided by the min load in that 24 hour period
                      (pct_load/minLoad) * 
                      # the monthly load divided by the min monthly load in 12 months
                      (d_relative_peak_month/minLoadMonth) *
                      # scale the data UPWARD by the min_gw_demand of the year
                      min_gw_demand
    )
    
    # return the data frame
    temp
}

# set up the annual demand profile
demandProfile <- setAnnualProfile(df, 
                                  simModel$hourlyDemandProfiles[,-1],
                                  simModel$monthlyDemandProfiles[,-1], 
                                  simModel$annualDemand[,-1]
)
head(demandProfile)
hiLoProfile <- setHiLoDemandProfile(demandProfile)
head(hiLoProfile)

# build transmission schedule
# tProfile <- setAnnualProfile(df, 
#                              simModel$hourlyTransmissionSchedules[,-1],
#                              simModel$monthlyTransmissionSchedule[,-1],
#                              simModel$annualTransmissionForecast[,-1]
# )
# head(tProfile)
# using the data above, we need to create power flow in and out 
addPowerFlowTransmission <- function(transDF){
    
    transDF <- mutate(transDF, tPowerInGWh = 
                          # the installed capacity * ratio flowing in
                          gw_flow*inflow_ratio *
                          # if the max flow in changes by month, scale it
                          t_relative_peak_month*
                          # the total line utilization (same for in and out)
                          trans_cap)
    
    transDF <- mutate(transDF, tPowerOutGWh = 
                          # installed capacity * outflow ratio
                          gw_flow*(1-inflow_ratio)*
                          # if the max flow changes by month, scale it
                          t_relative_peak_month*
                          # the total line utilization (same for inand out)
                          trans_cap
                          )
    
    # return the df
    transDF
}
# test it out
tProfile <- setAnnualProfile(df, 
                             simModel$hourlyTransmissionSchedules[,-1],
                             simModel$monthlyTransmissionSchedule[,-1],
                             simModel$annualTransmissionForecast[,-1]
)
transPower <- addPowerFlowTransmission(tProfile)
head(transPower)

# apply variable factors to data frame
# chp on off levels, by 



# returns a 365 day TRUE/FALSE vector
# you can then elongate this with the padValues or other function
setCHPdates <- function(startDay, startMonth, endDay, endMonth){
    # choose a non-leap year end year
    # there are two cases to consider
    # case 1: start date < end date, ex: Apr 10 to Aug 8
    # Case 2: start date > end date, ex: Nov 15 to Apr 4 
    # create a list of the days/months
    # if the chp date is in this list, use it 
    endYear <- ifelse(startMonth < endMonth, "2014-","2015-")
    
    start <- as.POSIXlt(paste("2014-",startMonth,"-",startDay," 00:00",sep=""),tz="GMT")
    end <- as.POSIXlt(paste(endYear,endMonth,"-",endDay," 00:00",sep=""),tz="GMT")
    temp <- seq(from=start, to=end, by='hour')
    
    # create a vector string to compare in/out dates to one year of data
    tempCompare <- paste(month(temp),"-",day(temp))
    oneYear <- seq(from=as.POSIXlt('2014-01-01 00:00'),to=as.POSIXlt('2014-12-31 23:00'),by='hour')
    oneYear <- paste(month(oneYear),"-",day(oneYear))
    
    result <- oneYear %in% tempCompare
    result
    
}
# test it out
chpDates <- addDataToDateRange(setCHPdates(day(simModel$chpDates$date_start),
                                           month(simModel$chpDates$date_start),
                                           day(simModel$chpDates$date_end),
                                           month(simModel$chpDates$date_end)),
                               unique(dateRange$year))
sum(chpDates)

# create pmin, pmax for a supplyOutput type
# for each supply_type
supply = 'coal'
pmin_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                         simModel$supplyOutput$is_chp == TRUE]
pmin_no_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]
pmax_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                          simModel$supplyOutput$is_chp == TRUE]
pmax_no_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]



setPminPmaxVals <- function(dateDF, chpRatios, chpDates, supply, pmin_chp=0, pmax_chp=1, pmin_no_chp=0, pmax_no_chp=1){
    
    # if none exists, make a fake pmin/pax
    if(supply %in% chpRatios$supply_type){
        # filter out the ratios to supply type
        # NOTE: there should only be two rows of data here
        temp <- filter(chpRatios, supply_type == supply)
        temp <- select(temp, year, chp_ratio)
        
    }else{
        temp <- data.frame(year=unique(dateDF$year))
        temp$chp_ratio <- 0
    }
    
    # stretch out the ratios for all years
    temp <- left_join(dateDF,temp, by='year')
    
    # bind the T/F dates 
    temp <- cbind(temp, chpDates)
    
    # use this logic for pmin and pmax
    temp <- mutate(temp, pminTemp =ifelse(chpDates, 
                                          # pmin is a combo of chp/non-chp plant pmins
                                          chp_ratio*pmin_chp + (1-chp_ratio)*pmin_no_chp,
                                          # or its just pmin without chp during off-season
                                          pmin_no_chp
    ) )
    names(temp)[length(names(temp))] = paste("pminRatio_",supply,sep='')
    
    temp <- mutate(temp, pmaxTemp = ifelse(chpDates,
                                           # pmax is a combo of chp/non-chp plants adjusted downward
                                           chp_ratio*pmax_chp + (1-chp_ratio)*pmax_no_chp,
                                           # during off-season, just use pmax no chp (usually 1)
                                           pmax_no_chp))
    names(temp)[length(names(temp))] = paste("pmaxRatio_",supply,sep='')
    
    # return the last two columns
    select(temp, contains("Ratio_"))
    
}
# test it out
supply = 'coal'
pmin_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                          simModel$supplyOutput$is_chp == TRUE]
pmin_no_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]
pmax_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                          simModel$supplyOutput$is_chp == TRUE]
pmax_no_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]

coalPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'coal', 
                               pmin_chp, 
                               pmax_chp, 
                               pmin_no_chp, 
                               pmax_no_chp)

# can set min power output of wind 
windPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'wind')
head(temp)



# decrease demand by transmission levels
# estimate pmin
# 

# wrapper function to build data frame for simulation
