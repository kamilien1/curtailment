## be sure you are in the correct directory
## we want to start in the project directory
# getwd() -- gets the current wd
# setwd() -- sets the wd
# example: setwd("~/Tongbupan/Projects/wind_curtailment")

# use the code analysis functions to analyze the functions
# also load all package libraries
source("libraries.R")
source("code_analysis.R")



# read data into workspace
# to read files in, source the functions from read_files.R
source("read_files.R")

# choose data to upload
getSpreadsheetData(dir = "../data/")

# pseudo clean the data
allData <- loadAll(files, load_file_index, sheets_to_read)
allData <- trimColumns(allData, sheets_to_read)



# clean data into proper format
source('clean_files.R')

# splits the supplySolar field and removes it
allData <- splitSolarData(allData)

# remove the NA values
allData <- trimNAs(allData)


# want to update one worksheet?
# use the below, and everything is updated without additional code required
#allData <- loadOne(files, load_file_index, 'solarSupply', allData)


# format the data objects in preparation for simulation
#source("code/prepare_data_for_simulation.R")

# check mem used at this point
# from the pryr library package
#mem_used()


### back up here
backup <- allData
# allData <- backup







# data is read into allData df
# now build profiles and begin simulation
timeDF <- generateHourlyDates(2012,2025)


model = 'shaanxi'

# check for missing data
missingData <- checkModels(df = allData, gridModelName = model, checkList = checkList)
# missingData
# get the subset of missing data
# subset <- allData[missingData]

# choose the model and subset all data
checkList <- names(allData)[names(allData) != c("gridModels")]
simModel <- chooseModel(dfList = allData, dfNames = checkList, modelChoice = model)
# str(simModel)
# note, may need to get back to this step and replace simModel$windHourly$model = current model


### TODO starting here
# create the demand profile
dProfile <- setHiLoDemandProfile(setAnnualProfile(df = timeDF, 
                               hd = simModel$hourlyDemandProfiles[,-1],
                               md = simModel$monthlyDemandProfiles[,-1], 
                               ad = simModel$annualDemand[,-1]
                               )
)
# str(dProfile)



# create the transmission profile
tProfile <- setAnnualProfile(df = timeDF, 
                             hd = simModel$hourlyTransmissionSchedules[,-1],
                             md = simModel$monthlyTransmissionSchedule[,-1],
                             ad = simModel$annualTransmissionForecast[,-1]
                             )
# str(tProfile)
# str(simModel)
# create power flow in and out
transPower <- addPowerFlowTransmission(tProfile)
#str(transPower)


# set up the solar and wind profiles

# make a 1 year solar profile
solarAnnualHourly <- dayMonthToAnnualProfile(simModel$solarDay, simModel$solarMonth)
solarAll <- addDataToDateRange(solarAnnualHourly, unique(timeDF$year)) 
solarGWh <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast, solarAll, "solar")

windAll <- addDataToDateRange(simModel$windHourly$kWh_kW, unique(timeDF$year))
windGWh <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast, windAll, "wind")

solarThermalGWh <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast, solarAll, "solar_thermal")
windOffshoreGWh <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast, windAll, "wind_offshore")

# make a supply 'profile' of 1kwh/kw, effectively creating the iCap level
# unique(simModel$supplyForecast$supply_type)
coalICapGW <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast,rep(1, length(windAll)) , "coal") 
names(coalICapGW) <- 'coalICapGW'
natgasICapGW <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast,rep(1, length(windAll)) , "natgas") 
names(natgasICapGW) <- 'natgasICapGW'
hydroICapGW <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast,rep(1, length(windAll)) , "hydro") 
names(hydroICapGW) <- 'hydroICapGW'
biomassICapGW <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast,rep(1, length(windAll)) , "biomass") 
names(biomassICapGW) <- 'biomassICapGW'
hydroPumpedICapGW <- setAnnualSupplyProfile(timeDF, simModel$supplyForecast,rep(1, length(windAll)) , "hydro_pumped") 
names(hydroPumpedICapGW) <- 'hydroPumpedICapGW'

powerProfile <- cbind(solarGWh, windGWh, solarThermalGWh, windOffshoreGWh, coalICapGW, 
                      natgasICapGW, hydroICapGW, biomassICapGW, hydroPumpedICapGW)

#head(windAll)

# bind everything together to start the simulation
# head(dProfile)
# head(tProfile)



deterministicSupply <- cbind(timeDF, solarGWh, windGWh, solarThermalGWh, windOffshoreGWh)
#head(deterministicSupply)

# get chpDates 
chpDates <- addDataToDateRange(setCHPdates(day(simModel$chpDates$date_start),
                                            month(simModel$chpDates$date_start),
                                            day(simModel$chpDates$date_end),
                                            month(simModel$chpDates$date_end)),
                               unique(timeDF$year))
                    
                    
# get pmin/pmax for coal, natgas
supply = 'coal'
pmin_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                          simModel$supplyOutput$is_chp == TRUE]
pmin_no_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]
pmax_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                          simModel$supplyOutput$is_chp == TRUE]
pmax_no_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]
# (dateDF, chpRatios, chpDates, supply, pmin_chp=0, pmax_chp=1, pmin_no_chp=0, pmax_no_chp=1)
coalPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'coal', 
                               pmin_chp, 
                               pmax_chp, 
                               pmin_no_chp, 
                               pmax_no_chp)

supply = 'natgas'
pmin_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                          simModel$supplyOutput$is_chp == TRUE]
pmin_no_chp = simModel$supplyOutput$pmin[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]
pmax_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                          simModel$supplyOutput$is_chp == TRUE]
pmax_no_chp = simModel$supplyOutput$pmax[simModel$supplyOutput$supply_type == supply & 
                                             simModel$supplyOutput$is_chp == FALSE]
# (dateDF, chpRatios, chpDates, supply, pmin_chp=0, pmax_chp=1, pmin_no_chp=0, pmax_no_chp=1)
natgasPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'natgas', 
                                 pmin_chp, 
                                 pmax_chp, 
                                 pmin_no_chp, 
                                 pmax_no_chp)



# (dateDF, chpRatios, chpDates, supply, pmin_chp=0, pmax_chp=1, pmin_no_chp=0, pmax_no_chp=1)
# pmin for all other: wind, solar, wind_offshore, solar_thermal, hydro, biomass, hydro_pumped
windPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'wind')
windOffshorePLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'wind_offshore')
solarPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'solar')
solarThermalPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'solar_thermal')
hydroPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'hydro')
biomassPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'biomass')
hydroPumpedPLevels <- setPminPmaxVals(timeDF, simModel$chpRatios, chpDates, supply = 'hydro_pumped')


powerLevels <- cbind(coalPLevels, natgasPLevels, windPLevels, windOffshorePLevels, solarPLevels,
                     solarThermalPLevels, hydroPLevels, biomassPLevels, hydroPumpedPLevels)
print(object.size(powerLevels), units = "Mb")
# head(powerLevels)


# merge df, dProfile, transPower
# deterministicSupply
# powerLevels, powerProfile
head(deterministicSupply)
head(powerLevels)
head(transPower)
head(df)
head(dProfile)
head(powerProfile)

# merge into one df
# 
