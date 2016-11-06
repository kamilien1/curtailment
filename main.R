## be sure you are in the correct directory
## we want to start in the project directory
# getwd() -- gets the current wd
# setwd() -- sets the wd
# example: setwd("~/Tongbupan/Projects/wind_curtailment")

# use the code analysis functions to analyze the functions
# also load all package libraries
source("code/libraries.R")
source("code/code_analysis.R")



# read data into workspace
# to read files in, source the functions from read_files.R
source("code/read_files.R")

# choose data to upload
getSpreadsheetData(dir = "data/")

# pseudo clean the data
allData <- loadAll(files, load_file_index, sheets_to_read)
allData <- trimColumns(allData, sheets_to_read)



# clean data into proper format
source('code/clean_files.R')

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


# TODO
# data is read into allData df
# now build profiles and begin simulation
df <- generateHourlyDates(2012,2020)

# choose the model, in this case jjt as a test
checkList <- names(allData)[-1]
simModel <- chooseModel(allData, checkList, modelChoice = 'shaanxi')

# create the demand profile
dProfile <- setHiLoDemandProfile(setAnnualProfile(df, 
                               simModel$hourlyDemandProfiles[,-1],
                               simModel$monthlyDemandProfiles[,-1], 
                               simModel$annualDemand[,-1]
                               )
)




# create the transmission profile
tProfile <- setAnnualProfile(df, 
                             simModel$hourlyTransmissionSchedules[,-1],
                             simModel$monthlyTransmissionSchedule[,-1],
                             simModel$annualTransmissionForecast[,-1]
                             )
transPower <- addPowerFlowTransmission(tProfile)


# set up the solar and wind profiles
solarAll <- addDataToDateRange(solarAnnualHourly, unique(dateRange$year))
windAll <- addDataToDateRange(allData$windHourly$kWh_kW, unique(dateRange$year))
solarGWh <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast, solarAll, "solar")
windGWh <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast, windAll, "wind")
solarThermalGWh <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast, solarAll, "solar_thermal")
windOffshoreGWh <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast, windAll, "wind_offshore")
# make a supply 'profile' of 1kwh/kw, effectively creating the iCap level
# unique(simModel$supplyForecast$supply_type)
coalICapGW <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast,rep(1, length(windAll)) , "coal") 
names(coalICapGW) <- 'coalICapGW'
natgasICapGW <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast,rep(1, length(windAll)) , "natgas") 
names(natgasICapGW) <- 'natgasICapGW'
hydroICapGW <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast,rep(1, length(windAll)) , "hydro") 
names(hydroICapGW) <- 'hydroICapGW'
biomassICapGW <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast,rep(1, length(windAll)) , "biomass") 
names(biomassICapGW) <- 'biomassICapGW'
hydroPumpedICapGW <- setAnnualSupplyProfile(dateRange, simModel$supplyForecast,rep(1, length(windAll)) , "hydro_pumped") 
names(hydroPumpedICapGW) <- 'hydroPumpedICapGW'

powerProfile <- cbind(solarGWh, windGWh, solarThermalGWh, windOffshoreGWh, coalICapGW, 
                      natgasICapGW, hydroICapGW, biomassICapGW, hydroPumpedICapGW)

head(windAll)

# bind everything together to start the simulation
head(dProfile)
head(tProfile)



deterministicSupply <- cbind(dateRange, solarGWh, windGWh, solarThermalGWh, windOffshoreGWh)
head(deterministicSupply)

# get chpDates 
chpDates <- addDataToDateRange(setCHPdates(day(simModel$chpDates$date_start),
                                            month(simModel$chpDates$date_start),
                                            day(simModel$chpDates$date_end),
                                            month(simModel$chpDates$date_end)),
                               unique(dateRange$year))
                    
                    
# get pmin/pmax for coal, natgas
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

natgasPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'natgas', 
                                 pmin_chp, 
                                 pmax_chp, 
                                 pmin_no_chp, 
                                 pmax_no_chp)



# pmin for all other: wind, solar, wind_offshore, solar_thermal, hydro, biomass, hydro_pumped
windPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'wind')
windOffshorePLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'wind_offshore')
solarPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'solar')
solarThermalPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'solar_thermal')
hydroPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'hydro')
biomassPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'biomass')
hydroPumpedPLevels <- setPminPmaxVals(dateRange, simModel$chpRatios, chpDates, supply = 'hydro_pumped')


powerLevels <- cbind(coalPLevels, natgasPLevels, windPLevels, windOffshorePLevels, solarPLevels,
                     solarThermalPLevels, hydroPLevels, biomassPLevels, hydroPumpedPLevels)
print(object.size(powerLevels), units = "Mb")
head(powerLevels)


# merge df, dProfile, transPower
# deterministicSupply
# powerLevels, powerProfile
head(deterministicSupply)
head(powerLevels)
head(transPower)
head(df)
head(dProfile)
head(powerProfile)


