#!/share/apps/R-3.2.2_gcc/bin/Rscript
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

#################################################
## FUNCTIONS
#################################################

CodlingMothPercentPopulation<-function(CodMothParams,metdata_data.table) {
  stage_gen_toiterate<-length(CodMothParams[,1])
  # store relative numbers
  masterdata<-data.frame(metdata_data.table$dayofyear,metdata_data.table$year, metdata_data.table$month, metdata_data.table$Cum_dd_F)
  colnames(masterdata)<-c("dayofyear","year","month","Cum_dd_F")
  for (i in 1:stage_gen_toiterate) {
    relnum<-pweibull(metdata_data.table$Cum_dd_F, shape=CodMothParams[i,3], scale=CodMothParams[i,4]) 
    relnum<-data.frame(relnum)
    colnames(relnum)<-paste("Perc", CodMothParams[i,1], "Gen", CodMothParams[i,2], sep="")
    masterdata<-cbind(masterdata,relnum)
    head(masterdata)
  }
  head(masterdata)
  #head(allrelnum)
  allrelnum<-masterdata
  allrelnum$PercEgg = 0
  allrelnum$PercLarva = 0
  allrelnum$PercPupa = 0
  allrelnum$PercAdult = 0
  i = 1
  #allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6], 5] <- allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6], paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")]
  # for (i in 1:4) {
  #   columnname<-paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
  #   columnnumber<-which( colnames(allrelnum)==columnname )
  #   #allrelnum$PercEgg[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6]] <- allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6],columnnumber]
  #   allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6], columnnumber] <- allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6],columnnumber]
  # }
  for (i in 1:4) {
    columnname <- paste("Perc", CodMothParams[i,1], "Gen", CodMothParams[i,2], sep="")
    columnnumber <- which( colnames(allrelnum)==columnname )
    allrelnum$PercEgg[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6]] <-allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6],columnnumber]
  }
  for (i in 5:8) {
    columnname <- paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2], sep="")
    columnnumber <- which( colnames(allrelnum)==columnname )
    allrelnum$PercLarva[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6]] <-allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6],columnnumber]
    #check 0 or NA
    #allrelnum[!(allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6]), columnnumber] <- NA
  }  
  for (i in 9:12) {
    columnname<-paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber<-which( colnames(allrelnum)==columnname )
    allrelnum$PercPupa[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6]] <-allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6],columnnumber]
  } 
  for (i in 13:16) {
    columnname<-paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber<-which( colnames(allrelnum)==columnname )
    allrelnum$PercAdult[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6]] <-allrelnum[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6],columnnumber]
    #allrelnum[!(allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6]), columnnumber] <- NA
  } 
  head(allrelnum)
  allrelnum[,"PercAdult"]
  allrelnum<-allrelnum[, c( #"PercEggGen1", "PercEggGen2", "PercEggGen3", "PercEggGen4",
                            "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4",
                            #"PercPupaGen1", "PercPupaGen2", "PercPupaGen3", "PercPupaGen4",
                            "PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4",
                            "PercEgg", "PercLarva","PercPupa","PercAdult",
                            "dayofyear","year","month")]
  return(allrelnum)
}

CodlingMothRelPopulation<-function(CodMothParams,metdata_data.table) {
  stage_gen_toiterate<-length(CodMothParams[,1])
  masterdata<-data.frame(metdata_data.table$dayofyear,metdata_data.table$year, metdata_data.table$month, metdata_data.table$Cum_dd_F)
  colnames(masterdata)<-c("dayofyear","year","month","CumddF")
  for (i in 1:stage_gen_toiterate) {
    relnum<-dweibull(metdata_data.table$Cum_dd_F, shape=CodMothParams[i,3], scale=CodMothParams[i,4]) * 10000
    relnum<-data.frame(relnum)
    colnames(relnum)<-paste(CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    masterdata<-cbind(masterdata,relnum)
    head(masterdata)
  }
  head(masterdata)
  allrelnum<-masterdata
  allrelnum$SumEgg = allrelnum$EggGen1 + allrelnum$EggGen2 + allrelnum$EggGen3 + allrelnum$EggGen4
  allrelnum$SumLarva = allrelnum$LarvaGen1 + allrelnum$LarvaGen2 + allrelnum$LarvaGen3 + allrelnum$LarvaGen4
  allrelnum$SumPupa = allrelnum$PupaGen1 + allrelnum$PupaGen2 + allrelnum$PupaGen3 + allrelnum$PupaGen4
  allrelnum$SumAdult = allrelnum$AdultGen1 + allrelnum$AdultGen2 + allrelnum$AdultGen3 + allrelnum$AdultGen4
  head(allrelnum)
  #allrelnum<-allrelnum[, c("SumEgg", "SumLarva","SumPupa","SumAdult","dayofyear","year","month")]
  allrelnum<-allrelnum[, c(#"EggGen1", "EggGen2", "EggGen3", "EggGen4", 
                           "LarvaGen1", "LarvaGen2", "LarvaGen3", "LarvaGen4", 
                           #"PupaGen1", "PupaGen2", "PupaGen3", "PupaGen4",
                           "AdultGen1", "AdultGen2", "AdultGen3", "AdultGen4", 
                           "SumEgg", "SumLarva","SumPupa","SumAdult","dayofyear","year","month")]
  return(allrelnum)
}

#  function to append gdd and cumulative gdd columns to met data
# add_dd_cumudd <- function(metdata_data.table, lower, upper) {
#   # temporary variables 
#   twice_pi = 2*pi
#   half_pi = pi/2
#   twice_lower = 2* lower   #fk1
#   gdd_temp1 = 0
#   twice_upper = 2* upper #fk1b
#   diffmaxmin = metdata_data.table$tmax - metdata_data.table$tmin  # column diff
#   summaxmin = metdata_data.table$tmax + metdata_data.table$tmin # column  tsum
#   twice_lower_minus_summaxmin = twice_lower - summaxmin   # d2
#   twice_upper_minus_summaxmin <- twice_upper - summaxmin # d2b
#   theta = atan(twice_lower_minus_summaxmin/sqrt(diffmaxmin^2 -twice_lower_minus_summaxmin^2))
#   theta2 = atan(twice_upper_minus_summaxmin/sqrt(diffmaxmin^2 -twice_upper_minus_summaxmin^2))
#   tmin<-metdata_data.table$tmin
#   tmax<-metdata_data.table$tmax
#   tempdata<-data.frame(tmin, tmax, diffmaxmin,summaxmin, twice_lower_minus_summaxmin, theta, twice_upper_minus_summaxmin, theta2)
#   tempdata[is.na(tempdata)]<--9999  
#   tempdata$theta[tempdata$theta> 0 & tempdata$twice_lower_minus_summaxmin < 0] <- tempdata$theta[tempdata$theta> 0 & tempdata$twice_lower_minus_summaxmin < 0] - 2*half_pi
#   tempdata$theta2[tempdata$theta2> 0 & tempdata$twice_upper_minus_summaxmin < 0] <- tempdata$theta2[tempdata$theta2> 0 & tempdata$twice_upper_minus_summaxmin < 0]- 2*half_pi
#   tempdata$heat<-0
#   tempdata$heat<-(tempdata$diffmaxmin*cos(tempdata$theta)- tempdata$twice_lower_minus_summaxmin*(half_pi -tempdata$theta ))/ twice_pi
#   tempdata$heat[tempdata$tmin >= lower] <-(tempdata$summaxmin[tempdata$tmin >= lower] - twice_lower)/2
#   tempdata$heat2 <- tempdata$heat
#   tempdata$heat2<-(tempdata$diffmaxmin*cos(tempdata$theta2)- tempdata$twice_upper_minus_summaxmin*(half_pi -tempdata$theta2 ))/ twice_pi
#   tempdata$heat2 = tempdata$heat - tempdata$heat2
#   tempdata$dd <- -9999
#   tempdata$dd[tempdata$tmin > tempdata$tmax ] <- 0
#   tempdata$dd[tempdata$tmin >= upper ] <- upper-lower
#   tempdata$dd[tempdata$tmax <= lower ] <- 0
#   tempdata$dd[tempdata$tmin >= lower ] <- tempdata$heat[tempdata$tmin >= lower ]
#   tempdata$dd[tempdata$tmax > upper ] <- tempdata$heat2[tempdata$tmax > upper ]
#   tempdata$dd[tempdata$dd == -9999 ] <-tempdata$heat[tempdata$dd == -9999 ]
#   head(tempdata)
#   metdata_data.table$dd <-tempdata$dd
#   metdata_data.table[, Cum_dd := cumsum(dd), by=list(year)]
#   head(metdata_data.table)
#   return(metdata_data.table)
# }

#  function to append gdd and cumulative gdd columns to met data
add_dd_cumudd <- function(metdata_data.table, lower, upper) {
  twopi = 2 * pi
  pihalf = pi / 2
  diff = metdata_data.table$tmax - metdata_data.table$tmin # column diff
  tsum = metdata_data.table$tmax + metdata_data.table$tmin # column  tsum
  aveminlt = (tsum/2) - lower
  alpha1 = diff / 2
  avetemp = tsum / 2
  theta1 = asin((lower - avetemp) / alpha1)
  theta2 = asin((upper - avetemp) / alpha1)
  tmin = metdata_data.table$tmin
  tmax = metdata_data.table$tmax
  
  tempdata = data.frame(tmin, tmax, diff, tsum, aveminlt, alpha1, avetemp, theta1, theta2)
  tempdata$heat = 0
  #case 1 tmin >= upper threshold
  tempdata$heat[tempdata$tmin >= upper] = upper - lower
  
  #case 2 is handled by the default where heat is set to 0 initially it is when tmax<lower threshold
  
  #case 3 tmin>= lower threshold and the max <= upper threshold
  tempdata$heat[tempdata$tmin >= lower & tempdata$tmax <= upper] = tempdata$aveminlt[tempdata$tmin >= lower & tempdata$tmax <= upper]
  
  #case 4 tmin<lower threshold and tmax>lower and tmax<=upper threshold
  tempdata$heat[tempdata$tmin < lower & tempdata$tmax > lower & tempdata$tmax <= upper] = (((tempdata$aveminlt[tempdata$tmin < lower & tempdata$tmax > lower & tempdata$tmax <= upper] * (pihalf - tempdata$theta1[tempdata$tmin < lower & tempdata$tmax > lower & tempdata$tmax <= upper])) + (tempdata$alpha1[tempdata$tmin < lower & tempdata$tmax > lower & tempdata$tmax <= upper] * cos(tempdata$theta1[tempdata$tmin < lower & tempdata$tmax > lower & tempdata$tmax <= upper]))) / pi)#[tempdata$tmin < lower & tempdata$tmax > lower & tempdata$tmax <= upper]
  
  #case 5 tmin>=lower threshold & tmin<upper & tmax>upper threshold
  tempdata$heat[tempdata$tmin >= lower & tempdata$tmin < upper & tempdata$tmax > upper] = (((tempdata$aveminlt[tempdata$tmin >= lower & tempdata$tmin < upper & tempdata$tmax > upper] * (tempdata$theta2[tempdata$tmin >= lower & tempdata$tmin < upper & tempdata$tmax > upper] + pihalf)) + ((upper - lower) * (pihalf - tempdata$theta2[tempdata$tmin >= lower & tempdata$tmin < upper & tempdata$tmax > upper])) - (tempdata$alpha1[tempdata$tmin >= lower & tempdata$tmin < upper & tempdata$tmax > upper] * cos(tempdata$theta2[tempdata$tmin >= lower & tempdata$tmin < upper & tempdata$tmax > upper]))) / pi) #[tempdata$tmin >= lower & tempdata$tmin < upper & tempdata$tmax > upper]
 
  #case 6 tmin<lower threshold & tmax>upper threshold
  tempdata$heat[tempdata$tmin < lower & tempdata$tmax > upper] = (((tempdata$aveminlt[tempdata$tmin < lower & tempdata$tmax > upper] * (tempdata$theta2[tempdata$tmin < lower & tempdata$tmax > upper] - tempdata$theta1[tempdata$tmin < lower & tempdata$tmax > upper])) + (tempdata$alpha1[tempdata$tmin < lower & tempdata$tmax > upper] * (cos(tempdata$theta1[tempdata$tmin < lower & tempdata$tmax > upper]) - cos(tempdata$theta2[tempdata$tmin < lower & tempdata$tmax > upper]))) + ((upper - lower) * (pihalf - tempdata$theta2[tempdata$tmin < lower & tempdata$tmax > upper]))) / pi) #[tempdata$tmin < lower & tempdata$tmax > upper]
  
  metdata_data.table$dd <-tempdata$heat
  metdata_data.table[, Cum_dd := cumsum(dd), by=list(year)]
  head(metdata_data.table)
  return(metdata_data.table)
}

# function to create year month day dataframe to append to metadata dataframe
create_ymdvalues <- function(nYears, Years, leap.year) {
  daycount_in_year <- 0
  moncount_in_year <- 0
  yearrep_in_year <- 0
  for(i in 1:nYears){
    ly <- leap.year(Years[i])
    
    if(ly == TRUE){
      days_in_mon <- c(31,29,31,30,31,30,31,31,30,31,30,31)
    }
    
    else{
      days_in_mon <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    }
    for( j in 1:12){
      daycount_in_year <- c(daycount_in_year,seq(1,days_in_mon[j]))
      moncount_in_year <- c(moncount_in_year,rep(j,days_in_mon[j]))
      yearrep_in_year. <- c(yearrep_in_year,rep(Years[i],days_in_mon[j]))
    }
  }
  head(daycount_in_year)
  daycount_in_year <- daycount_in_year[-1] #delete the leading 0
  moncount_in_year <- moncount_in_year[-1]
  yearrep_in_year  <- yearrep_in_year[-1]
  ymd <- cbind(yearrep_in_year,moncount_in_year,daycount_in_year)
  head(ymd)
  colnames(ymd) <- c("year","month","day")
  return(ymd)
}

# function to read binary data
readbinarydata_addmdy <- function(filename, Nrecords, Nofvariables, ymd, ind) {
  fileCon = file(filename, "rb")
  temp<-readBin(fileCon, integer(), size =2, n = Nrecords * Nofvariables, endian="little")
  dataM<-matrix(0, Nrecords, 4)
  k<-1
  dataM[1:Nrecords,1]<-temp[ind]/40.00  #precip data
  dataM[1:Nrecords,2]<-temp[ind+1]/100.00  #Max temperature data
  dataM[1:Nrecords,3]<-temp[ind+2]/100.00  #Min temperature data
  dataM[1:Nrecords,4]<-temp[ind+3]/100.00  #Wind speed data
  AllData<-cbind( ymd,dataM)
  head(AllData)
  # calculate daily GDD
  head(AllData)
  colnames(AllData)<-c("year","month","day","precip","tmax","tmin","winspeed")
  close(fileCon)
  return(AllData)
}
