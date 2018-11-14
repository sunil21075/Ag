CodlingMothPercentPopulation <- function(CodMothParams, metdata_data.table) {
  stage_gen_toiterate <- length(CodMothParams[, 1])
  # store relative numbers
  masterdata <- data.frame(metdata_data.table$dayofyear, 
                           metdata_data.table$year, 
                           metdata_data.table$month, 
                           metdata_data.table$Cum_dd_F)
  colnames(masterdata) <- c("dayofyear","year","month","Cum_dd_F")
  
  for (i in 1:stage_gen_toiterate) {
    relnum <- pweibull(metdata_data.table$Cum_dd_F, shape=CodMothParams[i,3], scale=CodMothParams[i,4]) 
    relnum <- data.frame(relnum)
    colnames(relnum) <- paste("Perc", CodMothParams[i,1], "Gen", CodMothParams[i,2], sep="")
    masterdata <- cbind(masterdata, relnum)
  }
  
  all_perc_num <- masterdata
  rm (masterdata)
  all_perc_num$PercEgg = 0
  all_perc_num$PercLarva = 0
  all_perc_num$PercPupa = 0
  all_perc_num$PercAdult = 0
  i = 1

  for (i in 1:4) {
    columnname <- paste("Perc", CodMothParams[i, 1], "Gen", CodMothParams[i,2], sep="")
    columnnumber <- which( colnames(all_perc_num)==columnname )
    all_perc_num$PercEgg[all_perc_num$Cum_dd_F > CodMothParams [i,5] & 
                      all_perc_num$Cum_dd_F <= CodMothParams [i,6]] <- all_perc_num[all_perc_num$Cum_dd_F > 
                                                                     CodMothParams [i,5] & all_perc_num$Cum_dd_F <= 
                                                                     CodMothParams [i,6], columnnumber]
  }
  for (i in 5:8) {
    columnname <- paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber <- which( colnames(all_perc_num)==columnname )
    all_perc_num$PercLarva[all_perc_num$Cum_dd_F > CodMothParams [i,5] & 
                                                all_perc_num$Cum_dd_F <= CodMothParams [i,6]] <- all_perc_num[all_perc_num$Cum_dd_F > 
                                                CodMothParams [i,5] & all_perc_num$Cum_dd_F <= CodMothParams [i,6], columnnumber]
  }  
  for (i in 9:12) {
    columnname <- paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber <- which( colnames(all_perc_num)==columnname )
    all_perc_num$PercPupa[all_perc_num$Cum_dd_F > CodMothParams [i,5] & all_perc_num$Cum_dd_F <= 
                                         CodMothParams [i,6]] <- all_perc_num[all_perc_num$Cum_dd_F > 
                                        CodMothParams [i,5] & all_perc_num$Cum_dd_F <= CodMothParams [i,6], columnnumber]
  } 
  for (i in 13:16) {
    columnname<-paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber<-which( colnames(all_perc_num)==columnname )
    all_perc_num$PercAdult[all_perc_num$Cum_dd_F > CodMothParams [i,5] & 
                             all_perc_num$Cum_dd_F <= CodMothParams [i,6]] <- all_perc_num[all_perc_num$Cum_dd_F > 
                             CodMothParams [i,5] & all_perc_num$Cum_dd_F <= CodMothParams [i,6], columnnumber]
  } 

  all_perc_num[, "PercAdult"]
  all_perc_num <- all_perc_num[, c("PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4",
                             "PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4",
                             "PercEgg", "PercLarva","PercPupa","PercAdult",
                             "dayofyear","year","month")]
  return(all_perc_num)
}




#########################################################################################################
#########################################################################################################
########################################          ORIGINAL
#########################################################################################################
#########################################################################################################
CodlingMothPercentPopulation <- function(CodMothParams, metdata_data.table) {
  stage_gen_toiterate <- length(CodMothParams[, 1])
  # store relative numbers
  masterdata <- data.frame(metdata_data.table$dayofyear, 
                           metdata_data.table$year, 
                           metdata_data.table$month, 
                           metdata_data.table$Cum_dd_F)
  colnames(masterdata) <- c("dayofyear","year","month","Cum_dd_F")
  
  for (i in 1:stage_gen_toiterate) {
    relnum <- pweibull(metdata_data.table$Cum_dd_F, shape=CodMothParams[i,3], scale=CodMothParams[i,4]) 
    relnum <- data.frame(relnum)
    colnames(relnum) <- paste("Perc", CodMothParams[i,1], "Gen", CodMothParams[i,2], sep="")
    masterdata <- cbind(masterdata, relnum)
  }
  
  allrelnum <- masterdata
  rm (masterdata)
  allrelnum$PercEgg = 0
  allrelnum$PercLarva = 0
  allrelnum$PercPupa = 0
  allrelnum$PercAdult = 0
  i = 1

  for (i in 1:4) {
    columnname <- paste("Perc", CodMothParams[i, 1], "Gen", CodMothParams[i,2], sep="")
    columnnumber <- which( colnames(allrelnum)==columnname )
    allrelnum$PercEgg[allrelnum$Cum_dd_F > CodMothParams [i,5] & 
                      allrelnum$Cum_dd_F <= CodMothParams [i,6]] <- allrelnum[allrelnum$Cum_dd_F > 
                                                                     CodMothParams [i,5] & allrelnum$Cum_dd_F <= 
                                                                     CodMothParams [i,6], columnnumber]
  }
  for (i in 5:8) {
    columnname <- paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber <- which( colnames(allrelnum)==columnname )
    allrelnum$PercLarva[allrelnum$Cum_dd_F > CodMothParams [i,5] & 
                                                allrelnum$Cum_dd_F <= CodMothParams [i,6]] <- allrelnum[allrelnum$Cum_dd_F > 
                                                CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6], columnnumber]
  }  
  for (i in 9:12) {
    columnname<-paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber<-which( colnames(allrelnum)==columnname )
    allrelnum$PercPupa[allrelnum$Cum_dd_F > CodMothParams [i,5] & allrelnum$Cum_dd_F <= 
                                         CodMothParams [i,6]] <- allrelnum[allrelnum$Cum_dd_F > 
                                        CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6], columnnumber]
  } 
  for (i in 13:16) {
    columnname<-paste("Perc",CodMothParams[i,1], "Gen",CodMothParams[i,2],sep="")
    columnnumber<-which( colnames(allrelnum)==columnname )
    allrelnum$PercAdult[allrelnum$Cum_dd_F > CodMothParams [i,5] & 
                             allrelnum$Cum_dd_F <= CodMothParams [i,6]] <- allrelnum[allrelnum$Cum_dd_F > 
                             CodMothParams [i,5] & allrelnum$Cum_dd_F <= CodMothParams [i,6], columnnumber]
  } 

  allrelnum[, "PercAdult"]
  allrelnum <- allrelnum[, c("PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4",
                             "PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4",
                             "PercEgg", "PercLarva","PercPupa","PercAdult",
                             "dayofyear","year","month")]
  return(allrelnum)
}