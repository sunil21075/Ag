

genPcts = c("LGen3_0.25", "LGen3_0.75")
freq_data = subset(data, select = c(year, ClimateScenario, location, LGen3_0.25))

freq_data_melted = melt(freq_data, id.vars = c("location", "year", "ClimateScenario"), na.rm = FALSE)
f1 = freq_data_melted[, .(years_range = (max(year) - min(year) + 1)), by = list(location, year, ClimateScenario)][order(location)]
f2 = freq_data_melted[complete.cases(freq_data_melted$value), .(years_freq = uniqueN(year)), by = list(location, ClimateScenario)][order(location)]

f1g = freq_data_melted[, .(years_range = (max(year) - min(year) + 1)), by = list(location)][order(location)]
f2g = freq_data_melted[complete.cases(freq_data_melted$value), .(years_freq = uniqueN(year)), by = list(location)][order(location)]

# left join - merge both tables
f = merge(f1, f2, by = c("location"), all.x = TRUE)
# replace na values by 0
f[is.na(years_freq), years_freq := 0]
f$percentage = (f$years_freq / f$years_range) * 100
    
    
###################################################### PEST Risk
output$map_risk <- renderLeaflet({
    type_risk = "L"
    genPct = paste0(type_risk, input$gen_risk, "_", input$percent_risk)
    if(input$cg_risk == "Historical") {
      climate_group = input$cg_risk
      future_version = "rcp85"
    }
    else {
      temp = tstrsplit(input$cg_risk, "_")
      climate_group = unlist(temp[1])
      future_version = unlist(temp[2])
    }

    if(future_version == "rcp45") { data = d_rcp45
    } else { data = d }

    layerlist = levels(data$timeFrame) #c("Historical", "2040's", "2060's", "2080's")
    
    freq_data = subset(data, !is.na(timeFrame) & timeFrame == climate_group, select = c(timeFrame, year, location, get(genPct)))
    freq_data_melted = melt(freq_data, id.vars = c("timeFrame", "location", "year"), na.rm = FALSE)
    f1 = freq_data_melted[, .(years_range = (max(year) - min(year) + 1)), by = list(timeFrame, location)][order(timeFrame, location)]
    f2 = freq_data_melted[complete.cases(freq_data_melted$value), .(years_freq = uniqueN(year)), by = list(timeFrame, location)][order(timeFrame, location)]
    # left join - merge both tables
    f = merge(f1, f2, by = c("timeFrame", "location"), all.x = TRUE)
    # replace na values by 0
    f[is.na(years_freq), years_freq := 0]
    f$percentage = (f$years_freq / f$years_range) * 100
    
    riskGen = list(hist   = subset(f, timeFrame == layerlist[1]),
                   `2040` = subset(f, timeFrame == layerlist[2]),
                   `2060` = subset(f, timeFrame == layerlist[3]),
                   `2080` = subset(f, timeFrame == layerlist[4]))

########################################################################
############
############ Diapause, we need escaped fraction for all gens.
############
########################################################################
generate_rel_escap_diap <- function(data){
  

  for (diap_gen in 1:4){
  col = paste0("RelPctNonDiapGen", diap_gen)
  sub_Diap = subset(data, select = c(location, year, ClimateScenario, get(col)))
  }
}



output$map_diap_pop <- renderLeaflet({
    #$input$diap_pop = "RelPct"
    # input$diapaused = escaped for our use

    col = paste0("RelPct", input$diapaused, if(input$diap_gen == "all") "" else input$diap_gen)

    if(input$cg_diap == "Historical") {
      climate_group = input$cg_diap
      future_version = "rcp85"
    } else {
      temp = tstrsplit(input$cg_diap, "_")
      climate_group = unlist(temp[1])
      future_version = unlist(temp[2])
    }

    if(future_version == "rcp45") { diap_d = diap_rcp45
    } else { diap_d = diap
    }

    layerlist = levels(diap_d$ClimateGroup) # Historical, 2040, 2060, 2080

    sub_Diap = subset(diap_d, ClimateGroup == climate_group, select = c(ClimateGroup, CountyGroup, latitude, longitude, get(col)))
    sub_Diap$location = paste0(sub_Diap$latitude, "_", sub_Diap$longitude)
    
    meanDiap = list( hist   = subset(sub_Diap, ClimateGroup == layerlist[1]),
                     `2040` = subset(sub_Diap, ClimateGroup == layerlist[2]),
                     `2060` = subset(sub_Diap, ClimateGroup == layerlist[3]),
                     `2080` = subset(sub_Diap, ClimateGroup == layerlist[4]))
############################################################
############################################################
############################################################
##################
################## It seems the following has to come from CMPOP 
##################


##########
########## GDD accumulation
##########


