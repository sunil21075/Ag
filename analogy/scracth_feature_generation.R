########################################################################
############
############ Diapause, we need escaped fraction for all gens.
############
########################################################################
generate_rel_escap_diap <- function(dt){

  for (diap_gen in 1:4){
  col = paste0("RelPctNonDiapGen", diap_gen)
  sub_Diap = subset(dt, select = c(location, year, ClimateScenario, get(col)))
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


