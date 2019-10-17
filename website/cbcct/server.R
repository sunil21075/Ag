# Create the server logic

shinyServer(function(input, output, session) {
  ## Interactive Map ## 
  
  map = leaflet(data = cbStates) %>%
    addPolygons(color="black", weight=3, stroke = TRUE)%>%
    addTiles(
      urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) 
  output$map <- renderLeaflet(map)

  pal = colorBin(palette = "Reds", domain = as.numeric(crb_locations$num_crops), bins = 8, pretty=FALSE)
  leafletProxy("map") %>%
    addLegend("topleft", pal = pal, values = NULL, title = "Number of Crops") 
  
  ## List of crops based on location ##
  #output$selected_crops <- select_crops()

###################################################################################################
#Plots
###################################################################################################
  output$image2040 <- renderImage({
    loc <- select_location()
    image_src <- paste0(paste(paste("data/mahalanobis", 
                                    future_model, 
                                    future_rcp, 
                                    "2040", 
                                    mahalanobis_vars, 
                                    "data", sep="/"),
                              loc$latitude, loc$longitude, sep="_"), ".png")
    
    list(src = image_src,
         contentType = 'image/png',
         width = 400,
         height = 400,
         alt = "image of climate similarity in 2040.")
  }, deleteFile = FALSE)
  
  output$image2060 <- renderImage({
    loc <- select_location()
    image_src <- paste0(paste(paste("data/mahalanobis", 
                                    future_model, 
                                    future_rcp, 
                                    "2060", 
                                    mahalanobis_vars, 
                                    "data", sep="/"),
                              loc$latitude, loc$longitude, sep="_"), ".png")
    
    list(src = image_src,
         contentType = 'image/png',
         width = 400,
         height = 400,
         alt = "image of climate similarity in 2060.")
  }, deleteFile = FALSE)
  
  output$image2080 <- renderImage({
    loc <- select_location()
    image_src <- paste0(paste(paste("data/mahalanobis", 
                                    future_model, 
                                    future_rcp, 
                                    "2080", 
                                    mahalanobis_vars, 
                                    "data", sep="/"),
                              loc$latitude, loc$longitude, sep="_"), ".png")
    
    list(src = image_src,
         contentType = 'image/png',
         width = 400,
         height = 400,
         alt = "image of climate similarity in 2080.")
  }, deleteFile = FALSE)
  
  
  
  output$gdd_forecast_plotly <- renderPlotly({
    #dependent on when "gddButton" is pressed
    
    #UI data isolated so that we don't replot until gddButton is clicked.
    planting_date <- isolate(input$planting_date)
    
    crop = subset_crop()
    
    #Data
    hd <- summarize_historical_over_day()
    hy <- summarize_historical_over_year()
    pd <- summarize_present_over_day()
    
    #Stage Dates
    stage_dates <- hd[, c("monthday", "first_emergence_day", "first_maturity_day"), with=FALSE]
    stage_dates[, date := as.Date(paste0("2016-", monthday))]
    
    #Calculate GEFS and CFS GDD
    cgdd_yesterday <- pd[date==(today-1), cgdd]
    gefs <- select_gefs()
    gefs[, gdd := calc_gdd(temp, 
                           temp_base=crop$temp.base, 
                           temp_max=crop$temp.max, 
                           temp_min=crop$temp.min), by=list(ens, date)]
    gefs[date < planting_date, gdd := 0]
    gefs[, cgdd := cumsum(gdd) + cgdd_yesterday, by=list(ens, year(date))]
    gefs_final <- gefs[, .(cgdd_mean = mean(cgdd, na.rm=T),
                           cgdd_min = min(cgdd, na.rm=T),
                           cgdd_max = max(cgdd, na.rm=T)), 
                       by=list(date)]
    
    #NMME data
    nmme1 <- select_nmme()
    nmme2 <- merge(nmme1, hy[, c("monthday", "temp_mean"), with=FALSE], by="monthday")
    nmme2[, temp := temp + temp_mean]
    nmme2[, gdd := calc_gdd(temp, 
                            temp_base=crop$temp.base, 
                            temp_max=crop$temp.max, 
                            temp_min=crop$temp.min), by=list(ens, date)]
    nmme2[date < planting_date, gdd := 0]
    nmme2[!is.na(gdd) & year(date) == year(today), cgdd := cgdd_yesterday + cumsum(gdd), by=list(ens, year(date))]
    nmme_final <- nmme2[, .(cgdd_mean = mean(cgdd, na.rm=T),
                            cgdd_min = min(cgdd, na.rm=T),
                            cgdd_max = max(cgdd, na.rm=T)
    ), by=list(date)]
    setkey(nmme_final, date)
    
    #Plots for the GDD/Precip Page
    gddly <- plot_gdd_forecast(crop, 
                               hy[year(date) == this_year], 
                               pd[year(date) == this_year], 
                               gefs_final[year(date) == this_year], 
                               nmme_final[year(date) == this_year],
                               stage_dates,
                               colors = color_scheme,
                               engine="plotly")
    
    gddly
  })
  
  output$gdd_outlook_plotly <- renderPlotly({
    
    #UI data isolated so that we don't replot until gddButton is clicked.
    planting_date <- isolate(input$planting_date)
    
    crop <- subset_crop()
    #Data
    hd <- summarize_historical_over_day()
    hy <- summarize_historical_over_year()
    
    fd <- summarize_future_over_day()
    
    fy2040 <- summarizeOverYear(fd[year > 2025 & year <= 2055]) #collapse across years so 365 records
    fy2040$date <- as.Date(paste0("2016-", fy2040$monthday))
    setkey(fy2040, date)
    
    fy2060 <- summarizeOverYear(fd[year > 2045 & year <= 2075]) #collapse across years so 365 records
    fy2060$date <- as.Date(paste0("2016-", fy2060$monthday))
    setkey(fy2060, date)
    
    fy2080 <- summarizeOverYear(fd[year > 2065 & year <= 2095]) #collapse across years so 365 records
    fy2080$date <- as.Date(paste0("2016-", fy2080$monthday))
    setkey(fy2080, date)
    
    #Stage Dates
    stage_dates <- rbind(
      hd[, c("year", "monthday", "first_emergence_day", "first_maturity_day"), with=FALSE],
      fd[, c("year", "monthday", "first_emergence_day", "first_maturity_day"), with=FALSE]
    )
    stage_dates[, date := as.Date(paste0(this_year, "-", monthday))]
    setkey(stage_dates, date)
    
    #Plots for the GDD/Precip Page
    gddly <- plot_gdd_outlook(crop, 
                     hy[year(date) == this_year], 
                     fy2040[year(date) == this_year], 
                     fy2060[year(date) == this_year], 
                     fy2080[year(date) == this_year], 
                     stage_dates = stage_dates,
                     colors = color_scheme,
                     engine="plotly")
    
    gddly
  })
  
  output$gdd_similarity_plotly <- renderPlotly({
    #Isolated UI inputs
    crop <- isolate(subset_crop())
    nass <- subset_nass_yields()
    
    ## Historical data
    hd <- summarize_historical_over_day()
    hy <- summarize_historical_over_year() 
    hm <- hd[, .(cgdd = sum(gdd)), by=list(month(date), year(date))]
    
    ## Present Data plus NMME 
    pd <- summarize_present_over_day()
    
    nmme1 <- select_nmme()
    nmme2 <- merge(nmme1, hy[, c("monthday", "temp_mean"), with=FALSE], by="monthday")
    nmme2[, temp := temp + temp_mean]
    nmme2[, gdd := calc_gdd(temp), by=list(date, ens)]
    nmme_final <- nmme2[date >= today, .(gdd = mean(gdd, na.rm=T)), 
                        by=list(date)]
    setkey(nmme_final, date)
    
    
    #add the NMME and present
    cols <- c("date", "gdd")
    pres <- rbind(pd[, cols, with=FALSE], nmme_final[, cols, with=FALSE])
    
    pm <- pres[, .(cgdd = sum(gdd)),
               by=list(month(date), year(date))][year == this_year]
    
    ## Combine and calculate distance
    dat <- calc_time_dissimilarity(hm, pm, "cgdd", this_year = this_year)
    
    cgdd_yld <- na.omit(merge(dat, nass, all.x = TRUE)) #[year<2016]
    cgdd_yld[, `:=`(dis_rank = rank(dis), maha_rank = rank(maha))]
    
    p_plotly <- plot_time_dissimilarity(cgdd_yld, gdd_colors, 
                                        title_str = "GDD", legend_str = "Dissimilarity",
                                        xlab_str="", ylab_str="Yield", detrend = FALSE,
                                        engine = "plotly")
    p_plotly
  })
  
  output$precip_similarity_plotly <- renderPlotly({
    input$gddButton
    
    #Isolated UI inputs
    crop <- isolate(subset_crop())
    nass <- subset_nass_yields()
    
    ## Historical data
    hd <- summarize_historical_over_day()
    hy <- summarize_historical_over_year() 
    hm <- hd[, .(cprecip = sum(precipitation)), by=list(month(date), year(date))]
    
    ## Present Data plus NMME 
    pd <- summarize_present_over_day()
    
    nmme1 <- select_nmme()
    nmme2 <- merge(nmme1, hy[, c("monthday", "precip_mean"), with=FALSE], by="monthday")
    nmme2[, precip := precip + precip_mean]
    nmme_final <- nmme2[date >= today, .(precipitation = mean(precip, na.rm=T)), 
                        by=list(date)]
    setkey(nmme_final, date)
    
    
    #add the NMME and present
    cols <- c("date", "precipitation")
    pres <- rbind(pd[, cols, with=FALSE], nmme_final[, cols, with=FALSE])
    
    pm <- pres[, .(cprecip = sum(precipitation)),
               by=list(month(date), year(date))][year == this_year]
    
    ## Combine and calculate distance
    dat <- calc_time_dissimilarity(hm, pm, "cprecip", this_year = this_year)
    
    cprecip_yld <- na.omit(merge(dat, nass, all.x = TRUE)) #[year<2016]
    cprecip_yld[, `:=`(dis_rank = rank(dis), maha_rank = rank(maha))]
    
    p_plotly <- plot_time_dissimilarity(cprecip_yld, precip_colors, 
                                        title_str = "Precipitation", legend_str = "Dissimilarity",
                                        xlab_str="", ylab_str="Yield", detrend = FALSE,
                                        engine = "plotly")
    p_plotly
  })

  output$gdd_similarity_detrended_plotly <- renderPlotly({
    input$gddButton
    
    #Isolated UI inputs
    crop <- isolate(subset_crop())
    nass <- subset_nass_yields()
    
    ## Historical data
    hd <- summarize_historical_over_day()
    hy <- summarize_historical_over_year() 
    hm <- hd[, .(cgdd = sum(gdd)), by=list(month(date), year(date))]
    
    ## Present Data plus NMME 
    pd <- summarize_present_over_day()
    
    nmme1 <- select_nmme()
    nmme2 <- merge(nmme1, hy[, c("monthday", "temp_mean"), with=FALSE], by="monthday")
    nmme2[, temp := temp + temp_mean]
    nmme2[, gdd := calc_gdd(temp), by=list(date, ens)]
    nmme_final <- nmme2[date >= today, .(gdd = mean(gdd, na.rm=T)), 
                        by=list(date)]
    setkey(nmme_final, date)
    
    
    #add the NMME and present
    cols <- c("date", "gdd")
    pres <- rbind(pd[, cols, with=FALSE], nmme_final[, cols, with=FALSE])
    
    pm <- pres[, .(cgdd = sum(gdd)),
               by=list(month(date), year(date))][year == this_year]
    
    ## Combine and calculate distance
    dat <- calc_time_dissimilarity(hm, pm, "cgdd", this_year = this_year)
    
    cgdd_yld <- na.omit(merge(dat, nass, all.x = TRUE)) #[year<2016]
    cgdd_yld[, `:=`(dis_rank = rank(dis), maha_rank = rank(maha))]
    
    p_plotly <- plot_time_dissimilarity(cgdd_yld, gdd_colors, 
                                        title_str = "GDD",
                                        xlab_str="", ylab_str="Yield (Difference from Trend)", 
                                        engine = "plotly")
    p_plotly
  })
  
  output$precip_similarity_detrended_plotly <- renderPlotly({
    input$gddButton
    
    #Isolated UI inputs
    crop <- isolate(subset_crop())
    nass <- subset_nass_yields()
    
    ## Historical data
    hd <- summarize_historical_over_day()
    hy <- summarize_historical_over_year() 
    hm <- hd[, .(cprecip = sum(precipitation)), by=list(month(date), year(date))]
    
    ## Present Data plus NMME 
    pd <- summarize_present_over_day()
    
    nmme1 <- select_nmme()
    nmme2 <- merge(nmme1, hy[, c("monthday", "precip_mean"), with=FALSE], by="monthday")
    nmme2[, precip := precip + precip_mean]
    nmme_final <- nmme2[date >= today, .(precipitation = mean(precip, na.rm=T)), 
                        by=list(date)]
    setkey(nmme_final, date)
    
    
    #add the NMME and present
    cols <- c("date", "precipitation")
    pres <- rbind(pd[, cols, with=FALSE], nmme_final[, cols, with=FALSE])
    
    pm <- pres[, .(cprecip = sum(precipitation)),
               by=list(month(date), year(date))][year == this_year]
    
    ## Combine and calculate distance
    dat <- calc_time_dissimilarity(hm, pm, "cprecip", this_year = this_year)
    
    cprecip_yld <- na.omit(merge(dat, nass, all.x = TRUE)) #[year<2016]
    cprecip_yld[, `:=`(dis_rank = rank(dis), maha_rank = rank(maha))]
    
    p_plotly <- plot_time_dissimilarity(cprecip_yld, precip_colors, 
                                        title_str = "Precipitation",
                                        xlab_str="", ylab_str="Yield (Difference from Trend)", 
                                        engine = "plotly")
    p_plotly
  })
  
  ########### SPATIAL DISTANCE PLOT ###########
  output$spatialDissimilarityPlot <- renderPlot({
    
    #Progress bar
    #     withProgress(message = 'Creating Analogue Maps...',
    #                  detail = 'Please hold...', value = 0, {
    #                    for (i in 1:15) {
    #                      incProgress(1/15)
    #                      Sys.sleep(0.25)
    #                    }
    #                  })
    #     
    loc <- select_location()
    
    #create data.table
    fut <- spatial_future()
    cnames <- c(names(fut), "lat", "lon")
    fut <- rbind(colMeans(fut[year >= (2035 - 10) & year <= (2035 + 10)]),
                 colMeans(fut[year >= (2085 - 10) & year <= (2085 + 10)]))
    fut <- data.frame(cbind(fut, loc$latitude, loc$longitude))
    names(fut) <- cnames
    # spatial_historical is a static dataset loaded in global.R
    dat <- data.table(rbind(fut, spatial_historical[, cnames], fill=TRUE))
    #make 4 plots
    plts <- list()
    plts[]
    years <- c(2085, 2035)
    for(j in 1:length(years)) {
      y <- years[j]
      y2 <- years[which(years != y)]
      d2 <- dat[year!=y]
      mdat <- d2[, -c("year", "lat", "lon"), with=FALSE]
      nrows <- nrow(mdat)
      
      #center and covariance
      center <- unlist(mdat[1,])
      sx_inv <- pseudoinverse(cov(mdat))
      
      res <- data.table(data.frame(
        lat = d2$lat,
        lon = d2$lon,
        dis = dissimilarity(mdat, center, collapse=FALSE),
        maha = mahalanobis(mdat, center, sx_inv, inverted=TRUE)
      ))
      res[, `:=`(dis_rank = rank(dis), maha_rank = rank(maha))]
      setkey(res, lat, lon)
      
      d_maha <- res[maha_rank <= (as.integer(input$analogue_number) + 1)][order(maha_rank)]
      d_dis  <- res[dis_rank <= (as.integer(input$analogue_number) + 1)][order(dis_rank)]
      
      plts[[paste0("maha_", j)]] <- plot_spatial_distance(d = d_maha, title = paste("Mahalanobis", y2))
      plts[[paste0("dis_", j)]]  <- plot_spatial_distance(d = d_dis, title = paste("Dissimilarity", y2))
    }
    p <- grid.arrange(plts$maha_1, plts$maha_2, plts$dis_1, plts$dis_2,
                      top = textGrob("GDD Climate Analogues", gp = gpar(fontsize=20,font=1)),
                      ncol = 2, nrow = 2)
    return(p)
  })

  
  
  
  
  
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################
  
  ####################################################################################################################################

  output$precip_forecast_snow <- renderPlotly({
    
    hist = precip_hist() 
    snow = precip_snow() 
    rain = precip_rain() 
    gefs = precip_gefs() 
    nmme = precip_nmme()

    ht = max(c(max(hist$snow), max(hist$rain), 
                   max(snow$cum_snow), max(rain$cum_rain), 
                   max(gefs$max_snow), max(gefs$max_rain), 
                   max(nmme$max_snow), max(nmme$max_rain)))
    
    plot_precip_forecast_snow(hist = hist, snow = snow, gefs = gefs, nmme = nmme, ht = ht)
  })
  
  output$precip_forecast_rain <- renderPlotly({
    hist = precip_hist() 
    snow = precip_snow() 
    rain = precip_rain() 
    gefs = precip_gefs() 
    nmme = precip_nmme()
    
    ht = max(c(max(hist$snow), max(hist$rain), 
               max(snow$cum_snow), max(rain$cum_rain), 
               max(gefs$max_snow), max(gefs$max_rain), 
               max(nmme$max_snow), max(nmme$max_rain)))
    
    plot_precip_forecast_rain(hist = hist, rain = rain, gefs = gefs, nmme = nmme, ht = ht)
  })

  output$precip_forecast_total <- renderPlotly({

    pd <- data.frame("date" = precip_rain()$date, 
                     "cum_rain" = precip_rain()$cum_rain, 
                     "cum_snow" = precip_snow()$cum_snow)

    plot_precip_forecast_total(hist = precip_hist(),
                              pd = pd,
                              gefs = precip_gefs(),
                              nmme = precip_nmme())
  })
  
  output$precip_outlook_snow <- renderPlotly({
    hist = precip_hist()
    fut3_40 = precip_fut()[[1]]
    fut3_60 = precip_fut()[[2]]
    fut3_80 = precip_fut()[[3]]

    ht = max(c(max(hist$snow), max(hist$rain),
               max(fut3_40$snow), max(fut3_40$rain),
               max(fut3_60$snow), max(fut3_60$rain),
               max(fut3_80$snow), max(fut3_80$rain)))

    plot_precip_outlook_snow(hist = hist, 
                            fut3_40 = fut3_40,
                            fut3_60 = fut3_60,
                            fut3_80 = fut3_80,
                            ht = ht)
  })
  
  output$precip_outlook_rain <- renderPlotly({
    hist = precip_hist()
    fut3_40 = precip_fut()[[1]]
    fut3_60 = precip_fut()[[2]]
    fut3_80 = precip_fut()[[3]]
    
    ht = max(c(max(hist$snow), max(hist$rain),
               max(fut3_40$snow), max(fut3_40$rain),
               max(fut3_60$snow), max(fut3_60$rain),
               max(fut3_80$snow), max(fut3_80$rain)))
  
    plot_precip_outlook_rain(hist = hist, 
                            fut3_40 = fut3_40,
                            fut3_60 = fut3_60,
                            fut3_80 = fut3_80,
                            ht = ht)
  })
  
  output$precip_outlook_total <- renderPlotly({
    hist = precip_hist()
    fut3_40 = precip_fut()[[1]]
    fut3_60 = precip_fut()[[2]]
    fut3_80 = precip_fut()[[3]]

    plot_precip_outlook_total(hist = precip_hist(),  
                             fut3_40 = fut3_40,
                             fut3_60 = fut3_60,
                             fut3_80 = fut3_80)
  })
  
  output$precip_outlook_frequency <- renderPlotly({
    plot_precip_outlook_frequency(fut3_40 = precip_fut3()[[1]],
                                 fut3_60 = precip_fut3()[[2]],
                                 fut3_80 = precip_fut3()[[3]],
                                 hist3 = precip_hist3())
  })

  output$temp_forecast <- renderPlotly({

    d <- data.frame("last_frost" = temp_hist_frost()[[1]]$last_frost, "first_frost" = temp_hist_frost()[[2]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    hist_med_len <- median(d$length)

    
    pd = temp_pd()
    gefs = temp_gefs()
    nmme = temp_nmme()
    hist_last_frost_pct = temp_hist_frost()[[1]]
    hist_first_frost_pct = temp_hist_frost()[[2]]
    
    #left is the parameter name, right is the passed in parameter
    plot_temp_forecast(hist = temp_hist(),
                      pd = pd,
                      gefs = gefs,
                      nmme = nmme,
                      hist_last_frost_pct = hist_last_frost_pct,
                      hist_first_frost_pct = hist_first_frost_pct,
                      
                      hist_med_len = hist_med_len)
  })

  
  
  
  
  
  #add_trace(data = d, x = monthday, y = temperature, mode="markers")
  output$temp_outlook1 <- renderPlotly({
    
    d <- data.frame("last_frost" = temp_hist_frost()[[1]]$last_frost, "first_frost" = temp_hist_frost()[[2]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    hist_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[1]]$last_frost, "first_frost" = temp_fut_frost()[[4]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2040_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[2]]$last_frost, "first_frost" = temp_fut_frost()[[5]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2060_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[3]]$last_frost, "first_frost" = temp_fut_frost()[[6]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2080_med_len <- median(d$length)

    fut2040 = temp_fut()[[1]]
    fut2060 = temp_fut()[[2]]
    fut2080 = temp_fut()[[3]]
    
    hist_last_frost_pct = temp_hist_frost()[[1]]
    hist_first_frost_pct = temp_hist_frost()[[2]]
    fut_last_frost_pct2040 = temp_fut_frost()[[1]]
    fut_first_frost_pct2040 = temp_fut_frost()[[4]]
    fut_last_frost_pct2060 = temp_fut_frost()[[2]]
    fut_first_frost_pct2060 = temp_fut_frost()[[5]]
    fut_last_frost_pct2080 = temp_fut_frost()[[3]]
    fut_first_frost_pct2080 = temp_fut_frost()[[6]]

    plot_temp_outlook1(hist = temp_hist(),
                     fut2040 = fut2040,
                     fut2060 = fut2060,
                     fut2080 = fut2080,

                     hist_last_frost_pct = hist_last_frost_pct,
                     hist_first_frost_pct = hist_first_frost_pct,
                     
                     fut_last_frost_pct2040 = fut_last_frost_pct2040,
                     fut_first_frost_pct2040 = fut_first_frost_pct2040,
                     
                     fut_last_frost_pct2060 = fut_last_frost_pct2060,
                     fut_first_frost_pct2060 = fut_first_frost_pct2060,
                     
                     fut_last_frost_pct2080 = fut_last_frost_pct2080,
                     fut_first_frost_pct2080 = fut_first_frost_pct2080,
                     
                     hist_med_len, fut2040_med_len, fut2060_med_len, fut2080_med_len)
  })
  
  output$temp_outlook2 <- renderPlotly({
    
    d <- data.frame("last_frost" = temp_hist_frost()[[1]]$last_frost, "first_frost" = temp_hist_frost()[[2]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    hist_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[1]]$last_frost, "first_frost" = temp_fut_frost()[[4]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2040_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[2]]$last_frost, "first_frost" = temp_fut_frost()[[5]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2060_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[3]]$last_frost, "first_frost" = temp_fut_frost()[[6]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2080_med_len <- median(d$length)
    
    plot_temp_outlook2(hist = temp_hist(),
                          fut2040 = temp_fut()[[1]],
                          fut2060 = temp_fut()[[2]],
                          fut2080 = temp_fut()[[3]],
                      
                      
                      hist_last_frost_pct = temp_hist_frost()[[1]],
                      hist_first_frost_pct = temp_hist_frost()[[2]],
                      
                      fut_last_frost_pct2040 = temp_fut_frost()[[1]],
                      fut_first_frost_pct2040 = temp_fut_frost()[[4]],
                      
                      fut_last_frost_pct2060 = temp_fut_frost()[[2]],
                      fut_first_frost_pct2060 = temp_fut_frost()[[5]],
                      
                      fut_last_frost_pct2080 = temp_fut_frost()[[3]],
                      fut_first_frost_pct2080 = temp_fut_frost()[[6]],
                      
                      hist_med_len, fut2040_med_len, fut2060_med_len, fut2080_med_len)
  })
  
  output$temp_outlook3 <- renderPlotly({
    
    d <- data.frame("last_frost" = temp_hist_frost()[[1]]$last_frost, "first_frost" = temp_hist_frost()[[2]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    hist_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[1]]$last_frost, "first_frost" = temp_fut_frost()[[4]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2040_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[2]]$last_frost, "first_frost" = temp_fut_frost()[[5]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2060_med_len <- median(d$length)
    
    d <- data.frame("last_frost" = temp_fut_frost()[[3]]$last_frost, "first_frost" = temp_fut_frost()[[6]]$first_frost)
    d$length <- d$first_frost - d$last_frost
    fut2080_med_len <- median(d$length)
    
    plot_temp_outlook3(hist = temp_hist(),
                          fut2040 = temp_fut()[[1]],
                          fut2060 = temp_fut()[[2]],
                          fut2080 = temp_fut()[[3]],
                          
                          hist_last_frost_pct = temp_hist_frost()[[1]],
                          hist_first_frost_pct = temp_hist_frost()[[2]],
                          
                          fut_last_frost_pct2040 = temp_fut_frost()[[1]],
                          fut_first_frost_pct2040 = temp_fut_frost()[[4]],
                          
                          fut_last_frost_pct2060 = temp_fut_frost()[[2]],
                          fut_first_frost_pct2060 = temp_fut_frost()[[5]],
                          
                          fut_last_frost_pct2080 = temp_fut_frost()[[3]],
                          fut_first_frost_pct2080 = temp_fut_frost()[[6]],
                          
                          hist_med_len, fut2040_med_len, fut2060_med_len, fut2080_med_len)
  })
  1
  output$temp_heat_risk <- renderPlotly({
    hist = temp_hist_heat_risk()
    fut3_40 = temp_fut_heat_risk()[[1]]
    fut3_60 = temp_fut_heat_risk()[[2]]
    fut3_80 = temp_fut_heat_risk()[[3]]
    
    plot_temp_heat_risk(hist = hist, 
                        fut3_40 = fut3_40, 
                        fut3_60 = fut3_60, 
                        fut3_80 = fut3_80)
  })
  
  output$temp_frost_risk <- renderPlotly({
    hist = temp_hist_frost_risk()
    fut3_40 = temp_fut_frost_risk()[[1]]
    fut3_60 = temp_fut_frost_risk()[[2]]
    fut3_80 = temp_fut_frost_risk()[[3]]
    
    plot_temp_frost_risk(hist = hist, 
      fut3_40 = fut3_40, 
      fut3_60 = fut3_60, 
      fut3_80 = fut3_80)
  })

  ########### FROST HEAT DAYS PLOT ###########
  output$fhPlot <- renderPlot({
    plotFrostHeatDays(select_Historical(), input$temp.max)
  })
  
  ########### COUNTY PLOT ###########
  output$countyPlot <- renderRbokeh({
    #p <- plot_county(summarized_county())
    #return(p)
  })
  
  ########### COUNTY SUMMARY TABLE ###########
  output$countyTable <- renderDataTable({ summarized_county() })
  output$countyText <- renderPrint({ nass }) 

###################################################################################################
#Reactives
###################################################################################################

  precip_hist <- reactive({
    hist <- summarize_historical_over_day()
    hist$rain <- hist$precipitation
    hist$snow <- hist$precipitation
    hist[temperature <= 0.6]$rain = 0
    hist[0.6 < temperature &  temperature < 3.6]$rain = hist[0.6 < temperature &  temperature < 3.6]$rain * ((hist[0.6 < temperature &  temperature < 3.6]$temperature / 3) - 0.2)
    hist[3.6 <= temperature]$snow = 0
    hist[0.6 < temperature &  temperature < 3.6]$snow = hist[0.6 < temperature &  temperature < 3.6]$snow * (1 - ((hist[0.6 < temperature &  temperature < 3.6]$temperature / 3) - 0.2))
    hist <- hist[,  .(monthday = monthday,
                      date = date,
                      cum_snow = cumsum(snow), 
                      cum_rain = cumsum(rain)), by=list(year(date), month(date))]
    hist <- hist[,  .(monthday = monthday,
                      snow = max(cum_snow), 
                      rain = max(cum_rain)), by=list(year(date), month(date))]
    
    hist$date <- as.Date(paste0(this_year, paste0("-", hist$monthday)))
    hist <- rbind(hist[monthday == "01-15"], hist[monthday == "02-15"], hist[monthday == "03-15"], hist[monthday == "04-15"], hist[monthday == "05-15"],
                   hist[monthday == "06-15"], hist[monthday == "07-15"], hist[monthday == "08-15"], hist[monthday == "09-15"], hist[monthday == "10-15"], 
                   hist[monthday == "11-15"], hist[monthday == "12-15"])
    hist <- hist[order(as.Date(hist$date, format="%Y-%m-%d")),]
    hist
  })
  
  precip_hist3 <- reactive({
    precip_risk = input$change_risk_range
    hist3 <- summarize_historical_over_day()
    hist3 <- hist3[order(as.Date(hist3$date, format="%Y-%m-%d")),]
    hist3 <- hist3[monthday != "02-29"]
    
    hist3$risk_day = 0
    hist3[precip_risk < precipitation]$risk_day = 1
    hist3[precip_risk >=  precipitation]$risk_day = 0
    hist3 <- hist3[,  .(monthday = monthday,
                      year = year,
                      month = month,
                      date = date,
                      risk_count = cumsum(risk_day)), by=list(year(date), month(date))]
    hist3 <- hist3[,  .(monthday = monthday,
                      year = year,
                      month = month,
                      risk_count = max(risk_count)), by=list(year(date), month(date))]
    
    hist3$date <- as.Date(paste0(this_year, paste0("-", hist3$monthday)))
    
    hist3 <- rbind(hist3[monthday == "01-15"], hist3[monthday == "02-15"], hist3[monthday == "03-15"], hist3[monthday == "04-15"], hist3[monthday == "05-15"],
                   hist3[monthday == "06-15"], hist3[monthday == "07-15"], hist3[monthday == "08-15"], hist3[monthday == "09-15"], hist3[monthday == "10-15"], 
                   hist3[monthday == "11-15"], hist3[monthday == "12-15"])
    hist3
  })

  precip_fut <- reactive({
    fut <- summarize_future_over_day()
    fut$rain <- fut$precipitation
    fut$snow <- fut$precipitation
    fut[temperature <= 0.6]$rain = 0
    fut[0.6 < temperature &  temperature < 3.6]$rain = fut[0.6 < temperature &  temperature < 3.6]$rain * ((fut[0.6 < temperature &  temperature < 3.6]$temperature / 3) - 0.2)
    fut[3.6 <= temperature]$snow = 0
    fut[0.6 < temperature &  temperature < 3.6]$snow = fut[0.6 < temperature &  temperature < 3.6]$snow * (1 - ((fut[0.6 < temperature &  temperature < 3.6]$temperature / 3) - 0.2))
    fut <- fut[,  .(monthday = monthday,
                      date = date,
                      cum_snow = cumsum(snow), 
                      cum_rain = cumsum(rain)), by=list(year(date), month(date))]
    fut <- fut[,  .(monthday = monthday,
                      snow = max(cum_snow), 
                      rain = max(cum_rain)), by=list(year(date), month(date))]
    
    fut$date <- as.Date(paste0(this_year, paste0("-", fut$monthday)))
    fut <- rbind(fut[monthday == "01-15"], fut[monthday == "02-15"], fut[monthday == "03-15"], fut[monthday == "04-15"], fut[monthday == "05-15"],
                  fut[monthday == "06-15"], fut[monthday == "07-15"], fut[monthday == "08-15"], fut[monthday == "09-15"], fut[monthday == "10-15"], 
                  fut[monthday == "11-15"], fut[monthday == "12-15"])
    fut <- fut[order(as.Date(fut$date, format="%Y-%m-%d")),]
    
    fut3_40 <- fut[2040 - 15 < year & year <= 2040 + 15]
    fut3_60 <- fut[2060 - 15 < year & year <= 2060 + 15]
    fut3_80 <- fut[2080 - 15 < year & year <= 2080 + 15]
    
    list(fut3_40, fut3_60, fut3_80)
  })
  
  precip_fut3 <- reactive({
    precip_risk = input$change_risk_range
    fut3 <- summarize_future_over_day()
    fut3 <- fut3[order(as.Date(fut3$date, format="%Y-%m-%d")),]
    fut3 <- fut3[monthday != "02-29"]
    
    fut3$risk_day = 0
    fut3[precip_risk < precipitation]$risk_day = 1
    fut3[precip_risk >=  precipitation]$risk_day = 0
    fut3 <- fut3[,  .(monthday = monthday,
                        year = year,
                        month = month,
                        date = date,
                        risk_count = cumsum(risk_day)), by=list(year(date), month(date))]
    fut3 <- fut3[,  .(monthday = monthday,
                        year = year,
                        month = month,
                        risk_count = max(risk_count)), by=list(year(date), month(date))]
    
    fut3$date <- as.Date(paste0(this_year, paste0("-", fut3$monthday)))

    fut3 <- rbind(fut3[monthday == "01-15"], fut3[monthday == "02-15"], fut3[monthday == "03-15"], fut3[monthday == "04-15"], fut3[monthday == "05-15"],
                  fut3[monthday == "06-15"], fut3[monthday == "07-15"], fut3[monthday == "08-15"], fut3[monthday == "09-15"], fut3[monthday == "10-15"], 
                  fut3[monthday == "11-15"], fut3[monthday == "12-15"])

    fut3_40 <- fut3[2040 - 15 < year & year <= 2040 + 15]
    fut3_60 <- fut3[2060 - 15 < year & year <= 2060 + 15]
    fut3_80 <- fut3[2080 - 15 < year & year <= 2080 + 15]
    
    list(fut3_40, fut3_60, fut3_80)
  })
  
  precip_snow <- reactive({
    snow <- summarize_present_over_day()
    snow[3.6 <= temperature]$precipitation = 0
    snow[0.6 < temperature & temperature < 3.6]$precipitation = snow[0.6 < temperature & temperature < 3.6]$precipitation * (1 - ((snow[0.6 < temperature & temperature < 3.6]$temperature / 3) - .2))
    snow <- data.frame("date" = snow$date, "precip" = snow$precipitation)
    snow <- data.table(snow)
    snow <- snow[,  .(cum_snow = cumsum(precip), date = date), by=month(date)]
    snow
  })
  
  precip_rain <- reactive({
    rain <- summarize_present_over_day()
    rain[temperature <= 0.6]$precipitation = 0
    rain[0.6 < temperature & temperature < 3.6]$precipitation = rain[0.6 < temperature & temperature < 3.6]$precipitation * ((rain[0.6 < temperature & temperature < 3.6]$temperature / 3) - .2)
    rain <- data.frame("date" = rain$date, "precip" = rain$precipitation)
    rain <- data.table(rain)
    rain <- rain[,  .(cum_rain = cumsum(precip), date = date), by=month(date)]
    rain
  })
  
  precip_gefs <- reactive({
    gefs <- select_gefs()
    gefs <- gefs[is.na(precip) != TRUE]

    gefs$rain <- gefs$precip
    gefs$snow <- gefs$precip
    
    gefs[temp <= 0.6]$rain = 0
    gefs[0.6 < temp &  temp < 3.6]$rain = gefs[0.6 < temp &  temp < 3.6]$rain * ((gefs[0.6 < temp &  temp < 3.6]$temp / 3) - 0.2)
    gefs[3.6 <= temp]$snow = 0
    gefs[0.6 < temp &  temp < 3.6]$snow = gefs[0.6 < temp &  temp < 3.6]$snow * (1 - ((gefs[0.6 < temp &  temp < 3.6]$temp / 3) - 0.2))
   
    gefs <- gefs[, .(rain_mean = mean(rain), snow_mean = mean(snow)), by=list(date)]
    gefs <- gefs[,  .(date = date, cum_snow = cumsum(snow_mean), cum_rain = cumsum(rain_mean)), by=list(year(date), month(date))]
    gefs <- gefs[,  .(date = date, max_snow = max(cum_snow), max_rain = max(cum_rain)), by=list(year(date), month(date))]
    gefs <- gefs[order(as.Date(gefs$date, format="%Y-%m-%d")),]

    gefs
  })
  
  precip_nmme <- reactive({
    hy <- summarize_historical_over_year()
    nmme <- select_nmme()
    nmme[, `:=`(year = as.integer(format(date, "%y")))]
    nmme <- subset(nmme, year == format(Sys.Date(), "%y"))
    nmme2 <- merge(nmme, hy[, c("monthday", "precip_mean", "temp_mean"), with=FALSE], by="monthday")
    nmme2[, `:=`(precip = precip + precip_mean), by=list(date, ens)]
    nmme2[, `:=`(temp = temp + temp_mean), by=list(date, ens)]
    nmme <- nmme2[, .(temp = mean(temp, na.rm = TRUE)), by=monthday]
    nmme$precip <- nmme2[, .(precip = mean(precip, na.rm = TRUE)), by=monthday]$precip
    nmme2$rain <- nmme2$precip
    nmme2$snow <- nmme2$precip
    nmme2[temp <= 0.6]$rain = 0
    nmme2[0.6 < temp &  temp < 3.6]$rain = nmme2[0.6 < temp &  temp < 3.6]$rain * ((nmme2[0.6 < temp &  temp < 3.6]$temp / 3) - 0.2)
    nmme2[3.6 <= temp]$snow = 0
    nmme2[0.6 < temp &  temp < 3.6]$snow = nmme2[0.6 < temp &  temp < 3.6]$snow * (1 - ((nmme2[0.6 < temp &  temp < 3.6]$temp / 3) - 0.2))
    nmme <- nmme2[, .(rain_mean = mean(rain, na.rm=T), snow_mean = mean(snow, na.rm=T)), by=list(date)]
    nmme <- nmme[,  .(date = date, cum_snow = cumsum(snow_mean), cum_rain = cumsum(rain_mean)), by=list(year(date), month(date))]
    nmme <- nmme[,  .(date = date, max_snow = max(cum_snow), max_rain = max(cum_rain)), by=list(year(date), month(date))]
    nmme <- nmme[order(as.Date(nmme$date, format="%Y-%m-%d")),]
    nmme
  })

  temp_hist <- reactive({
    hd <- summarize_historical_over_day()
    hd$day <- as.numeric(strftime(hd$date, format = "%j"))
    hist_temp_min <- calc_summary(hd, var = temp_min, group_var = monthday)
    hist_temp_max <- calc_summary(hd, var = temp_max, group_var = monthday)
    hist_temp_mean <- calc_summary(hd, var = temperature, group_var = monthday)
    hist <- data.table(monthday = hist_temp_mean$monthday,
                       temp_min_mean  = hist_temp_min$temp_min_mean,
                       temp_min_90pct = hist_temp_min$temp_min_90pct, 
                       temp_min_75pct = hist_temp_min$temp_min_75pct, 
                       temp_min_25pct = hist_temp_min$temp_min_25pct, 
                       temp_min_10pct = hist_temp_min$temp_min_10pct,
                       temp_max_mean  = hist_temp_max$temp_max_mean,
                       temp_max_90pct = hist_temp_max$temp_max_90pct, 
                       temp_max_75pct = hist_temp_max$temp_max_75pct,
                       temp_max_25pct = hist_temp_max$temp_max_25pct, 
                       temp_max_10pct = hist_temp_max$temp_max_10pct, 
                       temp_mean_mean  = hist_temp_mean$temperature_mean,
                       temp_mean_90pct = hist_temp_mean$temperature_90pct, 
                       temp_mean_75pct = hist_temp_mean$temperature_75pct, 
                       temp_mean_25pct = hist_temp_mean$temperature_25pct, 
                       temp_mean_10pct = hist_temp_mean$temperature_10pct)
    hist$date <- as.Date(paste0(this_year, paste0("-", hist$monthday)))
    hist <- hist[order(as.Date(hist$date, format="%Y-%m-%d")),]
    hist
  })
  
  temp_fut <- reactive({
    fd <- summarize_future_over_day()
    #fd2 <- fd[year >= input$future_range - 15 & year <= input$future_range + 15]
    fd$day <- as.numeric(strftime(fd$date, format = "%j"))
    #fd <- fd2

    
    fd2040 <- fd[2025 <= year & year <= 2055]
    fd2060 <- fd[2045 <= year & year <= 2075]
    fd2080 <- fd[2065 <= year & year <= 2095]

    fut_temp_min <- calc_summary(fd2040, var = temp_min, group_var = monthday)
    fut_temp_max <- calc_summary(fd2040, var = temp_max, group_var = monthday)
    fut_temp_mean <- calc_summary(fd2040, var = temperature, group_var = monthday)
    fut2040 <- data.table(
                      monthday = fut_temp_mean$monthday,
                      temp_min_mean  = fut_temp_min$temp_min_mean,
                      temp_min_90pct = fut_temp_min$temp_min_90pct,
                      temp_min_75pct = fut_temp_min$temp_min_75pct,
                      temp_min_25pct = fut_temp_min$temp_min_25pct,
                      temp_min_10pct = fut_temp_min$temp_min_10pct,
                      temp_max_mean  = fut_temp_max$temp_max_mean,
                      temp_max_90pct = fut_temp_max$temp_max_90pct,
                      temp_max_75pct = fut_temp_max$temp_max_75pct,
                      temp_max_25pct = fut_temp_max$temp_max_25pct,
                      temp_max_10pct = fut_temp_max$temp_max_10pct,
                      temp_mean_mean  = fut_temp_mean$temperature_mean,
                      temp_mean_90pct = fut_temp_mean$temperature_90pct,
                      temp_mean_75pct = fut_temp_mean$temperature_75pct,
                      temp_mean_25pct = fut_temp_mean$temperature_25pct,
                      temp_mean_10pct = fut_temp_mean$temperature_10pct)

    fut2040$date <- as.Date(paste0(this_year, paste0("-", fut2040$monthday)))
    setkey(fut2040, date)
    fut2040$year <- year(fut2040$date)
    fut_temp_min <- calc_summary(fd2060, var = temp_min, group_var = monthday)
    fut_temp_max <- calc_summary(fd2060, var = temp_max, group_var = monthday)
    fut_temp_mean <- calc_summary(fd2060, var = temperature, group_var = monthday)
    fut2060 <- data.table(
      monthday = fut_temp_mean$monthday,
      temp_min_mean  = fut_temp_min$temp_min_mean,
      temp_min_90pct = fut_temp_min$temp_min_90pct,
      temp_min_75pct = fut_temp_min$temp_min_75pct,
      temp_min_25pct = fut_temp_min$temp_min_25pct,
      temp_min_10pct = fut_temp_min$temp_min_10pct,
      temp_max_mean  = fut_temp_max$temp_max_mean,
      temp_max_90pct = fut_temp_max$temp_max_90pct,
      temp_max_75pct = fut_temp_max$temp_max_75pct,
      temp_max_25pct = fut_temp_max$temp_max_25pct,
      temp_max_10pct = fut_temp_max$temp_max_10pct,
      temp_mean_mean  = fut_temp_mean$temperature_mean,
      temp_mean_90pct = fut_temp_mean$temperature_90pct,
      temp_mean_75pct = fut_temp_mean$temperature_75pct,
      temp_mean_25pct = fut_temp_mean$temperature_25pct,
      temp_mean_10pct = fut_temp_mean$temperature_10pct)

    fut2060$date <- as.Date(paste0(this_year, paste0("-", fut2060$monthday)))
    setkey(fut2060, date)
    fut2060$year <- year(fut2060$date)
    fut_temp_min <- calc_summary(fd2080, var = temp_min, group_var = monthday)
    fut_temp_max <- calc_summary(fd2080, var = temp_max, group_var = monthday)
    fut_temp_mean <- calc_summary(fd2080, var = temperature, group_var = monthday)
    fut2080 <- data.table(
      monthday = fut_temp_mean$monthday,
      temp_min_mean  = fut_temp_min$temp_min_mean,
      temp_min_90pct = fut_temp_min$temp_min_90pct,
      temp_min_75pct = fut_temp_min$temp_min_75pct,
      temp_min_25pct = fut_temp_min$temp_min_25pct,
      temp_min_10pct = fut_temp_min$temp_min_10pct,
      temp_max_mean  = fut_temp_max$temp_max_mean,
      temp_max_90pct = fut_temp_max$temp_max_90pct,
      temp_max_75pct = fut_temp_max$temp_max_75pct,
      temp_max_25pct = fut_temp_max$temp_max_25pct,
      temp_max_10pct = fut_temp_max$temp_max_10pct,
      temp_mean_mean  = fut_temp_mean$temperature_mean,
      temp_mean_90pct = fut_temp_mean$temperature_90pct,
      temp_mean_75pct = fut_temp_mean$temperature_75pct,
      temp_mean_25pct = fut_temp_mean$temperature_25pct,
      temp_mean_10pct = fut_temp_mean$temperature_10pct)

    fut2080$date <- as.Date(paste0(this_year, paste0("-", fut2080$monthday)))
    setkey(fut2080, date)
    fut2080$year <- year(fut2080$date)
    list(fut2040, fut2060, fut2080)
  })
  
  temp_hist_frost <- reactive({
    hd <- summarize_historical_over_day()
    hd$day <- as.numeric(strftime(hd$date, format = "%j"))
    
    ####### GET LAST FROST DATA #######
    last_frost <- hd[last_frost_day == 1, 
                     .(last_frost = as.integer(strftime(date, format="%j")),
                       date = as.Date(paste0(this_year, "-", monthday))), 
                     by=year]
    setkey(last_frost, date) 
    
    # ####### GET FIRST FROST DATA #######    
    first_frost <- hd[first_frost_day == 1, 
                      .(first_frost = as.integer(strftime(date, format="%j")),
                        date = as.Date(paste0(this_year, "-", monthday))), 
                      by=year]
    setkey(first_frost, date)  
    list(last_frost, first_frost)
})
  
  temp_fut_frost <- reactive({
    fd <- summarize_future_over_day()
    
    ####### GET FUTURE LAST FROST DATA #######
    frost_data <- fd[last_frost_day == 1, 
                     .(last_frost = as.integer(strftime(date, format="%j")),
                       date = as.Date(paste0(this_year, "-", monthday))), 
                     by=year]
    setkey(frost_data, date)

    l2040 <- subset(frost_data, 2025 <= year & year <= 2055)
    l2060 <- subset(frost_data, 2045 <= year & year <= 2075)
    l2080 <- subset(frost_data, 2065 <= year & year <= 2095)
    ####### GET FUTURE FIRST FROST DATA #######    
    frost_data <- fd[first_frost_day == 1, 
                     .(first_frost = as.integer(strftime(date, format="%j")),
                       date = as.Date(paste0(this_year, "-", monthday))), 
                     by=year]
    setkey(frost_data, date)

    f2040 <- subset(frost_data, 2025 <= year & year <= 2055)
    f2060 <- subset(frost_data, 2045 <= year & year <= 2075)
    f2080 <- subset(frost_data, 2065 <= year & year <= 2095)

    list(l2040, l2060, l2080, 
         f2040, f2060, f2080)
  })

  temp_hist_heat_risk <- reactive({ 
    heat_risk <- (input$heat_risk_range - 32)*(5/9) #convert to celcius
    hd <- summarize_historical_over_day()
    hd$day <- as.numeric(strftime(hd$date, format = "%j"))
    hd <- hd[order(as.Date(hd$date, format="%Y-%m-%d")),]
    hd <- hd[monthday != "02-29"]
    
    hd$risk_day = 0
    
    hd[heat_risk < temp_max]$risk_day = 1
    hd[heat_risk >=  temp_max]$risk_day = 0
    
    hd <- hd[,  .(monthday = monthday,
                  year = year,
                  month = month,
                  date = date,
                  risk_count = cumsum(risk_day)), by=list(year(date), month(date))]
    
    hd <- hd[,  .(monthday = monthday,
                  year = year,
                  month = month,
                  risk_count = max(risk_count)), by=list(year(date), month(date))]
    
    hd$date <- as.Date(paste0(this_year, paste0("-", hd$monthday)))
    
    hd <- rbind(hd[monthday == "01-15"], hd[monthday == "02-15"], hd[monthday == "03-15"], hd[monthday == "04-15"], hd[monthday == "05-15"],
                hd[monthday == "06-15"], hd[monthday == "07-15"], hd[monthday == "08-15"], hd[monthday == "09-15"], hd[monthday == "10-15"], 
                hd[monthday == "11-15"], hd[monthday == "12-15"])
    
    hist <- hd[order(as.Date(hd$date, format="%Y-%m-%d")),]
  })
  
  temp_fut_heat_risk <- reactive({ 
    heat_risk <- (input$heat_risk_range - 32)*(5/9)
    fd <- summarize_future_over_day()
    fd$day <- as.numeric(strftime(fd$date, format = "%j"))
    fd <- fd[order(as.Date(fd$date, format="%Y-%m-%d")),]
    fd <- fd[monthday != "02-29"]
    
    fd$risk_day = 0
    
    fd[heat_risk < temp_max]$risk_day = 1
    fd[heat_risk >=  temp_max]$risk_day = 0
    
    fd <- fd[,  .(monthday = monthday,
                        year = year,
                        month = month,
                        date = date,
                        risk_count = cumsum(risk_day)), by=list(year(date), month(date))]
    
    fd <- fd[,  .(monthday = monthday,
                        year = year,
                        month = month,
                        risk_count = max(risk_count)), by=list(year(date), month(date))]

    fd$date <- as.Date(paste0(this_year, paste0("-", fd$monthday)))
    
    fd <- rbind(fd[monthday == "01-15"], fd[monthday == "02-15"], fd[monthday == "03-15"], fd[monthday == "04-15"], fd[monthday == "05-15"],
                  fd[monthday == "06-15"], fd[monthday == "07-15"], fd[monthday == "08-15"], fd[monthday == "09-15"], fd[monthday == "10-15"], 
                  fd[monthday == "11-15"], fd[monthday == "12-15"])
    
    fut <- fd[order(as.Date(fd$date, format="%Y-%m-%d")),]

    fut3_40 <- fut[2040 - 15 < year & year <= 2040 + 15]
    fut3_60 <- fut[2060 - 15 < year & year <= 2060 + 15]
    fut3_80 <- fut[2080 - 15 < year & year <= 2080 + 15]

    list(fut3_40, fut3_60, fut3_80)
  })

  temp_hist_frost_risk <- reactive({
    frost_risk <- 0
    hd <- summarize_historical_over_day()
    hd$day <- as.numeric(strftime(hd$date, format = "%j"))
    hd <- hd[order(as.Date(hd$date, format="%Y-%m-%d")),]
    hd <- hd[monthday != "02-29"]
    hd$risk_day = 0
    
    hd[temp_min < frost_risk]$risk_day = 1
    hd[frost_risk <=  temp_min]$risk_day = 0
    
    hd <- hd[,  .(monthday = monthday,
                  year = year,
                  month = month,
                  date = date,
                  risk_count = cumsum(risk_day)), by=list(year(date), month(date))]
    
    hd <- hd[,  .(monthday = monthday,
                  year = year,
                  month = month,
                  risk_count = max(risk_count)), by=list(year(date), month(date))]
    
    hd$date <- as.Date(paste0(this_year, paste0("-", hd$monthday)))
    
    hd <- rbind(hd[monthday == "01-15"], hd[monthday == "02-15"], hd[monthday == "03-15"], hd[monthday == "04-15"], hd[monthday == "05-15"],
                hd[monthday == "06-15"], hd[monthday == "07-15"], hd[monthday == "08-15"], hd[monthday == "09-15"], hd[monthday == "10-15"], 
                hd[monthday == "11-15"], hd[monthday == "12-15"])
    
    hist <- hd[order(as.Date(hd$date, format="%Y-%m-%d")),]
  })
  
  temp_fut_frost_risk <- reactive({
    frost_risk <- 0
    fd <- summarize_future_over_day()
    fd$day <- as.numeric(strftime(fd$date, format = "%j"))
    fd <- fd[order(as.Date(fd$date, format="%Y-%m-%d")),]
    fd <- fd[monthday != "02-29"]
    fd$risk_day = 0
    
    fd[temp_min < frost_risk]$risk_day = 1
    fd[frost_risk <=  temp_min]$risk_day = 0
    
    fd <- fd[,  .(monthday = monthday,
                  year = year,
                  month = month,
                  date = date,
                  risk_count = cumsum(risk_day)), by=list(year(date), month(date))]
    
    fd <- fd[,  .(monthday = monthday,
                  year = year,
                  month = month,
                  risk_count = max(risk_count)), by=list(year(date), month(date))]

    fd$date <- as.Date(paste0(this_year, paste0("-", fd$monthday)))
    
    fd <- rbind(fd[monthday == "01-15"], fd[monthday == "02-15"], fd[monthday == "03-15"], fd[monthday == "04-15"], fd[monthday == "05-15"],
                fd[monthday == "06-15"], fd[monthday == "07-15"], fd[monthday == "08-15"], fd[monthday == "09-15"], fd[monthday == "10-15"], 
                fd[monthday == "11-15"], fd[monthday == "12-15"])
    
    fut <- fd[order(as.Date(fd$date, format="%Y-%m-%d")),]
    
    fut3_40 <- fut[2040 - 15 < year & year <= 2040 + 15]
    fut3_60 <- fut[2060 - 15 < year & year <= 2060 + 15]
    fut3_80 <- fut[2080 - 15 < year & year <= 2080 + 15]
    
    list(fut3_40, fut3_60, fut3_80)
  })
  
  temp_pd <- reactive({
    pd <- summarize_present_over_day()
    pd
  })

  temp_gefs <- reactive({
    gefs <- select_gefs()[, 
                          .(temp_mean = mean(temp),
                            temp_max = mean(temp_max),
                            temp_min = mean(temp_min)), 
                          by=list(date)]
    gefs <- gefs[order(as.Date(gefs$date, format="%Y-%m-%d")),]
    gefs
  })
  
  temp_nmme <- reactive({
    hy <- summarize_historical_over_year()
    nmme1 <- select_nmme()
    nmme1[, `:=`(year = as.integer(format(date, "%y")))]
    nmme1 <- subset(nmme1, year == format(Sys.Date(), "%y"))
    nmme1[, monthday := format(date, "%m-%d")]
    nmme2 <- merge(nmme1, hy[, c("monthday", "temp_mean"), with=FALSE], by="monthday")
    nmme2[, temp := temp + temp_mean]
    nmme2[, gdd := calc_gdd(temp), by=list(date, ens)]
    
    nmme <- nmme2[, .(temp_mean = mean(temp, na.rm=T),
                      temp_min = min(temp, na.rm=T),
                      temp_max = max(temp, na.rm=T)), 
                  by=list(date)]

    nmme <- nmme[order(as.Date(nmme$date, format="%Y-%m-%d")),]
  })

  ########### SUMMARIZE FUNCTIONS ###########
  summarized_county <- reactive({
    summarize_county(select_nass(), subset_crop()$name, year, yield)
  })

  summarize_future_over_day = reactive({
    planting_date <- isolate(input$planting_date)
    dt <- summarizeOverDay(select_future(), subset_crop(), gdd_start_date = planting_date)
  })

  summarize_future_over_month = reactive({
    df <- summarize_future_over_day()[
      ,
      .(cgdd = max(cgdd, na.rm=T),
        precip = sum(precipitation)),
      c("month", "year")
      ]
    df
  })

  summarize_future_over_year = reactive({
    dt <- summarizeOverYear(summarize_future_over_day())[,
      date := as.Date(paste0(this_year, "-", monthday))
      ]
    setkey(dt, date)
    if(!leap_year(this_year)) res <- dt[date != as.Date(paste0(this_year, "-02-29"))]
    else res <- dt
    res
  })

  summarize_historical_over_day = reactive({
    planting_date <- isolate(input$planting_date)
    dt <- summarizeOverDay(select_historical(), subset_crop(), gdd_start_date = planting_date)
    dt
  })

  summarize_historical_over_month = reactive({
    dt <- summarize_historical_over_day()[
      ,
      .(cgdd = max(cgdd, na.rm=T),
        precip = sum(precipitation)),
      c("month", "year")
      ]
    dt
  })

  summarize_historical_over_year = reactive({
    dt <- summarizeOverYear(summarize_historical_over_day())[,
      date := as.Date(paste0(this_year, "-", monthday))
      ]
    setkey(dt, date)
    if(!leap_year(this_year)) res <- dt[date != as.Date(paste0(this_year, "-02-29"))]
    else res <- dt
    res
  })

  summarize_present_over_day = reactive({
    planting_date <- isolate(input$planting_date)
    dt <- summarizeOverDay(select_reacch(), subset_crop(), gdd_start_date = planting_date)
    dt[date < today]
  })

  summarize_present_over_month = reactive({
    df <- summarize_present_over_day()[,
                                       .(cgdd = max(cgdd, na.rm=T),
                                         precip = sum(precipitation)),
                                       month]
    df
  })

  ########### SELECTION FUNCTIONS ###########

  select_cfs <- reactive({
    loc <- select_location()
    cfs <- read_forecast(lat = loc$latitude,
                          lon = loc$longitude,
                          model = "cfs",
                          today = today,
                          data_dir = paste0(data_dir, "cfs/"))
    cfs
  })

  select_crops <- reactive({
    loc <- select_location()
    if(is.null(loc)) selected_crops <- crops$name
    else selected_crops <- availableCrops(loc$latitude, loc$longitude, crops, crop_locations)
    selected_crops
  })

  select_future <- reactive({
    loc <- select_location()
    read_future(loc$latitude, loc$longitude)
  })

  select_gefs <- reactive({
    loc <- select_location()
    read_forecast(lat = loc$latitude,
                  lon = loc$longitude,
                  model = "gefs",
                  today = today,
                  data_dir = paste0(data_dir, "gefs/"))
  })

  select_historical <- reactive({ #on adjusting the range of years
    loc <- select_location()
    read_historical(loc$latitude, loc$longitude)
  })

  select_location <- reactive({
    if(!is.null(input$map_shape_click)) {
      event = input$map_shape_click
      res <- list(latitude=event$lat, longitude=event$lng)
    }
    else if (!is.null(input$map_shape_mouseover))
    {
      event = input$map_shape_mouseover
      res <- list(latitude=event$lat, longitude=event$lng)
    }
    else {
      res <- NULL      
    }
    res
  })

  select_nass <- reactive({
    loc <- select_location()
    nass <- data.table(read_nass.latlon(loc$latitude, loc$longitude))
    names(nass) <- str_to_lower(names(nass))
    nass[commodity_desc=="WHEATWINTER"]$commodity_desc = "WHEAT, WINTER"
    nass[commodity_desc=="WHEATSPRING, (EXCL DURUM)"]$commodity_desc = "WHEAT, SPRING, (EXCL DURUM)"
    nass[commodity_desc=="BEANSDRY EDIBLE"]$commodity_desc = "BEANS, DRY EDIBLE, CHICKPEAS"
    nass[commodity_desc=="HAYALFALFA"]$commodity_desc = "HAY, ALFALFA"
    nass[commodity_desc=="HAY(EXCL ALFALFA)"]$commodity_desc = "HAY, (EXCL ALFALFA)"
    nass
  })
  
  select_nmme <- reactive({
    loc <- select_location()
    dt <- read_forecast(loc$latitude, 
                        loc$longitude,
                        model="nmme",
                        today = today,
                        data_dir = paste0(data_dir, "nmme/"))
    dt[, monthday := format(date, "%m-%d")]
    dt[date>=today & date <= last_day_of_year]
  })
  
  select_reacch <- reactive({
    loc <- select_location()
    dt <- read_present(loc$latitude,
                       loc$longitude,
                       today = today)
  })

  spatial_future <- reactive({
    dt <- create_distance_dt(summarize_future_over_day(), varlist = input$analogue_vars)
    dt
  })

  subset_crop <- reactive({
    input$gddButton
    crop_name <- isolate(input$crop_name)
    filter(crops, name == crop_name)
  })
  
  subset_future <- reactive({
    select_future()[]
  })

  subset_historical <- reactive({
    select_historical()[year >= input$historical_range[1] &
                          year <= input$historical_range[2]]
  })

  subset_spatial_historical <- reactive({
    nh <- names(spatial_historical)
    vars <- c("year", "lat", "lon")
    for(i in input$analogue_vars) {
      this_var <- nh[grepl(i, nh)]
      vars <- c(vars, this_var)
    }
    spatial_historical[, vars, with=FALSE]
  })

  subset_nass_yields <- reactive({
    crop <- subset_crop()
    nass <- select_nass()
    
    #Fix unit based on crop
    if (crop$nass_name == "BEANS, DRY EDIBLE, CHICKPEAS") unit_d <- "LB / ACRE" else unit_d <- "BU / ACRE"
    nass_yields <- nass[commodity_desc == crop$nass_name & 
                          unit_desc == unit_d &
                          year >= min(historical_years), 
                        c("year", "yield"), 
                        with = FALSE]
  })
  
  time_historical <- reactive({
    dt <- create_distance_dt(summarize_historical_over_day(), varlist = c("cum_gdd"))
    dt
  })
  
  getTimeFrame <- reactive({
    if (input$timeFrame == "Historical")
      return (summarize_historical_over_year())
    else if (input$timeFrame == "Future")
      return (summarize_future_over_year())
  })

###################################################################################################
#Observers
###################################################################################################

  observe({
    if (input$tileSelect == "Satellite"){
      leafletProxy("map") %>%
        addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
                 layerId = "Satellite")
      
      # Update layer id 
      layer_id = 0;  
    }
    else if (input$tileSelect == "Topographic"){
      leafletProxy("map") %>%
        addTiles(urlTemplate = "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
                 layerId = "Topographic")
      
      # Update layer id 
      layer_id = 1;
    }
    else if (input$tileSelect == "Basic"){
      leafletProxy("map") %>%
        addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
                 layerId = "Basic")
      
      # Update layer id 
      layer_id = 2;
    }
  })
  
  ############### edit here
  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    if (is.null(event$group))
      return()
    if (event$group == "locations")
      leafletProxy("map") %>% clearPopups()
    
    selected_crops <- subset(crops, name %in% availableCrops(event$lat, event$lng, crops, crop_locations))

    default_crop_name <- head(selected_crops, 1)
  
    updateSelectInput(session, "crop_name", "Crops:", selected_crops$name, selected = default_crop_name)
    updateSelectInput(session, "crop_name_longterm", "Crops:", selected_crops$name, selected = default_crop_name)
    toggleModal(session, modalId="graphs", toggle = "open")
  })

  observe({
  
    if (is.null(input$cropSelect))
    {
      leafletProxy("map") %>%
        clearGroup("locations")
      return();
    }
    
    crop_id <- subset(crops, name %in% input$cropSelect)$id
    selected_locations <- subset(crop_locations, as.numeric(cropid) %in% as.numeric(crop_id))
    selected_locations <- selected_locations[!duplicated(selected_locations[,-4]), ]

    pal = colorBin(palette = "Reds", domain = as.numeric(selected_locations$num_crops), bins = 8, pretty=FALSE)
   
    leafletProxy("map") %>%
    clearGroup("locations") %>%
    addCircles(data = selected_locations, lng = ~longitude, lat = ~latitude,
               radius=r,
               stroke=FALSE,
               fillOpacity=0.6,
               color = ~pal(as.numeric(num_crops)),
               group = "locations") %>%
      
    setView(lng = -117, lat = 45, zoom = 6)

  })

  observe({
    event <- input$map_shape_mouseover
    if (is.null(event)) { return() } 
    if (is.null(event$group)) { return() } 
    if (event$group == "locations") 
    {
      irr <- irrigation[lat == event$lat & long == event$lng]
      content <- sprintf("Coordinates: %f, %f", event$lat, event$lng)
      
      if (0 < nrow(irr))
      {
        content <- paste0(content, sprintf("<br/>County: %s", head(irr$countyname, 1)))
        content <- paste0(content, paste0("<br/>Irrigated Crops: ", paste(irr[0 < irrtype]$cropname, collapse = ", ")))
        content <- paste0(content, paste0("<br/>Non-irrigated Crops: ", paste(irr[irrtype < 1]$cropname, collapse = ", ")))
      }
      
      leafletProxy("map") %>% addPopups(lat = event$lat + 0.05, lng = event$lng, content)
    }
  })
  
  observe({
    event <- input$map_shape_mouseout
    leafletProxy("map") %>% clearPopups()
  })
  
})











