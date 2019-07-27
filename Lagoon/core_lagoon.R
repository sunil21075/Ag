options(digits=9)
options(digits=9)

########################################################################
# source_path = "/home/hnoorazar/reading_binary/read_binary_core.R"      #
# source(source_path)                                                    #
########################################################################

compute_median_diff_4_map <- function(dt, tgt_col){

  ################################################
  #
  # We need to do the following lines in order
  # to make the subtraction within groups work.
  #
  dt_observed <- dt %>% 
                 filter(time_period == "1979-2016") %>%
                 select(-c("emission")) %>%
                 unique()%>%
                 data.table()

  dt_observed$emission <- "observed"

  dt <- dt %>% filter(time_period != "1979-2016") %>% data.table()
  dt <- rbind(dt, dt_observed)

  #
  # Clean unwanted data
  #
  if ("evap" %in% colnames(dt)){
    dt <- within(dt, remove(evap, runoff, base_flow, run_p_base))
  }
  if ("wtr_yr" %in% colnames(dt)) {
    dt <- within(dt, remove(wtr_yr))
  }

  dt <- within(dt, remove(year, month, day, precip, model, cluster))

  dt <- dt %>% 
        filter(time_period != "1950-2005" & 
               time_period != "2006-2025") %>% 
        data.table()

  medians <- data.frame(dt) %>% 
             group_by(location, time_period, emission) %>% 
             summarise( medians = median(get(tgt_col)))  %>% 
             data.table()

  median_diffs <- medians %>%
                  group_by(location) %>%
                  mutate(diff = medians - medians[time_period == "1979-2016"])%>%
                  data.table()

  median_diffs <- median_diffs %>% 
                  filter(time_period != "1979-2016")%>%
                  data.table()

  # to do percentages
  obs_medians <- medians %>% 
                 filter(emission == "observed") %>% 
                 data.table()
  obs_medians <- within(obs_medians, remove(time_period, emission))
  setnames(obs_medians, old=c("medians"), new=c("obs_median"))

  median_diffs <- merge(median_diffs, obs_medians, by="location", all.x=T)
  median_diffs$perc_diff <- (median_diffs$diff * 100) / (median_diffs$obs_median)
  median_diffs <- within(median_diffs, remove(medians, obs_median))
  return(median_diffs)
}

########################################################################
add_coord_from_location <- function(dt){
  x <- sapply(dt$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;
  return(dt)
}

cluster_numeric_2_str <- function(B){
  B$cluster <- as.character(B$cluster)
  B$cluster <- recode(B$cluster, "4" = "most precip",
                                 "3" = "less precip",
                                 "2" = "lesser precip",
                                 "1" = "least precip")

  return(B)
}

read_min_file <- function(conn){
  RLData <- readLines(conn)
  RLData_df <- as.data.frame(RLData)
  RLData_df <- stringr::str_split_fixed(RLData_df$RLData, "\t", 7)
  RLData_df <- data.table(RLData_df)

  col_name <- c("year", "month", "day", 
                "precip", "evap", "runoff", "base_flow")
  colnames(RLData_df) <- col_name
  return(RLData_df)
}

cluster_yr_time_series <- function(observed_dt, no_clusters=4){
  observed_dt <- subset(observed_dt, select=c(location, 
                                              year, 
                                              annual_cum_precip))
  
  observed_dt <- reshape(observed_dt, idvar = "location", 
                                      timevar = "year", 
                                      direction = "wide")
  locations <- observed_dt$location
  observed_dt <- within(observed_dt, remove(location))

  ts_clusters <- kmeans(observed_dt, centers = no_clusters, nstart = 25)
  return(ts_clusters)
}

cluster_yr_avging <- function(observed_dt, scale=FALSE, no_clusters=4){
  #
  # max_clusters: maximum number of clusters to try and pick
  #               the best.
  #
  if (scale == FALSE){
    observed_dt <- observed_dt %>% 
                   group_by(location) %>% 
                   summarise(target_col = mean(annual_cum_precip)) %>% 
                   data.table()
     } else {
      observed_dt <- observed_dt %>% 
                     group_by(location)%>% 
                     summarise(mean_annual_precip = mean(annual_cum_precip),
                               sd = sd(annual_cum_precip))%>%
                     mutate(target_col = (mean_annual_precip/sd)) %>% 
                     data.table()
  }
  # for_elbow = data.table(no_clusters = c(1:max_clusters),
  #                        total_within_cluster_ss = rep(-666, max_clusters))
  set.seed(100)
  clusters_obj <- kmeans(observed_dt$target_col, 
                         centers = no_clusters, 
                         nstart = 50)

  # for_elbow[k, "total_within_cluster_ss"] <- clusters$betweenss
  clusters = data.table(location = observed_dt$location,
                        ann_prec_mean = observed_dt$target_col,
                        cluster_label = clusters_obj$cluster
                        )

  ##### Sort according descending order 4 is most rainy, 1 least
  # centroids_dt <- data.table(centroid=as.vector(clusters_obj$centers),
  #                            cluster_label = 1:no_clusters)
  # clusters <- merge(clusters, centroids_dt, 
  #                   by="cluster_label", all.x=TRUE)
  
  # 1st method
  clusters <- clusters %>% 
              group_by(cluster_label) %>% #create the centroids variable
              mutate(centroid = mean(ann_prec_mean)) %>%
              # ungroup() %>%
              data.table()
  
  # re-order cluster labels so that the max label 
  # corresponds to max rain and so on
  clusters <- clusters %>% 
              mutate("cluster" = frankv(centroid, 
                                 ties.method = "dense"))

  clusters <- within(clusters, remove(cluster_label))
  return(list(clusters, clusters_obj))
}

#********************************************************
design_storm_4_allLoc_allMod_from_raw <- function(data_tbl, observed=FALSE){
  locations <- unique(data_tbl$location)
  models <- unique(data_tbl$model)
  
  no_locs <- length(locations)
  no_models <- length(models)
  emission <- unique(data_tbl$emission)

  ###################################################
  #
  # set up the output table:
  #
  if (observed==TRUE){
     mini_tab_nrow <- 1
     } else {
     mini_tab_nrow <- 4
  }
  n_rows <- no_locs * no_models * mini_tab_nrow
  col_names <- c("location", "model", "return_period", "five_years", "ten_years", 
                 "fifteen_years", "twenty_years", "twenty_five_years")
  final_table <- setNames(data.table(matrix(nrow = n_rows, 
                                     ncol = length(col_names))), col_names)

  final_table$location <- as.character(final_table$location)
  final_table$model <- as.character(final_table$model)
  final_table$return_period <- as.character(final_table$return_period)
  
  final_table$five_years <- as.numeric(final_table$five_years)
  final_table$ten_years <- as.numeric(final_table$ten_years)
  final_table$fifteen_years <- as.numeric(final_table$fifteen_years)
  final_table$twenty_years <- as.numeric(final_table$twenty_years)
  final_table$twenty_five_years <- as.numeric(final_table$twenty_five_years)
  ###################################################
  row_pointer <- 1
  
  for (loc in locations){
     for (mod in models){
       curr_dt <- data_tbl %>% filter(location==loc & model==mod)
       curr_storm <- design_storm_4_oneLoc_oneMod_from_raw(curr_dt, observed)
       final_table[row_pointer:(row_pointer + (mini_tab_nrow-1)), ] <- curr_storm
       row_pointer <- row_pointer + mini_tab_nrow
     }
  }
  return(final_table)
}

design_storm_4_oneLoc_oneMod_from_raw <- function(data_tbl, observed=FALSE){
  ################################################
  # This function is written to be applied to "an individual"
  # (location, model) pair
  # input : data_tbl has to have columns: 
  ################################################

  data_tbl <- find_annual_max_24_hr(data_tbl)
  data_tbl <- convert_precip_2_intens(data_tb=data_tbl,
                                      col_name="max_24_hr_precip_annual")
  
  # data_tbl <- put_time_period(data_tb=data_tbl, observed=obs)
  data_tbl <- design_storm_all_timePeriods(data_tbl)
  return(data_tbl)
}

design_storm_all_timePeriods <- function(data_tbl){
  ################################################################
  #
  # This function is written to be applied to "an individual"
  # (location, model)
  # input : data_tbl has to have columns: 
  #                  year, location, max_24_hr_precip_annual, 
  #                  max_24_hr_intens,
  #                  time_period.
  #
  ################################################################

  # initiate the table to be populated:
  data = data.table()

  time_periods <- unique(data_tbl$time_period)
  time_period_stats <- intensity_stats_by_time_period(data_tbl)

  for (time in time_periods){
    data_t = data_tbl %>% filter(time_period == time)
    stat_time <- time_period_stats %>% filter(time_period == time)
    new_row_vec = design_storm_4_1_time_period(data_tb = data_t, 
                                               avg = stat_time$mean, 
                                               std = stat_time$sd)
    new_row = data.table(return_period = time,
                         five_years = new_row_vec[1],
                         ten_years = new_row_vec[2],
                         fifteen_years = new_row_vec[3],
                         twenty_years = new_row_vec[4],
                         twenty_five_years = new_row_vec[5]
                         )
    data <- rbind(data, new_row)
  }
  col_n <- colnames(data)
  data$location <- unique(data_tbl$location)
  data$model <- unique(data_tbl$model)

  setcolorder(data, c("location", "model", col_n))
  return(data)
}

design_storm_4_1_time_period <- function(data_tb, avg, std){
  years_intervals <- c(5, 10, 15, 20, 25)
  gumbel_constants <- compute_gumbel_constant(years_intervals)
  storm_dens <- avg + (std * gumbel_constants)
  return(storm_dens)
}

compute_gumbel_constant <- function(n_years){
  return(-1 * (sqrt(6) / pi) * (0.5772 + log(log(n_years / (n_years-1) ))))
}

intensity_stats <- function(data_tb){
  stats <- data_tb[ , list(mean=mean(max_24_hr_intens), 
                           sd=sd(max_24_hr_intens)),
                     by = c("model", "emission")]
  return(stats)
}

intensity_stats_by_time_period <- function(data_tb){
  stats <- data_tb[ , list(mean = mean(max_24_hr_intens), 
                           sd = sd(max_24_hr_intens)), 
                      by = c("time_period", "model", "emission")]
  return(stats)
}

convert_precip_2_intens <- function(data_tb, col_name="max_24_hr_precip_annual"){
  # input:  data_tb: data of class data.table
  #         col_name: name of the column to be converted
  #                   to intensity. The colum containing
  #                   max_24_hr Event thing
  
  # output: data_tb with a new column added to it
  #         that is result of dividing column given by col_name
  #         by 24.
  data_tb$max_24_hr_intens <- data_tb[, get(col_name)]/24.00
  return(data_tb)
}

find_annual_max_24_hr <- function(data_tb){
  ##############################################################
  # input: data_tb                                             #
  #                                                            #
  # output: a data table that has one row per                  #
  #         (location, year) with the max_24_hr_precip         #
  #         column in it.                                      #
  ##############################################################
  
  data_tb <- data_tb %>%
             group_by(year, location, model, emission, time_period) %>%
             summarise(max_24_hr_precip_annual = max(precip)) %>%
             data.table()
  return (data_tb)
}

find_monthly_max_24_hr <- function(data_tb){
  ##############################################################
  # input: data_tb                                             #
  #        precip_col_name: The name of the column containing  #
  #                         daily precipitation.               #
  #                                                            #
  # output: a data table that has one row per                  #
  #         (location, year) with the max_24_hr_precip         #
  #        column in it.                                       #
  ##############################################################
  
  data_tb <- data_tb %>%
             group_by(year, month, location, model, emission, time_period) %>%
             summarise(max_24_hr_precip_monthly = max(precip)) %>%
             data.table()
  return (data_tb)
}

find_chunk_max_24_hr <- function(data_tb, start_month, end_month){
  # input: data_tb the data containing precip column in it.
  #        start_month and end_month are the beginning and end of
  #        the period we are interested in like from 10 (oct) to 3 (March).
  #        
  # output: 
  #
  # Group by year, and create new calendar year, and do the fucking thing.

  data_tb <- create_wtr_calendar(data_tb, wtr_yr_start=start_month)

  # get rid of unwanted months:
  if (start_chunk > end_chunk){
     data_tb <- data_tb %>% 
                filter(!(month %in% ((end_month+1):(start_month-1))))
     } else {
         data_tb <- data_tb %>%
                    filter(month %in% (start_month:end_month))
  }

  data_tb$chunk_month_max <- data_tb %>%
                             group_by(wtr_yr, location, model, emission, time_period) %>%
                             summarise(max_24_hr_precip_chunky = max(precip)) %>%
                             data.table()
  return(data_tb)
}


########################################################################
#
#        Cumulation of precipitation section
#
########################################################################
compute_chunky_cum <- function(data_tb, start_month, end_month){
  #
  # first, put water_calendar so proper months are grouped together
  # second, toss out unwanted months.
  # third, find, cumu. over a wtr_year
  #
  data_tb <- create_wtr_calendar(data_tb, wtr_yr_start = start_month)

  data_tb <- data_tb %>%
             filter(!(month %in% ((end_month+1):(start_month-1))))%>%
             data.table()

  if (precip %in% colnames(data_tb)){
    data_tb <- data_tb %>%
             group_by(location, wtr_yr, model, emission, time_period) %>%
             mutate(chunk_cum_precip = cumsum(precip)) %>%
             # slice(n()) %>%
             data.table()
  } else{
    data_tb <- data_tb %>%
             group_by(location, wtr_yr, model, emission, time_period) %>%
             mutate(chunk_cum_runbase = cumsum(run_p_base)) %>%
             # slice(n()) %>%
             data.table()
  }
 return(data_tb)
}

# **********************************************************************
compute_wtr_yr_cum <- function(data_tb){
  #
  # input: data_tb has to have the water_year column in it
  # output: cumulative precip in each water_year
  #
  if ("precip" %in% colnames(data_tb)){
    data_tb <- data_tb %>%
             group_by(location, wtr_yr, model, emission, time_period) %>%
             mutate(annual_cum_precip = cumsum(precip)) %>%
             # slice(n()) %>%
             data.table()
  } else {
    data_tb <- data_tb %>%
             group_by(location, wtr_yr, model, emission, time_period) %>%
             mutate(annual_cum_runbase = cumsum(run_p_base)) %>%
             # slice(n()) %>%
             data.table()
  }
  return (data_tb)
}
# **********************************************************************

compute_annual_cum <- function(data_tb){
  ##############################################################
  # input: data_tb                                             #
  #                                                            #
  # output:                                                    #
  #                                                            #
  #                                                            #
  ##############################################################
  if ("run_p_base" %in% colnames(data_tb)){
  data_tb <- data_tb %>%
             group_by(location, year, model, emission, time_period) %>%
             mutate(annual_cum_runbase = cumsum(run_p_base)) %>%
             # filter(month==12 & day==31) %>%
             data.table()
    } else {
      data_tb <- data_tb %>%
                 group_by(location, year, model, emission, time_period) %>%
                 mutate(annual_cum_precip = cumsum(precip)) %>%
                 # filter(month==12 & day==31) %>%
                 data.table()
  }
  return (data_tb)
}
# **********************************************************************

compute_monthly_cum <- function(data_tb){
  ##############################################################
  # input: data_tb                                             #
  #                                                            #
  # output:                                                    #
  #                                                            #
  #                                                            #
  ##############################################################
  if ("runoff" %in% colnames(data_tb)){
    data_tb <- data_tb %>%
               group_by(location, year, month, model, emission) %>%
               mutate(monthly_cum_runbase = cumsum(run_p_base)) %>%
               # slice(which.max(day)) %>%
               data.table()
    } else {
      data_tb <- data_tb %>%
               group_by(location, year, month, model, emission) %>%
               mutate(monthly_cum_precip = cumsum(precip)) %>%
               # slice(which.max(day)) %>%
               data.table()
  }
  return (data_tb)
}
# **********************************************************************

########################################################################
#
#        Create Water Calendar
#
########################################################################

create_wtr_calendar <- function(data_tb, wtr_yr_start){
  # input:
  # output:
  data_tb <- data_tb %>%
             mutate(wtr_yr = case_when(# If Jan:Sept then part of H2O yr of prev year - current year
                                       month %in% c(1:(wtr_yr_start-1)) ~ paste0("wtr_", (year - 1), "-", year),
                                       
                                       # If Oct:Dec then part of H2O yr of current year - next year
                                       month %in% c(wtr_yr_start:12) ~ paste0("wtr_", year, "-", (year + 1))
                                       )
                    ) %>%
             data.table()

  # cut the beginning and end of the data that do not belong
  # to any water year calendar!
  data_tb <- data_tb %>% 
             filter(!(year == min(data_tb$year) & (month < wtr_yr_start))) %>%
             filter(!(year == max(data_tb$year) & (month >= wtr_yr_start))) %>%
             data.table()
  return(data_tb)
}

put_time_period <- function(data_tb, observed){
  if (observed==TRUE){
     data_tb$time_period <- "1979-2016"
     } else {
      data_tb <- data_tb %>%
                 mutate(time_period = case_when(year %in% c(1950:2005) ~ "1950-2005",
                                                year %in% c(2006:2025) ~ "2006-2025",
                                                year %in% c(2026:2050) ~ "2026-2050",
                                                year %in% c(2051:2075) ~ "2051-2075",
                                                year %in% c(2076:2099) ~ "2076-2099")
                        ) %>%
                 data.table()
  }
  return(data_tb)
}

