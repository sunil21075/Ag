map_of_all_models_anlgs_freq_color <- function(a_dt, county2, title_p, target_county_map_info){
  title_p <- unlist(strsplit(title_p, " "))
  title_p <- title_p[-4]
  title_p <- paste(title_p, collapse=" ")

  count_of_county_rep <- a_dt %>% group_by(model) %>% count(analog_NNs_county)

  color_ord = c("grey47" , "dodgerblue", "olivedrab4", "yellow", "orange2", "blue4") # 
  county_fill_in = c("cyan3" , "dodgerblue", "olivedrab4", "yellow", "orange2", "blue4") # 
  
  categ_lab = c( "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M")
  
  cols_4_arrow <- c("long", "lat", "group", "order", "region", "subregion", "polyname")
  start_end_df <- target_county_map_info[1,]
  start_end_df <- start_end_df[colnames(start_end_df) %in% cols_4_arrow]

  for (modelsss in categ_lab){
    aaa <- a_dt %>% filter(model == modelsss)
    # avg_long <- mean(aaa$long); avg_lat <- mean(aaa$lat)
    aaa <- aaa[1,]
    # aaa$long <- avg_long; aaa$lat <- avg_lat
    aaa <- aaa[colnames(aaa) %in% cols_4_arrow]
    start_end_df <- rbind(start_end_df, aaa[1,])
  }

  count_of_counties <- a_dt %>% 
                      group_by(model) %>% 
                      count(analog_NNs_county) %>% 
                      group_by(analog_NNs_county) %>% 
                      count() %>% 
                      data.table()
  a_dt$county_count = 0L

  for (ii in seq(1:nrow(count_of_counties))){
    a_dt$county_count[a_dt$analog_NNs_county == as.integer(count_of_counties[ii, 1])] <- 
                                                  as.character(count_of_counties[ii, 2])
  }
  
  arrow_size = 0.01
  arrow_color <- "black"
  
  curr_plot <- ggplot(a_dt, aes(long, lat, group = group)) + 
               geom_polygon(data = county2, color="black", size=.05, fill="lightgrey") +
               geom_polygon(aes(fill = county_count), colour = rgb(1, 1, .11, .2), size = .01) +
               geom_polygon(data = target_county_map_info, color="red", size = .75, fill=NA) +
               borders("state") +
               coord_quickmap() + 
               theme(plot.title = element_text(size=18, face="bold"),
                     plot.subtitle = element_text(size=14, face="bold"),
                     plot.margin = unit(c(t=2, r=4, b=-1, l=0), "pt"),
                     legend.spacing.x = unit(5, 'pt'),
                     # legend.spacing.y = unit(1, 'cm'),
                     legend.title = element_blank(),
                     legend.position = c(.87, .2), # 
                     legend.background = element_rect(fill = "grey92"),
                     legend.key.size = unit(2, "line"),
                     legend.text = element_text(size=15, face="plain"),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank()) + 
               ggtitle(title_p) + 
               # scale_fill_continuous(limits = c(1, 6), breaks = 1:6,
               #                        guide = guide_colourbar(nbin=6, draw.ulim = TRUE, 
               #                                                draw.llim = TRUE)) + 
               scale_fill_manual(values = county_fill_in, name= "Repition", 
                                 guide = guide_legend(reverse = TRUE)) + 
               geom_curve(aes( x = start_end_df[1, "long"], y = start_end_df[1, "lat"], 
                               xend = start_end_df[2, "long"], yend = start_end_df[2, "lat"]), 
                          colour = "white", data = start_end_df,
                          arrow = arrow(length = unit(arrow_size, "npc")), 
                          curvature = compute_curvature (x = start_end_df[1, "long"], 
                                                         y = start_end_df[1, "lat"], 
                                                         xend = start_end_df[2, "long"], 
                                                         yend = start_end_df[2, "lat"])
                          ) + 
               geom_curve(aes( x = start_end_df[1, "long"], y = start_end_df[1, "lat"], 
                               xend = start_end_df[3, "long"], yend = start_end_df[3, "lat"]), 
                          colour = arrow_color, data = start_end_df,
                          arrow = arrow(length = unit(arrow_size, "npc")), 
                          curvature = compute_curvature (x = start_end_df[1, "long"], 
                                                         y = start_end_df[1, "lat"], 
                                                         xend = start_end_df[3, "long"], 
                                                         yend = start_end_df[3, "lat"])
                          ) + 
               geom_curve(aes( x = start_end_df[1, "long"], y = start_end_df[1, "lat"], 
                               xend = start_end_df[4, "long"], yend = start_end_df[4, "lat"]), 
                          colour = arrow_color, data = start_end_df,
                          arrow = arrow(length = unit(arrow_size, "npc")), 
                          curvature = compute_curvature (x = start_end_df[1, "long"], 
                                                         y = start_end_df[1, "lat"], 
                                                         xend = start_end_df[4, "long"], 
                                                         yend = start_end_df[4, "lat"])
                          ) + 
               geom_curve(aes( x = start_end_df[1, "long"], y = start_end_df[1, "lat"], 
                               xend = start_end_df[5, "long"], yend = start_end_df[5, "lat"]), 
                          colour = arrow_color, data = start_end_df,
                          arrow = arrow(length = unit(arrow_size, "npc")), 
                          curvature = compute_curvature (x = start_end_df[1, "long"], 
                                                         y = start_end_df[1, "lat"], 
                                                         xend = start_end_df[5, "long"], 
                                                         yend = start_end_df[5, "lat"])
                          ) + 
               geom_curve(aes( x = start_end_df[1, "long"], y = start_end_df[1, "lat"], 
                               xend = start_end_df[6, "long"], yend = start_end_df[6, "lat"]), 
                          colour = arrow_color, data = start_end_df,
                          arrow = arrow(length = unit(arrow_size, "npc")), 
                          curvature = compute_curvature (x = start_end_df[1, "long"], 
                                                         y = start_end_df[1, "lat"], 
                                                         xend = start_end_df[6, "long"], 
                                                         yend = start_end_df[6, "lat"])
                          ) + 
               geom_curve( aes( x = start_end_df[1, "long"], y = start_end_df[1, "lat"], 
                               xend = start_end_df[7, "long"], yend = start_end_df[7, "lat"]), 
                           colour = arrow_color, data = start_end_df,
                           arrow = arrow(length = unit(arrow_size, "npc")), 
                           curvature = compute_curvature (x = start_end_df[1, "long"], 
                                                          y = start_end_df[1, "lat"], 
                                                          xend = start_end_df[7, "long"], 
                                                          yend = start_end_df[7, "lat"])
                          )
  
  return(curr_plot) 
}