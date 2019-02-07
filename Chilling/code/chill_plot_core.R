rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

stupid_plot <- function(data, weat_type, write_dir){
	# iof = interval of interest
	iof = c(c(-Inf, -2), c(-2, 4), 
	        c(4, 6), c(6, 8), 
	        c(8, 13), c(13, 16), 
	        c(16, Inf))
    iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)

    # These are the order of months in climate calendar!
	month_no = c(9, 10, 11, 12, 
		         1, 2, 3, 4, 
		         5, 6, 7, 8)
	month_name = c("Jan", "Feb", "Mar", "Apr", 
		           "May", "Jun", "Jul", "Aug" ,
		           "Sept", "Oct", "Nov", "Dec")
	
	data <- data %>% 
            mutate(temp_cat=cut(Temp, breaks=iof_breaks)) %>% 
            group_by(Chill_season, Year, Month, climateScenario, temp_cat) %>% 
            summarise(no_hours = n())

	data = subset(data, select=c("Year", "temp_cat", "no_hours"))

	for(scenario in unique(data$climateScenario)) {
		for(month in month_no){ 
			assign(x = paste(weat_type, scenario, month_name[month], sep="_"),
		           value = { model_month_plot(filter(data, Month==month & climateScenario==scenario), 
		           	                          scenario_name = scenario, month=month)}
		           )
		}
	}
	thresh50_45_figs <- ggarrange(plotlist = list(observed_map_thresh50_45,
                                              ensemble_map_thresh50_45,
                                              bcc_csm1_1_m_map_thresh50_45,
                                              bcc_csm1_1_map_thresh50_45,
                                              BNU_ESM_map_thresh50_45,
                                              CanESM2_map_thresh50_45,
                                              CCSM4_map_thresh50_45, 
                                              CNRM_CM5_map_thresh50_45,
                                              CSIRO_Mk3_6_0_map_thresh50_45,
                                              GFDL_ESM2G_map_thresh50_45,
                                              GFDL_ESM2M_map_thresh50_45,
                                              HadGEM2_CC365_map_thresh50_45,
                                              HadGEM2_ES365_map_thresh50_45,
                                              inmcm4_map_thresh50_45,
                                              IPSL_CM5A_LR_map_thresh50_45, 
                                              IPSL_CM5A_MR_map_thresh50_45,
                                              IPSL_CM5B_LR_map_thresh50_45,
                                              MIROC_ESM_CHEM_map_thresh50_45,
                                              MIROC5_map_thresh50_45, 
                                              MRI_CGCM3_map_thresh50_45,
                                              NorESM1_M_map_thresh50_45),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)
}

model_month_plot <- function(data, scenario_name, month_name) {
	the_theme = theme(legend.position="bottom", 
                      legend.margin=margin(t=-.1, r=0, b=5, l=0, unit = 'cm'),
                      legend.title = element_blank(),
                      legend.text = element_text(size=7, face="plain"),
                      legend.key.size = unit(.5, "cm"), 
                      panel.grid.major = element_line(size = 0.1),
                      panel.grid.minor = element_line(size = 0.1),
                      panel.spacing=unit(.5, "cm"),
                      strip.text = element_text(size= 6, face = "plain"),
                      axis.text = element_text(face = "plain", size = 4, color="black"),
                      axis.ticks = element_line(color = "black", size = .2),
                      axis.title.x = element_text(face = "plain", size = 10, 
                                                  margin = margin(t=10, r=0, b=0, l=0)),
                      axis.text.x = element_text(size = 6, color="black"), # tick text font size
                      axis.title.y = element_text(face = "plain", size = 10, 
	                                              margin = margin(t=0, r=7, b=0, l=0)),
                      axis.text.y  = element_blank(),
                      axis.ticks.y = element_blank(),
                      plot.margin = unit(c(t=-0.35, r=.7, b=-4.7, l=0.3), "cm")
                      ) + theme_bw()
	ggplot(data, aes(x=Year, y=no_hours)) + 
    geom_point() +
    the_theme + 
    facet_wrap(~temp_cat) + facet_grid(cols=vars(data$temp_cat)) + 
    ggtitle(paste(scenario_name, month_name, sep=" - "))
}

# this is useful for observed data
model_plot_double_facet <- function(data, scenario_name) {
	the_theme = theme(legend.position="bottom", 
                      legend.margin=margin(t=-.1, r=0, b=5, l=0, unit = 'cm'),
                      legend.title = element_blank(),
                      legend.text = element_text(size=7, face="plain"),
                      legend.key.size = unit(.5, "cm"), 
                      panel.grid.major = element_line(size = 0.1),
                      panel.grid.minor = element_line(size = 0.1),
                      panel.spacing=unit(.5, "cm"),
                      strip.text = element_text(size= 6, face = "plain"),
                      axis.text = element_text(face = "plain", size = 4, color="black"),
                      axis.ticks = element_line(color = "black", size = .2),
                      axis.title.x = element_text(face = "plain", size = 10, 
                                                  margin = margin(t=10, r=0, b=0, l=0)),
                      axis.text.x = element_text(size = 6, color="black"), # tick text font size
                      axis.title.y = element_text(face = "plain", size = 10, 
	                                              margin = margin(t=0, r=7, b=0, l=0)),
                      axis.text.y  = element_blank(),
                      axis.ticks.y = element_blank(),
                      plot.margin = unit(c(t=-0.35, r=.7, b=-4.7, l=0.3), "cm")
                      ) + theme_bw()
	ggplot(data, aes(x=Year, y=no_hours)) + 
    geom_point() +
    the_theme + 
    facet_wrap(. ~ temp_cat ~ Month)  + 
    ggtitle(scenario_name)
}