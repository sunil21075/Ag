options(digits=9)
options(digit=9)

#############************************************************************
#############
############# This is the plot in the old version folder
#############
#############************************************************************
#############************************************************************

thresh_old_plots <- function(data, percentile){
  y = eval(parse(text =paste0( "data$", "thresh_", percentile, "_med")))
  lab = paste0("Median days to reach ", percentile , " accumulated chill units")
  the_plot <- ggplot(data = data) +
              geom_point(aes(x = year, y = y, color = scenario)) +
              geom_smooth(aes(x = year, y = y, color = scenario),
                          method = "lm", size=0.5, se = F) +
              facet_wrap( ~ climate_type) +
              scale_color_viridis_d(option = "plasma", begin = .4, end = .7, 
                                    name = "Scenario") +
              ylab("Median days") +
              xlab("Year") +
              ggtitle(label = lab,
                      subtitle = "by cool/warm location and climate scenario") +
              theme_bw() + 
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5)) 

  return (the_plot)
}

# Threshold plots

# Two plots, each plotting only median model points
# For now not exporting this plot but leaving the code here

# Take a median within each year, collapsing 295 locations to just 2 groups
# (cool/warm) and collapsing all models (removing observed hist)
# but retaining scenarios.

summary_comp_medians <- summary_comp %>%
                        filter(model != "observed", scenario != "historical") %>%
                        group_by(climate_type, year, scenario) %>%
                        summarise_at(.funs = funs(med = median), vars(thresh_20:sum_A1))

thresh_50_plot <- thresh_old_plots(summary_comp_medians, percentile="50")
thresh_75_plot <- thresh_old_plots(summary_comp_medians, percentile="75")

thresh_figs_medians <- ggarrange(thresh_50_plot,
                                 thresh_75_plot,
                                 ncol = 1, nrow = 2)

#ggsave(plot = thresh_figs_medians, "chill-plot_thresholds.png",
#       height = 15, width = 8, units = "in")

#############************************************************************
#############
############# Above This Line is the old version plot
#############
#############************************************************************
#############************************************************************