rm(list=ls())
library(data.table)
library(dplyr)
#library(ggplot2)
library(ggpubr)
################################################################################################

input_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
plot_path = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
setwd(input_dir)
# color_ord <- c("grey47", "dodgerblue", "olivedrab4", "red")

file_name = "combined_CM_rcp45.rds"
plot_output_name = "adult_emergence_rcp45"


emissions <- c("RCP 4.5", "RCP 8.5")
time_periods <- c("Historical", "2040's","2060's","2080's")
time_periods_n <- c("Historical", "2026-2050","2051-2075","2076-2095")

color_ord <- c("grey47", "dodgerblue", "olivedrab4", "red")
plot_dpi <- 350
box_width = 0.7
#
# These plots are produced by combined_CM files.
#

the_theme <- theme(plot.title = element_text(size=30, face="bold"),
                   plot.margin = margin(t=1, r = 0.5, b = 0, l=0.1, unit = 'cm'),
                   panel.grid.minor = element_blank(),
                   panel.spacing=unit(.5, "cm"),
                   legend.margin=margin(t=.5, r = 0, b = 0, l = 0, unit = 'cm'),
                   legend.title = element_blank(),
                   legend.position="bottom", 
                   legend.key.size = unit(3, "line"),
                   legend.spacing.x = unit(.05, 'cm'),
                   panel.grid.major = element_line(size = 0.1),
                   axis.ticks = element_line(color="black", size = .2),
                   strip.text = element_text(size=25, face = "bold"),
                   legend.text=element_text(size=25),
                   axis.title.x = element_text(size=25, face = "bold",  margin = margin(t=8, r=0, b=0, l=0)),    
                   axis.text.x = element_text(size= 20, face = "bold", color="black"),
                   axis.title.y = element_blank(),
                   axis.text.y  = element_blank(),
                   axis.ticks.y = element_blank()
                  ) 

for (em in c("rcp45", "rcp85")){
  if (em=="rcp45"){plot_title <- "RCP 4.5"} else {plot_title <- "RCP 8.5"}
  data <- data.table(readRDS(paste0("combined_CM_", em, ".rds")))
  data <- subset(data, select = c("Emergence", "ClimateGroup", 
                                  "ClimateScenario", 
                                  "CountyGroup"))

  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'

  data = data[, .(Emergence = Emergence),
        by = c("ClimateGroup", "CountyGroup")]

  data <- subset(data, select = c("ClimateGroup", "CountyGroup", "Emergence"))

  ###### Compute medians of each group to annotate in the plot, if possible!!!

  df <- data.frame(data)
  df <- (df %>% group_by(CountyGroup, ClimateGroup))
  medians <- (df %>% summarise(med = median(Emergence)))
  medians_vec <- medians$med

  p = ggplot(data = data, aes(x=ClimateGroup, y=Emergence, fill=ClimateGroup))+
      geom_boxplot(outlier.size=-.15, notch=FALSE, width=box_width, lwd=.25) +
      scale_x_discrete(expand=c(0, 2), limits = levels(data$ClimateGroup[1])) +
      scale_y_continuous(breaks = round(seq(40, 170, by = 20), 1)) +
      labs(x="Time period", y="Julian day", color = "Climate Group") +
      facet_wrap(~CountyGroup) +
      the_theme +
      scale_fill_manual(values=color_ord, name="Time\nPeriod", labels=time_periods) + 
      scale_color_manual(values=color_ord, name="Time\nPeriod", limits = color_ord, labels=time_periods) +
      coord_flip() + 
      ggtitle(label = plot_title)
  assign(x = paste0("adult_emerge_", em), value ={p})
}

adult_emerge <- ggpubr::ggarrange(plotlist = list(adult_emerge_rcp45, adult_emerge_rcp85),
                                  ncol = 2, nrow = 1,
                                  common.legend = TRUE, legend = "bottom")

A <- annotate_figure(adult_emerge, 
                     top = text_grob("Julian day of adult emergence", color="black", face="bold", size=35)
                     # fig.lab = "Julian day of adult emergence", fig.lab.size = 35, fig.lab.face = "bold")
                     )
ggsave("adult_emerge.png", A, path="./", width=20, height=7, unit="in", dpi=plot_dpi)





