rm(list=ls())
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)
plot_No_generations <- function(input_dir,
                                file_name,
                                stage,
                                dead_line,
                                box_width=.25,
                                plot_path,
                                version,
                                color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
){
  # stage: either larva or adult
  # version either rcp45 or rcp85
  # 
  # This function does not run on Aeolus with R.3.2.2. 
  # I will produce it on my computer.
  #
  file_name <- paste0(input_dir, file_name)
  data <- data.table(readRDS(file_name))
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  
  if (stage=="Larva"){var = "NumLarvaGens"
  } else {var = "NumAdultGens"}
  
  data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
  ######
  ###### Compute medians of each group to annotate in the plot, if possible!!!
  ######
  df <- data.frame(data)
  df <- (df %>% group_by(CountyGroup, ClimateGroup))
  medians <- (df %>% summarise(med = median(!!sym(var))))
  rm(df)
  if (dead_line=="Aug"){
    y_lab = paste0("Number of ", stage, " Generations by August 23")
  }
  else{
    y_lab = paste0("Number of ", stage, " Generations by November 5")
  }
  
  box_plot = ggplot(data = data, aes(x = ClimateGroup, y = !!sym(var), fill = ClimateGroup)) + 
    geom_boxplot( outlier.size=0, notch=TRUE, width=.2) +
    # The bigger the number in expand below, the smaller the space between y-ticks
    scale_x_discrete(expand=c(0, 3), limits = levels(data$ClimateGroup[1])) +
    scale_y_continuous(limits = c(.5, 4), breaks=seq(1, 5, by=1)) + 
    theme_bw() +
    labs(x="Time Period", 
         y=y_lab, 
         color = "Climate Group") +
    facet_wrap(~CountyGroup) +
    theme(legend.position="bottom", 
          legend.key.size = unit(.75,"line"),
          legend.text=element_text(size=5),
          legend.margin=margin(t=-.1, r=0, b=0, l=0, unit='cm'),
          # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.title = element_blank(),
          #panel.grid.major = element_line(size = 0.1),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(face = "plain", size = 10),
          axis.text.x = element_text(size = 7),
          axis.title.x = element_text(face = "plain", 
                                      size=8, 
                                      margin = margin(t=2, r=0, b=0, l=0)),
          
          axis.title.y = element_text(face = "plain", 
                                      size=8, 
                                      margin=margin(t=0, r=1.5, b=0, l=0)),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank()
    ) +
    scale_fill_manual(values=color_ord, name="Time\nPeriod") + 
    scale_color_manual(values=color_ord, name="Time\nPeriod", limits = color_ord) + 
    geom_text(data = medians, 
              aes(label = sprintf("%1.1f", medians$med), y=medians$med), 
              size=1.75, 
              position =  position_dodge(.09),
              vjust = -1) +
    coord_flip()
  
  plot_name = paste0(stage, "_Gen_", dead_line, "_", version)
  ggsave(paste0(plot_name, ".png"), 
         box_plot, 
         path=plot_path, 
         device="png", 
         width=6.5, height=2.5, units = "in")
}
########################################################################################

input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/"
plot_path = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/"
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")

stages = c("Larva", "Adult")
dead_lines = c("Aug", "Nov")
versions = c("rcp45", "rcp85")

file_pref = "generations_" 
file_mid = "_combined_CMPOP_"
file_end = ".rds"

for (dead_line in dead_lines){
  for (version in versions){
    file_name = paste0(file_pref, dead_line, file_mid)
    file_name = paste0(file_name, version, file_end)
    for (stage in stages){
      plot_No_generations(input_dir,
                          file_name,
                          stage,
                          dead_line = dead_line,
                          box_width=.25,
                          plot_path,
                          version=version,
                          color_ord)
    }
  }
}