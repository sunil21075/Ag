rm(list=ls())
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)

plot_adult_emergence <- function(input_dir, file_name, 
                                 box_width=.25, plot_path, output_name, 
                                 color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
){
  #
  # These plots are produced by combined_CM files.
  #
  output_name = paste0(output_name, ".png")
  file_name <- paste0(input_dir, file_name)
  data <- data.table(readRDS(file_name))
  data <- subset(data, select = c("Emergence", "ClimateGroup", 
                                  "ClimateScenario", 
                                  "CountyGroup"))
  
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  
  data = data[, .(Emergence = Emergence),
              by = c("ClimateGroup", "CountyGroup")]
  
  data <- subset(data, select = c("ClimateGroup", "CountyGroup", "Emergence"))
  ######
  ###### Compute medians of each group to annotate in the plot, if possible!!!
  ######
  df <- data.frame(data)
  df <- (df %>% group_by(CountyGroup, ClimateGroup))
  medians <- (df %>% summarise(med = median(Emergence)))
  medians_vec <- medians$med
  
  p = ggplot(data = data, aes(x=ClimateGroup, y=Emergence, fill=ClimateGroup))+
      geom_boxplot(outlier.size=0, notch=TRUE, width=.2) +
      theme_bw() +
      scale_x_discrete(expand=c(0, 2), limits = levels(data$ClimateGroup[1])) +
    scale_y_continuous(breaks = round(seq(40, 170, by = 10), 1)) +
    labs(x="Time Period", y="Day of Year", color = "Climate Group") +
    facet_wrap(~CountyGroup) +
    theme(legend.position="bottom", 
          legend.key.size = unit(.75,"line"),
          panel.grid.minor = element_blank(),
          panel.spacing=unit(.5,"cm"),
          legend.text=element_text(size=5),
          legend.margin=margin(t = -.1, r = 0, b = 0, l = 0, unit = 'cm'),
          # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.title = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          axis.text = element_text(face = "plain", size = 10),
          axis.title.x = element_text(face = "plain", size = 8, margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size = 5),
          axis.title.y = element_text(face = "plain", size = 8, margin = margin(t = 0, r = 1, b = 0, l = 0)),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank()
    ) +
    scale_fill_manual(values=color_ord,
                      name="Time\nPeriod", 
                      labels=c("Historical","2040","2060","2080")) + 
    scale_color_manual(values=color_ord,
                       name="Time\nPeriod", 
                       limits = color_ord,
                       labels=c("Historical","2040","2060","2080")) + 
    geom_text(data = medians, 
              aes(label = sprintf("%1.0f", medians$med), y=medians$med), 
              size=1.5, 
              position =  position_dodge(.09),
              vjust = -1.5) +
    #stat_summary(geom="text", fun.y=quantile,
    #             aes(label=sprintf("%1.1f", ..x..), color=factor(ClimateGroup)),
    #             position=position_nudge(x=0.33), size=.5) +
    coord_flip()
  ggsave(output_name, p, path=plot_path, width=5.5, height=3.1, unit="in")
}

################################################################################################

input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
plot_path = "/Users/hn/Desktop/"
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
box_width = 0.25 

file_name = "combined_CM_rcp45.rds"
plot_output_name = "adult_emergence_rcp45"
plot_adult_emergence(input_dir=input_dir, file_name, box_width=.25, plot_path, plot_output_name)


file_name = "combined_CM_rcp85.rds"
plot_output_name = "adult_emergence_rcp85"
plot_adult_emergence(input_dir=input_dir, file_name, box_width=.25, plot_path, plot_output_name)
