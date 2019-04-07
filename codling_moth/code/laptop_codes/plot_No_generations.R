rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

########################################################################################

plot_No_generations <- function(input_dir, file_name,
                                stage, dead_line,
                                box_width=.7, plot_with = 6.5, plot_height = 2.5,
                                plot_path, version,
                                color_ord = c("grey47", "dodgerblue", "olivedrab4", "red")
                                ){
  if (version=="rcp45"){ title <- "RCP 4.5" } else {title <- "RCP 8.5"}

  file_name <- paste0(input_dir, file_name)
  data <- data.table(readRDS(file_name))
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  
  if (stage=="Larva"){ var = "NumLarvaGens" } else { var = "NumAdultGens" }

  data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
  ######
  ###### Compute medians of each group to annotate in the plot, if possible!!!
  ######
  df <- data.frame(data)
  df <- (df %>% group_by(CountyGroup, ClimateGroup))
  medians <- (df %>% summarise(med = median(!!sym(var))))
  rm(df)
  y_lab = paste0("No. of generations")

  box_plot = ggplot(data = data, aes(x = ClimateGroup, y = !!sym(var), fill = ClimateGroup)) + 
             geom_boxplot(outlier.size=-.15, lwd=0.25, notch=F, width=box_width) +
             # The bigger the number in expand below, the smaller the space between y-ticks
             scale_x_discrete(expand=c(0, 2), limits = levels(data$ClimateGroup[1])) +
             scale_y_continuous(limits = c(.5, 4), breaks=seq(1, 5, by=1)) + 
             labs(x="", y=y_lab, color = "Climate Group") +
             facet_wrap(~CountyGroup) +
             theme(plot.title = element_text(size=30, face="bold"),
                   plot.margin = margin(t=1, r=0.5, b=0, l=0.1, unit = 'cm'),
                   legend.position="bottom", 
                   legend.key.size = unit(3, "line"),
                   legend.margin=margin(t=.5, r=0, b=0, l=0, unit='cm'),
                   legend.spacing.x = unit(.05, 'cm'),
                   legend.title = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.spacing=unit(.5, "cm"),
                   strip.text = element_text(size=25, face = "bold"),
                   legend.text=element_text(size=25),
                   axis.ticks = element_line(color = "black", size = .2),
                   axis.text.x = element_text(size =20, face = "bold", color="black"),
                   axis.title.x = element_text(size=25, face = "bold",
                                               margin = margin(t=5, r=0, b=0, l=0)),
                  
                   axis.title.y = element_text(face = "bold", size=8, 
                                               margin=margin(t=0, r=1.5, b=0, l=0)),
                   axis.text.y  = element_blank(),
                   axis.ticks.y = element_blank()
                  ) +
              scale_fill_manual(values=color_ord, name="Time\nPeriod") + 
              scale_color_manual(values=color_ord, name="Time\nPeriod", limits = color_ord) + 
              coord_flip() +
              ggtitle(label = title)

  return(box_plot)
}

########################################################################################
input_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
plot_path = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
setwd(input_dir)

color_ord <- c("grey47", "dodgerblue", "olivedrab4", "red")

stages = c("Larva", "Adult")
dead_lines = c("Aug") # , "Nov"
versions = c("rcp45", "rcp85")

file_pref = "generations_" 
file_mid = "_combined_CMPOP_"
file_end = ".rds"

plot_with = 6.5
plot_height = 2.5

for (dead_line in dead_lines){
  for (version in versions){
    file_name = paste0(file_pref, dead_line, file_mid)
    file_name = paste0(file_name, version, file_end)
    for (stage in stages){
      p <- plot_No_generations(input_dir, file_name,
                               stage, dead_line = dead_line,
                               box_width=.7, plot_with = plot_with, 
                               plot_height= plot_height, plot_path,
                               version=version, color_ord)
      assign(x = paste0(stage, "_", dead_line, "_", version), value ={p})
            
        }
    }
}

larva <- ggpubr::ggarrange(plotlist = list(Larva_Aug_rcp45, Larva_Aug_rcp85),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend = "bottom")


if (dead_line=="Aug"){
    plot_title <- paste0("Number of larva generations by Aug. 23")
  } else{
    plot_title <- paste0("Number of larva generations by Nov. 5")
}

A <- annotate_figure(larva,
                     top = text_grob(plot_title, color = "black", face = "bold", size = 35)
                     # fig.lab = plot_title, fig.lab.size = 35, fig.lab.face = "bold", fig.lab.pos = c("top.left")
                     )

plot_dpi <- 300
ggsave("larva_gen_aug.png", A, path="./", width=20, height=7, unit="in", dpi=400)


adult <- ggpubr::ggarrange(plotlist = list(Adult_Aug_rcp45, Adult_Aug_rcp85),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend = "bottom")

if (dead_line=="Aug"){
    plot_title <- paste0("Number of adult generations by Aug. 23")
  } else{
    plot_title <- paste0("Number of adult generations by Nov. 5")
}

A <- annotate_figure(adult,
                     top = text_grob(plot_title, color = "black", face = "bold", size = 35)
                     # fig.lab = plot_title, fig.lab.size = 35, fig.lab.face = "bold", fig.lab.pos = c("top.left")
                     )

plot_dpi <- 300
ggsave("adult_gen_aug.png", A, path="./", width=20, height=7, unit="in", dpi=400)



