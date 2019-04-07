.libPaths("/data/hydro/R_libs35")
.libPaths()
library(tidyverse)
library(lubridate)

rm(list=ls())
library(data.table)
# library(ggplot2)
library(ggpubr)
library(dplyr)

start_time <- Sys.time()

input_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/4_cumdd/"
# input_dir = "/data/hydro/users/Hossein/codling_moth_new/local/processed/overlaping/cumdd_data/"
version = c("rcp45", "rcp85")

setwd(input_dir)

color_ord <- c("grey47", "dodgerblue", "olivedrab4", "red")
time_periods <- c("Historical", "2040's","2060's","2080's")

for (vers in version){
  print (vers)
  if (vers == "rcp45"){
    plot_title <- "RCP 4.5"
    } else{
      plot_title <- "RCP 8.5"
  }
  
  data = readRDS(paste0("./", "cumdd_CMPOP_", vers, ".rds"))
  data = subset(data, select = c("ClimateGroup", "CountyGroup", "dayofyear", "CumDDinF"))
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  # add the new season column
  data[, season := as.character(0)]
  data[data[ , data$dayofyear <= 90]]$season = "Qr. 1"
  data[data[ , data$dayofyear >= 91 & data$dayofyear <= 181]]$season = "Qr. 2"
  data[data[ , data$dayofyear >= 182 & data$dayofyear <= 273]]$season = "Qr. 3"
  data[data[ , data$dayofyear >= 274]]$season = "Qr. 4"
  data = within(data, remove(dayofyear))
  data$season = factor(data$season, levels = c("Qr. 1", "Qr. 2", "Qr. 3", "Qr. 4"))

  #df <- data.frame(data)
  #df <- (df %>% group_by(CountyGroup, ClimateGroup, season))
  #medians <- (df %>% summarise(med = median(CumDDinF)))
  #medians <- medians$med
  #rm(df)

  data = melt(data, id = c("ClimateGroup", "CountyGroup", "season"))
  data = within(data, remove(variable))
  
  bplot <- ggplot(data = data, aes(x=season, y=value), group = season) + 
           geom_boxplot(outlier.size=-.15, notch=FALSE, width=.5, lwd=.25, aes(fill=ClimateGroup), 
                        position=position_dodge(width=0.7)) + 
           scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 1000)) + 
           facet_wrap(~CountyGroup, scales="free", ncol=6, dir="v") + 
           labs(x="", y="", color = "Climate Group") + 
           theme(plot.title = element_text(size = 35, face="bold"),
                 legend.position="bottom", 
                 legend.margin=margin(t=-.1, r=0, b=.3, l=0, unit = 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size=35, face="plain"),
                 legend.key.size = unit(2, "cm"), 
                 legend.spacing.x = unit(0.5, 'cm'),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(size = 0.1),
                 panel.grid.minor.y = element_line(size = 0.1),
                 strip.text = element_text(size= 30, face = "bold"),
                 axis.text.x = element_text(size = 25, face="bold", color="black"), # tick text font size
                 axis.text.y = element_text(size = 25, face="bold", color="black"),
                 axis.title.y= element_text(size = 35, face = "bold", 
                                            margin = margin(t=0, r=20, b=0, l=0)) #,
                 # plot.margin = unit(c(t=0.1, r=.7, b=-.1, l=0.3), "cm")
                ) + 
           scale_fill_manual(values=color_ord,
                             name="Time\nPeriod", 
                             labels = time_periods) + 
           scale_color_manual(values = color_ord,
                              name="Time\nPeriod", 
                              limits = color_ord,
                              labels = time_periods) +
           #scale_x_discrete(breaks = NULL) + 
           ggtitle(label = plot_title)
  
  assign(x = paste0("cumdd_plot_", vers), value ={bplot})
  rm(bplot)
}
rm(data)

cumdd_plot_qrt <- ggpubr::ggarrange(plotlist = list(cumdd_plot_rcp45, cumdd_plot_rcp85),
                                    ncol = 2, nrow = 1,
                                    common.legend = TRUE, legend = "bottom")

A <- annotate_figure(cumdd_plot_qrt,
                     left = text_grob("cumulative degree day", face = "bold", 
                                      size = 35, color = "black", rot = 90)
                    )

ggsave("cumdd_qrt_sep_300.png", A, width=30, height=8, unit="in", path="./", dpi=300, device="png")
ggsave("cumdd_qrt_sep_350.png", A, width=30, height=8, unit="in", path="./", dpi=350, device="png")
ggsave("cumdd_qrt_sep_400.png", A, width=30, height=8, unit="in", path="./", dpi=400, device="png")

print( Sys.time() - start_time)



