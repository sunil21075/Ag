#!/share/apps/R-3.2.2_gcc/bin/Rscript
library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)

################################################################################
## *****************               Plotting Codes          *********************
################################################################################


##########################################
####### Bloom Plots
##########################################
plot_bloom_filling <- function(data_dir, file_name = "vertdd_combined_CMPOP_", version, plot_path, output_name, x_limits = c(45, 150)){
  output_name = paste0(output_name, "_", version, ".png")
  filename <- paste0(data_dir, file_name, version, ".rds")

  data <- data.table(readRDS(filename))
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'

  d1 = subset(data, select = c("latitude", "longitude", 
                               "CountyGroup", "ClimateGroup", "ClimateScenario", 
                               "year", "month", "day", "dayofyear", 
                               "vert_Cum_dd_F", 
                               "cripps_pink", "gala", "red_deli"))

  d1 = melt(d1, id = c("latitude", "longitude", 
                       "CountyGroup", "ClimateGroup", "ClimateScenario", 
                       "year", "month", "day", "dayofyear", 
                       "vert_Cum_dd_F"))

  d1[variable == "red_deli"]$variable = "Red Delicious"
  d1[variable == "gala"]$variable = "Gala"
  d1[variable == "cripps_pink"]$variable = "Cripps Pink"

  p1 = ggplot(d1, aes(x=dayofyear, y=value, fill=factor(ClimateGroup))) +
                  stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, fun.ymin=function(z) { quantile(z,0.1) }, fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.3) +
                  stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, fun.ymin=function(z) { quantile(z,0.25) }, fun.ymax=function(z) { quantile(z,0.75) }, alpha=0.7) +
                  stat_summary(geom="line", fun.y=function(z) { quantile(z,0.5) }, size = 1)+
                  scale_color_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+
                  scale_fill_manual(values =c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+
                  facet_grid(. ~ variable ~ CountyGroup, scales = "free") +
                  scale_x_continuous(breaks=seq(x_limits[1], x_limits[2], 10), limits = x_limits) +
                  theme_bw() +
                  labs(x = "Julian Day", y = "Proportion Full Bloom Completed", fill = "Climate Group") +
                  theme(panel.grid.major = element_line(size = 0.7),
                        legend.title = element_text(face = "plain", size = 16),
                        legend.text = element_text(size = 12),
                        legend.position = "top",
                        strip.text = element_text(size=12, face = "plain"),
                        axis.text = element_text(face="plain", size = 10),
                        axis.title.x = element_text(face= "plain", size = 16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.title.y = element_text(face="plain", size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0))
                    )
  ggsave(output_name, p1, path=plot_path)
}

plot_bloom <- function(data_dir, file_name = "vertdd_combined_CMPOP_", version, plot_path, output_name, x_limits = c(45, 150)){
  output_name = paste0(output_name, "_", version, ".png")
  filename <- paste0(data_dir, file_name, version, ".rds")
  
  data <- data.table(readRDS(filename))
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'

  d1 = subset(data, select = c("latitude", "longitude", 
                               "CountyGroup", "ClimateGroup", "ClimateScenario", 
                               "year", "month", "day", "dayofyear", 
                               "vert_Cum_dd_F", 
                               "cripps_pink", "gala", "red_deli"))

  d1 = melt(d1, id = c("latitude", "longitude", 
                       "CountyGroup", "ClimateGroup", "ClimateScenario", 
                       "year", "month", "day", "dayofyear", 
                       "vert_Cum_dd_F"))

  d1[variable == "red_deli"]$variable = "Red Delicious"
  d1[variable == "gala"]$variable = "Gala"
  d1[variable == "cripps_pink"]$variable = "Cripps Pink"

  p1 = ggplot(d1, aes(x=dayofyear, y=value, fill=factor(ClimateGroup))) +
    #stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, fun.ymin=function(z) { quantile(z,0.1) }, fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.3) +
    #stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, fun.ymin=function(z) { quantile(z,0.25) }, fun.ymax=function(z) { quantile(z,0.75) }, alpha=0.7) +
    stat_summary(geom="line", fun.y=function(z) { quantile(z,0.5) }, aes(color=factor(ClimateGroup)), size = 1)+ #, aes(color=factor(Timeframe))) + , # aes(color=factor(ClimateGroup))
    scale_color_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
    #scale_fill_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
    facet_grid(. ~ variable ~ CountyGroup, scales = "free") +
    #xlim(45, 165) +
    scale_x_continuous(breaks=seq(x_limits[1], x_limits[2], 10), limits = x_limits) +
    theme_bw() +
    labs(x = "Julian Day", y = "Proportion Completing Full Bloom", color = "Climate Group") +
    theme(panel.grid.major = element_line(size = 0.7),
          # axis.title = element_text(face = "plain", size = 16, margin=margin(2)),
          legend.title = element_text(face = "plain", size = 16),
          legend.text = element_text(size = 12),
          legend.position = "top",
          #plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          strip.text = element_text(size = 12, face = "plain"),
          axis.text = element_text(face = "plain", size = 10),
          axis.title.x = element_text(face = "plain", size = 16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face = "plain", size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0))
    )

  ggsave(output_name, p1, path=plot_path)
}

#####################
#######
#####################

plot_cumdd_eggHatch <- function(input_dir, file_name ="combined_CMPOP_", version, output_dir, output_type="cumdd"){
  out_name = paste0("plot_", output_type, "_", version ,".png")

  if (output_type == "eggHatch"){
    data = compute_cumdd_eggHatch(input_dir=data_dir, file_name="combined_CMPOP_", version)
    data$CountyGroup = as.character(data$CountyGroup)
    data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
    data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
    data = melt(data, id = c("ClimateGroup", "CountyGroup", "latitude", "longitude", "ClimateScenario", "year", "dayofyear"))
    plot = ggplot(data[value >=0.01 & value <.98 & dayofyear <300], aes(x=dayofyear, y=value, fill=factor(variable))) +
    #geom_line(aes(fill=factor(Timeframe), color=factor(Timeframe) )) +
    stat_summary(geom="ribbon", 
                 fun.y=function(z) { quantile(z,0.5) }, 
                 fun.ymin=function(z) { quantile(z,0.1) }, 
                 fun.ymax=function(z) { quantile(z,0.9) }, 
                 alpha=0.3) +
    stat_summary(geom="ribbon", 
                 fun.y=function(z) { quantile(z,0.5) }, 
                 fun.ymin=function(z) { quantile(z,0.25) }, 
                 fun.ymax=function(z) { quantile(z,0.75) }, 
                 alpha=0.8) + 
    stat_summary(geom="line", 
                 fun.y=function(z) { quantile(z,0.5) })+ #, aes(color=factor(Timeframe))) + 
    
    scale_color_manual(values=c(rgb(29, 67, 111, max=255), 
                                rgb(92, 160, 201, max=255), 
                                rgb(211, 91, 76, max=255), 
                                rgb(125, 7, 37, max=255))) + #c("black", "red","darkgreen","blue")) +
    scale_fill_manual(values=c(rgb(29, 67, 111, max=255), 
                               rgb(92, 160, 201, max=255), 
                               rgb(211, 91, 76, max=255), 
                               rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
    
    facet_grid(. ~ ClimateGroup ~ CountyGroup, scales="free") +
    scale_x_continuous(breaks=seq(0, 300, 50)) +
    theme_bw() +
    geom_vline(xintercept=c(100, 150, 200, 250, 300), linetype="solid", color ="grey")+
    geom_vline(xintercept=c(120, 226), linetype="solid", color ="red")+
    geom_vline(xintercept=seq(70,   300, 10), linetype="dotdash")+
    geom_hline(yintercept=c(.25, .5, .75), linetype="dotted", color = "black")+
    labs(x = "Julian Day", y = "Cumulative Population Fraction", fill = "Larva Generation") +
    theme(
      panel.grid.major = element_line(size = 0.7),
      legend.title = element_text(face = "plain", size = 16),
      legend.text = element_text(size = 12),
      legend.position = "top",
      strip.text = element_text(size=12, face = "plain"),
      axis.text = element_text(face= "plain", size = 10),
      axis.title.x = element_text(face= "plain", size = 16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(face= "plain", size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0)))
  
  ggsave(out_name, plot, path=output_dir)
  #saveRDS(plot, "cumdd_plot.rds")
  }

  #############################################
  ### cumdd
  #############################################
  if (output_type == "cumdd"){
    filename = paste0(input_dir, file_name, version, ".rds")
    data <- data.table(readRDS(filename))

    data$CountyGroup = as.character(data$CountyGroup)
    data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
    data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
    data = subset(data, select = c("ClimateGroup", "CountyGroup", 
                                   "latitude", "longitude", 
                                   "ClimateScenario", 
                                   "year", "dayofyear", "CumDDinF"))
    data$CumDD = data$CumDDinF

    data <- data[, .(CumDD = median(CumDDinF)), by = c("CountyGroup", "latitude", "longitude", "ClimateScenario", "ClimateGroup", "dayofyear")]
    # data <- data[, .(CumDD =   mean(CumDDinF)), by = c("CountyGroup", "latitude", "longitude", "ClimateScenario", "ClimateGroup", "dayofyear")]
    ###################
    ###################   rcp85
    ###################
    if (version == "rcp85"){
      plot = ggplot(data, aes(x=dayofyear, y=CumDD, fill=factor(ClimateGroup))) +
        #geom_line(aes(fill=factor(Timeframe), color=factor(Timeframe) )) +
        stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                                    fun.ymin=function(z) { quantile(z,0.1) }, 
                                    fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.4) +
        stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                                    fun.ymin=function(z) { quantile(z,0.25) }, 
                                    fun.ymax=function(z) { quantile(z,0.75) }, alpha=0.8) + 
        stat_summary(geom="line", fun.y=function(z) { quantile(z,0.5) })+ #, aes(color=factor(Timeframe))) + 
        
        scale_color_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), 
                                    rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
        scale_fill_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), 
                                   rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
        
        facet_grid(. ~ CountyGroup, scales="free") +
        scale_x_continuous(breaks=seq(0, 370, 50)) +
        scale_y_continuous(breaks=seq(0, 5750, 500)) +
        #scale_y_continuous(breaks=seq(0,4500,250)) +
        theme_bw() +
        #geom_vline(xintercept=c(100,150,200,250,300), linetype="solid", color ="grey")+
        #geom_vline(xintercept=c(120,226), linetype="solid", color ="red")+
        #geom_vline(xintercept=seq(70,300,10), linetype="dotdash")+
        #geom_hline(yintercept=c(.25,.5,.75), linetype="dotted", color = "black")+
        labs(x = "Julian Day", y = "Cumulative Degree Days (in F)", fill = "Climate Group") +
        theme(
          panel.grid.major = element_line(size = 0.7),
          legend.title = element_text(face = "plain", size = 16),
          legend.text = element_text(size = 12),
          legend.position = "top",
          strip.text = element_text(size = 12, face = "plain"),
          axis.text = element_text(face = "plain", size = 10),
          axis.title.x = element_text(face = "plain", size = 16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face = "plain", size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0)))
        ###################
        ###################   rcp45
        ###################
        } else if (version == "rcp45"){
          plot = ggplot(data, aes(x=dayofyear, y=CumDD, fill=factor(ClimateGroup))) +
          #geom_line(aes(fill=factor(Timeframe), color=factor(Timeframe) )) +
          stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, fun.ymin=function(z) { quantile(z,0.1) }, fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.4) +
          stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, fun.ymin=function(z) { quantile(z,0.25) }, fun.ymax=function(z) { quantile(z,0.75) }, alpha=0.8) + 
          stat_summary(geom="line", fun.y=function(z) { quantile(z,0.5) })+ #, aes(color=factor(Timeframe))) + 
          
          scale_color_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
          scale_fill_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
          
          facet_grid(. ~ CountyGroup, scales="free") +
          scale_x_continuous(breaks=seq(0, 370, 50)) +
          scale_y_continuous(breaks=seq(0, 4500, 500)) +
          theme_bw() +
          #geom_vline(xintercept=c(100,150,200,250,300), linetype="solid", color ="grey")+
          #geom_vline(xintercept=c(120,226), linetype="solid", color ="red")+
          #geom_vline(xintercept=seq(70,300,10), linetype="dotdash")+
          #geom_hline(yintercept=c(.25,.5,.75), linetype="dotted", color = "black")+
          labs(x = "Julian Day", y = "Cumulative Degree Days (in F)", fill = "Climate Group") +
          theme(
            panel.grid.major = element_line(size = 0.7),
            legend.title = element_text(face = "plain", size = 16),
            legend.text = element_text(size = 12),
            legend.position = "top",
            strip.text = element_text(size=12, face = "plain"),
            axis.text = element_text(face= "plain", size = 10),
            axis.title.x = element_text(face= "plain", size = 16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(face= "plain", size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0)))
        }
    out_name = paste0("plot_", output_type, "_", version, ".png")
    ggsave(out_name, plot, path=output_dir)

  }

  if (output_type==3){
    filename = paste0(input_dir, file_name, version, ".rds")
    data <- data.table(readRDS(filename))

    data$CountyGroup = as.character(data$CountyGroup)
    data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
    data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
    data = subset(data, select = c("ClimateGroup", "CountyGroup", "latitude", 
                                   "longitude", "ClimateScenario", "year", "dayofyear", "CumDDinF"))
    data$CumDD = data$CumDDinF

    plot = ggplot(data, aes(x=dayofyear, y=CumDD, fill=factor(ClimateGroup))) +
    #geom_line(aes(fill=factor(Timeframe), color=factor(Timeframe) )) +
    stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                                fun.ymin=function(z) { quantile(z,0.1) }, 
                                fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.4) +
    stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                                fun.ymin=function(z) { quantile(z,0.25) }, 
                                fun.ymax=function(z) { quantile(z,0.75) }, alpha=0.8) + 
    stat_summary(geom="line", fun.y=function(z) { quantile(z,0.5) })+ #, aes(color=factor(Timeframe))) + 
    
    scale_color_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), 
                                rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
    scale_fill_manual(values=c(rgb(29, 67, 111, max=255), rgb(92, 160, 201, max=255), 
                               rgb(211, 91, 76, max=255), rgb(125, 7, 37, max=255)))+#c("black", "red","darkgreen","blue")) +
    
    facet_grid(CountyGroup ~ ClimateGroup ~ ., scales = "fixed") +
    scale_x_continuous(breaks=seq(0, 370, 50)) +
    scale_y_continuous(breaks=seq(0, 5000, 1000)) +
    theme_bw() +
    #geom_vline(xintercept=c(100,150,200,250,300), linetype="solid", color ="grey")+
    #geom_vline(xintercept=c(120,226), linetype="solid", color ="red")+
    #geom_vline(xintercept=seq(70,300,10), linetype="dotdash")+
    #geom_hline(yintercept=c(.25,.5,.75), linetype="dotted", color = "black")+
    labs(x = "Julian Day", y = "Cumulative Degree Days (in F)", fill = "Climate Group") +
    theme(
      panel.grid.major = element_line(size = 0.7),
      
      legend.title = element_text(face = "plain", size = 16),
      legend.text = element_text(size = 12),
      legend.position = "top",
      
      strip.text = element_text(size= 12, face = "plain"),
      axis.text = element_text(face="plain", size = 10),
      axis.title.x = element_text(face= "plain", size = 16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(face= "plain", size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0)))
  out_name = paste0("plot_cumdd_" , version, "_type", output_type ,".png")
  ggsave(out_name, plot, path=output_dir)
  }
}
####################################################################################################
##################                                                                ##################
##################                       Box Plots                                ##################
##################                                                                ##################
####################################################################################################
##################
##################   Adult Emergence
##################
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
  data <- subset(data, select = c("Emergence", "ClimateGroup", "ClimateScenario", 
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
    geom_boxplot(# outlier.shape = NA, 
      outlier.size=0, notch=TRUE, width=.2) +
    theme_bw() +
    scale_x_discrete(expand=c(0, 2), limits = levels(data$ClimateGroup[1])) +
    #scale_x_discrete(limits=color_ord,limits=c(10, 20, 30)) +
    # scale_y_discrete(limits=c(1, 2, 3, 4),labels=levels(data$ClimateGroup)) +
    scale_y_continuous(breaks = round(seq(40, 170, by = 10),1)) +
    labs(x="Time Period", y="Day of Year", color = "Climate Group") +
    facet_wrap(~CountyGroup) +
    theme(legend.position="bottom", 
          legend.margin=margin(t = -.1, r = 0, b = 0, l = 0, unit = 'cm'),
          # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.title = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          axis.text = element_text(face = "plain", size = 10),
          axis.title.x = element_text(face = "plain", size = 10, margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size = 7),
          axis.title.y = element_text(face = "plain", size = 10, margin = margin(t = 0, r = 1, b = 0, l = 0)),
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
  ggsave(output_name, p, path=plot_path, width=4.5, height=3.1, unit="in")
}
##################
##################   Generations of Adults or Larva by Aug 23
##################
plot_generations_Aug23 <- function(input_dir,
                                   file_name,
                                   stage,
                                   box_width=.25,
                                   plot_path,
                                   version = "rcp45",
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
  box_plot = ggplot(data = data, aes(x = ClimateGroup, y = !!sym(var), fill = ClimateGroup)) + 
    geom_boxplot(#outlier.shape = NA, 
      outlier.size=0,
      notch=TRUE, width=.2) +
    theme_bw() +
    # The bigger the number in expand below, the smaller the space between y-ticks
    scale_x_discrete(expand=c(0, 3), limits = levels(data$ClimateGroup[1])) +
    labs(x="Time Period", y=paste0("Number of ", stage, " Generations by August 23"), color = "Climate Group") +
    facet_wrap(~CountyGroup) +
    theme(legend.position="bottom", 
          legend.margin=margin(t=-.1, r=0, b=0, l=0, unit='cm'),
          # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.title = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          axis.text = element_text(face = "plain", size = 10),
          axis.title.x = element_text(face = "plain", size = 10, margin = margin(t = 1, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size = 7),
          axis.title.y = element_text(face = "plain", size = 10, margin = margin(t = 0, r = 1, b = 0, l = 0)),
          #axis.title.y = element_blank(),
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
  
  plot_name = paste0(stage, "_Gen_Aug23_", version)
  ggsave(paste0(plot_name, ".png"), box_plot, path=plot_path, device="png", width=4.5, height=3.1, units = "in")
}
#####################################################################################
#######################                                   ###########################
#######################          Diapause Plots           ###########################
#######################                                   ###########################
#####################################################################################
plot_diapause <- function(input_dir, file_name_extension, version, , pop_type, plot_path){
  # input_dir
  # file_name_extension: file name including extention .rds
  # version
  # pop_type either abs or rel
  # plot_path
  if (pop_type == "abs"){plot_abs_diapause(input_dir, file_name_extension, version, plot_path)}
  else {plot_rel_diapause(input_dir, file_name_extension, version, plot_path)}
}

plot_abs_diapause <- function(input_dir, file_name_extension, version, plot_path){
  ##
  ## input_dir 
  ## file_name_extension, 
  ## version either rcp45 or rcp85
  ## plot_path 
  ## 
  file_name = paste0(input_dir, file_name_extension)
  data <- readRDS(file_name)
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  data <- data[variable =="AbsLarvaPop" | variable =="AbsNonDiap"]
  data <- subset(data, select=c("ClimateGroup", "CountyGroup", "CumulativeDDF", "variable", "value"))
  data$variable <- factor(data$variable)
  
  diap_plot <- ggplot(data, aes(x=CumulativeDDF, y=value, color=variable, fill=factor(variable))) + 
    theme_bw() +
    facet_grid(. ~ CountyGroup ~ ClimateGroup, scales = "free") +
    labs(x = "Cumulative Degree (in F)", y = "Absolute Population", color = "Absolute Population") +
    theme(axis.text = element_text(face= "plain", size = 8),
          axis.title.x = element_text(face= "plain", size = 12, margin = margin(t=10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face= "plain", size = 12, margin = margin(t=0, r = 10, b = 0, l = 0)),
          legend.position="bottom"
    ) + 
    scale_fill_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), name = "Absolute Population") +
    scale_color_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), guide = FALSE) +
    stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                 fun.ymin=function(z) { 0 }, 
                 fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.7)+
    scale_x_continuous(limits = c(0, max(data$CumulativeDDF)+10))
  
  plot_name = paste0("diapause_abs_", version, ".png")
  ggsave(plot_name, diap_plot, device="png", path=plot_path, width=7, height=7, unit="in")
}



plot_rel_diap <- function(input_dir, file_name_extension, version, plot_path){
  file_name = paste0(input_dir, file_name_extension)
  data <- readRDS(file_name)

  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  data <- data[variable =="RelLarvaPop" | variable =="RelNonDiap"]
  data <- subset(data, select=c("ClimateGroup", "CountyGroup", "CumulativeDDF", "variable", "value"))
  data$variable <- factor(data$variable)


  pp = ggplot(data, aes(x=CumulativeDDF, y=value, color=variable, fill=factor(variable))) + 
        theme_bw() +
        facet_grid(. ~ CountyGroup ~ ClimateGroup, scales = "free") +
        labs(x = "Cumulative Degree (in F)", y = "Relative Population", color = "Relative Population") +
        theme(axis.text = element_text(face= "plain", size = 8),
              axis.title.x = element_text(face= "plain", size = 12, margin = margin(t=10, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(face= "plain", size = 12, margin = margin(t=0, r = 10, b = 0, l = 0)),
              legend.position="bottom"
              ) + 
        scale_fill_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), name = "Relative Population") +
        scale_color_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), guide = FALSE) +
        stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                                    fun.ymin=function(z) { 0 }, 
                                    fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.7)+
        scale_x_continuous(limits = c(0, max(data$CumulativeDDF)+10)) 
  
  plot_name = paste0("diapause_rel_", version,".png")
  ggsave(plot_name, pp, device="png", path=plot_path, width=7, height=7, unit="in")
}


