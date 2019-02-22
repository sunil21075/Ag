rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange

############################################################################

plot_dens <- function(data, month_name){
    color_ord = c("grey70", "dodgerblue", "olivedrab4", "khaki2")
    iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)
    x_breaks = c(-30, -20, -10, -2, 0, 4, 6, 8, 10, 13, 16, 20, 30, 40, 50)
    the_theme <- theme_bw() + 
                 theme(plot.margin = unit(c(t=0.4, r=0.3, b=.3, l=0.1), "cm"),
                       panel.border = element_rect(fill=NA, size=.3),
                       plot.title = element_text(hjust = 0.5),
                       plot.subtitle = element_text(hjust = 0.5),
                       panel.grid.major = element_line(size = 0.05),
                       panel.grid.minor = element_blank(),
                       panel.spacing=unit(.3,"cm"),
                       legend.position="bottom", 
                       legend.title = element_blank(),
                       legend.key.size = unit(1, "line"),
                       legend.text=element_text(size=9),
                       legend.margin=margin(t= -0.1, r = 0, b = 0, l = 0, unit = 'cm'),
                       legend.spacing.x = unit(.08, 'cm'),
                       strip.text.x = element_text(size = 14),
                       axis.ticks = element_line(color = "black", size = .2),
                       axis.title.x = element_text(face = "plain", size=15, 
                                                   margin = margin(t=4, r=0, b=0, l=0)),
                       axis.text.x = element_text(size = 12, face = "plain", color="black", angle=-30),
                       axis.title.y = element_text(face = "plain", size = 15, 
                                                   margin = margin(t=0, r=6, b=0, l=0)),
                       axis.text.y = element_text(size = 12, face="plain", color="black")
                       )
    obs_plot = ggplot(data, aes(x=Temp, fill=factor(ClimateGroup))) + 
               geom_density(alpha=.5, size=.1) + 
               geom_vline(xintercept = iof_breaks, 
                          linetype = "solid", color = "red", size = 0.2) +
               facet_grid( ~ CountyGroup) +
               xlab("hourly temp.") + 
               ggtitle(label = paste0("The density of hourly temp. in the month of ", month_name, ".")) +
               scale_fill_manual(values=color_ord,
                      name="Time\nPeriod", 
                      labels=c("1950-2005","2025-2050","2051-2075","2076-2099")) + 
               scale_color_manual(values=color_ord,
                       name="Time\nPeriod", 
                       limits = color_ord,
                       labels=c("1950-2005","2025-2050","2051-2075","2076-2099")) + 
               scale_x_continuous(name="hourly temp.", breaks=x_breaks, limits=c(-30, 50)) + 
               the_theme
    return(obs_plot)
}

############################################################################
data_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_time_intervals_data/"
month_names = c("Jan", "Feb", "Mar", "Sept", "Oct", "Nov", "Dec")

for (month in month_names){
    data = data.table(readRDS(paste0(data_dir, month, ".rds")))
    # data$ClimateGroup[data$Year >= 1950 & data$Year <= 2005] <- "Historical"
    data$ClimateGroup[data$Year <= 2005] <- "1950-2005"
    data$ClimateGroup[data$Year > 2025 & data$Year <= 2050] <- "2025-2050"
    data$ClimateGroup[data$Year > 2050 & data$Year <= 2075] <- "2051-2075"
    data$ClimateGroup[data$Year > 2075] <- "2076-2099"

    # There are years between (2006) and 2025 which ... becomes NA
    data = na.omit(data)

    # order the climate groups
    data$ClimateGroup <- factor(data$ClimateGroup, 
                                levels = c("1950-2005", "2025-2050",
                                           "2051-2075", "2076-2099"))

    data_45 = data %>% filter(scenario %in% c("historical", "rcp45"))
    data_85 = data %>% filter(scenario %in% c("historical", "rcp85"))

    assign(x = paste0(month, "_density_plot_", "rcp45"),
           value = {plot_dens(data=data_45,
                              month_name=month)})
    assign(x = paste0(month, "_density_plot_", "rcp85"),
           value = { plot_dens(data=data_85,
                               month_name=month)})
}

#######
####### RCP45
#######

big_plot_45 <- ggarrange(Sept_density_plot_rcp45, 
                         Oct_density_plot_rcp45,
                         Nov_density_plot_rcp45,
                         Dec_density_plot_rcp45,
                         Jan_density_plot_rcp45,
                         Feb_density_plot_rcp45,
                         Mar_density_plot_rcp45,
                         label.x = "hourly temp.",
                         label.y = "density",
                         ncol = 1, 
                         nrow = 7, 
                         common.legend = F,
                         legend = "bottom")

ggsave(filename = "density_45.png", 
       path = "/Users/hn/Desktop/", 
       plot = big_plot_45,
       width = 15, height = 40, units = "in",
       dpi=400, 
       device = "png")

#######
####### RCP85
#######

big_plot_85 <- ggarrange(Sept_density_plot_rcp85, 
                         Oct_density_plot_rcp85,
                         Nov_density_plot_rcp85,
                         Dec_density_plot_rcp85,
                         Jan_density_plot_rcp85,
                         Feb_density_plot_rcp85,
                         Mar_density_plot_rcp85,
                         label.x = "hourly temp.",
                         label.y = "density",
                         ncol = 1, 
                         nrow = 7, 
                         common.legend = F,
                         legend = "bottom")
ggsave(filename = "density_85.png", 
       path = "/Users/hn/Desktop/", 
       plot = big_plot_85,
       width = 15, height = 40, units = "in",
       dpi=400, 
       device = "png")

#######
####### RCP 45 and 85 combined
#######
# big_plot_combined <- ggarrange(Sept_density_plot, 
#                              Oct_density_plot,
#                              Nov_density_plot,
#                              Dec_density_plot,
#                              Jan_density_plot,
#                              Feb_density_plot,
#                              Mar_density_plot,
#                              label.x = "Hourly temp.",
#                              label.y = "Density",
#                              ncol = 1, 
#                              nrow = 7, 
#                              common.legend = T,
#                             legend = "bottom")
# ggsave(filename = "45_and_85_combined.png", 
#        path = "/Users/hn/Desktop/", 
#        plot = big_plot_combined,
#        width = 15, height = 40, units = "in",
#        dpi=400, 
#        device = "png")
#####################################################
###                                               ###
###             Sept. thru Dec and Jan            ###
###                                               ###
#####################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange
rm(data)

plot_dens <- function(data, month_name){
    color_ord = c("grey70", "dodgerblue", "olivedrab4", "khaki2")
    iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)
    x_breaks = c(-30, -20, -10, -2, 0, 4, 6, 8, 10, 13, 16, 20, 30, 40, 50)
    the_theme <- theme_bw() + 
                 theme(plot.margin = unit(c(t=0.4, r=0.3, b=.3, l=0.1), "cm"),
                       panel.border = element_rect(fill=NA, size=.3),
                       plot.subtitle = element_text(hjust = 0.5),
                       panel.grid.major = element_line(size = 0.05),
                       panel.grid.minor = element_blank(),
                       panel.spacing=unit(.3,"cm"),
                       legend.position="bottom", 
                       legend.title = element_blank(),
                       legend.key.size = unit(.5, "line"),
                       legend.text=element_text(size=4),
                       legend.margin=margin(t= -0.1, r = 0, b = 0, l = 0, unit = 'cm'),
                       legend.spacing.x = unit(.08, 'cm'),
                       strip.text.x = element_text(size = 7),
                       axis.ticks = element_line(color="black", size=.2),
                       axis.title.x = element_text(face = "plain", size=7, margin = margin(t=4, r=0, b=0, l=0)),
                       axis.title.y = element_text(face = "plain", size=7, margin = margin(t=0, r=4, b=0, l=0)),

                       axis.text.x = element_text(size = 5, face = "plain", color="black", 
                                                  angle=-30, margin=margin(t=0 , r=0, b=0, l=0,"pt")),
                       axis.text.y = element_text(size = 5, face = "plain", color="black"),
                       plot.title = element_text(size = 8, hjust = 0.5)
                       )
    
    if (month == "sept_thru_dec_modeled"){
      gtitle = paste0("The density of hourly temp. from Sept. to Dec. 31 ")
    } else {
      gtitle = paste0("The density of hourly temp. from Sept. to Jan. 31 ")
    }
    obs_plot = ggplot(data, aes(x=Temp, fill=factor(ClimateGroup))) + 
               geom_density(alpha=.5, size=.1) + 
               geom_vline(xintercept = iof_breaks, 
                          linetype = "solid", color = "red", size = 0.2) +
               facet_grid( ~ CountyGroup) +
               xlab("hourly temp.") + 
               ggtitle(label = gtitle) +
               scale_fill_manual(values=color_ord,
                      name="Time\nPeriod", 
                      labels=c("1950-2005", "2025-2050", "2051-2075", "2076-2099")) + 
               scale_color_manual(values=color_ord,
                       name="Time\nPeriod", 
                       limits = color_ord,
                       labels=c("1950-2005", "2025-2050", "2051-2075", "2076-2099")) + 
               scale_x_continuous(name="hourly temp.", breaks=x_breaks, limits=c(-30, 50)) + 
               the_theme
    return(obs_plot)
}

data_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_time_intervals_data/"

month_names = c("sept_thru_dec_modeled", "sept_thru_jan_modeled")

for (month in month_names){
    data = data.table(readRDS(paste0(data_dir, month, ".rds")))
    # data$ClimateGroup[data$Year >= 1950 & data$Year <= 2005] <- "Historical"
    data$ClimateGroup[data$Year <= 2005] <- "1950-2005"
    data$ClimateGroup[data$Year > 2025 & data$Year <= 2050] <- "2025-2050"
    data$ClimateGroup[data$Year > 2050 & data$Year <= 2075] <- "2051-2075"
    data$ClimateGroup[data$Year > 2075] <- "2076-2099"

    # There are years between (2006) and 2025 which ... becomes NA
    data = na.omit(data)

    # order the climate groups
    data$ClimateGroup <- factor(data$ClimateGroup, 
                                levels = c("1950-2005", "2025-2050", 
                                           "2051-2075", "2076-2099"))

    data_45 = data %>% filter(scenario %in% c("historical", "rcp45"))
    data_85 = data %>% filter(scenario %in% c("historical", "rcp85"))
    rm(data)
    assign(x = paste0(month, "_density_", "rcp45"),
           value = {plot_dens(data=data_45,
                              month_name=month)})
    assign(x = paste0(month, "_density_", "rcp85"),
           value = { plot_dens(data=data_85,
                               month_name=month)})
    
    ggsave(filename = paste0(month, "_density_rcp85.png"), 
           path = "/Users/hn/Desktop/", 
           plot = sept_thru_dec_rcp85,
           width=8, height=3, units = "in",
           dpi=400, 
           device = "png")

    ggsave(filename = paste0(month, "_density_rcp45.png"), 
           path = "/Users/hn/Desktop/", 
           plot = sept_thru_dec_rcp45,
           width=8, height=3, units = "in",
           dpi=400, 
           device = "png")
    rm(data_45, data_85)
}
