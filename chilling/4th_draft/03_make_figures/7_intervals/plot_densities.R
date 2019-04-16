
.libPaths("/data/hydro/R_libs35")
.libPaths()

#####################################################
###                                               ###
###             Sept. thru Dec and Jan            ###
###                                               ###
#####################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange
options(digit=9)
options(digits=9)

base_in <- "/Users/hn/Desktop"
data_dir <- file.path(base_in, "Desktop/Kirti/check_point/chilling/7_temp_interval/")
plot_dir <- base_in

base_in <- "/data/hydro/users/Hossein/chill/7_time_intervals/"
data_dir <- file.path(base_in, "RDS_files/")
plot_dir <- file.path(base_in, "plots/AUC/")
if (dir.exists(file.path(plot_dir)) == F) {
  dir.create(path = plot_dir, recursive = T)
}

quality <- 300 # dpi quality
big_pic_width <- 45
big_pic_height <- 65
small_pic_width <- 30
small_pic_height <- 6 # 4

#####################################################
#####################################################
#####################################################

plot_dens <- function(data, month_name){
    color_ord = c("grey70", "dodgerblue", "olivedrab4", "khaki2")
    iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)
    x_breaks = c(-30, -20, -10, -2, 0, 4, 6, 8, 10, 13, 16, 20, 30, 40, 50)
    the_theme <- theme_bw() + 
                 theme(plot.margin = unit(c(t=0.4, r=0.3, b=.3, l=0.1), "cm"),
                       panel.border = element_rect(fill=NA, size=.3),
                       plot.title = element_text(size = 14, hjust = 0.5),
                       plot.subtitle = element_text(hjust = 0.5),
                       panel.grid.major = element_line(size = 0.05),
                       panel.grid.minor = element_blank(),
                       panel.spacing=unit(.3, "cm"),
                       legend.position="bottom", 
                       legend.title = element_blank(),
                       legend.key.size = unit(.9, "line"),
                       legend.text=element_text(size=16),
                       legend.margin=margin(t= -0.1, r=0, b=0, l=0, unit='cm'),
                       legend.spacing.x = unit(.08, 'cm'),
                       strip.text.x = element_text(size=20),
                       axis.ticks = element_line(size=.2, color="black"),
                       axis.title.x = element_text(size=20, face = "plain", margin = margin(t=8, r=0, b=0, l=0)),
                       axis.title.y = element_text(size=20, face = "plain", margin = margin(t=0, r=8, b=0, l=0)),

                       axis.text.x = element_text(size = 16, face = "plain", color="black", angle=-90),
                       axis.text.y = element_text(size = 16, face = "plain", color="black")
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
               facet_grid( ~ city) +
               xlab("hourly temp.") + 
               ggtitle(label = gtitle) +
               scale_fill_manual(values=color_ord,
                      name="Time\nPeriod", 
                      labels=c("Historical", "2025-2050", "2051-2075", "2076-2099")) + 
               scale_color_manual(values=color_ord,
                       name="Time\nPeriod", 
                       limits = color_ord,
                       labels=c("Historical", "2025-2050", "2051-2075", "2076-2099")) + 
               scale_x_continuous(name="hourly temp.", breaks=x_breaks, limits=c(-30, 50)) + 
               the_theme
    return(obs_plot)
}

month_names = c("sept_thru_dec_modeled", "sept_thru_jan_modeled")

for (month in month_names){
    data = data.table(readRDS(paste0(data_dir, month, ".rds")))
    data$scenario[data$scenario == "rcp45"] = "RCP 4.5"
    data$scenario[data$scenario == "rcp85"] = "RCP 8.5"
    data$scenario[data$scenario == "historical"] = "Historical"

    data$ClimateGroup[data$year <= 2005] <- "Historical"
    data$ClimateGroup[data$year >= 2025 & data$year <= 2050] <- "2025-2050"
    data$ClimateGroup[data$year > 2050  & data$year <= 2075] <- "2051-2075"
    data$ClimateGroup[data$year > 2075] <- "2076-2099"

    # There are years between (2006) and 2025 which ... becomes NA
    data = na.omit(data)

    data$city[data$location == "48.40625_-119.53125"] = "Omak"
    data$city[data$location == "46.28125_-119.34375"] = "Richland"
    data$city[data$location == "47.40625_-120.34375"] = "Wenatchee"
    data$city[data$location == "45.53125_-123.15625"] = "Hilsboro"
    data$city[data$location == "44.09375_-123.34375"] = "Elmira"

    # order the climate groups
    data$ClimateGroup <- factor(data$ClimateGroup, 
                                levels = c("Historical", "2025-2050", 
                                           "2051-2075", "2076-2099"))

    data_45 = data %>% filter(scenario %in% c("Historical", "RCP 4.5"))
    data_85 = data %>% filter(scenario %in% c("Historical", "RCP 8.5"))
    rm(data)
    
    assign(x = paste0(month, "_density_", "rcp45"),
           value = {plot_dens(data=data_45,
                              month_name=month)})
    assign(x = paste0(month, "_density_", "rcp85"),
           value = { plot_dens(data=data_85,
                               month_name=month)})
    rm(data_45, data_85)
}
print ("line 128")
ggsave(filename = "sept_thru_dec_modeled_density_rcp85.png",
       path = plot_dir, 
       plot = sept_thru_dec_modeled_density_rcp85,
       width = small_pic_width, height = small_pic_height, units = "in",
       dpi = quality, 
       device = "png",
       limitsize = FALSE)

ggsave(filename = "sept_thru_dec_modeled_density_rcp45.png",
       path = plot_dir, 
       plot = sept_thru_dec_modeled_density_rcp45,
       width = small_pic_width, height = small_pic_height, units = "in",
       dpi = quality, 
       device = "png",
       limitsize = FALSE)


ggsave(filename = "sept_thru_jan_modeled_density_rcp85.png", 
        path = plot_dir, 
        plot = sept_thru_jan_modeled_density_rcp85,
        width=small_pic_width, height=small_pic_height, units = "in",
        dpi=quality, 
        device = "png",
        limitsize = FALSE)

ggsave(filename = "sept_thru_jan_modeled_density_rcp45.png", 
        path = plot_dir, 
        plot = sept_thru_jan_modeled_density_rcp45,
        width=small_pic_width, height=small_pic_height, units = "in",
        dpi=quality, 
        device = "png",
        limitsize = FALSE)


big_plot_85 <- ggarrange(sept_thru_dec_modeled_density_rcp85, 
                         sept_thru_jan_modeled_density_rcp85,
                         label.x = "hourly temp.",
                         label.y = "density",
                         ncol = 1, 
                         nrow = 2, 
                         common.legend = T,
                         legend = "bottom")

ggsave(filename = "sept_thru_dec_and_Jan_modeled_big_85.png", 
       path = plot_dir, 
       plot = big_plot_85,
       width = small_pic_width, height=small_pic_height, units = "in",
       dpi=quality, 
       device = "png",
       limitsize = FALSE)


############################################################################
############################################################################
############################################################################
############################################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange

############################################################################
base_in <- "/Users/hn/Desktop"
data_dir <- file.path(base_in, "Desktop/Kirti/check_point/chilling/7_temp_interval/")
plot_dir <- base_in

base_in <- "/data/hydro/users/Hossein/chill/7_time_intervals/"
data_dir <- file.path(base_in, "RDS_files/")
plot_dir <- file.path(base_in, "plots/AUC/")
if (dir.exists(file.path(plot_dir)) == F) {
  dir.create(path = plot_dir, recursive = T)
}

print("line 24")
quality <- 300 # dpi quality
big_pic_width <- 45
big_pic_height <- 65
small_pic_width <- 8
small_pic_height <- 8

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
                       legend.key.size = unit(2, "line"),
                       legend.text=element_text(size=24),
                       legend.margin=margin(t= -0.1, r = 0, b = 0, l = 0, unit = 'cm'),
                       legend.spacing.x = unit(.08, 'cm'),
                       strip.text.x = element_text(size = 24),
                       axis.ticks = element_line(color = "black", size = .2),
                       axis.title.x = element_text(face = "plain", size=17, 
                                                   margin = margin(t=4, r=0, b=0, l=0)),
                       axis.text.x = element_text(size = 24, face = "plain", color="black", angle=-90),
                       axis.title.y = element_text(face = "plain", size = 17, 
                                                   margin = margin(t=0, r=6, b=0, l=0)),
                       axis.text.y = element_text(size = 24, face="plain", color="black")
                       )
    obs_plot = ggplot(data, aes(x=Temp, fill=factor(ClimateGroup))) + 
               geom_density(alpha=.5, size=.1) + 
               geom_vline(xintercept = iof_breaks, 
                          linetype = "solid", color = "red", size = 0.2) +
               facet_grid( ~ city) +
               xlab("hourly temp.") + 
               ggtitle(label = paste0("The density of hourly temp. in the month of ", month_name, ".")) +
               scale_fill_manual(values=color_ord,
                      name="Time\nPeriod", 
                      labels=c("Historical","2025-2050","2051-2075","2076-2099")) + 
               scale_color_manual(values=color_ord,
                       name="Time\nPeriod", 
                       limits = color_ord,
                       labels=c("Historical","2025-2050","2051-2075","2076-2099")) + 
               scale_x_continuous(name="hourly temp.", breaks=x_breaks, limits=c(-30, 50)) + 
               the_theme
    return(obs_plot)
}
print("line 78")
############################################################################
month_names = c("Jan", "Feb", "Mar", "Sept", "Oct", "Nov", "Dec")

location_coord = c("48.40625_-119.53125", # Omak
                   "47.40625_-120.34375", # Wenatchee
                   "46.28125_-119.34375", # Richland
                   "45.53125_-123.15625", # Hilsboro
                   "44.09375_-123.34375") # Elmira
city_names = c("Omak", "Wenatchee", "Richland", "Hilsboro", "Elmira")

for (month in month_names){
  print (month)
  data = data.table(readRDS(paste0(data_dir, "/", month, ".rds")))
  data$scenario[data$scenario == "rcp45"] = "RCP 4.5"
  data$scenario[data$scenario == "rcp85"] = "RCP 8.5"
  data$scenario[data$scenario == "historical"] = "Historical"
  
  data$ClimateGroup[data$year <= 2005] <- "Historical"
  data$ClimateGroup[data$year >= 2025 & data$year <= 2050] <- "2025-2050"
  data$ClimateGroup[data$year > 2050  & data$year <= 2075] <- "2051-2075"
  data$ClimateGroup[data$year > 2075] <- "2076-2099"

  # There are years between (2006) and 2025 which ... becomes NA
  data = na.omit(data)

  ### rename location by city names
  data$city[data$location == "48.40625_-119.53125"] = "Omak"
  data$city[data$location == "47.40625_-120.34375"] = "Wenatchee"
  data$city[data$location == "46.28125_-119.34375"] = "Richland"
  data$city[data$location == "45.53125_-123.15625"] = "Hilsboro"
  data$city[data$location == "44.09375_-123.34375"] = "Elmira"

  # order the climate groups
  data$ClimateGroup <- factor(data$ClimateGroup, 
                              levels = c("Historical", "2025-2050",
                                         "2051-2075", "2076-2099"))
  data$city <- factor(data$city, levels = city_names)
  
  data_45 = data %>% filter(scenario %in% c("Historical", "RCP 4.5"))
  data_85 = data %>% filter(scenario %in% c("Historical", "RCP 8.5"))

  assign(x = paste0(month, "_density_plot_", "rcp45"),
         value = {plot_dens(data=data_45,
                            month_name=month)})
  assign(x = paste0(month, "_density_plot_", "rcp85"),
         value = { plot_dens(data=data_85,
                             month_name=month)})

}
print ("line 133")
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
       path = plot_dir,
       plot = big_plot_45,
       width = big_pic_width, height = big_pic_height, units = "in",
       dpi = quality, 
       device = "png",
       limitsize = FALSE)
print ("line 159")
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
       path = plot_dir, 
       plot = big_plot_85,
       width = big_pic_width, height = big_pic_height, units = "in",
       dpi = quality, 
       device = "png",
       limitsize = FALSE)

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
#        path = plot_dir, 
#        plot = big_plot_combined,
#        width = 15, height = 40, units = "in",
#        dpi=quality, 
#        device = "png")



