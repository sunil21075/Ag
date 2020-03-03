#
# This is obtaied by copying and modifying "plot_densities.R"
# To plot densities for specific locations
#


#####################################################
###                                               ###
###             Sept. thru Apr.                   ###
###                                               ###
#####################################################
rm(list=ls())

.libPaths("/data/hydro/R_libs35")
.libPaths()

library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange
options(digit=9)
options(digits=9)

base_in <- "/data/hydro/users/Hossein/chill/7_time_intervals/"
data_dir <- file.path(base_in, "RDS_files/")
plot_dir <- file.path(base_in, "plots/AUC/")

                     
# data_dir <- file.path(paste0("/Users/hn/Documents/01_research_data/Ag_check_point/", 
#                              "chilling/7_temp_int_limit_locs/untitled/RDS_files/"))
# plot_dir <- data_dir

if (dir.exists(file.path(plot_dir)) == F) {
  dir.create(path = plot_dir, recursive = T)
}

###########################################################

quality <- 500 # dpi quality
big_pic_width <- 70
big_pic_height <- 65
small_pic_width <- 50
small_pic_height <- 6 # 4

#####################################################
#####################################################
#####################################################

plot_dens <- function(data, month_name){
    color_ord = c("grey47", "dodgerblue", "olivedrab4", "khaki2")
    iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)
    x_breaks = c(-30, -20, -10, -2, 0, 4, 6, 8, 10, 13, 16, 20, 30, 40, 50)
    the_theme <- theme(plot.margin = unit(c(t=0.4, r=0.3, b=.3, l=0.1), "cm"),
                       panel.border = element_rect(fill=NA, size=.3),
                       panel.grid.major = element_line(size = 0.05),
                       panel.grid.minor = element_blank(),
                       panel.spacing=unit(.3, "cm"),
                       legend.position="bottom", 
                       legend.title = element_blank(),
                       legend.key.size = unit(.9, "line"),
                       legend.margin=margin(t= -0.1, r=0, b=0, l=0, unit='cm'),
                       legend.spacing.x = unit(.08, 'cm'),
                       legend.text=element_text(size=12),
                       strip.text.x = element_text(face="bold", size=12),
                       strip.text.y = element_text(face="bold", size=12),
                       axis.ticks = element_line(size=.2, color="black"),
                       plot.title = element_text(size=12, face="bold"),
                       axis.title.x = element_text(size=12, face = "bold", margin = margin(t=8, r=0, b=0, l=0)),
                       axis.title.y = element_text(size=12, face = "bold", margin = margin(t=0, r=8, b=0, l=0)),
                       axis.text.x = element_text(size =8, face = "plain", color="black", angle=-90),
                       axis.text.y = element_text(size =8, face = "bold", color="black")
                       )
    
    
    gtitle = paste0("The density of hourly temp. over chill season")
    
    obs_plot = ggplot(data, aes(x=Temp, fill=factor(time_period))) + 
               geom_density(alpha=.5, size=.1) + 
               geom_vline(xintercept = iof_breaks, 
                          linetype = "solid", color = "red", size = 0.2) +
               facet_grid( ~ city) +
               xlab("hourly temp.") + 
               # ggtitle(label = gtitle) +
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

print ("line 94")

modeled  <- readRDS(paste0(data_dir, "/modeled.rds")) %>% data.table()

# toss out modeled historical
modeled <- modeled %>% filter(scenario != "historical")

modeled$scenario[modeled$scenario=="rcp45"] <- "RCP 4.5"
modeled$scenario[modeled$scenario=="rcp85"] <- "RCP 8.5"

observed <- readRDS(paste0(data_dir, "/observed.rds")) %>% data.table()
observed$scenario <- "Historical"

dt_df <- rbind(modeled, observed)
rm(modeled, observed)

dt_df$time_period[dt_df$year <= 2016] <- "Historical"
dt_df$time_period[dt_df$year > 2026 & dt_df$year <= 2050] <- "2026-2050"
dt_df$time_period[dt_df$year > 2051  & dt_df$year <= 2075] <- "2051-2075"
dt_df$time_period[dt_df$year > 2076] <- "2076-2099"

# order the climate groups
dt_df$time_period <- factor(dt_df$time_period, 
                            levels = c("Historical", "2025-2050", "2051-2075", "2076-2099"))

dt_df <- na.omit(dt_df)

for (ct in unique(dt_df$city)){
  dt_df_ct <- dt_df %>% filter(city == ct)

  dt_df_45 <- dt_df_ct %>% filter(scenario %in% c("Historical", "RCP 4.5"))
  dt_df_85 <- dt_df_ct %>% filter(scenario %in% c("Historical", "RCP 8.5"))

  plot_85 <- plot_dens(data=dt_df_85)
  plot_45 <- plot_dens(data=dt_df_45)

  print ("line 132")

  ggsave(filename = paste0("sept_apr1_dens_", ct, "rcp85.png"),
         path = plot_dir, 
         plot = plot_85,
         width = 6, height = 4, units = "in",
         dpi = quality, 
         device = "png",
         limitsize = FALSE)

  ggsave(filename = paste0("sept_apr1_dens_", ct, "rcp45.png"),
         path = plot_dir, 
         plot = plot_45,
         width = 6, height = 4, units = "in",
         dpi = quality, 
         device = "png",
         limitsize = FALSE)
  print ("line 149")

}
