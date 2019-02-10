rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(ggpubr) # for ggarrange


file_dir = file.path("/Users/hn/Documents/GitHub/Kirti/Chilling/Vince_CSV/")
file_name = "MDpesticide_effects.csv"

file_name = paste0(file_dir, file_name)

data = data.table(read.csv(file_name))
##########################################
##### Below we have the plot for 
##### both rcp's combined.
#####
##########################################
needed_cols = c("mdeffect", "ceffectnmd", "ceffectmd",
                "loc", "scenario", "year")
data = subset(data, select=needed_cols)

data$ClimateGroup[data$year >= 1979 & data$year <= 2005] <- "Historical"
data$ClimateGroup[data$year > 2025 & data$year <= 2055] <- "2040's"
data$ClimateGroup[data$year > 2045 & data$year <= 2075] <- "2060's"
data$ClimateGroup[data$year > 2065 & data$year <= 2095] <- "2080's"

# There are years between 2006 and 2015 which ... becomes NA
data = na.omit(data)

data_45 = filter(data, scenario %in% c("Historical", "RCP4.5"))
data_85 = filter(data, scenario %in% c("Historical", "RCP8.5"))

data_45$scenario = factor(data_45$scenario)
data_85$scenario = factor(data_85$scenario)

data <- data %>% 
        rename(MD = mdeffect, Spray = ceffectnmd, MD_Spray = ceffectmd) %>% 
        select(-c(year, scenario))
data_45 <- data_45 %>% 
           rename(MD = mdeffect, Spray = ceffectnmd, MD_Spray = ceffectmd) %>% 
           select(-c(year, scenario))
data_85 <- data_85 %>% 
           rename(MD = mdeffect, Spray = ceffectnmd, MD_Spray = ceffectmd) %>% 
           select(-c(year, scenario))

data = melt(data, id = c("loc", "ClimateGroup"))
data_45 = melt(data_45, id = c("loc", "ClimateGroup"))
data_85 = melt(data_85, id = c("loc", "ClimateGroup"))

# Fix the order of levels of variables
data$variable <- factor(data$variable, ordered = TRUE)
data$ClimateGroup <- factor(data$ClimateGroup, 
                            levels = c("Historical", "2040's", "2060's", "2080's"))

data_45$variable <- factor(data_45$variable, ordered = TRUE)
data_45$ClimateGroup <- factor(data_45$ClimateGroup, 
                               levels = c("Historical", "2040's", "2060's", "2080's"))

data_85$variable <- factor(data_85$variable, ordered = TRUE)
data_85$ClimateGroup <- factor(data_85$ClimateGroup, 
                               levels = c("Historical", "2040's", "2060's", "2080's"))

######
###### Plot's cosmetic settings
######
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
the_theme <- theme_bw() + 
               theme(plot.margin = unit(c(t=1, r=1, b=.5, l=.5), "cm"),
                     panel.border = element_rect(fill=NA, size=.3),
                     # plot.title = element_text(hjust = 0.5),
                     #plot.subtitle = element_text(hjust = 0.5),
                     panel.grid.major = element_line(size = 0.05),
                     panel.grid.minor = element_blank(),
                     panel.spacing=unit(.25,"cm"),
                     legend.position="bottom", 
                     legend.title = element_blank(),
                     legend.key.size = unit(1, "line"),
                     legend.text=element_text(size=7),
                     legend.margin=margin(t= .1, r=0, b=.5, l=0, unit = 'cm'),
                     legend.spacing.x = unit(.05, 'cm'),
                     strip.text.x = element_text(size = 10),
                     axis.ticks = element_line(color = "black", size = .2),
                     #axis.text = element_text(face = "plain", size = 2.5, color="black"),
                     axis.title.x = element_text(face = "plain", size=12, 
                                                  margin = margin(t=4, r=0, b=0, l=0)),
                     # axis.title.x=element_blank(),
                     axis.text.x = element_text(size = 8, face = "plain", 
                                                color="black"),
                     axis.ticks.x = element_blank(),
                     axis.title.y = element_text(face = "plain", size = 12, 
                                                 margin = margin(t=0, r=3, b=0, l=0)),
                     axis.text.y = element_text(size = 8, face="plain", color="black")
                     # axis.title.y = element_blank()
                     )

vince_box0 <- ggplot(data = data, aes(x=variable, y=value), group = variable) + 
              geom_boxplot(outlier.size=-.15, notch=FALSE, width=.4, lwd=.25, aes(fill=ClimateGroup), 
			            position=position_dodge(width=0.5)) + 
              facet_wrap(~loc, scales="free", ncol=6, dir="v") + 
              labs(x="control method", 
                   y="population percentage", 
                   color = "Climate Group") + 
              ggtitle(label = paste0("The % of the untreated control population")) + 
              scale_color_manual(values=color_ord,
                                 name="Time\nPeriod", 
                                 limits = color_ord,
                                 labels=c("Historical", "2040", "2060", "2080")) +
              scale_fill_manual(values=color_ord,
                                name="Time\nPeriod", 
                                labels=c("Historical", "2040", "2060", "2080")) +
              the_theme
vince_45 <- ggplot(data = data_45, aes(x=variable, y=value), group = variable) + 
            geom_boxplot(outlier.size=-.15, notch=FALSE, width=.4, lwd=.25, aes(fill=ClimateGroup), 
                         position=position_dodge(width=0.5)) + 
            facet_wrap(~loc, scales="free", ncol=6, dir="v") + 
            labs(x="control method", 
                 y="population percentage", 
                 color = "Climate Group") + 
            ggtitle(label = paste0("The % of the untreated control population", " RCP 4.5")) + 
            scale_color_manual(values=color_ord,
                               name="Time\nPeriod", 
                               limits = color_ord,
                               labels=c("Historical", "2040", "2060", "2080")) +
            scale_fill_manual(values=color_ord,
                              name="Time\nPeriod", 
                              labels=c("Historical", "2040", "2060", "2080")) +
            the_theme
vince_85 <- ggplot(data = data_85, aes(x=variable, y=value), group = variable) + 
                geom_boxplot(outlier.size=-.15, notch=FALSE, width=.4, lwd=.25, aes(fill=ClimateGroup), 
                             position=position_dodge(width=0.5)) + 
                facet_wrap(~loc, scales="free", ncol=6, dir="v") + 
                labs(x="control method", 
                     y="population percentage", 
                     color = "Climate Group") + 
                ggtitle(label = paste0("The % of the untreated control population", " RCP 8.5")) +
                scale_color_manual(values=color_ord,
                                   name="Time\nPeriod", 
                                   limits = color_ord,
                                   labels=c("Historical", "2040", "2060", "2080")) +
                scale_fill_manual(values=color_ord,
                                  name="Time\nPeriod", 
                                  labels=c("Historical", "2040", "2060", "2080")) +
                the_theme

big_plot <- ggarrange(vince_box0, vince_45,
                      vince_85,
                      label.x = "control method",
                      label.y = "population percentage",
                      ncol = 1, 
                      nrow = 3, 
                      common.legend = T,
                      legend = "bottom")
#big_plot
ggsave(filename = "vince_big_box.png", 
       path = "/Users/hn/Desktop/", 
       plot = big_plot,
       width = 14, height = 18, units = "in",
       dpi=400, 
       device = "png")

big_plot <- ggarrange(vince_45,
                      vince_85,
                      label.x = "control method",
                      label.y = "population percentage",
                      ncol = 1, 
                      nrow = 2, 
                      common.legend = T,
                      legend = "bottom")
ggsave(filename = "rcp45_85.png", 
       path = "/Users/hn/Desktop/", 
       plot = big_plot,
       width = 16, height = 18, units = "in",
       dpi=400, 
       device = "png")
#big_plot


# ggsave(filename = "vince_csv_boxplot.png", 
# 	   path = "/Users/hn/Desktop/", 
# 	   plot = vince_box0,
#        width = 16, height = 6, units = "in",
#        dpi=400, 
#        device = "png")

# ggsave(filename = "vince_csv_boxplot_85.png", 
#        path = "/Users/hn/Desktop/", 
#        plot = vince_box0,
#        width = 16, height = 6, units = "in",
#        dpi=400, 
#        device = "png")


# ggsave(filename = "vince_csv_boxplot_85.png", 
#      path = "/Users/hn/Desktop/", 
#      plot = vince_box0,
#        width = 16, height = 6, units = "in",
#        dpi=400, 
#        device = "png")


