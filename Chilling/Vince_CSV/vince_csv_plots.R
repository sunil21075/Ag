rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)


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

data <- data %>% 
        rename(MD = mdeffect, Spray = ceffectnmd, MD_Spray = ceffectmd) %>% 
        select(-c(year, scenario))

data_melted = melt(data, id = c("loc", "ClimateGroup"))

# Fix the order of levels of variables
data_melted$variable <- factor(data_melted$variable, ordered = TRUE)
data_melted$ClimateGroup <- factor(data_melted$ClimateGroup, 
                                   levels = c("Historical", "2040's", "2060's", "2080's"))

######
###### Plot's cosmetic settings
######
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
the_theme = theme_bw() +
            theme(plot.margin = unit(c(t=0.35, r=.7, b=-4.7, l=0.3), "cm"),
            	  legend.position="bottom", 
                  legend.margin=margin(t=-.1, r=0, b=5, l=0, unit = 'cm'),
                  legend.title = element_blank(),
                  legend.text = element_text(size=10, face="plain"),
                  legend.key.size = unit(.5, "cm"), 
                  panel.grid.major = element_line(size = 0.1),
                  panel.grid.minor = element_line(size = 0.1),
                  panel.spacing=unit(.5, "cm"),
                  strip.text = element_text(size = 8, face = "plain"),
                  axis.text = element_text(face = "plain", size = 4, color="black"),
                  axis.ticks = element_line(color = "black", size = .2),
                  axis.title.x = element_text(face = "plain", size = 12, 
                                              margin = margin(t=10, r=0, b=0, l=0)),
                  axis.text.x = element_text(size = 8, color="black"), # tick text font size
                  axis.text.y = element_text(size = 8, color="black"), # tick text font size
                  axis.title.y = element_text(face = "plain", size = 12, 
                                              margin = margin(t=0, r=7, b=0, l=0))
                 )
vince_box0 <- ggplot(data = data_melted, aes(x=variable, y=value), group = variable) + 
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

ggsave(filename = "vince_csv_boxplot.png", 
	   path = "/Users/hn/Desktop/", 
	   plot = vince_box0,
       width = 16, height = 6, units = "in",
       dpi=400, 
       device = "png")