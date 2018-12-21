rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)

input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/small_samples/4_cumdd/"
version = c("rcp45", "rcp85")
version = version[2]
for (vers in version){
data = readRDS(paste0(input_dir, "cumdd_CMPOP_", vers, ".rds"))
data = subset(data, select = c("ClimateGroup", "CountyGroup",
                               "dayofyear", "CumDDinF"))
data$CountyGroup = as.character(data$CountyGroup)
data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
# add the new season column
data[, season := as.character(0)]
data[data[ , data$dayofyear <= 90]]$season = "QTR 1"
data[data[ , data$dayofyear >= 91 & data$dayofyear <= 181]]$season = "QTR 2"
data[data[ , data$dayofyear >= 182 & data$dayofyear <= 273]]$season = "QTR 3"
data[data[ , data$dayofyear >= 274]]$season = "QTR 4"
data = within(data, remove(dayofyear))
data$season = factor(data$season, levels = c("QTR 1", "QTR 2", "QTR 3", "QTR 4"))

#df <- data.frame(data)
#df <- (df %>% group_by(CountyGroup, ClimateGroup, season))
#medians <- (df %>% summarise(med = median(CumDDinF)))
#medians <- medians$med
#rm(df)

data = melt(data, id = c("ClimateGroup", "CountyGroup", "season"))
data = within(data, remove(variable))

bplot <- ggplot(data = data, aes(x=season, y=value), group = season) + 
  geom_boxplot(outlier.size=-.15, notch=FALSE, width=.4, lwd=.25, aes(fill=ClimateGroup), 
               position=position_dodge(width=0.5)) + 
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 500)) + 
  facet_wrap(~CountyGroup, scales="free", ncol=6, dir="v") + 
  labs(x="", y="Cumulative degree day", color = "Climate Group") + 
  theme_bw() +
  theme(legend.position="bottom", 
        legend.margin=margin(t=-.7, r=0, b=5, l=0, unit = 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=10, face="plain"),
        legend.key.size = unit(.5, "cm"), 
        panel.grid.major = element_line(size = 0.1),
        panel.grid.minor = element_line(size = 0.1),
        strip.text = element_text(size= 10, face = "plain"),
        axis.text = element_text(face = "plain", size = 4, color="black"),
        axis.title.x = element_text(face = "plain", size = 10, 
                                    margin = margin(t=10, r=0, b=0, l=0)),
        axis.text.x = element_text(size = 10, color="black"), # tick text font size
        axis.text.y = element_text(size = 10, color="black"),
        axis.title.y = element_text(face = "plain", size = 12, 
                                    margin = margin(t=0, r=7, b=0, l=0)),
        plot.margin = unit(c(t=0.3, r=.7, b=-4.7, l=0.3), "cm")
        )

out_name = paste0("cumdd_qrt_", vers, ".png")
output_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/small_samples/4_cumdd/"
ggsave(out_name, bplot, width=15, height=7, unit="in", path=output_dir, dpi=300, device="png")
}