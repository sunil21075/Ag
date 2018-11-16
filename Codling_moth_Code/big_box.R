rm(list=ls())
library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

plot_path <- "/Users/hn/Desktop/Kirti/check_point/my_aeolus_rds/"
output_name = "test.png"
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")

combined_CM_rcp45 <- readRDS("/Users/hn/Desktop/Kirti/check_point/my_aeolus_rds/combined_CM_rcp45.rds")
data <- combined_CM_rcp45
data <- subset(data, select = c("AGen1_0.25", "AGen1_0.5", "AGen1_0.75", 
                                "AGen2_0.25", "AGen2_0.5", "AGen3_0.75",
                                "AGen3_0.25", "AGen3_0.5", "AGen3_0.75", 
                                "AGen4_0.25", "AGen4_0.5", "AGen4_0.75", ))

data$CountyGroup = as.character(data$CountyGroup)
data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'

data = data[, .(AGen1_0.25 = AGen1_0.25), by = c("ClimateGroup", "CountyGroup")]
data <- subset(data, select = c("ClimateGroup", "CountyGroup", "AGen1_0.25"))

df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(AGen1_0.25)))
medians_vec <- medians$med



bplot = ggplot(data = data, aes(x=ClimateGroup, y=AGen1_0.25, fill=ClimateGroup))+
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
ggsave(output_name, bplot, path=plot_path, width=4.5, height=3.1, unit="in")
