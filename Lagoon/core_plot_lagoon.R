
library(ggplot2)

options(digits=9)
options(digits=9)

storm_box_plot <- function(data_tb){
  categ_lab <- sort(unique(data_tb$return_period))
  color_ord = c("grey47", "dodgerblue2", "olivedrab4", "red", "steelblue1")

  box_width = 0.45
  x_ticks <- c("5", "10", "15", "20", "25")

  medians <- data.frame(data_tb) %>% 
             group_by(model, return_period, emission) %>% 
             summarise( med_5 = median(five_years),
                       med_10 = median(ten_years),
                       med_15 = median(fifteen_years),
                       med_20 = median(twenty_years),
                       med_25 = median(twenty_five_years))  %>% 
             data.table()

  melted <- melt(data_tb, id=c("location", "model", "return_period", "emission"))

  box_p <- ggplot(data = melted, aes(x=variable, y=value, fill=return_period)) +
           geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.6)) +
           # labs(x="", y="") + # theme_bw() + 
           facet_grid(~ emission) +
           scale_x_discrete(labels=c("five_years" = "5", 
                                     "ten_years" = "10",
                                     "fifteen_years" = "15",
                                     "twenty_years" = "20",
                                     "twenty_five_years" = "25")) + 
           xlab("time interval (years)") + 
           ylab("24 hr design storm int. (mm/hr)") + 
           scale_fill_manual(values = color_ord,
                             name = "Return\nPeriod", 
                             labels = categ_lab) + 
          theme(plot.margin = unit(c(t=.2, r=.2, b=.2, l=0.2), "cm"),
                panel.border = element_rect(fill=NA, size=.3),
                panel.grid.major = element_line(size = 0.05),
                panel.grid.minor = element_blank(),
                panel.spacing = unit(.35, "line"),
                legend.position = "bottom", 
                legend.key.size = unit(.7, "line"),
                legend.spacing.x = unit(.1, 'line'),
                panel.spacing.y = unit(.5, 'line'),
                legend.text = element_text(size = 7),
                legend.margin = margin(t=.4, r=0, b=0, l=0, unit = 'line'),
                legend.title = element_blank(),
                plot.title = element_text(size = 7, face = "bold"),
                plot.subtitle = element_text(face = "bold"),
                strip.text.x = element_text(size=7, face = "bold"),
                strip.text.y = element_text(size=7, face = "bold"),
                axis.ticks = element_line(size = .1, color = "black"),
                axis.text.y = element_text(size = 6, face = "bold", color = "black"),
                axis.text.x = element_text(size = 6, face = "bold", color="black"),
                axis.title.y = element_text(size = 7, face = "bold", margin = margin(t=0, r=2, b=0, l=0)),
                axis.title.x = element_text(size = 7, face = "bold", margin = margin(t=2, r=0, b=-10, l=0)),
                    )
  ggsave(filename = paste0("box_p.png"), 
         plot = box_p, 
         width = 4, height = 2, units = "in", 
         dpi=600, device = "png",
         path="/Users/hn/Desktop/")






