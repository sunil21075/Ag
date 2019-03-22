
the_theme <-theme_bw() + 
            theme(plot.margin = unit(c(t=.2, r=.2, b=.2, l=0.2), "cm"),
                  plot.title = element_text(size = 13, face = "bold", margin = margin(t=1, r=0, b=0, l=0)),
                  panel.border = element_rect(fill=NA, size=.3),
                  panel.grid.major = element_line(size = 0.05),
                  panel.grid.minor = element_blank(),
                  panel.spacing.y = unit(.25, "cm"),
                  panel.spacing.x = unit(.25, "cm"),
                  legend.position = "bottom", 
                  legend.key.size = unit(2, "line"),
                  legend.spacing.x = unit(.1, 'cm'),
                  legend.title=element_text(size = 12),
                  legend.text = element_text(size = 10),
                  legend.margin = margin(t=0, r=0, b=0, l=0, unit = 'cm'),
                  strip.text.x = element_text(size = 12, color = "black"),
                  strip.text.y = element_text(size = 12, color = "black"),
                  axis.ticks = element_line(size = .1, color = "black"),
                  axis.text.x = element_text(size = 12, face = "plain", color = "black"),
                  axis.text.y = element_text(size = 12, face = "plain", color = "black"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size = 14, face="plain", 
                                              margin = margin(t=0, r=3, b=0, l=0))
                  )

noch <- FALSE
box_width <- 1
color_ord = c("grey70" , "dodgerblue", "olivedrab4", "red", "yellow") # 
time_lab = c("Historical", "2025-2050", "2051-2075", "2076-2099")
exp_size <- c(0.2, .1)
y_limits <- c(0, 140)

title_s <- "Accumulated CP by Jan. 1"
jan_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_J1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(y = "accumulated CP") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab, expand = exp_size) + #breaks = x_breaks,
             scale_y_continuous(limits = y_limits) + 
             ggtitle(title_s) +
             the_theme

title_s <- "Accumulated CP by Feb. 1"
feb_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_F1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(y = "") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab, expand = exp_size) + #breaks = x_breaks,
             scale_y_continuous(limits = y_limits) + 
             ggtitle(title_s) +
             the_theme

title_s <- "Accumulated CP by Mar. 1"
mar_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_M1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(y = "accumulated CP") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab, expand=exp_size) + #breaks = x_breaks,
             scale_y_continuous(limits = y_limits) + 
             ggtitle(title_s) +
             the_theme

title_s <- "Accumulated CP by Apr. 1"
apr_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_A1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) +
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab, expand=exp_size) + #breaks = x_breaks,
             scale_y_continuous(limits = y_limits) + 
             ggtitle(title_s) +
             the_theme

all_boxes <- ggarrange(jan_1_box, feb_1_box,
                       mar_1_box, apr_1_box,
                       ncol = 2, nrow = 2, common.legend = T,
                       legend = "bottom")

ggsave("all_boxes_1.png", all_boxes, path = "/Users/hn/Desktop/", width=20, height=8, unit="in", dpi=400)


