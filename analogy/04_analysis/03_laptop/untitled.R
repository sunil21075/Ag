gsub("-", "_", model_n)
target_st_cnty <- gsub(", ", "_", target_cnty_name)
assign(x = paste0("plot_", target_st_cnty, "_F2") , 
       value={ggarrange(plotlist = list(get(paste0("map_", gsub("-", "_", model_n), "_F2")), 
                                        get(paste0("pie_con_", gsub("-", "_", model_n), "_F2"))),
                        heights= c(1.5, 1), 
                        widths= c(3, 1),
                        ncol = 1, nrow = 2, common.legend=FALSE)})

ggsave(filename = paste0("triple_", target_st_cnty, "_F2.png"),
       plot = get(paste0("plot_", target_st_cnty, "_F2")), 
       path= "/Users/hn/Desktop/", device="png",
       dpi = 200, width = 12, height = 12, unit="in", limitsize = FALSE)