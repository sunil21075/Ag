.libPaths("/data/hydro/R_libs35")
.libPaths()

library(data.table)
library(ggpubr)

source_path1 = "/home/hnoorazar/cleaner_codes/core.R"
source_path2 = "/home/hnoorazar/cleaner_codes/core_plot.R"
source(source_path1)
source(source_path2)

options(digits=9)
options(digit=9)

start_time <- Sys.time()
data_dir = "/data/hydro/users/Hossein/codling_moth_new/local/processed/overlaping/"
plot_path = "/data/hydro/users/Hossein/codling_moth_new/local/processed/overlaping/plots/"

egg_45 <- plot_cumdd_eggHatch(input_dir=data_dir, 
                              file_name ="combined_CMPOP_", 
                              version="rcp45", 
                              output_dir=plot_path, output_type="eggHatch")

egg_85 <- plot_cumdd_eggHatch(input_dir=data_dir, 
                              file_name ="combined_CMPOP_", 
                              version="rcp85", 
                              output_dir=plot_path, output_type="eggHatch")

adult_DoY_mean <- ggpubr::ggarrange(plotlist = list(egg_45, egg_85),
                                    ncol = 2, nrow = 1,
                                    common.legend = TRUE, legend = "bottom")

<<<<<<< HEAD
ggsave("EggHatch.png", adult_DoY_mean, width=15, height=7.5, path=plot_path, dpi=350)
=======
ggsave("EggHatch.png", adult_DoY_mean, width=11, height=7.5, path=plot_path, dpi=500)
>>>>>>> a255bd425a6f23bebc1f80714626251bfb7c2646


print( Sys.time()- start_time)

