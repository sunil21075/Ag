1 through 7 corresponds to 7 different diapause functions.
1 is the original diapause function.

library(ggplot2)
# original
diapause <- function(x){
  102.6077 * exp(-exp(1.306483 * (x - 16.95815)))
}
# red solid one
diapause_2 <- function(x){
  103 * exp(-exp(.8 * (x - 15.95)))
}

# blue solid
diapause_3 <- function(x){
  103 * exp(-exp(.95 * (x - 16.5 )))
}
# bright green solid
diapause_4 <- function(x){
  104 * exp(-exp(.8 * (x - 16.3 )))
}
# dot line
diapause_5 <- function(x){
  104 * exp(-exp(.7 * (x - 15.6 )))
}

diapause_6 <- function(x){
  104 * exp(-exp(.6 * (x - 15.3 )))
}

# orange
diapause_7 <- function(x){
  103 * exp(-exp(1.1 * (x - 16.8 )))
}

diapause_reshaped_plot = ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_bw()+
                         labs(x="Day length hours", y="% larva diapaused induced") + 
                         xlim(5, 20) + 
  stat_function(fun = diapause) +
  stat_function(fun = diapause_2, color="red") + 
  stat_function(fun = diapause_3, color="blue") +
  stat_function(fun = diapause_4, color="green")+
  stat_function(fun = diapause_5, color="black", linetype=3) + 
  stat_function(fun = diapause_6, color="red", linetype=2) + 
  stat_function(fun = diapause_7, color="orange", linetype=1)

diapause_reshaped_plot

plot_name = paste0("diapause_reshaped_plot.png")
plot_path = "/Users/hn/Documents/GitHub/Kirti/codling_moth/to_write_paper/figures/sensitivity/Diapause/"
ggsave(plot_name, diapause_reshaped_plot, device="png", path=plot_path, width=10, height=7, unit="in", dpi=500)