####################################################################################
########################################## Educational - For reference
# file %>%
# filter(Year > 2025 & Year <= 2055,
# Chill_season != "chill_2025-2026" &
# Chill_season != "chill_2055-2056") %>% 
# group_by(Chill_season) %>%
##########################################

# assign(x = paste0(month, "_density_plot_", "rcp45"),
#        value = {plot_dens(data=data_45, month_name=month)})

##########################################
# df[4, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)

A_filtered <- A %>% filter_all(any_vars(is.na(.)))
A <- summary_comp %>% filter_all(any_vars(is.na(.)))
A <- summary_compy %>% filter_all(any_vars(is.na(.)))
A <- summary_compy %>% filter(any_vars(is.na(.)))
A <- A %>% filter_all(any_vars(is.na(.)))