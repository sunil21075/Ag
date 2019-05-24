# a) add biofix to the data so each location can see its corresponding biofix
# b) compare the accum_GDD_in_C to biofix column and set the greater values to zero.
# c) copy daily_dd to non-zero values in the biofix column
# d) find index of first non-zero values in each group_by(loc, year) and set it to 97
# e) compute cum_dd_in_C using biofix column
# f) Done

bad_CMPOP_non_local <- data.table(readRDS("/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/00_databases/usa/CMPOP_loc_ddd_acDD.rds"))
bad_CMPOP_non_local <- merge(bad_CMPOP_non_local, biofix_param, by="location")
bad_CMPOP_non_local$biofix_b <- bad_CMPOP_non_local$biofix
bad_CMPOP_non_local$biofix_b[bad_CMPOP_non_local$CumDDinC < bad_CMPOP_non_local$biofix_b] <- 0
bad_CMPOP_non_local$biofix_c <- bad_CMPOP_non_local$biofix_b
bad_CMPOP_non_local$biofix_c[bad_CMPOP_non_local$biofix_c != 0] <- bad_CMPOP_non_local$DailyDD[bad_CMPOP_non_local$biofix_c != 0]

# some of the 0 values for some reason are gone to 0.0000000 in biofix column
# we can set them back to zero, so, it is exact, so, step (d) above is exact
bad_CMPOP_non_local$biofix_c[bad_CMPOP_non_local$biofix_c < 0.0001] <- 0

bad_CMPOP_non_local$biofix_d <- bad_CMPOP_non_local$biofix_c
bad_CMPOP_non_local <- bad_CMPOP_non_local %>%
                       group_by(location, year) %>%
                       mutate(biofix_d = replace(biofix_d, which.max(biofix_d != 0), 97)) %>%
                       ungroup() %>%
                       data.table()



bad_CMPOP_non_local[, Cum_dd_in_C := cumsum(biofix_d), by = list(location, year)]






