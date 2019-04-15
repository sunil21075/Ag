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

######## The same
A = A[, .(mean_gdd = mean(CumDDinF)), by = c("location", "year")]

B <- B %>%
     group_by(location, year) %>%
     summarise_at(.funs = funs(mean(., na.rm=TRUE)), vars(CumDDinF))%>% 
     data.table()
######## 
# Chnage name of a columns
colnames(data)[colnames(data)=="old_name"] <- "new_name"
setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))

# order a data by a/multiple column. Adding a negative would make the ordering reverse
A <- A[order(location), ]

result <- dataT %>%
            mutate(thresh_range = cut(get(col_name), breaks = bks )) %>%
            group_by(lat, long, climate_type, time_period, 
                     thresh_range, model, scenario) %>%
            summarize(no_years = n_distinct(Chill_season)) %>% 
            data.table()


quan_per_feb <- feb_result %>% 
                group_by(climate_type, time_period, scenario, thresh_range) %>% 
                summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% 
                data.table()

# count number of NA in each column
all_data_dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:9]


# change order of columns of data table
setcolorder(x, c("c", "b", "a"))

# reshape a vector into matrix
d <- matrix(NN.dist, nrow = 70, byrow = FALSE)


df.melted <- melt(myDF[, -1], id.vars = NULL)
myNewDF <- t(df.melted[, 2])
colnames(myNewDF) <- paste0("r", rownames(myDF), df.melted[, 1])


# initialize data frame data table dataframe datatable
table = data.frame()
data <- setNames(data.table(matrix(nrow = 0, ncol = 3)), c("va", "vb", "vc"))
data <- data.table(lat=numeric(), long=numeric(), distances=numeric(), sigma=numeric())
############################################################
#################### Install packages on aeolus

https://docs.aeolus.wsu.edu/docs_running_applications.html

qsub -I [job script].sh

module load gcc/7.3.0
module load r/3.5.1/gcc/7.3.0
R
install.packages(c("chron", "shinydashboard"), lib="~/.local/lib/R3.5.1", repos="https://ftp.osuosl.org/pub/cran")

Ctrl-d
exit

Finally, you’ll need to create a file to tell R where 
your packages live. Create a file called .Renviron 
in your home directory, and specify your library directory:
R_LIBS_USER=~/.local/lib/R3.5.1


#################### Install packages on aeolus ^^^^^^^^^^^^
############################################################
# count number of NA in columns
colSums(is.na(dt)|dt == '')


strsplit vector 

x <- sapply(all_us_locations_cod_moth, function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
lat = x[1, ]
long = x[2, ]



# not in, opposite of %in%
D2 = subset(local_locs, !(local_locs %in% local_files))

