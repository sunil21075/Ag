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
setnames(data, old=c("old_name", "another_old_name"), new=c("new_name", "another_new_name"))

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


# initialize data frame data table dataframe data table
table = data.frame()
data <- setNames(data.table(matrix(nrow = 0, ncol = 3)), c("va", "vb", "vc"))
data <- data.table(lat=numeric(), long=numeric(), distances=numeric(), sigma=numeric())

data <- setNames(data.table(matrix(nrow = 3, ncol = 6)), 
                 c("future_fip",  "model", "time_period", 
                   "top_1_fip", "top_2_fip", "top_3_fip"))

data = data.table(future_fip = c(target_fip, target_fip, target_fip),
                  model = c(model_n, model_n, model_n),
                  time_period = c("F1", "F2", "F3"),
                  emission = c(emission, emission, emission),
                  top_1_fip = c("NA", "NA", "NA"),
                  top_2_fip = c("NA", "NA", "NA"),
                  top_3_fip = c("NA", "NA", "NA")
                  )


DT = data.table(row_count = c("b","b","b","a","a","c"),
                a = 1:6,
                b = 7:12,
                c = 13:18)

DT = data.table(row_count = 1:3)

dtr <- structure(list(location = c("NYC", "NYC", "NYC","NYC", "NYC", 
                                   "LA", "LA", "LA", "LA", "LA"), 
                 year = c(2026, 2026, 2026, 2026, 2026,
                          2026, 2026, 2026, 2026, 2026),
                 value = c(1, 2, 3, 4, 5,
                           6, 7, 8, 9, 10)),
                 class = "data.table", 
                 row.names = c(NA, -10L))

dtr <- structure(list(location = c("NYC", "LA"), 
                 year = c(2026, 2026),
                 value = c(2, 7)),
                 class = "data.table", 
                 row.names = c(NA, -10L))


start <- data.table(start=rep("start", nrow(something)))
############################################################
#################### Install packages on aeolus

https://docs.aeolus.wsu.edu/docs_running_applications.html

qsub -I [job script].sh

module load gcc/7.3.0
module load r/3.5.1/gcc/7.3.0
R
install.packages(c("foreign"), lib="~/.local/lib/R3.5.1", repos="https://ftp.osuosl.org/pub/cran")

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


###### Convert a data table vector to a list:
unlist(as.list(t(analogs)))
as.list(as.data.table(t(analogs)))
x <- sapply(analogs, function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
lat = x[1, ]; long = x[2, ];
analogs <- paste0(lat, "_", long)


Drop the word county: https://stackoverflow.com/questions/55599225/drop-a-word-in-a-column-of-data-table-in-r/55599424#55599424
counties[, COUNTY := sub("\\s+County$", "", COUNTY)]
df$county = gsub("county", "", df$county)



######## compute frequency, aggregation

values <- data.frame(query = c("q1", "q1", "q1","q2", "q2"),
                     NN = c("a", "a", "b", "b", "b"),
                     freq=c(53, 20, 10, 3, 10),
                     model = c("m1", "m1", "m1", "m1", "m1"))

nr.of.appearances <- aggregate(x = values, 
                               by = list(unique.values = values$value), 
                               FUN = length)

values[, .(var = sum(freq)), by = c("query", "NN", "model")]


# Cartesian paste

model_namess <- c("bcc_csm1_1_m", "BNU_ESM", "CanESM2", "CNRM_CM5", "GFDL_ESM2G", "GFDL_ESM2M")
local_county_names <- unique(local_cnty_fips$st_county)
local_county_names <- sapply(local_county_names, function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
local_county_names = paste0(tolower(x[2, ]), "_")
plot_names <- do.call(paste, expand.grid(model_namess, local_county_names, sep='_', stringsAsFactors=FALSE))


###### mask


A_sigma
year          location    sigma_NN_1 sigma_NN_2 sigma_NN_3
2076 43.59375_-116.78125  1.4681173   1.664289   1.735974
2077 43.59375_-116.78125  1.3798515   1.550524   1.551269
2078 43.59375_-116.78125  0.7934367   1.064248   1.177981
2079 43.59375_-116.78125  1.8235574   1.991018   2.288402
2080 43.59375_-116.78125  2.5560329   2.578093   2.589334


A_NN
year            location       location_NN_1      location_NN_2      location_NN_3
2076 43.59375_-116.78125  41.15625_-90.65625 41.21875_-90.65625 41.15625_-90.65625
2077 43.59375_-116.78125  43.34375_-78.15625 43.34375_-78.21875 43.28125_-78.15625
2078 43.59375_-116.78125  41.34375_-90.78125 41.21875_-90.65625 41.53125_-73.96875
2079 43.59375_-116.78125 43.53125_-116.78125 41.34375_-90.78125 41.71875_-74.15625
2080 43.59375_-116.78125  41.34375_-90.78125 41.96875_-86.21875 41.21875_-90.65625


output
year            location       location_NN_1      location_NN_2      location_NN_3
2076 43.59375_-116.78125  41.15625_-90.65625 41.21875_-90.65625 41.15625_-90.65625
2077 43.59375_-116.78125  43.34375_-78.15625 43.34375_-78.21875 43.28125_-78.15625
2078 43.59375_-116.78125  41.34375_-90.78125 41.21875_-90.65625 41.53125_-73.96875
2079 43.59375_-116.78125 43.53125_-116.78125 41.34375_-90.78125                 NA
2080 43.59375_-116.78125                  NA                 NA                 NA


nm1 <- grep("sigma", names(A_sigma), value = TRUE)
i1 <- setDT(A_sigma)[, Reduce(`&`, lapply(.SD, <, 2)), .SDcols = nm1]
setDT(A_NN)[i1] 

A_NN A_sigma
A_NN[, -c(1:2)][A_sigma[, -c(1:2)] >= 2] <- NA

dfb[-c(1,2)][!(dfa[-c(1,2)] < 2)] <- NA
dfb[-c(1:2)][dfa[-c(1:2)] >= 2] <- NA

nm1 <- grep("sigma", names(A), value = TRUE)
i1 <- setDT(A)[, Reduce(`&`, lapply(.SD, `<`, 2)), .SDcols = nm1]
setDT(B)[i1] 


nm2 <- grep("sigma", names(A))
B[, (nm2) := Map(function(x, y) replace(x, y >= 2, NA_character_),
        .SD, A[, nm2, with = FALSE]), .SDcols = nm2][]


df
year            location       location_NN_1      location_NN_2      location_NN_3
2076 43.59375_-116.78125  41.15625_-90.65625  41.21875_-90.65625 41.15625_-90.65625
2077 43.59375_-116.78125  43.34375_-78.15625  43.34375_-78.21875 43.28125_-78.15625
2078 43.59375_-116.78125  41.34375_-90.78125  41.21875_-90.65625 41.53125_-73.96875
2079 43.59375_-116.78125  43.53125_-116.78125 41.34375_-90.78125               <NA>
2080 43.59375_-116.78125                <NA>               <NA>                <NA>


counties
fips   location
36073  43.34375_-78.15625
17161  41.34375_-90.78125

output = 
year            location       location_NN_1      location_NN_2      location_NN_3
2076 43.59375_-116.78125               17131             so_on             so_on
2077 43.59375_-116.78125               36073             so_on             so_on
2078 43.59375_-116.78125               17161             so_on             so_on
2079 43.59375_-116.78125      so_on, so_forth            so_on               <NA>
2080 43.59375_-116.78125                <NA>               <NA>              <NA>


dataframe[, seq(3, ncol(dataframe))] <- mapvalues(dataframe[, seq(3, ncol(dataframe))], 
                                        from = counties$location, to = counties$fips)

df_A_NN[3:5] <- lapply(df_A_NN[3:5], function(x) counties$fips[match(x, counties$location)])




df
location   NN_1    NN_2   NN_3
    NYC    17      17      17
    NYC    17      16      1
    LA     1        1      10
    LA     16      10      1

output
location   NNs  freq
    NYC    17      4
    NYC    16      1
    NYC     1      1
    LA      1      3
    LA      16     1
    LA      10     2

df.groupby('location').count()

############################################################
df <- structure(list(location = c("NYC", "NYC", "LA", "LA"), 
                     NN_1 = c(17, 17, 1, 16), 
                     NN_2 = c(17, 16, 1, 10), 
                     NN_3 = c(17, 1, 10, 1)),
                     class = "data.frame", 
                     row.names = c(NA, -4L))

df %>% 
tidyr::gather("key", "NNs", 2:ncol(.)) %>% 
group_by(location, NNs) %>% 
summarize(freq = n()) %>% 
arrange(desc(location), desc(NNs))

as.data.frame(table(cbind(df[1], NNs=unlist(df[-1]))))

################################################################

# count number of unique elements in a column after filtering

a %>% filter(fips == target_fip) %>% summarise(count = n_distinct(location))
a  %>% filter(fips == target_fip) %>% distinct(location) %>% count()

setDT(dt)[year == 2026, .(count = uniqueN(location))]


# summary summerize summerise summarize summarise
ddply(us_feat, ~ fips, summarise, mean=mean(age), sd=sd(age))
dt[,list(mean=mean(age),sd=sd(age)),by=group]



kth smallest element in group by
https://stackoverflow.com/questions/56084877/k-th-smallest-element-per-group-in-r/56085151#56085151

# Replace values in column ifelse quick, short
https://mgimond.github.io/ES218/Week03a.html
dat.overwrite <- mutate(dat, Country = ifelse(Country == "Canada", "CAN", "USA"))


data = data.table(year = c(2005, 2006, 2006, 2006, 2006),
                  month = c(1, 1, 1, 2, 10),
                  day = c(10, 20, 30, 40, 50))

data = data.table(city = c("NYC", "NYC", "NYC", "LA", "LA", "LA", "LA"),
                  year = c(2000, 2000, 2000, 2000, 2000, 2000, 2000),
                  target = c(0, 1, 1, 0, 0, 1, 1))

data = data.table(city = c("NYC", "NYC", "NYC", "LA", "LA", "LA", "LA"),
                  year = c(2000, 2000, 2000, 2000, 2000, 2000, 2000),
                  target = c(0, 666, 1, 0, 0, 666, 1))


# replace the first nonzero with another thing after group_by:

data %>%
group_by(city, year) %>%
mutate(target = replace(target, which.max(target != 0), 666))

OR 
i1 <- data[, .I[target != 0][1], .(city, year)]$V1
data[i1, target := 666][]


OR (not tested)
library(tidyverse)
data %>%
   group_by(city, year) %>% 
   mutate(target = replace(target, which(target != 0)[1], 666))

OR (not tested)
data %>% 
   group_by(city, year) %>%
   mutate(target = replace(target, match(1, target), 666))




