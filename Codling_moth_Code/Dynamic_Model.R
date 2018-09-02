# convert the Dynamic Model of the excel file to R.

# constants:
e0 = 4.1535E+03
e1 = 1.28888E+04

a0 = 1.40E+05
a1 = 2.57E+18

slp = 1.6
tetmlt = 277
aa = a0 / a1
ee = e1 - e0

setClass("constants", slots = list(e0 = "numeric", 
                                   e1 = "numeric", 
                                   a0 = "numeric",
                                   a1 = "numeric",
                                   slp = "numeric",
                                   tetmlt = "numeric",
                                   aa = "numeric",
                                   ee = "numeric")
         )

const = new("constants", 
            e0 = e0,
            e1 = e1,
            a0 = a0,
            a1 = a1,
            slp = slp,
            tetmlt = tetmlt,
            aa = a0 / a1,
            ee = e1 - e0
            )

# read the binary RDA data off the disk
# Is type of the data read a data frame??
# the output of typeof(raw_data) is a list. even when I convert it to a data frame
# again it is of type list! So, how should I know if it is a list or data frame?
# is.data.frame(raw_data) : so, yes, it is!
data_path = "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/data_RD.rds"
raw_data = readRDS(data_path)


col_names = c("X1", "date", 
              "temp_c", "temp_k", 
              "ftmprt", "sr", 
              "xi", "xs", 
              "ak1", "Inter-S", 
              "Inter-E", "delt", 
              "Portions")

init_temp_c = c(15, 12)

# create initial data frame.
initiate_data_frame = function(col_names, init_temp_c, const)
                       {
                        # Number of columns
                        no_cols = length(col_names)
                          
                        # initiate data fame with number of columns and 2 rows
                        df = data.frame(matrix(ncol = no_cols, nrow = 2))
                        
                        # change the name of columns
                        colnames(df) = col_names
                        
                        # initialize the temp. columns
                        df[1:2, "temp_c"] = init_temp_c
                        df["temp_k"] = df["temp_c"] + 273
                         
                        
                        return(df)
                        }



# add/append new columns, which need to be computed/filled later,
# to the data frame.

raw_data[c('temp_K', 'ftmprt', 'sr', 'xi', 'xs', 'ak1', 'Inter-S', 'Inter-E', 'delt')] = NA




