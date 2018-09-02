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
col_names = c("X1", 
              "date", 
              "temp_c", 
              "temp_k", 
              "ftmprt", 
              "sr", 
              "xi", 
              "xs", 
              "ak1", 
              "Inter-S", 
              "Inter-E", 
              "delt", 
              "Portions")

# these are the rows 11 and 12 of the
# excel file in the temp(C) column. i.e. C11 and C12 cells.
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
                         
                        df["ftmprt"] = (const@slp * const@tetmlt) * (df["temp_k"] - const@tetmlt) / df["temp_k"]
                        df["sr"] = exp(df["ftmprt"])
                        
                        df["xi"] = df["sr"] / (1.0 + df["sr"])
                        df["xs"] = const@aa * exp(const@ee / df["temp_k"])
                        
                        df["ak1"] = const@a1 * exp(- const@e1 / df["temp_k"])
                        
                        df[1, "Inter-S"] = 0
                        df[1, "Inter-E"] = df[1, "xs"] - (df[1, "xs"]-df[1, "Inter-S"]) * exp(- df[1, "ak1"])
                        
                        if (df[1, "Inter-E"] < 1){
                          df[2, "Inter-S"] = df[1, "Inter-E"]
                        }
                        else {
                          df[2, "Inter-S"] = df[1, "Inter-E"] - (df[1, "Inter-E"] * df[1, "xi"])
                        }
                        
                        
                        df[2, "Inter-E"] = df[2, "xs"] - (df[2, "xs"] - df[2, "Inter-S"]) * exp(-df[2, "ak1"])
                        
                        df[1, "delt"] = 0
                        df[1, "Portions"] = df[1, "delt"]
                        
                        if (df[2, "Inter-E"] < 1){ 
                          df[2, "delt"]  = 0
                          }
                        else {
                          df[2, "delt"]  = df[2, "Inter-E"] * df[2, "xi"]
                        }
                        
                        df[2, "Portions"] = df[1, "Portions"] + df[2, "delt"]
                        return(df)
                        }



# read the binary RDA data off the disk
# Is type of the data read a data frame??
# the output of typeof(raw_data) is a list. even when I convert it to a data frame
# again it is of type list! So, how should I know if it is a list or data frame?
# is.data.frame(raw_data) : so, yes, it is!
data_path = "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/data_RD.rds"
raw_data = readRDS(data_path)

# add/append new columns, which need to be computed/filled later,
# to the data frame.

raw_data[c('temp_K', 'ftmprt', 'sr', 'xi', 'xs', 'ak1', 'Inter-S', 'Inter-E', 'delt')] = NA




