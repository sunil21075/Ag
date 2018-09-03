"
This is the dynamic model developed by Kitren Glozer (?)
This is written based on the excel file provided by Kitren Glozer
on http://ucanr.edu/sites/fruittree/How-to_Guides/Dynamic_Model_-_Chill_Accumulation/

How to use the excel file is explained here: http://ucanr.edu/sites/fruittree/files/49320.pdf

You must have hourly data in metric (Centigrade, Celsius) units

          • You must have hourly data in metric (Centigrade, Celsius) units
          • You need consecutive dates and times

The date and time column in there, does not matter. They do not play any
role in computing stuff, however, as mentioned above, they have to be consecutive
and hourly data.

• Online Data sources
     – UC Pestcast weather stations and CIMIS weather stations
       both found at http://www.ipm.ucdavis.edu/WEATHER/wxretrieve.html
"



"
rm(list=lsf.str())
ls()
"
#########################################################
# convert the Dynamic Model of the excel file to R.
#########################################################
#########################################################
#######   Functions
#########################################################

Fahrenheit_to_Celsius = function(temp_cel){
  temp_f = (temp_cel- 32) * 5 / 9
  return (temp_f)
}

# create initial dataframe.
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
  } else {
    df[2, "Inter-S"] = df[1, "Inter-E"] - (df[1, "Inter-E"] * df[1, "xi"])
  }
  
  
  df[2, "Inter-E"] = df[2, "xs"] - (df[2, "xs"] - df[2, "Inter-S"]) * exp(-df[2, "ak1"])
  
  df[1, "delt"] = 0
  df[1, "Portions"] = df[1, "delt"]
  
  if (df[2, "Inter-E"] < 1){ 
    df[2, "delt"]  = 0
  } else {
    df[2, "delt"]  = df[2, "Inter-E"] * df[2, "xi"]
  }
  
  df[2, "Portions"] = df[1, "Portions"] + df[2, "delt"]
  return(df)
}


fill_in_the_table = function(given_table, const){
  given_table[-c(1,2), "temp_k"] = given_table[-c(1,2), "temp_c"] + 273
  
  given_table[-c(1,2), "ftmprt"] = (const@slp * const@tetmlt) * (given_table[-c(1,2), "temp_k"] - const@tetmlt) / 
                                                                                  given_table[-c(1,2), "temp_k"]
  
  given_table[-c(1,2), "sr"] = exp(given_table[-c(1,2), "ftmprt"])
  
  given_table[-c(1,2), "xi"] = given_table[-c(1,2), "sr"] / (1 + given_table[-c(1,2), "sr"])
  
  given_table[-c(1,2), "xs"] = const@aa * exp(const@ee / given_table[-c(1,2), "temp_k"])
  
  given_table[-c(1,2), "ak1"] = const@a1 * exp(- const@e1 / given_table[-c(1,2), "temp_k"])
  
  for (row in 3:dim(given_table)[1]){
    
    # fill in the Inter-S columns
    if (given_table[row-1, "Inter-E"] < 1){
      given_table[row, "Inter-S"] = given_table[row-1, "Inter-E"]
                                    }else{
      given_table[row, "Inter-S"] = given_table[row-1, "Inter-E"] - (given_table[row-1, "Inter-E"] * given_table[row-1, "xi"])
                                    } # end of Inter-S "if" statement
    
    # fill in Inter-E column
    given_table[row, "Inter-E"] = given_table[row, "xs"] - 
                                 (given_table[row, "xs"] - given_table[row, "Inter-S"]) * exp(- given_table[row, "ak1"])
    # fill in the delt column
    if (given_table[row, "Inter-E"] < 1 ){
      given_table[row, "delt"] = 0
    } else { given_table[row, "delt"] = given_table[row, "Inter-E"] * given_table[row, "xi"] }
    # end of delt column "if" statement
    
    # fill in the Portion column
    given_table[row, "Portions"] = given_table[row-1, "Portions"] + given_table[row, "delt"]
    } # end of for loop
  return (given_table)
}


dynamic_model = function(path_to_data, col_names, init_temp_c, const){
  "
  input: path_to_data: path to data to be modeled
         col_names: These are names of columns from excel file
                     by which the model in built. They are supposed
                     to be exactly like that.
        init_temp_c: These are cells c11 and c12 of the excel file.
        const: an object containing all the constant parameters in the cells
                c1 through c8 of the excel file.
       
  output: a data frame called output which contains all the dara build by the
          dynamic model of the excel file.
  "
  
  # initialize data frame with 2 rows.
  all_data = initiate_data_frame(col_names, init_temp_c, const)
  
  # read the binary RDA data off the disk
  raw_data = read.csv(path_to_data)
  
  # number of rows in the raw_data data frame,
  # or equivalently length(raw_data[,1])
  no_rows = dim(raw_data)[1]
  x = data.frame(matrix(nrow = dim(raw_data)[1], ncol = dim(all_data)[2]))
  
  # rename columns of data frame x, so we can
  # concatenate it to all_data
  colnames(x) = colnames(all_data)
  
  # concatenate x and all_data
  all_data = rbind(all_data, x)
  rm(x)
  
  ## Assuming raw_data has three columns in them
  all_data[-c(1, 2), c(1)] = as.character(raw_data[, c(1)])
  all_data[-c(1, 2), c(2)] = raw_data[, c(2)]
  all_data[-c(1, 2), c(3)] = raw_data[, c(3)]
  rm(raw_data)
  
  output = fill_in_the_table(all_data, const)
  return(output)
}
#########################################################
#######   End of Functions
#########################################################


#########################################################
#######   Define parameters of the model
#########################################################

# Define constants
########################################
e0 = 4.1535E+03
e1 = 1.28888E+04

a0 = 1.40E+05
a1 = 2.57E+18

slp = 1.6
tetmlt = 277
aa = a0 / a1
ee = e1 - e0

# Define constant object
########################################
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

# Define column names 
########################################
col_names = c("date", 
              "time", 
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


#############################################
## driver and comparison
#############################################

original_data = read.csv("/Users/hn/Desktop/Kirti/Dynamic\ Model/test_data/model_with_data.csv")

path_to_data = "/Users/hn/Desktop/Kirti/Dynamic\ Model/test_data/raw_data.csv"
R_model_output = dynamic_model(path_to_data, col_names, init_temp_c, const)
head(original_data) - R_model_output[3:8,]

