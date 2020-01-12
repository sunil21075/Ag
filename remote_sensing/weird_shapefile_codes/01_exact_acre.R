


doublecrop_2018_double <- WSDACrop_2018_data[grepl('double',WSDACrop_2018_data$Notes), ]

doublecrop_2018_dbl <- WSDACrop_2018_data[grepl('dbl',WSDACrop_2018_data$Notes), ]

doublecrop_2018 <- rbind(doublecrop_2018_double, doublecrop_2018_dbl)

doublecrop_2015_double <- WSDACrop_2015_data[grepl('double',WSDACrop_2015_data$Notes), ]
doublecrop_2015_dbl <- WSDACrop_2015_data[grepl('dbl',WSDACrop_2015_data$Notes), ]
doublecrop_2015 <- rbind(doublecrop_2015_double, doublecrop_2015_dbl)


doublecrop_2016_double <- WSDACrop_2016_data[grepl('double',WSDACrop_2016_data$Notes), ]
doublecrop_2016_dbl <- WSDACrop_2016_data[grepl('dbl',WSDACrop_2016_data$Notes), ]
doublecrop_2016 <- rbind(doublecrop_2016_double, doublecrop_2016_dbl)

sum(doublecrop_2015$ExactAcres)
sum(doublecrop_2016$ExactAcres)
sum(doublecrop_2018$ExactAcres)


doublecrop_2015_cover <- WSDACrop_2015_data[grepl('cover',WSDACrop_2015_data$Notes), ]
doublecrop_2016_cover <- WSDACrop_2016_data[grepl('cover',WSDACrop_2016_data$Notes), ]
doublecrop_2018_cover <- WSDACrop_2018_data[grepl('cover',WSDACrop_2018_data$Notes), ]

sum(doublecrop_2015_cover$ExactAcres)
sum(doublecrop_2016_cover$ExactAcres)
sum(doublecrop_2018_cover$ExactAcres)



doublecrop_2015_cover <- WSDACrop_2015_data[grepl('cover crop',WSDACrop_2015_data$Notes), ]
doublecrop_2016_cover <- WSDACrop_2016_data[grepl('cover crop',WSDACrop_2016_data$Notes), ]
doublecrop_2018_cover <- WSDACrop_2018_data[grepl('cover crop',WSDACrop_2018_data$Notes), ]

sum(doublecrop_2015_cover$ExactAcres)
sum(doublecrop_2016_cover$ExactAcres)
sum(doublecrop_2018_cover$ExactAcres)