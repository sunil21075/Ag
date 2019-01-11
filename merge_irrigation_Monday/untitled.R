
countyTotalArea <- US_cnt %>% mutate(area = st_area(.)  %>% as.numeric() )
View(countyTotalArea)
dim(US_cnt)
countyTotalAreaColumnsToMerge <-subset(countyTotalArea, select =c("FIPS", "area"))
View(countyTotalAreaColumnsToMerge)
countyTotalAreaColumnsToMerge_df = data.frame(countyTotalAreaColumnsToMerge)
View(countyTotalAreaColumnsToMerge_df)
test<-subset(countyTotalAreaColumnsToMerge_df, select =c("FIPS", "area"))
View(test)
TotalAreaToMerge<-test
Colum_area_merged <-merge(Column_area, TotalAreaToMerge, by ="FIPS")
Colum_area_merged <-merge(Colum_area, TotalAreaToMerge, by ="FIPS")
dim(Colum_area_merged)
View(Colum_area_merged)
colnames(Colum_area_merged)
c = colnames(Colum_area_merged)
c[7] <- "Area_in_FPU"
c[8] <- "total_area"
colnames(Colum_area_merged) <- c
Colum_area_merged
Colum_area_merged$Fraction_Area_inFPU <- Colum_area_merged$Area_in_FPU/Colum_area_merged$total_area
