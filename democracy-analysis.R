library(readxl)
data <- read_excel("Democracy Cross-National Data V4.1 09092015.xlsx")
country_data<- (for i in data$Nation){}
gdp_fh_by_year<-data.frame(year=c(1971:2014),
                           )
