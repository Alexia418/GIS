library(sf)

shape <- st_read("/Users/alexia/Documents/CASA/GIS/GIS-wk1/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")
#用 st_read() 读取 Shapefile，返回一个 sf 对象并存到 shape
summary(shape) #查看 shape 的字段、要素数、坐标系等摘要信息
#plot(shape)
#快速把整个 sf 对象画出来。它会按属性字段分面绘制并给出“只画前9个字段”的警告；若想全部画：plot(shape, max.plot = 17)（把 17 换成实际字段数）
shape %>% 
st_geometry() %>%
plot()
library(tidyverse)
mycsv <-  read_csv("/Users/alexia/Documents/CASA/GIS/GIS-wk1/paid_employee.csv")
#加载 tidyverse；读取你整理好的 CSV 到 mycsv
mycsv
#检查列名
names(shape)
names(mycsv)
#通过将列设置为数值型来加入ID
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_", 
        by.y="Area_Code")

shape%>%
  head(., n=10)

library(tmap)
tmap_mode("plot")
# change the fill to your column name if different
my_map<-shape %>%
  qtm(.,fill = "Paid_employee")
plot(my_map)