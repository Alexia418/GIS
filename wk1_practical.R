install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"), 
                     repos = "https://www.stats.bris.ac.uk/R/")
library(sf)
library(tmap)
library(tmaptools)
library(RSQLite)
shape <- st_read("/Users/alexia/Documents/CASA/GIS/GIS-wk1/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
summary(shape) #获取形状文件数据（属性表）中保存的数据摘要
plot(shape) #快速查看形状文件的外观
shape %>% #只是想要几何形状（形状的轮廓）
  st_geometry() %>%
  plot()
library(tidyverse) #加载.scv文件
mycsv <- read_csv("/Users/alexia/Documents/CASA/GIS/GIS-wk1/fly-tipping-borough_new.csv")
mycsv #查看数据
install.packages("dplyr")
library(dplyr)
Hideshape2 <- shape%>% #将.csv加入形状文件。在这里，用.csv中调用的GSS_CODE替换行标签
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="row_labels")
shape%>% #检查合并是否成功，这只会显示前10行
  head(., n=10)
library(tmap)
library(dplyr)
library(tidyr)
library(janitor)
shape_wide <- shape2 %>%
  select(GSS_CODE, NAME, year, total_action_taken, geometry) %>%
  pivot_wider(
    names_from = year,
    values_from = total_action_taken
  ) %>%
  clean_names()   # 把 2011-12 变成 x2011_12 这样可用的列名
names(shape_wide)
library(tmap)
tmap_mode("plot")
qtm(shape_wide, fill = "x2012_13")
tm_shape(shape_wide) + #改蓝
  tm_polygons("x2012_13", palette = "Blues") +
  tm_layout(legend.outside = TRUE)
#最后将形状写入一个新的GeoPackage（.gpkg），给它您选择的层名称
install.packages("sf")
library(sf)
shape %>%
  st_write(.,"/Users/alexia/Documents/CASA/GIS/GIS-wk1/Rwk1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)
library(readr)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),dbname="/Users/alexia/Documents/CASA/GIS/GIS-wk1/Rwk1.gpkg")
con %>%
  dbListTables()
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

con %>% 
  dbDisconnect()