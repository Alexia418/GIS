

library(sf)
library(here)
st_layers(here("gadm41_AUS.gpkg")) #see what is inside it

Ausoutline <- st_read(here("gadm41_AUS.gpkg"), 
                      layer='ADM_ADM_0')
#read in the GeoPackage layer for the whole of Australia (layer ending in 0)
print(Ausoutline) #check that the coordinate reference systems
#虽然我们能够使用print来识别层的CRS，但另一种选择是找到proj4字符串
library(sf)
st_crs(Ausoutline)$proj4string
#WGS84世界大地测量系统的EPSG代码（通常是大多数空间数据的默认CRS）是4326 
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)
#or more concisely/this is only useful if there is no CRS when you load the data
Ausoutline <- st_read(here("gadm41_AUS.gpkg"), 
                      layer='ADM_ADM_0') %>% 
  st_set_crs(4326)
#重新投影空间数据
#要生成leaflet等软件包中的地图，您的地图需要在WGS84中
#从WGS84更改为GDA94，这是澳大利亚的本地CRS，具有EPSG代码3112
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)
#将几何列中的值与原始文件中的值进行比较
#将sp对象转换为sf并更改投影
#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()
#以上矢量数据 以下光栅数据 WorldClim
library(raster)
library(terra)
jan<-terra::rast(here("wc2","wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan
#projection of WGS84
plot(jan)
#要重新投影光栅，必须重新计算整个网格
#因为矢量只是形状的单个坐标），然后将属性重新估计到新网格。
# set the proj 4 to a new object
pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(pr1)
#back to WGS84
# set the proj 4 to a new object
pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(pr1)
#data loading
#先列出所有文件/搜索包含tif的文件名/选择路径
# look in our folder, find the files that end with .tif and 
library(fs)
dir_info("wc2/")
#从本质上讲，它只是获取您通常在文件资源管理器中看到的详细信息。
#然而，我们可以将此数据与dplyr来选择我们真正想要的数据。
#现在小心点！函数select()存在于dplyr和raster包中，
#因此请确保您使用正确的dplyr::select强制selectdplyr。
library(tidyverse)
listfiles<-dir_info("wc2/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles
#使用来自dplyrpull()），这与通常用于提取列的$相同
#将所有数据直接加载到SpatRaster中。SpatRaster是具有相同空间范围和分辨率的光栅层的集合
worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp
# there are 12 layers (nlyr).
#The stack has loaded the 12 months of average temperature data for us in order.
#To access single layers within the stack:
# access the january layer
worldclimtemp[[1]]
#rename layers
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month
#use our new layer name
worldclimtemp$Jan
#Raster location光栅位置
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)
#将城市名称添加到AUcitytemp的行中
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")
#描述性统计
#使用行名对数据进行子集
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
#or row location
Perthtemp <- Aucitytemp2[3,]
#Histogram直方图
hist(as.numeric(Perthtemp))
#制作珀斯温度的直方图。tibble将数据存储为double，
#basehisthist()函数需要将其存储为数字。
library(tidyverse)
#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

t<-Perthtemp %>%
  dplyr::select(Jan:Dec)

hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")
#查看直方图信息
histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)
histinfo
#breaks — the cut off points for the bins (or bars), we just specified these
#counts — the number of cells in each bin
#midpoints — the middle value for each bin
#density — the density of data per bin
plot(Ausoutline$geom)
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()
#将地图范围设置为澳大利亚的轮廓，然后将我们的WorldClim数据集裁剪到它
#make sure that both of our layers are in the same coordinate reference system when we combine them
print(Ausoutline)
#this works nicely for rasters
crs(worldclimtemp)
Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(Austemp)
#光栅切割
exactAus<-terra::mask(Austemp, Ausoutline)
#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")
#ggplot
#We need to make our raster into a data.frame to be compatible with ggplot2, using a dataframe or tibble
exactAusdf <- exactAus %>%
  as.data.frame()
library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
#put our variable (months) into a one column using pivot_longer()
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )
#Then subset the data, selecting two months using filter() from dplyr
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
#faceted plot
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))
#an interactive histogram using plotly
library(plotly)
# split the data for plotly based on month

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist
# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

# print the top 1
head(meanofall, n=1)
