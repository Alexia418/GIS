A <- 1
B <- 2
C <- A+B
C
ls()
rm(A) #remove
function(object, argument1, argument2, argument3)
X<-function(data, argument1, argument2, argument3)
#create some datasets, first a vector of 1-100 and 101-200
#c:将一串数字串成向量
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")  
#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")
df <- data.frame(Data1, Data2)
plot(df, col="green")
library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
  head()
df %>%
  tail()
#引用dataframe元素/子集 data.frame[row,column]
df[1:10, 1] #取第 1 到第 10 行，第 1 列
df[5:15,] #取第 5 到第 15 行，所有列
df[c(2,3,6),2] #取第 2、3、6 行的第 2 列
df[,1] #取所有行的第 1 列
library(dplyr) #数据操作/tidyverse的子包
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)
df %>% 
  dplyr::select(column1)
df$column1
df[["column1"]]
LondonDataOSK<- read.csv("ward-profiles-excel-version.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")
install.packages("here")
library(here)
here::here()
LondonDataOSK<- read.csv(here::here("ward-profiles-excel-version.csv"), 
                             header = TRUE, sep = ",",  
                             encoding = "latin1")
#wang the data in straight from the web using read_csv, 
#skipping over the 'n/a' entries as you go...
LondonData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
#告诉 R：这个文件是用 latin1 编码存的，按这个方式读取，避免乱码
#把文件里出现的 n/a 字符串当成缺失值（NA）
class(LondonData) #check what data type your new data set is
# or, if you have your old skool data
class(LondonDataOSK)
#我们还可以在另外两个函数——dplyr包的summarize_all()和tidyr包的pivot_longer()中使用base R的class()函数
#来检查数据是否正确读取，例如确保数值型数据没有被当作文本或其他变量读取
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist #可以看到所有数字的列都以数字的形式读取
#再次读取londondata，这次没有排除n/a
LondonData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))
LondonData <- edit(LondonData)#快速编辑数据 mac运行不了需要安装
summary(df)
LondonData%>%
  colnames()%>%
  # just look at the head, top5
  head()
#通过明确指定我们需要的行范围来选择我们需要的行
LondonBoroughs<-LondonData[626:658,]
#或者用slice函数
LondonBoroughs<-LondonData%>%
  dplyr::slice(626:658)
#根据一个变量进行过滤，例如提取所有女性预期寿命超过90的病房
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)
install.packages("stringr")
library(stringr)
LondonBoroughs<- LondonData %>% 
  filter(str_detect('New code', "^E09"))
#E09 refers to the things that start with E09 not the cell of E9
LondonBoroughs <- LondonData %>%
  dplyr::filter(stringr::str_detect(`New code`, "^E09"))
#之前是“向量”而不是“数据框 chatgpt改的
LondonBoroughs$`Ward name`
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()
library(dplyr)
LondonBoroughs<-LondonBoroughs %>%
  distinct()
#select columns 1,19,20 and 21 选择列
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
#select columns 1,19,20 and 21
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name")) 
#重命名列
install.packages("janitor")
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()
LondonBoroughs <- LondonBoroughs %>%
  #here the ., means all data
  clean_names(., case="big_camel")
#只选择自治市名称、平均预期寿命和归一化预期寿命，根据归一化预期寿命按降序排列输出
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (MaleLifeExpectancy2009_13 +
                                   FemaleLifeExpectancy2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(NewCode,
                Borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))
#又报错了 数据没洗干净 用了names(LondonBoroughs)
#top of data
slice_head(Life_expectancy, n=5)
#bottom of data
slice_tail(Life_expectancy,n=5)
#使用case_when()如果值大于81.16，我们可以为区分配一串“高于英国平均水平”
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2
#如果我们想知道伦敦行政区高于全国平均水平的预期寿命范围
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group
#再次计算出区预期寿命与全国平均水平之间的差异
#根据该列是否为数字，对整个表格进行四舍五入
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())
#将UKdiff列四舍五入至0位小数（不新增列）
#使用case_when()函数筛选平均年龄≥81岁的行政区，创建新列
#随后按UKcompare列分组
#最后统计每组数据的数量
Life_expectancy3
#制作成图
Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))
#画图
plot(LondonBoroughs$MaleLifeExpectancy2009_13,
     LondonBoroughs$PercentChildrenInReceptionYearWhoAreObese2011_12To2013_14)
install.packages("plotly")
library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~MaleLifeExpectancy2009_13, 
        #data for y axis
        y = ~PercentChildrenInReceptionYearWhoAreObese2011_12To2013_14, 
        #attribute to display when hovering 
        text = ~Borough, 
        type = "scatter", 
        mode = "markers")
install.packages(c("tmap"))

# might also need these ones
install.packages(c("tmaptools", "sf"))
#Load Packages (ignore any error messages about being built under a 
#different R version):
library(tmap)
library(tmaptools)
library(sf)
#制作地图
# this will take a few minutes
# geojson in local folder
# EW <- st_read(here::here("prac2_data",
#                          "LAD_Dec_2015_FCB_GB.geojson"))

# shapefile in local folder
EW <- st_read(here::here("LAD_Dec_2015_FCB_GB_2022_-1836983239597816196","LAD_Dec_2015_FCB_GB.shp"))
LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))
#我们将从sf对象的“lad15cd”列数据框架中查找与伦敦（E09）相关的区号

#plot it using the qtm function
qtm(LondonMap)
#将属性数据加到边界
LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)
BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))
library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

#mac需要java才行跳过这段
tmaplondon <- BoroughDataMap %>%
  # st_bbox gives the bounding x and y coordinates 
  st_bbox(.) %>% 
  #note type="osm" gives error atm - issue raised on github: https://github.com/r-tmap/tmaptools/issues/41
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tmap_mode("plot")

tm_shape(tmaplondon)+
  # add basemap as Red Green Blue raster
  tm_rgb()+
  # add sf data
  tm_shape(BoroughDataMap) + 
  # add polygon layer
  tm_polygons(fill="rate_of_job_seekers_allowance_jsa_claimants_2015",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "rate of claimants 2015", 
                                      size = 0.8))+
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Job seekers' Allowance Claimants", 
           size = 2,
           position = c("center", "top"))

BoroughDataMap84 <- BoroughDataMap %>%
  st_transform(.,4326)

tmap_mode("view")
tm_shape(BoroughDataMap84) + 
  # add polygon layer
  tm_polygons(fill="rate_of_job_seekers_allowance_jsa_claimants_2015",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "Job seekers' Allowance Claimants", 
                                      size = 0.8))+
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Job seekers' Allowance Claimants", 
           size = 2,
           position = c("center", "top"))

Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("lad15cd" = "NewCode"))%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

tmap_mode("plot")
  # add sf data
  tm_shape(Life_expectancy4map) + 
  # add polygon layer
  tm_polygons(fill="UKdiff",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "Number of years", 
                                      size = 0.8))+
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Difference in life expectancy", 
           size = 2,
           position = c("center", "top"))

#整理数据
library(readr)
flytipping <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv")
flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                            col_types = cols(
                              code = col_character(),
                              area = col_character(),
                              year = col_character(),
                              total_incidents = col_number(),
                              total_action_taken = col_number(),
                              warning_letters = col_number(),
                              fixed_penalty_notices = col_number(),
                              statutory_notices = col_number(),
                              formal_cautions = col_number(),
                              injunctions = col_number(),
                              prosecutions = col_number()
                            ))
# view the data
View(flytipping1)  

library(tidyr)
#convert the tibble into a tidy tibble
flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )

# view the data
View(flytipping_long)

#an alternative which just pulls everything out into a single table
flytipping2 <- flytipping1[,1:4]

#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

View(flytipping_wide)

widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)    
