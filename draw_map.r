library(sp)
library(maps)
library(mapdata)
library(maptools)
library(plyr)
library(ggplot2)
library(mapproj)

options(warn=-1)

# set cities：设置显示的省市名称及对应坐标
cities = c(
    c('Jiangsu', 33.4, 120.9),
    c('Hubei', 31.5, 112.3),
    c('Sichuan', 31.5, 102.1),
    c('Zhejiang', 28.0, 122.8),
    c('Guizhou', 26.5, 106.4),
    c('Hunan', 28.5, 111.5),
    c('Jiangxi', 27.1, 116.5),
    c('Fujian', 25.5, 119.5),
    c('Guangdong', 23.5, 114.1)
)
mat.cities = as.data.frame(matrix(cities, ncol = 3, byrow = T), stringsAsFactors = F)
names(mat.cities) = c('names', 'lat', 'long')
mat.cities$lat = as.numeric(as.character(mat.cities$lat))
mat.cities$long = as.numeric(as.character(mat.cities$long))

# specific names：加入种名，每个种名都对应一个坐标，在坐标上添加标记
spcific_list = c(
    c('C.aurantium L.', 29, 101),
    c('C.aurantium L.', 27, 111.5),
    c('C.aurantium L.', 28.8, 115.2),
    c('C.aurantium L.', 25.8, 117.0),
    c('C.aurantium L.', 22.5, 112),
    c('C.aurantium ‘Huangpi’', 30.5, 113),
    c('C.aurantium ‘Huangpi’', 27.5, 107),
    c('C.aurantium ‘Huangpi’', 27, 113),
    c('C.aurantium ‘Daidai’', 32, 120),
    c('C.aurantium ‘Daidai’', 29, 119),
    c('C.aurantium ‘Daidai’', 29, 103),
    c('C.aurantium ‘Daidai’', 28.8, 117),
    c('C.aurantium ‘Chuluan’', 29, 121),
    c('C.aurantium ‘Tangcheng’', 30, 120.1)
)
mat.spcific = as.data.frame(matrix(spcific_list, ncol = 3, byrow = T), stringsAsFactors = F)
names(mat.spcific) = c('names', 'lat', 'long')
mat.spcific$names = as.character(mat.spcific$names)
mat.spcific$lat = as.numeric(as.character(mat.spcific$lat))
mat.spcific$long = as.numeric(as.character(mat.spcific$long))

# draw city：每个省市分配一个值，按值的大小填充颜色
prov1 = c('江西省', 7)
prov2 = c('浙江省', 1)
prov3 = c('四川省', 5)
prov4 = c('湖南省', 2)

cities = c(prov1, prov2, prov3, prov4)
mat.color = as.data.frame(matrix(cities, ncol = 2, byrow = T), stringsAsFactors = F)
names(mat.color) = c('NAME', 'batch')
mat.color$batch = as.numeric(as.character(mat.color$batch))
mat.color$NAME = as.character(mat.color$NAME)

# read in the map data
china_map = readShapePoly("bou2_4p.shp")
x = china_map@data
xs = data.frame(x,id=seq(0:924)-1)
china_map1 = fortify(china_map)
china_map_data = join(china_map1, xs, type="full", by='id')
china_map_data$NAME = iconv(china_map_data$NAME,from="CP936", to="UTF-8")
china_data <- join(china_map_data, mat.color, type="full", by='NAME')
china_data$batch[is.na(china_data$batch)] <- 0

china <- map(china_map, plot = F)

p1 = ggplot() + 

    #投影方式，防止地理图像的变形
    coord_map("mercator") +

    # 按value值的大小，给省市添加颜色
    geom_polygon(data=china_data, aes(long, lat, group=group, fill=batch)) +
    scale_fill_gradient(low="white",high="red") +

    # 绘制地图边线
    geom_path(data=china, aes(long, lat, group=group), color='grey45', show.legend=F) +

    # 添加种名及种对应的三角标记
    geom_point(data=mat.spcific, aes(x=long, y=lat, color=names), alpha=1, size=1.5, shape=17) +

    # 添加地图城市名
    geom_text(data = mat.cities, aes(x=long, y=lat, label=names), check_overlap=TRUE, family="Times New Roman", size=3) +

    # 固定长宽比例
    # coord_fixed(ratio=1.4) + 

    # 背景消除
    theme(
        panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
    ) +

    # 文字注释
    annotate("text", x=116, y=0, label=expression(italic(C.aurantium)), color= "black", size=4, family = "Times New Roman") +
    annotate("text", x=103, y=0, label='Distribution of ', color= "black", size=4, family = "Times New Roman") 
p1

ggsave(file="map.jpeg", width=180, height=180, units='mm')