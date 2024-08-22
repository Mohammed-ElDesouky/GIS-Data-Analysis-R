#Scripted by: Mohammed ELDESOUKT
# Date: 16/11/2021
# Title: GIS data processing

# Setting up the work space  -----------------------------------------------

rm(list = ls())
memory.limit(size = 70000)

install.packages(c(
  "sp",
  "sf",
  "tidyverse",
  "raster",
  "rgdal",
  "shiny",
  "shinyjs",
  "tmap",
  "ggplot2",
  "FNN",
  "ggforce",
  "plotly"
))

library (sp)
library (rgdal)
library (raster)
library(shiny)
library(shinyjs)
library(ggplot2)
library(tmap)
library(sf)
library(tidyverse)
library(FNN)
library(ggforce)
library(plotly)



# Setting directory -------------------------------------------------------

getwd()
wd1 <- setwd("C:/Users/DELL/Downloads/World Bank/India project/Cleaned data")
wd2 <- "E:/"
dir()
setwd(wd2)

# Importing raster and shape files ----------------------------------------

library(data.table)

vdsa <- fread('E:/VDSA_all.csv')
well <- fread('tube_well_all.csv')
vill <- fread("lo_lat.csv")

well <- fread('near_r10.csv')
setwd(wd2)

vdsa <- fread('VDSA_all.csv')


vdsa_near10 <- merge(
  vdsa,
  well,
  by = c("year", "village"),
  all.x = T,
  all.y = F,
  allow.cartesian = TRUE
)


colnames(vdsa_near10)
View(vdsa_near10)
check <- head(vdsa_near10, 1000)


vdsa <- fread("E:/near_r10_2.csv")
fwrite(vdsa_near10, "E:/near_r10_2.csv")
wd2

DT <- setDT(vdsa)


# Addressing duplicates 
any(duplicated(DT))

any(duplicated(dat, by = c("var1", "var2")))
uniqueN(
  DT,
  by = c(
    "vdsid_hhid",
    "culthhvdsidentifier",
    "nameoftheseason",
    "nameofthecrop",
    "nameofthevarietyofcrop",
    "plotcropareainacres",
    "rentalvalueinrsacre",
    "percentageareaofthecrop",
    "nameoftheoperation",
    "typeoflabour",
    "date_of_operation",
    "nameofthematerial",
    "roundnumber",
    "sourceofmaterial",
    "year",
    "ageofthemember",
    "relationtothehead",
    "yearsofeducation",
    "serialnumberofthemember",
    "wlcode"
  )
)


dup <- duplicated(
  DT,
  by = c(
    "vdsid_hhid",
    "culthhvdsidentifier",
    "nameoftheseason",
    "nameofthecrop",
    "nameofthevarietyofcrop",
    "plotcropareainacres",
    "rentalvalueinrsacre",
    "percentageareaofthecrop",
    "nameoftheoperation",
    "typeoflabour",
    "date_of_operation",
    "nameofthematerial",
    "roundnumber",
    "sourceofmaterial",
    "year",
    "ageofthemember",
    "relationtothehead",
    "yearsofeducation",
    "serialnumberofthemember",
    "wlcode"
  )
)

sub <- DT
head(DT)

ls(well)
ls(vill)

#renaming features
colnames(df)
cor_well <- df[, c("state", "village", "well_lat", "well_long")]
cor_vill <- df[, c("state", "village", "latitude", "longitude")]

# Correlating wells to villages using coordinates
matches <- knnx.index(cor_well, cor_vill, k = 1)
matches

res <- vill
res$lat  <- well$lat[matches[, 1]]
res$lon  <- well$lon[matches[, 1]]
res$wlcode  <- well$wlcode[matches[, 1]]

ls(res)
res_sub <- res[, c("village", "lat", "lon", "wlcode")]

vdsa_wel <- merge.data.frame(vdsa, res_sub, by = "village", all = T)
length(vdsa_wel[, "wlcode"])
length(which(is.na((vdsa_wel[, "wlcode"]))))

vdsa_near1 <- merge(
  vdsa_wel,
  well,
  by = c("year", "wlcode"),
  all.x = T,
  all.y = F
)

ls(vdsa_near1)


#Graphing average ground water levels per season over the time period of 15 years
setwd(wd2)
df <- fread("near_r25_up.csv")

colnames(df)

select <- DT[, .(village, year, wl)]


setDT(df)
colnames(df)


x_axis_labels <- min(df[, year]):max(df[, year])

colors <- c(
  "av.monsoon" = "indianred1",
  "av.postmonsoonkharif" = "lightblue2",
  "av.premonsoon" = "limegreen",
  "av.premonsoon" = "light goldenrod1"
)


df2 <- df %>% tidyr::pivot_longer(
  cols = c(
    m_monsoon_yr,
    m_postmonsoonkharif_yr,
    m_postmonsoonrabi_yr,
    m_premonsoon_yr
  )
)

ch <- ggplot(data = df2, aes(x = year, y = value, color = name))
q1 <- ch + geom_line(size = .8) + geom_point(aes(shape = name), size = 3) + theme_classic() + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) + theme(legend.title = element_blank()) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "Black",
      size = 18
    ),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12)
  ) +
  ggtitle("Avg GW per Season (2001-2014)") +
  xlab("Year") + ylab("Average GW levels")


#Graphing average ground water levels per village over the time period of 15 years
ch1 <- ggplot(data = df, aes(
  x = year,
  y = avg_yr_vl,
  color = village,
  group = village
))
q2 <- ch1 + geom_line() + geom_point() +
  theme_classic() + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  theme(legend.title = element_blank()) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "Black",
      size = 18
    ),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12)
  ) +
  ggtitle("Avg annual GW levels per village (2001-2014)") +
  xlab("Year") + ylab("Average GW levels")


#Graphing average ground water levels per village and per season over the time period of 15 years
df_mod <- rename(
  df,
  monso = avg_moonsoon_all,
  kharif = avg_postmonsoonkharif_all_we,
  rabi = avg_postmonsoonrabi_all_we,
  premonso = avg_premonsoon_all_we
)

df3 <- df_mod %>% tidyr::pivot_longer(cols = c(monso, kharif, rabi, premonso))

ch3 <- ggplot(data = df3, aes(x = year, y = value, color = name))

pdf("~/villages_all.pdf", paper = 'A4r', w = 30, 30)

w <- ch3 + geom_line() + geom_point(aes(shape = name)) + facet_wrap(. ~ village) + theme_minimal() + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  theme(legend.title = element_blank()) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "Black",
      size = 18
    ),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12)
  ) +
  ggtitle("Avg Annual GW per Village per Season (2001-2014)") +
  xlab("Year") + ylab("Average GW levels")


#Graphing average ground water levels per state and per season over the time period of 15 years

df_mod <- rename(
  df,
  monso = avg_state_ms,
  kharif = avg_state_pmnk,
  rabi = avg_state_pmsr,
  premonso = avg_state_prems
)


df5 <- df_mod %>% tidyr::pivot_longer(cols = c(monso, kharif, rabi, premonso))
state_labs <- c(
  "andhra pradesh- 4 vils",
  "bihar- 4 vils",
  "gujarat- 4 vils",
  "jharkhand- 4 vils",
  "karnataka- 4 vils",
  "madhya pradesh- 2 vils",
  "maharashtra- 4 vils",
  "orissa- 4 vils"
)
names(state_labs) <- c(
  "andhra pradesh",
  "bihar",
  "gujarat",
  "jharkhand",
  "karnataka",
  "madhya pradesh",
  "maharashtra",
  "orissa"
)

ch4 <- ggplot(data = df5, aes(x = year, y = value, color = name))

v <-  ch4 + geom_line() + geom_point(aes(shape = name)) + facet_wrap(. ~
                                                                       state,
                                                                     ncol = 2,
                                                                     labeller = labeller(state = state_labs)) + theme_minimal() + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  theme(legend.title = element_blank()) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "Black",
      size = 18
    ),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12)
  ) +
  ggtitle("Avg Annual GW per State per Season (2001-2014)") +
  xlab("Year") + ylab("Average GW levels")



#A geo-spatial distribution map for each individual state; showing distribution of villages and the wells across each villages

setwd("C:/Users/DELL/Downloads/World Bank/GIS/shape")
shape0  <- readOGR('gadm36_IND_1.shp', stringsAsFactors = F)
shape <- readOGR('C:/Users/DELL/Downloads/New folder (2)/DSMW.shp',
                 stringsAsFactors = F)
shape1 <- subset(shape, COUNTRY == "INDIA")


summary(shape0@data)
map <- ggplot() + geom_polygon(
  data = shape0,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) + theme_void()

head(shape0@data$NAME_1, 100)

install.packages("rgeos")

ggplot() + geom_polygon(
  data = shape0,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +
  ggplot() + geom_polygon(data = shape1, aes(x = long, y = lat))

Gujarat <-  subset(shape0, NAME_1 == "Gujarat")
Andhra_Pradesh <- subset(shape0, NAME_1 == "Andhra Pradesh")
Bihar <- subset(shape0, NAME_1 == "Bihar")
Jharkhand <- subset(shape0, NAME_1 == "Jharkhand")
Karnataka <- subset(shape0, NAME_1 == "Karnataka")
Madhya_Pradesh <- subset(shape0, NAME_1 == "Madhya Pradesh")
Maharashtra <- subset(shape0, NAME_1 == "Maharashtra")
Odisha <- subset(shape0, NAME_1 == "Odisha")

cor_all <- df[, c("state",
                  "village",
                  "well_lat",
                  "well_long",
                  "latitude",
                  "longitude")]

cor_all_GU <- subset(cor_all, state == "gujarat")
cor_all_AP <- subset(cor_all, state == "andhra pradesh")
cor_all_BI <- subset(cor_all, state == "bihar")
cor_all_JH <- subset(cor_all, state == "jharkhand")
cor_all_KA <- subset(cor_all, state == "karnataka")
cor_all_MP <- subset(cor_all, state == "madhya pradesh")
cor_all_MA <- subset(cor_all, state == "maharashtra")
cor_all_OD <- subset(cor_all, state == "orissa")


GU <- ggplot() + geom_polygon(
  data = Gujarat,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_GU,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_GU,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles) - Gujarat") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))


AP <- ggplot() + geom_polygon(
  data = Andhra_Pradesh,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_AP,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_AP,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles- Andhra Pradesh)") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))

BI <- ggplot() + geom_polygon(
  data = Bihar,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_BI,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_BI,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles) - Bihar") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))

JH <- ggplot() + geom_polygon(
  data = Jharkhand,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_JH,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_JH,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles) - Jharkhand") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))

KA <- ggplot() + geom_polygon(
  data = Karnataka,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_KA,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_KA,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles) - Karnataka") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))

MP <- ggplot() + geom_polygon(
  data = Madhya_Pradesh,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_MP,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_MP,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles) - Madhya_Pradesh") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))

MA <- ggplot() + geom_polygon(
  data = Maharashtra,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_MA,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_MA,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles) - maharashtra") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))

OD <- ggplot() + geom_polygon(
  data = Odisha,
  aes(x = long, y = lat, group = group),
  colour = "black",
  fill = NA
) +  geom_point(
  data = cor_all_OD,
  aes(x = longitude, y = latitude, fill = village),
  alpha = 0.8,
  size = 3,
  shape = 21
) +
  geom_point(
    data = cor_all_OD,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  theme_void() + ggtitle("Distribution of villages (dots) and wells (black circles) - Odisha") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  ))




map_bounds <- c(
  left = 68.62,
  bottom = 7.75,
  right = 92,
  top = 29.2
)

coords.map <- get_stamenmap(map_bounds, zoom = 12, maptype = "terrain")

map <- ggmap(coords.map) +
  geom_point(
    data = cor_vill,
    aes(x = longitude, y = latitude),
    fill = "red",
    alpha = 0.8,
    size = 3,
    shape = 21
  ) +
  geom_point(
    data = cor_well,
    aes(x = well_long, y = well_lat),
    alpha = 0.8,
    size = 5,
    shape = 21
  ) +
  ggtitle("Distribution of villages (red dots) and wells (black circles)") +
  theme(plot.title = element_text(
    hjust = 0.5,
    color = "Black",
    size = 18
  )) + theme_minimal()



map2 <- ggmap(coords.map) +
  geom_point(
    data = cor_well,
    aes(x = well_long, y = well_lat, alpha = 0.8),
    size = 2,
    shape = 21
  ) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE)


mapgilbert <- get_map(
  location = c(
    lon = mean(cor_vill$lonitude),
    lat = mean(cor_vill$latitude)
  ),
  zoom = 4,
  maptype = "toner-lite",
  scale = 2
)



#Individual line graphs for each village showing the overtime ground water levels per season.
line_chart <- ggplot(data = df, aes(x = year))
x_axis_labels <- min(df[, year]):max(df[, year])

colors <- c(
  "av.monsoon" = "indianred1",
  "av.postmonsoonkharif" = "lightblue2",
  "av.premonsoon" = "limegreen",
  "av.premonsoon" = "light goldenrod1"
)


q <- line_chart + geom_line(aes(y = m_monsoon_yr, color = "av.monsoon"), size =
                              .8) +
  geom_line(aes(y = m_postmonsoonkharif_yr, color = "av.postmonsoonkharif"),
            size = .8) +
  geom_line(aes(y = m_postmonsoonrabi_yr, color = "av.postmonsoonrabi"),
            size = .8) +
  geom_line(aes(y = m_premonsoon_yr, color = "av.premonsoon"), size = .8) +
  theme_classic() + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  theme(legend.title = element_blank()) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "Black",
      size = 18
    ),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12)
  ) +
  ggtitle("Overall Avg GW Levels vs. Avg GW per Season (2001-2014)") +
  xlab("Year") + ylab("Average GW levels")


line_chart2 <- ggplot(data = df, aes(x = year, color = state))

w <-  plotly::ggplotly(
  line_chart2 + geom_line(aes(y = avg_state), size = .5) + geom_point(aes(y =
                                                                            avg_state), size = .5) +
    geom_errorbar(
      aes(ymin = avg_state - sd_sea_st_yr, ymax = avg_state + sd_sea_st_yr),
      width = .1
    ) +
    theme_minimal() + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
    theme(legend.title = element_blank()) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        color = "Black",
        size = 18
      ),
      axis.title.x = element_text(color = "black", size = 12),
      axis.title.y = element_text(color = "black", size = 12)
    ) +
    ggtitle("Avg GW per State + cross season variation(sd) (2001-2014)") +
    xlab("Year") + ylab("Average GW levels") + facet_grid(state ~ .)
)

d <- w + facet_grid(state ~ .)

scale_color_brewer(palette = "Paired") + theme_minimal()










geom_point(
  size = 0.6,
  alpha = 0.7,
  color = "DarkGrey",
  outlier.color = NA
)






Ind_map1 <- tm_shape(znl_stats) +
  tm_polygons(
    col = "X4" ,
    breaks = vector_stats ,
    palette = "cividis",
    n = 20,
    style = "cont",
    legend.show = F,
    lwd = 1.75,
    border.col = "black"
  ) +
  tm_layout(
    main.title = "Light Intensity in India: Averaged on the State's Level (2014)",
    main.title.fontface = "bold",
    legend.title.fontfamily = "serif",
    main.title.position = "center",
    scale = 1.2,
    frame = F
  ) +
  tm_add_legend(
    type = "fill",
    labels = c("Highest", "Lowest"),
    col = c("yellow", "darkblue")
  ) +
  tm_text(
    "NAME_1",
    size = .7,
    col = "black",
    fontfamily = "serif",
    remove.overlap = T
  ) +
  tm_credits(
    "Source: EOG Night Light Intensity - 2014",
    fontfamily = "serif",
    fontface = "italic",
    position = c("left", "bottom")
  )



tmap_save(
  Ind_map,
  filename = "India_light_intensity_eldesouky.jpeg",
  height = 8.5,
  width = 11,
  units = "in",
  dpi = 300
)
