#' script to extract data of interest from main dataset
#' sorry it's not really commented, just change the folders and follow along
#' to extract the data of interest

wd <- "C:\\Users\\Nazareno Campioni\\Desktop\\BS_MAXENT"
setwd(wd)

library(readxl)
library(ggplot2)
library(terra)

med <- "data/shapefiles/Mediterranean_Sea\\Mediterranean_Sea.shp"
med_poly <- vect(med)
med_poly$NAME <- "MEDITERRANEAN"

ices <- "data/shapefiles/ICES_ecoregion\\ICES_ecoregions_20171207_erase_ESRI.shp"
ices_poly <- vect(ices)
ices_poly <- crop(ices_poly, med_poly)

# define subregions
west <- ices_poly[ices_poly$OBJECTID == 2 | ices_poly$OBJECTID == 4]
west$NAME <- "WEST"

central_east <- ices_poly[ices_poly$OBJECTID == 5 | ices_poly$OBJECTID == 7 |
                            ices_poly$OBJECTID == 8]
central_east$NAME <- "CENTRAL_EAST"

year_max <- 2025
year_min <- 2015

output.name <- "occurrences_2025.csv"

# clean messy years
data <- read_excel("data/BS_Dataset_Prometheus.xlsx")

for (i in seq_len(nrow(data))) {
  if (!is.na(data[i,]$SEASON) & data[i,]$SEASON == 'WIn') {
    data[i,]$SEASON <- 'Win'
  }
}


data$YEAR <- as.numeric(data$YEAR)
data <- data[!is.na(data$YEAR), ]

# select 
data <- data[data$YEAR >= year_min, ]
data <- data[data$YEAR <= year_max, ]

# plot yearly obs count
ggplot(data, aes(YEAR)) +
  geom_histogram(binwidth = 1, fill = "grey70", color = "black") +
  # geom_vline(xintercept = c(2015), linetype = "dashed") +
  # geom_vline(xintercept = 2005, linetype = "dashed") +
  labs(x = "Year", y = "Number of records") +
  theme_classic()



check_groups <- function(data, group_var) {
  
  counts <- table(data[[group_var]])
  print(counts)
  
  p <- ggplot(data, aes(x = .data[[group_var]])) +
    geom_bar(fill = "grey70", colour = "black") +
    labs(x = group_var, y = "Number of records") +
    theme_classic()
  
  print(p)
  
  return(counts)
}

check_groups(data, "SEX")
check_groups(data, "LIFE STAGE")
check_groups(data, "SEASON")

## SEASON
season_df <- data[!is.na(data$SEASON), ]
check_groups(season_df, "SEASON")
season_df <- season_df[, c(11,10,9,5)]
names(season_df) <- c('species', 'dd long', 'dd lat', 'season')

write.csv(season_df, "season_df.csv", row.names = FALSE)

sea_df_spr <- season_df[season_df$season == 'Spr', ]
sea_df_sum <- season_df[season_df$season == 'Sum', ]
sea_df_aut <- season_df[season_df$season == 'Aut', ]
sea_df_win <- season_df[season_df$season == 'Win', ]

seasons <- list(spr = sea_df_spr, sum = sea_df_sum,
                aut = sea_df_aut, win = sea_df_win)

for (se in names(seasons)) {
  df <- seasons[[se]]
  
  outfile <- file.path("data/season_data",
                       paste0("occ_", se, ".jpg"))
  
  jpeg(outfile, width = 1600, height = 1600, res = 300)
  
  plot(med_poly, main = paste("Occurrences -", se))
  
  points(df$`dd long`,
         df$`dd lat`,
         col = "red",
         pch = 4,      # cross symbol
         cex = 1.2,
         lwd = 1.5)
  
  dev.off()
}

write.csv(sea_df_spr, "data/season_data/sea_df_spr.csv", row.names = FALSE)
write.csv(sea_df_sum, "data/season_data/sea_df_sum.csv", row.names = FALSE)
write.csv(sea_df_aut, "data/season_data/sea_df_aut.csv", row.names = FALSE)
write.csv(sea_df_win, "data/season_data/sea_df_win.csv", row.names = FALSE)

# bind spring - winter and summer - autumn
sea_df_winspr <- rbind(sea_df_win, sea_df_spr)
sea_df_sumaut <- rbind(sea_df_sum, sea_df_aut)

check_groups(sea_df_winspr, "season")
check_groups(sea_df_sumaut, "season")

seasons_ <- list(winspr = sea_df_winspr, sumaut = sea_df_sumaut)

for (se in names(seasons_)) {
  df <- seasons_[[se]]
  
  outfile <- file.path("data/season_data",
                       paste0("occ_", se, ".jpg"))
  
  jpeg(outfile, width = 1600, height = 1600, res = 300)
  
  plot(med_poly, main = paste("Occurrences -", se))
  
  points(df$`dd long`,
         df$`dd lat`,
         col = "red",
         pch = 4,      # cross symbol
         cex = 1.2,
         lwd = 1.5)
  
  dev.off()
}

write.csv(sea_df_winspr, "data/season_data/sea_df_winspr.csv", row.names = FALSE)
write.csv(sea_df_sumaut, "data/season_data/sea_df_sumaut.csv", row.names = FALSE)



## MATURITY
mat_df <- data[!is.na(data$`LIFE STAGE`), ]
mat_df <- mat_df[, c(11,10,9,17)]
names(mat_df) <- c('species', 'dd long', 'dd lat', 'maturity')
check_groups(mat_df, "maturity")

write.csv(mat_df, "mat_df.csv", row.names = FALSE)

mat_df_y <- mat_df[mat_df$maturity == 'Juvenile',]
mat_df_a <- mat_df[mat_df$maturity == 'Adult',]

maturities <- list(adult = mat_df_a, juvenile = mat_df_y)

for (se in names(maturities)) {
  df <- maturities[[se]]
  
  outfile <- file.path("data/maturity_data",
                       paste0("occ_", se, ".jpg"))
  
  jpeg(outfile, width = 1600, height = 1600, res = 300)
  
  plot(med_poly, main = paste("Occurrences -", se))
  
  points(df$`dd long`,
         df$`dd lat`,
         col = "red",
         pch = 4,      # cross symbol
         cex = 1.2,
         lwd = 1.5)
  
  dev.off()
}

write.csv(mat_df_y, "data/maturity_data/mat_y.csv", row.names = FALSE)
write.csv(mat_df_a, "data/maturity_data/mat_a.csv", row.names = FALSE)

## SEX
sex_df <- data[!is.na(data$SEX), ]
sex_df <- sex_df[sex_df$SEX != 'N.A.', ]
sex_df <- sex_df[, c(11,10,9,16)]
names(sex_df) <- c('species', 'dd long', 'dd lat', 'sex')
check_groups(sex_df, "sex")

write.csv(sex_df, "sex_df.csv", row.names = FALSE)

sex_df_m <- sex_df[sex_df$sex == 'M',]
sex_df_f <- sex_df[sex_df$sex == 'F',]

sexes <- list(female = sex_df_f, male = sex_df_m)

for (se in names(sexes)) {
  df <- sexes[[se]]
  
  outfile <- file.path("data/sex_data",
                       paste0("occ_", se, ".jpg"))
  
  jpeg(outfile, width = 1600, height = 1600, res = 300)
  
  plot(med_poly, main = paste("Occurrences -", se))
  
  points(df$`dd long`,
         df$`dd lat`,
         col = "red",
         pch = 4,      # cross symbol
         cex = 1.2,
         lwd = 1.5)
  
  dev.off()
}

write.csv(sex_df_m, "data/sex_data/sex_m.csv", row.names = FALSE)
write.csv(sex_df_f, "data/sex_data/sex_f.csv", row.names = FALSE)

# create occurrences
df <- data[, c(11,10,9)]
names(df) <- c('species', 'dd long', 'dd lat')

write.csv(df, output.name, row.names = FALSE)


# SEASON AND MATURITY
seasmat_df <- data[((!is.na(data$`LIFE STAGE`)) & (!is.na(data$SEASON))), ]
seasmat_df <- seasmat_df[, c(11,10,9,5,17)]
names(seasmat_df) <- c('species', 'dd long', 'dd lat', 'season', 'maturity')

seasmat_df$season <- ifelse(
  seasmat_df$season %in% c("Win", "Spr"),
  "WinSpr",
  "SumAut"
)

seasmat_df$group <- paste(seasmat_df$maturity, seasmat_df$season, sep = "_")

counts <- table(seasmat_df$group)

barplot(counts,
        col = "grey70",
        border = "black",
        ylab = "Number of records")

aw <- seasmat_df[seasmat_df$season == 'WinSpr'& seasmat_df$maturity == 'Adult',]
outfile <- file.path("data/ontogenetic_data",
                     paste0("occ_mature_winspr", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - mature, winspr")

points(aw$`dd long`,
       aw$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()

as <- seasmat_df[seasmat_df$season == 'SumAut'& seasmat_df$maturity == 'Adult',]
outfile <- file.path("data/ontogenetic_data",
                     paste0("occ_mature_sumaut", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - mature, sumaut")

points(as$`dd long`,
       as$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()


jw <- seasmat_df[seasmat_df$season == 'WinSpr'&
                   seasmat_df$maturity == 'Juvenile',]
outfile <- file.path("data/ontogenetic_data",
                     paste0("occ_young_winspr", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - juvenile, winspr")

points(jw$`dd long`,
       jw$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()


js <- seasmat_df[seasmat_df$season == 'SumAut'&
                   seasmat_df$maturity == 'Juvenile',]
outfile <- file.path("data/ontogenetic_data",
                     paste0("occ_young_sumaut", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - juvenile, sumaut")

points(js$`dd long`,
       js$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()



write.csv(aw, "data/ontogenetic_data/aw.csv", row.names = FALSE)
write.csv(as, "data/ontogenetic_data/as.csv", row.names = FALSE)
write.csv(jw, "data/ontogenetic_data/jw.csv", row.names = FALSE)
write.csv(js, "data/ontogenetic_data/js.csv", row.names = FALSE)


# SEASON AND SEX
seasex_df <- data[((!is.na(data$`SEX`)) & (!is.na(data$SEASON))), ]
seasex_df <- seasex_df[seasex_df$SEX != 'N.A.', ]
seasex_df <- seasex_df[, c(11,10,9,5,16)]
names(seasex_df) <- c('species', 'dd long', 'dd lat', 'season', 'sex')

seasex_df$season <- ifelse(
  seasex_df$season %in% c("Win", "Spr"),
  "WinSpr",
  "SumAut"
)

seasex_df$group <- paste(seasex_df$sex, seasex_df$season, sep = "_")

counts <- table(seasex_df$group)

barplot(counts,
        col = "grey70",
        border = "black",
        ylab = "Number of records")

mw <- seasex_df[seasex_df$season == 'WinSpr'& seasex_df$sex == 'M',]
outfile <- file.path("data/sexseason_data",
                     paste0("occ_male_winspr", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - male, winspr")

points(mw$`dd long`,
       mw$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()

ms <- seasex_df[seasex_df$season == 'SumAut'& seasex_df$sex == 'M',]
outfile <- file.path("data/sexseason_data",
                     paste0("occ_male_sumaut", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - male, sumaut")

points(ms$`dd long`,
       ms$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()


fw <- seasex_df[seasex_df$season == 'WinSpr'&
                  seasex_df$sex == 'F',]
outfile <- file.path("data/sexseason_data",
                     paste0("occ_female_winspr", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - female, winspr")

points(fw$`dd long`,
       fw$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()


fs <- seasex_df[seasex_df$season == 'SumAut'&
                  seasex_df$sex == 'F',]
outfile <- file.path("data/sexseason_data",
                     paste0("occ_female_sumaut", ".jpg"))

jpeg(outfile, width = 1600, height = 1600, res = 300)

plot(med_poly, main = "Occurrences - female, sumaut")

points(fs$`dd long`,
       fs$`dd lat`,
       col = "red",
       pch = 4,      # cross symbol
       cex = 1.2,
       lwd = 1.5)

dev.off()



write.csv(mw, "data/sexseason_data/mw.csv", row.names = FALSE)
write.csv(ms, "data/sexseason_data/ms.csv", row.names = FALSE)
write.csv(fw, "data/sexseason_data/fw.csv", row.names = FALSE)
write.csv(fs, "data/sexseason_data/fs.csv", row.names = FALSE)

