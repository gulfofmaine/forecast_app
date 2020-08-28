

##### OISST gulf of maine extent

OISSTgrd <- raster::stack("/Users/mdzaugis/Box/RES Data/OISST/ThroughFeb2020.grd")

gom_extent <- raster::extent(c(-71.33, -66.75, 41.75, 45))

gom_oisst <-raster::extract(OISSTgrd, gom_extent, fun= mean, na.rm = TRUE)

dates <- seq.Date(as.Date("1981-09-01"), as.Date("2020-02-29"), "day")

gom_oisst_df <- data.frame(gom_oisst)

times <- data.frame("times" = seq(0,14069, by = 1))

gom_oisst_df  <- gom_oisst_df %>% cbind(times) %>% mutate(Date = as.Date(times, origin = "1981-09-01"))

gom_oisst_df <- gom_oisst_df %>% mutate(Date = as.Date(times, origin = "1981-09-01")) %>% dplyr::select(-times)

write_csv(gom_oisst_df, "Data/gom_oisst.csv")



cmip5mean <- raster::stack("/Users/mdzaugis/Box/RES Data/CMIP5_SST/RCP85_mu.grd")

gom_85mean <-raster::extract(cmip5mean, gom_extent, fun= mean, na.rm = TRUE)

Date <- seq.Date(as.Date("1982-01-01"), as.Date("2300-12-10"), "month")

gom_85mean_df <- data.frame("Mean" = gom_85mean)

gom_85mean_df  <- gom_85mean_df %>% cbind(Date) %>% filter(Date<"2100-01-01")



cmip55 <- raster::stack("/Users/mdzaugis/Box/RES Data/CMIP5_SST/RCP85_5th.grd")

gom_855 <-raster::extract(cmip55, gom_extent, fun= mean, na.rm = TRUE)

Date <- seq.Date(as.Date("1982-01-01"), as.Date("2300-12-10"), "month")

gom_855_df <- data.frame("low" = gom_855)

gom_855_df  <- gom_855_df %>% cbind(Date) %>% filter(Date<"2100-01-01")



cmip595 <- raster::stack("/Users/mdzaugis/Box/RES Data/CMIP5_SST/RCP85_95th.grd")

gom_8595 <-raster::extract(cmip595, gom_extent, fun= mean, na.rm = TRUE)

Date <- seq.Date(as.Date("1982-01-01"), as.Date("2300-12-10"), "month")

gom_8595_df <- data.frame("high" = gom_8595)

gom_8595_df  <- gom_8595_df %>% cbind(Date) %>% filter(Date<"2100-01-01")


cmipsst <- gom_85mean_df %>% left_join(gom_855_df, by = "Date") %>% left_join(., gom_8595_df, by = "Date")

write_csv(cmipsst, "Data/CMIPSSTProjections.csv")


############### SNE extent


OISSTgrd <- raster::stack("/Users/mdzaugis/Box/RES Data/OISST/ThroughFeb2020.grd")

sne_extent <- raster::extent(c(-74.5, -70.75, 39.75, 41.66))

sne_oisst <-raster::extract(OISSTgrd, sne_extent, fun= mean, na.rm = TRUE)

dates <- seq.Date(as.Date("1981-09-01"), as.Date("2020-02-29"), "day")

sne_oisst_df <- data.frame(sne_oisst)

times <- data.frame("times" = seq(0,14069, by = 1))

sne_oisst_df  <- sne_oisst_df %>% cbind(times) %>% mutate(Date = as.Date(times, origin = "1981-09-01"))

sne_oisst_df <- sne_oisst_df %>% mutate(Date = as.Date(times, origin = "1981-09-01")) %>% dplyr::select(-times)

write_csv(sne_oisst_df, "Data/sne_oisst.csv")



cmip5mean <- raster::stack("/Users/mdzaugis/Box/RES Data/CMIP5_SST/RCP85_mu.grd")

sne_85mean <-raster::extract(cmip5mean, sne_extent, fun= mean, na.rm = TRUE)

Date <- seq.Date(as.Date("1982-01-01"), as.Date("2300-12-10"), "month")

sne_85mean_df <- data.frame("Mean" = sne_85mean)

sne_85mean_df  <- sne_85mean_df %>% cbind(Date) %>% filter(Date<"2100-01-01")



cmip55 <- raster::stack("/Users/mdzaugis/Box/RES Data/CMIP5_SST/RCP85_5th.grd")

sne_855 <-raster::extract(cmip55, sne_extent, fun= mean, na.rm = TRUE)

Date <- seq.Date(as.Date("1982-01-01"), as.Date("2300-12-10"), "month")

sne_855_df <- data.frame("low" = sne_855)

sne_855_df  <- sne_855_df %>% cbind(Date) %>% filter(Date<"2100-01-01")



cmip595 <- raster::stack("/Users/mdzaugis/Box/RES Data/CMIP5_SST/RCP85_95th.grd")

sne_8595 <-raster::extract(cmip595, sne_extent, fun= mean, na.rm = TRUE)

Date <- seq.Date(as.Date("1982-01-01"), as.Date("2300-12-10"), "month")

sne_8595_df <- data.frame("high" = sne_8595)

sne_8595_df  <- sne_8595_df %>% cbind(Date) %>% filter(Date<"2100-01-01")


cmipsst_sne <- sne_85mean_df %>% left_join(sne_855_df, by = "Date") %>% left_join(., sne_8595_df, by = "Date")

write_csv(cmipsst_sne, "Data/CMIPSSTProjections_sne.csv")

