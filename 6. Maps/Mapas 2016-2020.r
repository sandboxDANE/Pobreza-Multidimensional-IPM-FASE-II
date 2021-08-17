dev.off()
cat("\014")
rm(list=ls(all=T))

options(scipen = 999)

library(dplyr)
library(ggplot2)
library(raster)
library(sf)
library(fasterize)
library(rgdal)
library(RColorBrewer)
library(cowplot)
library(viridis)

rasterOptions(tmptime = 2, 
              timer = TRUE,
              #maxmemory = 12e+9,
              chunksize = 6e+9,
              datatype = "INT4S")

setwd("C:/Users/ghama/Desktop/DANE/2021/7 - IPM + Imágenes/Mapas 2016-2020/")

#================================================#
#           0. Entrada de los datos          #####
#================================================#

#====================================#
# Predicciones IPM en todos los años #
#====================================#
## Entrada datos
info = read.table("0. Insumos/pred_completo_2016-2020.csv", sep = ",", header = T, colClasses = c("COD_DANE_datos" = "character"))
ori = read.table("0. Insumos/pred_completo_2016-2020.csv", sep = ",", header = T, colClasses = c("COD_DANE_datos" = "character"))
## Limpieza y organización
info = info %>%
        dplyr::mutate(COD_DANE_datos = ifelse(nchar(COD_DANE_datos) == 21, paste0("0",COD_DANE_datos), COD_DANE_datos) ) %>% 
        plyr::rename(c(AÃ.O = "AÑO", COD_DANE_datos = "COD_DANE", prediction_rf = "RF", prediction_gbtr = "GBTR")) %>% 
        dplyr::select(AÑO, COD_DANE, GBTR, RF) %>% 
        data.table::as.data.table() %>% 
        data.table::dcast(COD_DANE ~ AÑO, value.var = c("GBTR", "RF") ) %>% 
        as.data.frame()

## Cálculo porcentajes de variación
info = info %>% 
        dplyr::mutate(V.GBTR.2016.2017 = ifelse(is.na(GBTR_2016), NA, 100*(GBTR_2017 - GBTR_2016)/GBTR_2016),
                      V.GBTR.2017.2018 = ifelse(is.na(GBTR_2017), NA, 100*(GBTR_2018 - GBTR_2017)/GBTR_2017),
                      V.GBTR.2018.2019 = ifelse(is.na(GBTR_2018), NA, 100*(GBTR_2019 - GBTR_2018)/GBTR_2018),
                      V.GBTR.2019.2020 = ifelse(is.na(GBTR_2019), NA, 100*(GBTR_2020 - GBTR_2019)/GBTR_2019),
                      V.RF.2016.2017 = ifelse(is.na(RF_2016),0,100*(RF_2017 - RF_2016)/RF_2016),
                      V.RF.2017.2018 = ifelse(is.na(RF_2017),0,100*(RF_2018 - RF_2017)/RF_2017),
                      V.RF.2018.2019 = ifelse(is.na(RF_2018),0,100*(RF_2019 - RF_2018)/RF_2018),
                      V.RF.2019.2020 = ifelse(is.na(RF_2019),0,100*(RF_2020 - RF_2019)/RF_2019) )

## Categorizar los porcentajes de cambio
info$V.GBTR.2016.2017.C = cut(info$V.GBTR.2016.2017, breaks = c(ceiling(min(info$V.GBTR.2016.2017, na.rm = T)), -45, -30, -15, -5, 0, 5, 15, 30, 45, ceiling(max(info$V.GBTR.2016.2017, na.rm = T))),  dig.lab = 6  )
info$V.GBTR.2017.2018.C = cut(info$V.GBTR.2017.2018, breaks = c(ceiling(min(info$V.GBTR.2017.2018, na.rm = T)), -45, -30, -15, -5, 0, 5, 15, 30, 45, ceiling(max(info$V.GBTR.2017.2018, na.rm = T))),  dig.lab = 6  )
info$V.GBTR.2018.2019.C = cut(info$V.GBTR.2018.2019, breaks = c(ceiling(min(info$V.GBTR.2018.2019, na.rm = T)), -45, -30, -15, -5, 0, 5, 15, 30, 45, ceiling(max(info$V.GBTR.2018.2019, na.rm = T))),  dig.lab = 6  )
info$V.GBTR.2019.2020.C = cut(info$V.GBTR.2019.2020, breaks = c(ceiling(min(info$V.GBTR.2019.2020, na.rm = T)), -45, -30, -15, -5, 0, 5, 15, 30, 45, ceiling(max(info$V.GBTR.2019.2020, na.rm = T))),  dig.lab = 6  )
# 
# lev1 = levels(info$V.GBTR.2016.2017.C )
# lev2 = levels(info$V.GBTR.2017.2018.C )
# lev3 = levels(info$V.GBTR.2018.2019.C )
# lev4 = levels(info$V.GBTR.2019.2020.C )
# 
# info$V.GBTR.2016.2017.C = as.character(info$V.GBTR.2016.2017.C)
# info$V.GBTR.2017.2018.C = as.character(info$V.GBTR.2017.2018.C)
# info$V.GBTR.2018.2019.C = as.character(info$V.GBTR.2018.2019.C)
# info$V.GBTR.2019.2020.C = as.character(info$V.GBTR.2019.2020.C)
# 
# info$V.GBTR.2016.2017.C[is.na(info$V.GBTR.2016.2017.C)] = "Sin información"
# info$V.GBTR.2017.2018.C[is.na(info$V.GBTR.2017.2018.C)] = "Sin información"
# info$V.GBTR.2018.2019.C[is.na(info$V.GBTR.2018.2019.C)] = "Sin información"
# info$V.GBTR.2019.2020.C[is.na(info$V.GBTR.2019.2020.C)] = "Sin información"
# 
# info$V.GBTR.2016.2017.C = factor(info$V.GBTR.2016.2017.C, levels = c(lev1, "Sin información"))
# info$V.GBTR.2017.2018.C = factor(info$V.GBTR.2017.2018.C, levels = c(lev2, "Sin información"))
# info$V.GBTR.2018.2019.C = factor(info$V.GBTR.2018.2019.C, levels = c(lev3, "Sin información"))
# info$V.GBTR.2019.2020.C = factor(info$V.GBTR.2019.2020.C, levels = c(lev4, "Sin información"))

rm(lev1, lev2, lev3, lev4)
# 
# levels(info$V.GBTR.2016.2017.C)
# levels(info$V.GBTR.2017.2018.C)
# levels(info$V.GBTR.2018.2019.C)
# levels(info$V.GBTR.2019.2020.C)
#============#
#   Shapes   #
#============#
## Sebas
sp18.sebas = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Manzanas/2018 Seba/MGN_INTEGRADO_VARIABLES_DCD.shp")

## A nivel de manzana
sp20 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Manzanas/2020")
sp18 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Manzanas/2018")
sp17 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Manzanas/2017")
sp12 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Manzanas/2012")

## A nivel departamental
dd20 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Departamentos/2020")
dd18 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Departamentos/2018")
dd17 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Departamentos/2017")
dd12 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Departamentos/2012")

## A nivel de sección urbana
sc20 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Sección Rural/2020")
sc18 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Sección Rural/2018")
sc17 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Sección Rural/2017")
sc12 = sf::st_read("C:/Users/ghama/Desktop/DANE/2021/0 - Insumos Comunes/Shapes/Sección Rural/2012")

unique(nchar(sc20$SECR_CCNCT))
unique(nchar(sc18$SECR_CCNCT))
unique(nchar(sc17$SECR_CCNCT))
unique(nchar(sc12$SECR_CCNCT))
unique(nchar(sp18.sebas$CODIGO_DAN))


sc20$SECR_CCNCT = ifelse(nchar(sc20$SECR_CCNCT) == 11, paste0(sc20$SECR_CCNCT, "00000000000"), sc20$SECR_CCNCT)
sc18$SECR_CCNCT = ifelse(nchar(sc18$SECR_CCNCT) == 11, paste0(sc18$SECR_CCNCT, "00000000000"), sc18$SECR_CCNCT)
sc17$SECR_CCNCT = ifelse(nchar(sc17$SECR_CCNCT) == 11, paste0(sc17$SECR_CCNCT, "00000000000"), sc17$SECR_CCNCT)
sc12$SECR_CCNCT = ifelse(nchar(sc12$SECR_CCNCT) == 11, paste0(sc12$SECR_CCNCT, "00000000000"), sc12$SECR_CCNCT)

#===================================================================================#
#              1. Unión de la información para calcular mapas                  ######
#===================================================================================#
info$GBTR_2016 = info$GBTR_2016*100
info$GBTR_2017 = info$GBTR_2017*100
info$GBTR_2018 = info$GBTR_2018*100
info$GBTR_2019 = info$GBTR_2019*100
info$GBTR_2020 = info$GBTR_2020*100
info$RF_2016 = info$RF_2016*100
info$RF_2017 = info$RF_2017*100
info$RF_2018 = info$RF_2018*100
info$RF_2019 = info$RF_2019*100
info$RF_2020 = info$RF_2020*100

um20 = sp20 %>% merge(y = info, by.x = "COD_DANE", by.y = "COD_DANE", all.x = T, all.y = F)
um18 = sp18 %>% merge(y = info, by.x = "MANZ_CCNCT", by.y = "COD_DANE", all.x = T, all.y = F)
um17 = sp17 %>% merge(y = info, by.x = "MANZ_CCNCT", by.y = "COD_DANE", all.x = T, all.y = F)
um12 = sp12 %>% merge(y = info, by.x = "MANZ_CCNCT", by.y = "COD_DANE", all.x = T, all.y = F)

us20 = sc20 %>% merge(y = info, by.x = "SECR_CCNCT", by.y = "COD_DANE", all.x = T, all.y = F)
us18 = sc18 %>% merge(y = info, by.x = "SECR_CCNCT", by.y = "COD_DANE", all.x = T, all.y = F)
us17 = sc17 %>% merge(y = info, by.x = "SECR_CCNCT", by.y = "COD_DANE", all.x = T, all.y = F)
us12 = sc12 %>% merge(y = info, by.x = "SECR_CCNCT", by.y = "COD_DANE", all.x = T, all.y = F)
sp18.sebas = sp18.sebas %>% merge(y = info, by.x = "CODIGO_DAN", by.y = "COD_DANE", all.x = T, all.y = F)

um20 %>% nrow == sp20 %>% nrow
um18 %>% nrow == sp18 %>% nrow
um17 %>% nrow == sp17 %>% nrow
um12 %>% nrow == sp12 %>% nrow
us20 %>% nrow == sc20 %>% nrow
us18 %>% nrow == sc18 %>% nrow
us17 %>% nrow == sc17 %>% nrow
us12 %>% nrow == sc12 %>% nrow

us18 %>% head
saveRDS(info, "1. Salidas/datos_finales_mapeo_11-08-2021.rds")

# rm(sp20, sc20, sp18, sc18, sp17, sc17, sp12, sc12)
#=========================================================================#
#                       2. Mapas                                       ####
#=========================================================================#

.dir = "1. Salidas/Mapas/"

#=======================#
#  2.1. Continuas #######
#=======================#

#================#
# Marco 2016  ####
#================#
i = "RF_2018"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us12 %>% filter(substr(SECR_CCNCT,1,2) != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = um12 %>% filter(substr(MANZ_CCNCT,1,2) != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = dd12 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                labs(fill = "IPM\n") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1.5,"cm"),
                      legend.title = element_text(size = 14) )
                # 
                # theme( legend.key.height = unit(1.5,"cm"),
                #        legend.key.width =  unit(0.75,"cm"))
                # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us12 %>% filter(substr(SECR_CCNCT,1,2) == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um12 %>% filter(substr(MANZ_CCNCT,1,2) == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd12 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us12 %>% filter(substr(SECR_CCNCT,1,2) == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um12 %>% filter(substr(MANZ_CCNCT,1,2) == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd12 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_sp16_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}

#================#
# Marco 2017  ####
#================#
i = "GBTR_2018"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us17 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = um17 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = dd17 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                labs(fill = "IPM\n") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1.5,"cm"),
                      legend.title = element_text(size = 14) )
        # 
        # theme( legend.key.height = unit(1.5,"cm"),
        #        legend.key.width =  unit(0.75,"cm"))
        # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd17 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd17 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_sp17_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}

#================#
# Marco 2018  ####
#================#
i = "RF_2019"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us18 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = um18 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                labs(fill = "IPM\n") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1.5,"cm"),
                      legend.title = element_text(size = 14) )
        # 
        # theme( legend.key.height = unit(1.5,"cm"),
        #        legend.key.width =  unit(0.75,"cm"))
        # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_sp18_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}
#================#
# Marco 2020  ####
#================#
i = "RF_2020"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us20 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = um20 %>% filter(COD_DPTO != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = dd20 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                labs(fill = "IPM\n") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1.5,"cm"),
                      legend.title = element_text(size = 14) )
        # 
        # theme( legend.key.height = unit(1.5,"cm"),
        #        legend.key.width =  unit(0.75,"cm"))
        # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us20 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um20 %>% filter(COD_DPTO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd20 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us20 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = um20 %>% filter(COD_DPTO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd20 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                #scale_fill_gradient(low = color_low, high = color_high, na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_sp20_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}
#=======================#
#  2.2. Discretas #######
#=======================#

#================#
# Marco 2016  ####
#================#
i = "V.GBTR.2018.2019.C"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us12 %>% filter(substr(SECR_CCNCT,1,2) != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = um12 %>% filter(substr(MANZ_CCNCT,1,2) != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = dd12 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE, labels = c(us12 %>% as.data.frame() %>% dplyr::select(i) %>% pull() %>% levels(), "Sin información")) +
                labs(fill = "Porcentaje\nde Cambio") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1,"cm"),
                      legend.title = element_text(size = 14) )
        # 
        # theme( legend.key.height = unit(1.5,"cm"),
        #        legend.key.width =  unit(0.75,"cm"))
        # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us12 %>% filter(substr(SECR_CCNCT,1,2) == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um12 %>% filter(substr(MANZ_CCNCT,1,2) == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd12 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us12 %>% filter(substr(SECR_CCNCT,1,2) == "88"), aes(fill = "get(i)"), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um12 %>% filter(substr(MANZ_CCNCT,1,2) == "88"), aes(fill = " get(i)"), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd12 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.37, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.37, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_porcentaje_sp16_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}

#================#
# Marco 2017  ####
#================#
i = "V.GBTR.2017.2018.C"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us17 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = um17 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = dd17 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE, labels = c(us17 %>% as.data.frame() %>% dplyr::select(i) %>% pull() %>% levels(), "Sin información")) +
                labs(fill = "Porcentaje\nde Cambio") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1,"cm"),
                      legend.title = element_text(size = 14) )

        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd17 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um17 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd17 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                geom_sf(data = dd17 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_porcentaje_sp17_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}

#================#
# Marco 2018  ####
#================#
i = "V.GBTR.2019.2020.C"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us18 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = um18 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE, labels = c(us18 %>% as.data.frame() %>% dplyr::select(i) %>% pull() %>% levels(), "Sin información")) +
                labs(fill = "Porcentaje\nde Cambio") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1,"cm"),
                      legend.title = element_text(size = 14) )
        # 
        # theme( legend.key.height = unit(1.5,"cm"),
        #        legend.key.width =  unit(0.75,"cm"))
        # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um18 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_porcentaje_sp18_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}
#================#
# Marco 2020  ####
#================#
i = "V.GBTR.2019.2020.C"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = us20 %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = um20 %>% filter(COD_DPTO != "88"), aes(fill = get(i)), show.legend = T, lwd = 0, colour = "transparent") +
                geom_sf(data = dd20 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE, labels = c(us20 %>% as.data.frame() %>% dplyr::select(i) %>% pull() %>% levels(), "Sin información")) +
                labs(fill = "Porcentaje\nde Cambio") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1,"cm"),
                      legend.title = element_text(size = 14) )
        # 
        # theme( legend.key.height = unit(1.5,"cm"),
        #        legend.key.width =  unit(0.75,"cm"))
        # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = us20 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um20 %>% filter(COD_DPTO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd20 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = us20 %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = um20 %>% filter(COD_DPTO == "88"), aes(fill = get(i)), show.legend = F, lwd = 0, colour = "transparent") +
                geom_sf(data = dd20 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "C", na.value = "transparent", discrete = T, drop = FALSE) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_porcentaje_sp20_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}

#===========================================================
i = "GBTR_2019"

{
        #================#
        # Departamental  #
        #================#
        dpto = ggplot() +
                geom_sf(data = sp18.sebas %>% filter(DPTO_CCDGO != "88"), aes(fill = get(i)), show.legend = T, lwd = NA) +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO != "88"), colour = "gray30", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                labs(fill = "IPM\n") +
                theme_void(base_size = 10) +
                theme(legend.key.height = unit(1.5,"cm"),
                      legend.title = element_text(size = 14) )
        # 
        # theme( legend.key.height = unit(1.5,"cm"),
        #        legend.key.width =  unit(0.75,"cm"))
        # 
        #===============# 
        #  San Andrés 1 #
        #===============# 
        s1 = ggplot() +
                geom_sf(data = sp18.sebas %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.75, -81.68)) +
                ylim(c(12.45, 12.62)) +
                theme_void()
        
        #===============# 
        #  San Andrés 2 #
        #===============# 
        s2 = ggplot() +
                geom_sf(data = sp18.sebas %>% filter(DPTO_CCDGO == "88"), aes(fill = get(i)), show.legend = F, lwd = NA) +
                geom_sf(data = dd18 %>% filter(DPTO_CCDGO == "88"), colour = "gray20", fill = NA, lwd = 0.1) +
                scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
                xlim(c(-81.4, -81.34)) +
                ylim(c(13.3, 13.4)) +
                theme_void()
        
        gg_fin = ggdraw(dpto) + 
                draw_plot(s1, x = -0.34, y = 0.35, scale = 0.12) + 
                draw_plot(s2, x = -0.30, y = 0.35, scale = 0.09) 
        
        ## Export data
        ggsave(paste0(.dir, "/mapa_prueba_", i, ".png"), gg_fin, width = 20, height = 20, dpi = 72, units = "cm")
        
        rm(dpto, s1, s2, gg_fin)
        print(paste0("Realizado: ", i))
}

## Sacar frecuencias del marco y de info

info %>% 
        dplyr::filter(!is.na(GBTR_2018)) %>% 
        dplyr::mutate(DPTO = substr(COD_DANE,1,2)) %>% 
        dplyr::select(DPTO) %>% 
        pull() %>% 
        table()

sp18 %>% 
        as.data.frame() %>% 
        dplyr::select(DPTO_CCDGO) %>% 
        pull() %>% 
        table()

i = "GBTR_2018"

ggplot() +
        geom_sf(data = sp18.sebas %>% filter(DPTO_CCDGO == "99", !is.na(GBTR_2018)), aes(fill = get(i)), show.legend = T, lwd = NA) +
        geom_sf(data = dd18 %>% filter(DPTO_CCDGO == "99"), colour = "gray30", fill = NA, lwd = 0.1) +
        scale_fill_viridis(option = "B", na.value = "transparent", limit = c(0,100), breaks = seq(0,100,10)) +
        labs(fill = "IPM\n") +
        theme_void(base_size = 10) +
        theme(legend.key.height = unit(1.5,"cm"),
              legend.title = element_text(size = 14) )


sp18.sebas %>% 
        filter(DPTO_CCDGO == "99", is.na(GBTR_2018)) %>% nrow


sum(is.na(info$GBTR_2018))

ori = ori %>% plyr::rename(c(AÃ.O = "AÑO"))

ori$AÑO %>% table
