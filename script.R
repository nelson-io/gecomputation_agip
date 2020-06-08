library(rio)
library(tidyverse)
library(janitor)
library(sf)

data <- import("Recorte base 5 de mayo 2020.xlsx") %>% 
  clean_names()

# let's look activities
actividades <-  data$actividad_comercial %>% table() %>% data.frame() %>% arrange(desc(Freq))
data[which(data$actividad_comercial == "Depósitos" & str_detect(data$observacion_local, "internet|web|Internet|INTERNET|Web|WEB|venta|Venta|VENTA|online|Online|ONLINE|ecommerce|ECOMMERCE|mail|email|telefonía|comercio")),"actividad_comercial"] <- "Depósito con ventas web" 

excluded <- c("Industrias","Depósitos","DESOCUPADO",
              "Peluquerías, manicuras,  pedicuras, centros de belleza, spas y  depilación",
              "Hoteles, hoteles familiares, pensiones y geriátricos",
              "Gimnasios, canchas de alquiler y yoga",
              "Locales para esparcimiento (peloteros, teatros, cines, locales bailables)",
              "Locutorios y servicios de Internet","Tatuajes y Piercings",
              "Paseos de compras (puestos con estructura de metal)")

data_comercios <- data %>% filter(!actividad_comercial %in% c("Industrias","Depósitos","DESOCUPADO")) %>% 
  group_by(`Actividad Comercial` = actividad_comercial) %>% 
  summarise(`Total de comercios` = n()) %>% 
  arrange(desc(`Total de comercios`)) %>% 
  adorn_totals()
export(data_comercios, "data_comercios.xlsx")

data_f <- data %>% 
  filter(!actividad_comercial %in% excluded) %>% 
  group_by(actividad_comercial) %>% 
  summarise(total = n()) %>% 
  arrange(total) %>% 
  mutate(actividad_comercial = case_when(actividad_comercial == "Materiales para la casa y construcción (corralones, plomería, gas, aberturas, vidrierias, marmolerías,madereras, carpinterías, herrerías, pisos, alfombras, sanitarios,etc.)" ~ "Materiales para la casa y construcción",
                                         actividad_comercial =="Clínicas privadas, centros de diagnóstico por imágenes, emergencias, medicinas prepagas, laboratorios, médicos, dentistas, kinesiología (SALUD PRIVADA)" ~ "Salud privada",
                                         T~ actividad_comercial),
         actividad_comercial = fct_inorder(actividad_comercial))

<<<<<<< HEAD


=======
>>>>>>> ebdab23e7f1da58f5e0f1a25d7eb31ebc32092ae
ggplot(data_f)+
  geom_col(aes(x = actividad_comercial, y = total), fill = 'steelblue')+
  coord_flip()+
  theme_bw()+
  ggtitle("Cantidad de usos por actividad comercial")+
  xlab("Total de usos")+
  ylab("Actividad comercial")


ggsave("usos_actividades.png",dpi = 'retina', width = 10, height = 9)



data_o <- data %>% 
  filter(actividad_comercial %in% excluded) %>% 
  group_by(actividad_comercial) %>% 
  summarise(total = n()) %>% 
  arrange(total) %>% 
  mutate(actividad_comercial = fct_inorder(actividad_comercial))

<<<<<<< HEAD
data_o_2 <- data %>% 
  filter(actividad_comercial %in% excluded)

=======
>>>>>>> ebdab23e7f1da58f5e0f1a25d7eb31ebc32092ae
ggplot(data_o)+
  geom_col(aes(x = actividad_comercial, y = total), fill = 'steelblue')+
  geom_text(aes(x = actividad_comercial, y = total, label = total), stat = 'sum',hjust = -.1)+
  coord_flip()+
  theme_bw()+
  ggtitle("Cantidad de usos por actividad comercial omitida")+
  xlab("Total de usos")+
  ylab("Actividades comerciales omitidas")+
  theme(legend.position = "none")

ggsave("usos_actividades_omitidas.png",dpi = 'retina', width = 18, height = 9)
# identificamos depósitos de venta online

# depositos <- data %>% 
#   filter(actividad_comercial == "Depósitos",
#          !is.na(observacion_local)) %>% pull(observacion_local) %>% as.character()
# 
# depositos_web <- str_detect(depositos, "internet|web|Internet|INTERNET|Web|WEB|venta|Venta|VENTA|online|Online|ONLINE|ecommerce|ECOMMERCE|mail|email|telefonía|comercio")
# 
# data %>% 
#   filter(actividad_comercial == "Depósitos",
#          !is.na(observacion_local)) %>% 
#   filter(depositos_web) %>% View()



my_list <- list()
my_list$todos <- data %>% 
  filter(!actividad_comercial %in% c("Industrias","Depósitos","DESOCUPADO")) %>% 
  group_by(actividad_comercial) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  mutate(actividad_comercial = case_when(actividad_comercial == "Materiales para la casa y construcción (corralones, plomería, gas, aberturas, vidrierias, marmolerías,madereras, carpinterías, herrerías, pisos, alfombras, sanitarios,etc.)" ~ "Materiales para la casa y construcción",
                                         actividad_comercial =="Clínicas privadas, centros de diagnóstico por imágenes, emergencias, medicinas prepagas, laboratorios, médicos, dentistas, kinesiología (SALUD PRIVADA)" ~ "Salud privada",
                                         T~ actividad_comercial),
         actividad_comercial = fct_inorder(actividad_comercial)) %>% 
  adorn_totals()

my_list$habilitados <- data_f %>% arrange(desc(total)) %>% 
  adorn_totals()

export(my_list,"tablas_agip_0520.xlsx")

# geo_data <- st_read("comercios_clasificados_v2.shp")
# 
# 
# 
# data_enhanced <- left_join(data, geo_data, by = c("id_local" = "id")) %>% 
#   filter(!actividad_comercial %in% c("DESOCUPADO"))
# 
# data_enhanced %>% View() 
# 
# omiited_from_data <- data %>% filter(!(id_local %in% geo_data$id))%>% 
#   filter(!actividad_comercial %in% c("DESOCUPADO"))
# 
# omittd_from_geo <- geo_data %>% filter(!id %in% data$id_local, activo == "OCUPADO")

# leemos data geo
library(ggthemes)
 
corredores <- st_read("corredores_lineales_modif.shp")
zonas_comerciales <- st_read("zonas_comerciales.geojson")
barrios <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

ggplot()+
  geom_sf(data = barrios)+
  geom_sf(data = zonas_comerciales, fill = 'red')+
  geom_sf(data = corredores, col = 'red', lwd = 1)+
  theme_bw()+
  ggtitle("Zonas exceptuadas CABA")

ggsave("zonas_exceptuadas.png", dpi = 'retina')

# Vamos a geocodificar
# direcciones <- str_c("http://servicios.usig.buenosaires.gob.ar/normalizar/?direccion=",
#                      data$nombre_calle, " ", data$altura_local, ", caba&geocodificar=TRUE")
# 
# library(jsonlite)
# 
# test <- fromJSON("http://servicios.usig.buenosaires.gob.ar/normalizar/?direccion=AV%20ANTARTIDA%20ARGENTINA%201160,%20caba&geocodificar=TRUE")
# 
# extractor <- function(x){
#   fromJSON(x)$direccionesNormalizadas$coordenadas
# }


data_inf <- data %>% 
  filter(!actividad_comercial %in% c("Industrias","Depósitos","DESOCUPADO")) %>% 
  mutate(comuna = as.numeric(comuna))


glimpse(data_inf)

geo_data <- st_read("comercios_clasificados_v2.shp")
glimpse(geo_data)

joined <- left_join(data_inf, geo_data %>% select(calle,altura,nombre,observacio,comuna, y, x),
                    by = c("nombre_calle" = "calle", "altura_local" = "altura", "nombre_comercial" = "nombre", 'comuna')) %>% 
  group_by(id_local) %>%
  slice(1) %>% 
  ungroup()


joined <- joined %>% st_as_sf()


comercios_zonas_comerciales <- data.frame(st_intersects(joined,zonas_comerciales, sparse = F)) %>% 
  transmute(intersects = ifelse(X1 == T| X2 == T, T,F)) %>% pull() %>% which()


ggplot(corredores)+
  geom_sf()

buffers <- st_buffer(corredores %>% st_transform(crs = 5347),dist = 13)

comercios_corredores <-  data.frame(st_intersects(joined %>% st_transform(crs = 5347), buffers, sparse = F)) %>% 
  rowSums()


comercios_corredores <- which(comercios_corredores > 0)

ggplot(buffers)+
  geom_sf()






# definimos vector de actividades esenciales

esenciales <- c("Restaurantes, bares, cafeterías, pizzerías, fast food",
                "Fruterías y verdulerías",
                "Carnicerías, fiambrerías, granjas, pescaderías",
                "Kioscos y golosinerías",
                "Ferreterías, cerrajerías, tornillos y bulonerías, alarmas, materiales eléctricos, iluminación",
                "Artículos limpieza, artículos de embalaje y pañaleras",
                "Supermercados de grandes cadenas e hipermercados (incluye locales Express)",
                "Bancos, financieras, tarjetas de crédito y casas de cambios",
                "Alimentos y bebidas en comercios especializados (dietéticas, herboristería, vinerías, etc.)",
                "Heladerías (elaboración y venta)",
                "Almacenes y supermercados chicos / Locales Express de cadenas",
                "Clínicas privadas, centros de diagnóstico por imágenes, emergencias, medicinas prepagas, laboratorios, médicos, dentistas, kinesiología (SALUD PRIVADA)",
                "Casas de comidas, casa de pastas y deliverys",
                "Veterinarias y alimentos para mascotas",
                "Combustibles, estaciones de servicio",
                "Panaderías, confiterías, bombonerías, chocolaterías y sandwicherías",
                "Farmacias, perfumerías, cosméticas y artículos de tocador",
                "Rapipago, Pago Fácil, Bapro Pago y lugares de pago de servicios diversos excepto Bancos",
                "Fotografía y ópticas"
                )


excluded_df <- joined %>% 
<<<<<<< HEAD
  slice(unique(c(comercios_zonas_comerciales,comercios_corredores))) %>% 
=======
  slice(c(comercios_zonas_comerciales,comercios_corredores)) %>% 
>>>>>>> ebdab23e7f1da58f5e0f1a25d7eb31ebc32092ae
  filter(!actividad_comercial %in% esenciales,
         !actividad_comercial %in% excluded)


ggplot(excluded_df)+
  geom_sf(data = barrios)+
  geom_sf(alpha = .03, col = "red")+
  theme_bw()  

joined_filtered_df <- joined %>% 
  filter(!actividad_comercial %in% excluded) %>% 
  anti_join(excluded_df %>% st_drop_geometry(), by = "id_local")


colors <- c("Habilitados" = "steelblue", "Excluidos" = "red")

ggplot(excluded_df)+
  geom_sf(data = barrios)+
  geom_sf(data = joined_filtered_df, alpha = .01)+
  geom_sf(alpha = .03)+
  theme_bw()+
  ggtitle("Comercios habilitados y comercios excluídos en zonas comerciales y corredores")+
  labs(color = "Legend")+
  scale_color_manual(values = c("steelblue, 'red"))

ggsave("mapa_zonas_hab.jpg",dpi = "retina", width = 10, height = 8)

excluded_df %>% 
  st_drop_geometry() %>% 
  group_by(`Actividad comercial` =  actividad_comercial) %>% 
  summarise(`Total de comercios` = n()) %>% 
  arrange(desc(`Total de comercios`)) %>% 
  adorn_totals() %>% 
  export("excluded_df.xlsx")
<<<<<<< HEAD


data_inf %>% 
  filter(!id_local %in% c(excluded_df$id_local, data_o_2$id_local)) %>% 
  nrow()


joined_filtered_df %>% 
  st_drop_geometry() %>% 
  group_by(`Actividad comercial` =  actividad_comercial) %>% 
  summarise(`Total de comercios` = n()) %>% 
  arrange(desc(`Total de comercios`)) %>% 
  adorn_totals() %>% 
  export("joined_filtered.xlsx")

=======
>>>>>>> ebdab23e7f1da58f5e0f1a25d7eb31ebc32092ae
