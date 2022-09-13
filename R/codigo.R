
# Librerías ---------------------------------------------------------------

library(dplyr)
library(DataExplorer)
library(readr)
library(stringr)
library(tidyr)
library(tidygeocoder)
library(osmdata)
library(mapview)
library(sf)


# Datos  ------------------------------------------------------------------


data <- read_delim("data/RM_SII_CBR.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Filtro comuna

est_central <- data %>% 
  dplyr::filter(COMUNA_SII == 14157)

# Nombre de columnas

colnames(est_central)

# Descripcion variables

config <- configure_report(add_plot_correlation = F, add_plot_prcomp = F)
DataExplorer::create_report(est_central, config = config )

# De lo que se observa en este reporte, es que las columnas como
# Ten_Metro, COLE_5MIN, COLE_10MIN, COLE_15MIN tienen celdas con 
# valores #N/A, por lo que se deben pasar a NA

# También se ve que las columnas Bodega, Estacionamiento y Cantidad_BOD
# tienen muchos datos NA, por lo que no es recomendable utilizarlas


# Todo lo #N/A se transforma a NA; y se ejecuta reporte

est_central[est_central == "#N/A"] <- NA

DataExplorer::create_report(est_central, config = config )

# De igual forma existen varias columnas con NA igual o mayor al 20%

# Cambio el nombre de la comuna

est_central$COMUNA <- "Estación Central"

# Limpio Direcciones, interesa separar departamentos

est_central <- est_central %>% 
  separate(DIRECCION, c("DIRECCION", "DPTO"), "DP ")

# Agrego comuna a Dirección

est_central$DIRECCION <- paste(est_central$DIRECCION,est_central$COMUNA, "Chile",  sep = ",") 


# Geocodificacion de direcciones, se demora aproximadamente 10 minutos

est_geo <- geo(est_central$DIRECCION, method="arcgis")

  # Visualización
est_geo %>% drop_na() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapview(legend = FALSE)
  # DataFrame a Simple Feature
est_geo <- est_geo %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# Se une info con informacion original

data_est <- cbind(est_central, est_geo) %>% 
  st_as_sf()


# Si se quiere analizar solo Deptos

est_dep <- data_est %>% 
  dplyr::filter(!is.na(DPTO))

# Se Exporta Resultado

sf::write_sf(est_geo, "est_central.shp")


