#Instalamos librerias
install.packages("ggplot2")
install.packages("terra")
install.packages("sf")
install.packages('ggspatial')

# Cargamos librerias
library(ggplot2)
library(terra)
library(sf)
library(ggspatial)

# Especificamos la ruta al archivo de la imagen multiespectral
ruta_imagen <- "D:/Ecublens_20160803_transparent_mosaic_group1.tif"

# Abrir la imagen multiespectral utilizando la función rast()
imagen_multiespectral <- rast(ruta_imagen)

# Visualizamos la imagen multiespectral
plot(imagen_multiespectral)

# Primero dividimos la pantalla en 2 filas y 2 columnas
par(mfrow = c(2, 2))

# Visualizamos las cuatro bandas de la imagen multiespectral

# Visualización de la banda 1
plot(imagen_multiespectral[[1]], main = "Banda 1 - Rojo")

# Visualización de la banda 2
plot(imagen_multiespectral[[2]], main = "Banda 2 - Verde")

# Visualización de la banda 3
plot(imagen_multiespectral[[3]], main = "Banda 3 - Azul")

# Visualización de la banda 4
plot(imagen_multiespectral[[4]], main = "Banda 4 - Infrarrojo")

# Visualización en color real
plotRGB(imagen_multiespectral, r = 1, g = 2, b = 3)

# Visualización en falso color
plotRGB(imagen_multiespectral, r = 4, g = 1, b = 2)

# Establecemos la ruta del polígono
ruta_poligono <-"D:/Zona_estudio.shp"

# Cargamos el polígono con la función read_sf()
poligono <- read_sf(ruta_poligono)

# Creamos una función para recortar la imagen multiespectral
recortar_imagen <- function(raster){
   
  # Comprobación del Sistema de Coordenadas 
  if (st_crs(poligono) != crs(raster)) {
    # Transformación del sistema de coordenadas
    poligono  <- st_transform(poligono , crs(raster))
  }
  
  # Recorte del Raster
  raster_recortado <- crop(raster, poligono)
  
  # Aplicación de la Máscara del Polígono
  raster_recortado <- mask(raster_recortado, poligono)
  
  # Devolución del Raster recortado y enmascarado
  return(raster_recortado)
}

#Aplicar función de recorte a la imagen
imagen_recortada <- recortar_imagen(imagen_multiespectral)

# Comprobamos de que se recorta correctamente
plotRGB(imagen_recortada, r = 1, g = 2, b = 3)

# Guardamos Imagen en formato .tiff
writeRaster(imagen_recortada, "D:/Imagen_recortada.tiff", overwrite=TRUE)

# Creamos la calculadora del NDVI
calculadora_NDVI <- function(raster){
  # Extracción de la banda roja
  banda_red <- raster[[1]] 
  
  # Extracción de la banda infraroja
  banda_nir <- raster[[4]] 
  
  # Calculo del NDVI
  ndvi <- (banda_nir - banda_red) / (banda_nir + banda_red)
  
  # Retorno de resultado
  return(ndvi)
}


# Creamos la calculadora de SAVI
# Se puede modificar el valor de L
calculadora_SAVI <- function(raster, L = 0.5) {
  # Extracción de la banda roja
  banda_red <- raster[[1]] 
  
  # Extracción de la banda infraroja
  banda_nir <- raster[[4]] 
  
  # Calculo del SAVI
  savi <- (banda_nir - banda_red) * (1 + L) / (banda_nir + banda_red + L)
  
  # Retorno de resultado
  return(savi)
}

# Calculamos el NDVI de la imagen recortada
ndvi <- calculadora_NDVI(imagen_recortada)

# Calculamos el SAVI de la imagen recortada
savi <- calculadora_SAVI(imagen_recortada)

# Visualización del NDVI
plot(ndvi, main = "NDVI (Índice de Vegetación de Diferencia Normalizada)")

# Visualización del SAVI
plot(savi, main = "SAVI (Índice de Vegetación Ajustado por el Suelo)")

# Convertimos a data frame
ndvi_df <- as.data.frame(ndvi, xy = TRUE)

# Modificamos los nombres de las columnas
colnames(ndvi_df) <- c("x", "y", "ndvi")

# Convertimos a data frame
savi_df <- as.data.frame(savi, xy = TRUE)

# Modificamos los nombres de las columnas
colnames(savi_df) <- c("x", "y", "savi")

# Creamos el gráfico base
ggplot() +
  # Se añaden los datos
  geom_raster(data = ndvi_df, aes(x = x, y = y, fill = get("ndvi"))) +
  
  # Configuración de la escala de colores
  scale_fill_gradientn(colors = c("brown", "yellow", "green"), 
                       na.value = "transparent",
                       name = "NDVI") +
  # Ajuste de coordenadas
  coord_fixed() +
  # Tema minimalista
  theme_minimal() +
  # Añadimos títulos y etiquetas
  labs(title = "Mapa de NDVI", x = "Longitud", y = "Latitud")+
  # Añadimos ecala
  annotation_scale() +
  # Añadimos flecha del norte
  annotation_north_arrow(location='tr')+
  # Personalización del tema
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dashed"),
    panel.grid.minor = element_blank())

ggplot() +
  geom_raster(data = savi_df, aes(x = x, y = y, fill = get("savi"))) +
  scale_fill_gradientn(colors = c("brown", "yellow", "green"), 
                       na.value = "transparent",
                       name = "SAVI") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Mapa de SAVI", x = "Longitud", y = "Latitud")+
  annotation_scale() +
  annotation_north_arrow(location='tr')+
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dashed"),
    panel.grid.minor = element_blank())

# Mapa NDVI

mapa_ndvi <- ggplot() +
  geom_raster(data = ndvi_df, aes(x = x, y = y, fill = get("ndvi"))) +
  scale_fill_gradientn(colors = c("brown", "yellow", "green"), 
                       na.value = "transparent",
                       name = "NDVI") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Mapa de NDVI", x = "Longitud", y = "Latitud")+
  annotation_scale() +
  annotation_north_arrow(location='tr')+
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dashed"),
    panel.grid.minor = element_blank())

# Mapa SAVI

mapa_savi <- ggplot() +
  geom_raster(data = savi_df, aes(x = x, y = y, fill = get("savi"))) +
  scale_fill_gradientn(colors = c("brown", "yellow", "green"), 
                       na.value = "transparent",
                       name = "SAVI") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Mapa de SAVI", x = "Longitud", y = "Latitud")+
  annotation_scale() +
  annotation_north_arrow(location='tr')+
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dashed"),
    panel.grid.minor = element_blank())

# Guardamos el Mapa del Índice NDVI
ggsave("D:/Mapa_NDVI.pdf", plot = mapa_ndvi, width = 10, height = 8)

# Guardamos el Mapa del Índice SAVI
ggsave("D:/Mapa_SAVI.pdf", plot = mapa_savi, width = 10, height = 8)

# Creamos una función para recortar la imagen multiespectral
recortar_imagen <- function(raster){
   
  # Comprobación del Sistema de Coordenadas 
  if (st_crs(poligono) != crs(raster)) {
    # Transformación del sistema de coordenadas
    poligono  <- st_transform(poligono , crs(raster))
  }
  
  # Recorte del Raster
  raster_recortado <- crop(raster, poligono)
  
  # Aplicación de la Máscara del Polígono
  raster_recortado <- mask(raster_recortado, poligono)
  
  # Devolución del Raster recortado y enmascarado
  return(raster_recortado)
}

# Creamos la calculadora del NDVI
calculadora_NDVI <- function(raster){
  # Extracción de la banda roja
  banda_red <- raster[[1]] 
  
  # Extracción de la banda infrarroja
  banda_nir <- raster[[4]] 
  
  # Calculo del NDVI
  ndvi <- (banda_nir - banda_red) / (banda_nir + banda_red)
  
  # Retorno de resultado
  return(ndvi)
}


# Creamos la calculadora de SAVI
# Se puede modificar el valor de L
calculadora_SAVI <- function(raster, L = 0.5) {
  # Extracción de la banda roja
  banda_red <- raster[[1]] 
  
  # Extracción de la banda infraroja
  banda_nir <- raster[[4]] 
  
  # Calculo del SAVI
  savi <- (banda_nir - banda_red) * (1 + L) / (banda_nir + banda_red + L)
  
  # Retorno de resultado
  return(savi)
}

# Creamos la función
crear_directorio_si_no_existe <- function(directorio) {
  
  # Si la ruta del directorio no existe
  if (!dir.exists(directorio)) {
    # Creame la carpeta
    dir.create(directorio)
  
  # En el caso que no se cumpla la condición de que no existe, es decir existe
  } else {
    
    # Imprime ya existe
    print("Ya existe")
  }
}

# Establecemos la ruta
directorio_proyecto <- "D:/Proyecto_multiespectral"

# Creamos la carpeta si no existe
crear_directorio_si_no_existe(directorio_proyecto)

# Creamos un conjunto con los nombres de las carpetas
carpetas <- c("Imagenes_multiespectrales","Imagenes_multiespectrales_recortadas"
              , "Poligonos", "Mapas_indices", "Indices")

# Por cada Carpeta en Carpetas
for (carpeta in carpetas) {
  
  # Construye la dirección
  direccion <- paste0(directorio_proyecto,"/",carpeta)
  
  # Créame la dirección si no existe
  crear_directorio_si_no_existe(direccion)
}

# Creamos un directorio de la carpeta de las imágenes multiespectrales
directorio_imagenes <- paste0(directorio_proyecto, "/", carpetas[1])


# Listar todos los archivos .tif o .tiff en el directorio de las imágenes
imagenes_multiespectrales <- list.files(directorio_imagenes, 
                                        #Aqui es donde se debería de modificar para otros formatos
                                        pattern = "\\.tif$|\\.tiff", 
                                        full.names = TRUE)

# Imprimimos la lista para comprobar
print(imagenes_multiespectrales)

extraer_nombre <- function(archivo) {
  
  # Extraemos el nombre del archivo con su extensión
  name <- basename(archivo)
 
  # Quitamos la extensión del nombre del archivo
  name <- tools::file_path_sans_ext(name)

  # Devolvemos el nombre del archivo sin la extensión. 
  return(name)
}

# Creamos una lista de las carpetas, pero sin la carpeta de las Imagenes_multiespectrales
carpetas_sin_imagenes <- carpetas[carpetas!= "Imagenes_multiespectrales"]

# Para cada imagen en la lista de imagenes multiespectrales
for (imagen in imagenes_multiespectrales) {
  
  # Sacamos el nombre de la imagen
  nombre <- extraer_nombre(imagen)
  
  # Cara cada carpeta en la lista de las carpetas en el conjunto de carpetas sin Imagenes_multiespectrales
  for (carpeta in carpetas_sin_imagenes) {
    
    # Creamos la ruta de cada subcarpeta
    ruta_subcarpeta <- paste0(directorio_proyecto,"/",carpeta,"/", nombre)
    
    # Creamos la subcarpeta si no existe
    crear_directorio_si_no_existe(ruta_subcarpeta)
  }
}

# Por cada imagen en imágenes
for (imagen_multiespectral in imagenes_multiespectrales) {
    
  # Extraemos el nombre de la imagen multiespectral
  nombre <- extraer_nombre(imagen_multiespectral)
  
  # Cargamos la imagen
  imagen <- rast(imagen_multiespectral)
  
  # Establecemos el directorio de la carpeta Poligonos/Imagen
  directorio_poligonos <- paste0(directorio_proyecto,"/Poligonos/", nombre)
  
  # Conseguimos la ruta del polígono
  ruta_poligono <- list.files(directorio_poligonos, pattern = "\\.shp",
                         full.names = TRUE)
  
  # Cargamos el polígono en el entorno, importante que sea únicamiente un
  # archivo .shp, si no dara Error
  poligonos <- read_sf(ruta_poligono)
  
  # Extraemos de la tabla de atributos la columna de id
  id_poligonos <- poligonos$id
  
  # Por cada id en id _poligono
  for (id in id_poligonos) {
    
    # Nos quedamos con el poligono "id" de poligonos
    poligono <- poligonos[id, ]
    
    # Recortamos la imagen multiespectral
    imagen_recortada <- recortar_imagen(imagen)
    
    # Creamos la ruta donde la guardaremos
    ruta_guardado <-paste0(directorio_proyecto,
                       "/Imagenes_multiespectrales_recortadas/", 
                       nombre, "/", nombre,"_poligono_",id,".tiff")
    
    # Guardamos La imagen
    writeRaster(imagen_recortada, 
                ruta_guardado, 
                overwrite=TRUE)
  }
}

# Establecemos el direcorio de las imagenes recortadas
directorio_recortadas <- paste0(directorio_proyecto,
                                "/Imagenes_multiespectrales_recortadas")

# Creamos un conjunto con los nombres de los índices
indices <- c("NDVI", "SAVI")

# Por cada imagen multiespectral en imagenes_multiespectrales
for (imagen_multiespectral in imagenes_multiespectrales) {
  
  # Extraemos el nombre
  nombre <- extraer_nombre(imagen_multiespectral)
  
  # Establecemos el directorio de las imagenes recortadas
  directorio_imagenes_recortadas <- paste0(directorio_recortadas, "/", nombre)
  
  # Sacamos una lista de las imágenes recortadas
  imagenes_multiespectrales_recortadas <- list.files(directorio_imagenes_recortadas, 
                                        pattern = "\\.tif$|\\.tiff", 
                                        full.names = TRUE)
  

  #########################Creación de subcarpetas##############################
  
  # Por cada índice en índices
  for (indice in indices) {
    
    # Creamos un directorio para las carpetas
    directorio_indice <- paste0(directorio_proyecto,"/Indices/", nombre, "/",indice)
    
    # Creamos las carpetas si no existen
    crear_directorio_si_no_existe(directorio_indice)
  
  }
  #########################Aplicación de índices################################
  
  # Por cada imagen recortada en imagenes_multiespectrales_recortadas
  for (imagen_recortada in imagenes_multiespectrales_recortadas) {
    
    # Cargamos la imagen
    imagen <- rast(imagen_recortada)
    
    # Extraemos el nombre de la imagen recortada, este incluye informacion del polígono
    nombre_imagen <- extraer_nombre(imagen_recortada)
    
    for (indice in indices) {
      
      # Creamos el directorio con el índice correspondiente
      directorio_indice <- paste0(directorio_proyecto,"/Indices/", nombre, "/",indice)
      
      # Construir la llamada a la función como una cadena
      expr <- paste0("calculadora_", indice, "(imagen)")
      
      # Evaluar la expresión y almacenar el resultado
      resultado <- eval(parse(text = expr))
      
      # Guardamos el archivo raster
      writeRaster(resultado, 
                  paste0(directorio_indice,"/",nombre_imagen,"_",indice,".tiff"), 
                  overwrite=TRUE)
    }
  }
}

# Establecemos el directorio de las imagenes recortadas
directorio_recortadas <- paste0(directorio_proyecto,
                                "/Imagenes_multiespectrales_recortadas")

# Creamos un conjunto con los nombres de los índices
indices <- c("NDVI", "SAVI")

# Por cada imagen multiespectral en imagenes_multiespectrales
for (imagen_multiespectral in imagenes_multiespectrales) {
  
  # Extraemos el nommbre
  nombre <- extraer_nombre(imagen_multiespectral)
  
  # Establecemos el directorio de las imagenes recortadas
  directorio_imagenes_recortadas <- paste0(directorio_recortadas, "/", nombre)
  
  # Sacamos una lista de las imagenes recortadas
  imagenes_multiespectrales_recortadas <- list.files(directorio_imagenes_recortadas, 
                                        pattern = "\\.tif$|\\.tiff", 
                                        full.names = TRUE)
  
  # Por cada índice en índices
  for (indice in indices) {
    
    # Creamos un di5rectorio para cada indice dentro de la carpeta de mapas 
    directorio_indice <- paste0(directorio_proyecto,"/Mapas_indices/", nombre, "/",indice)
    
    # Creamos el directorio si no existe
    crear_directorio_si_no_existe(directorio_indice)
  }
  
  # Por cada imagen recortada en imagenes recortadas
  for (imagen_recortada in imagenes_multiespectrales_recortadas) {
    
    # Extraemos el nombre de la imagen
    nombre_imagen <- extraer_nombre(imagen_recortada)
    
    # Por cada índice en índices
    for (indice in indices) {
      
      # Creamos directorio indice 
      directorio_indice <- paste0(directorio_proyecto,"/Indices/", nombre, "/",indice)
      
      # Creamos directorio mapas índice
      directorio_mapas_indice <- paste0(directorio_proyecto,"/Mapas_indices/", nombre, "/",indice)
      
      # Abrimos el índice en formato raster
      raster <- rast(paste0(directorio_indice,"/",nombre_imagen,"_",indice,".tiff"))
      
      # Convertimos a data frame
      raster_df <- as.data.frame(raster, xy = TRUE)
      
      # Establecemos nombres de columnas
      colnames(raster_df) <- c("x", "y", paste0(indice))
      
      # Creamos el mapa, igual que en los apartados anteriores pero automatizado
      # Automatización con paste0(indice)
      mapa <- ggplot() +
        geom_raster(data = raster_df, aes(x = x, y = y, fill = get(paste0(indice)))) +
        scale_fill_gradientn(colors = c("brown", "yellow", "green"), 
                             na.value = "transparent",
                             name = paste0(indice)) +
        coord_fixed() +
        theme_minimal() +
        labs(title = paste0("Mapa de ",indice), x = "Longitud", y = "Latitud")+
        annotation_scale() +
        annotation_north_arrow(location='tr')+
        theme(
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dashed"),
          panel.grid.minor = element_blank())
      
      # Ruta guardado en formato .pdf
      ruta_guardado <- paste0(directorio_mapas_indice,"/",
                              nombre_imagen,"_",indice,".pdf")
      
      # Guardamos mapa en pdf
      ggsave(ruta_guardado, plot = mapa, width = 10, height = 8)
    
    }
  }
}
