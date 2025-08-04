### HEADER ---------------------------------------------------------------------
#
#
# Date: 2023-06-21
# Author: Rayane Pacheco
# Email: rayane.costa@inteligenciaterritorial.org
#
# Working directory
subfolder_names <- c("input", "output", "model", "report")
for(name in subfolder_names){if (!file.exists(file.path("R", name))) {dir.create(paste0("R/", name))}}
#
# Configurations
options(stringsAsFactors = FALSE)
options(scipen = 999)
#
# Loading packages
library(tidyverse)
library(readxl)
library(plyr)
library(sf)
library(geobr)
library(extrafont)
library(gridExtra)
library(scales)
library(paletteer)
library(mapview)
library(terra)
library(spdep)
library(MASS)
library(INLA)
library(patchwork)
#
# Extract code
source("R/model/layouts.R")
# load("R/model/workspace_climate_reports.RData")
#
#
# BOUNDARY ----------------------------------------------------------------


south_america <-
  sf::read_sf("R/input/_geo/south_america/South_America.shp") %>%
  sf::st_transform(4674) %>%
  dplyr::mutate(
    iso_code = case_when(
      COUNTRY == "Brazil" ~ "BRA"
      , COUNTRY == "Colombia" ~ "COL"
      , COUNTRY == "Ecuador" ~ "ECU"
      , COUNTRY == "Guyana" ~ "GUY"
      , COUNTRY == "Suriname" ~ "SUR"
      , COUNTRY == "Peru" ~ "PER"
      , COUNTRY == "Venezuela" ~ "VEN"
      , COUNTRY == "Bolivia" ~ "BOL"
      , COUNTRY == "French Guiana (France)" ~ "GUF"
    ))

amazonia <-
  sf::read_sf("R/input/_geo/amazonia/amazonia_boundary.shp") %>%
  sf::st_transform(4674) %>%
  dplyr::select(-year)


# _ municipalities --------------------------------------------------------

# Brazil
municipios_br <-
  geobr::read_municipality(year = 2022, simplified = FALSE)

# Colombia
municipios_co <-
  sf::read_sf("R/input/colombia/mpio/mpio.shp") %>%
  sf::st_transform(4674)

# Bolivia
municipios_bo <-
  sf::read_sf("R/input/bolivia/RECURRENCIA 2002-2022/Shapefile/RECURRENCIA EPSG 4326.shp", options = "ENCODING=LATIN1") %>%
  sf::st_transform(4674)

# Ecuador
municipios_ec <-
  sf::read_sf("R/input/ecuador/Division Politico Administrativa CONALI Entregada 2017/ORGANIZACION TERRITORIAL DEL ESTADO CANTONAL.shp") %>%
  sf::st_transform(4674)

# Peru
municipios_pe <-
  sf::read_sf("R/input/peru/distrital/Distrital INEI 2023 geogpsperu SuyoPomalia.shp") %>%
  sf::st_transform(4674) %>%
  sf::st_make_valid()

# Suriname
municipios_su <-
  sf::read_sf("R/input/_geo/country_adminstrative_level/gadm41_SUR.gpkg", layer = "ADM_ADM_2") %>%
  sf::st_transform(4674) %>%
  sf::st_make_valid()

# Guyana
municipios_gu <-
  sf::read_sf("R/input/_geo/country_adminstrative_level/gadm41_GUY.gpkg", layer = "ADM_ADM_2") %>%
  sf::st_transform(4674) %>%
  sf::st_make_valid()

# French Guiana
municipios_fg <-
  sf::read_sf("R/input/_geo/country_adminstrative_level/gadm41_GUF.gpkg", layer = "ADM_ADM_2") %>%
  sf::st_transform(4674) %>%
  sf::st_make_valid()

# Venezuela
municipios_ve <-
  sf::read_sf("R/input/_geo/country_adminstrative_level/gadm41_VEN.gpkg", layer = "ADM_ADM_2") %>%
  sf::st_transform(4674) %>%
  sf::st_make_valid()


# __ all ------------------------------------------------------------------
glimpse(municipios_br)
glimpse(municipios_co)
glimpse(municipios_bo)
glimpse(municipios_ec)
glimpse(municipios_pe)
glimpse(municipios_fg)
glimpse(municipios_gu)
glimpse(municipios_su)
glimpse(municipios_ve)


municipios_all <-
  rbind(
    municipios_br %>%
      dplyr::mutate(country = "Brazil", geo_id = as.character(code_muni)) %>%
      dplyr::select(country, geo_id, name_muni, geometry = geom)
    , municipios_co %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "Colombia", geo_id = mpios) %>%
      dplyr::select(country, geo_id, name_muni = nombre_mpi, geometry)
    , municipios_bo %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "Bolivia", geo_id = c_ut) %>%
      dplyr::select(country, geo_id, name_muni = municipio, geometry)
    , municipios_ec %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "Ecuador", geo_id = gsub("(?<![0-9])0+", "", dpa_canton, perl = TRUE)) %>%
      dplyr::select(country, geo_id, name_muni = dpa_descan, geometry)
    , municipios_pe %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "Peru", geo_id = ubigeo) %>%
      dplyr::select(country, geo_id, name_muni = distrito, geometry)
    , municipios_fg %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "French Guiana", geo_id = tolower(paste0(name_1, "_", name_2))) %>%
      dplyr::select(country, geo_id,  name_muni = name_2, geometry = geom)
    , municipios_gu %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "Guyana", geo_id = tolower(paste0(name_1, "_", name_2))) %>%
      dplyr::select(country, geo_id,  name_muni = name_2, geometry = geom)
    , municipios_su %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "Suriname", geo_id = tolower(paste0(name_1, "_", name_2))) %>%
      dplyr::select(country, geo_id,  name_muni = name_2, geometry = geom)
    , municipios_ve %>%
      janitor::clean_names() %>%
      dplyr::mutate(country = "Venezuela", geo_id = tolower(paste0(name_1, "_", name_2))) %>%
      dplyr::select(country, geo_id,  name_muni = name_2, geometry = geom)
  ) %>%
  dplyr::mutate(
    total_area = sf::st_area(.)
    , geo_id = iconv(geo_id, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  )

municipios_amazonia_bioma <-
  municipios_all %>%
  sf::st_intersection(amazonia %>% dplyr::select(geometry)) %>%
  dplyr::mutate(
    amazon_area = sf::st_area(.)
    , perc_amazon_area = amazon_area/total_area
    , perc_amazon_area = units::drop_units(perc_amazon_area)
  ) %>%
  dplyr::filter(perc_amazon_area >= 0.5)

municipios_all_area <-
  municipios_all %>%
  dplyr::inner_join(
    municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::select(country, geo_id)
    , by = c('country', 'geo_id')
    , keep = FALSE
  )

sf::write_sf(municipios_amazonia_bioma, "R/input/_geo/amazonia_municipalities_50p.gpkg")
sf::write_sf(municipios_all_area, "R/input/_geo/amazonia_municipalities_50p_all_encoding.shp")

windows()
plot(sf::st_geometry(municipios_amazonia_bioma), border = "gray")
plot(sf::st_geometry(amazonia), add = TRUE, border = "green")
plot(sf::st_geometry(municipios_all_area), border = "gray")
plot(sf::st_geometry(amazonia), add = TRUE, border = "green")

others <-
  rbind(municipios_fg, municipios_su, municipios_gu, municipios_ve) %>%
  sf::st_intersection(amazonia)


# rm(municipios_br, municipios_co, municipios_bo, municipios_ec
#    , municipios_pe, municipios_fg, municipios_gu, municipios_su
#    , municipios_ve)


# + EVENT ----------------------------------------------------------------


# _ brazil ---------------------------------------------------------------

# listing files with disasters reports
list_files <- list.files("R/input/brazil", "*.xls")

# creating object to store all disasters registries
br_report <- NULL
for (i in seq_along(list_files)) {

  print(paste0("Brazil ", sub(".xlsx", "", i)))

  # reading files
  table_i <-
    readxl::read_xls(
      paste0("R/input/brazil/", list_files[i])
      , skip = 4
      , sheet = 1
      , col_names = TRUE
      , guess_max = 20000
      )

  # storing files
  br_report <- rbind(br_report, table_i)

}

# removing the temp object used in the loop (for)
rm(table_i)
glimpse(br_report)

## Data wrangling: tidying and transform
geo_id_br <- municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::filter(country == "Brazil") %>% dplyr::pull(geo_id)
disaster_amazonia_bioma_br <-
  br_report %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    geo_id = substr(protocolo, 6, 12)
    , year = if_else(
      substr(protocolo, 20, 23) == "1508"
      , substr(registro, 7, 10)
      , substr(protocolo, 20, 23)
    )
    , disaster = case_when(
      cobrade %in% c(
        "11313 - Quedas, Tombamentos e rolamentos - Matacões"
        , "11314 - Quedas, Tombamentos e rolamentos - Lajes"
        , "11321 - Deslizamentos"
        , "11331 - Corridas de Massa - Solo/Lama"
        , "11332 - Corridas de Massa - Rocha/detrito"
        , "11340 - Subsidências e colapsos"
      ) ~ "Landslides"
      , cobrade %in% c(
        "12100 - Inundações"
        , "12200 - Enxurradas"
        , "12300 - Alagamentos"
      )  ~ "Floods"
      , cobrade %in% c(
        "13212 - Tempestade Local/Convectiva - Tempestade de Raios"
        , "13213 - Tempestade Local/Convectiva - Granizo"
        , "13214 - Tempestade Local/Convectiva - Chuvas Intensas"
      ) ~ "Storms"
      , cobrade %in% c(
        "14110 - Estiagem"
        , "14120 - Seca"
      ) ~ "Droughts"
      , cobrade %in% c(
        "14131 - Incêndio Florestal - Incêndios em Parques, Áreas de Proteção Ambiental e Áreas de Preservação Permanente Nacionais, Estaduais ou Municipais"
        , "14132 - Incêndio Florestal - Incêndios em áreas não protegidas, com reflexos na qualidade do ar"
      ) ~ "Wildfires"
      , TRUE ~ "NA"
    )
  ) %>%
  dplyr::filter(
    year %in% as.character(2013:2023)
    & disaster != "NA"
    & geo_id %in% geo_id_br
    )


# verifying
glimpse(disaster_amazonia_bioma_br)
table(disaster_amazonia_bioma_br$disaster)
table(disaster_amazonia_bioma_br$year)

# grouping events per municipalities
disaster_amz_freq_mun_br <-
  disaster_amazonia_bioma_br %>%
  dplyr::group_by(disaster, year, uf, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = uf)


# _ colombia -------------------------------------------------------------

## these files needed to be crated by hand on account of pwd protection
list_files <- list.files("R/input/colombia", "*.xlsx$")
readxl::excel_sheets(paste0("R/input/colombia/", list_files[11]))

co_report <- NULL
for (i in seq_along(list_files)) {

  print(paste0("Colombia ", sub(".xlsx", "", i)))
  header_start_row <- ifelse(list_files[i] == "2013.xlsx", 1, 3)

  table_i <-
    openxlsx::read.xlsx(
      paste0("R/input/colombia/", list_files[i])
      , sheet = "REPORTE DE EMERGENCIAS"
      , startRow = header_start_row
      , colNames = FALSE
      , fillMergedCells = TRUE
    )

  if (str_detect(list_files[i], "2023") == TRUE) {

    header <-
      table_i %>%
      dplyr::slice(1:3) %>%
      dplyr::mutate(dplyr::across(everything(), ~replace(., . == " A F E C T A C I Ó N", "AFECTACIÓN")))

    n <- length(header) - length(table_i)
    if (n > 0) {
      for (j in 1:n) {
        table_i <- table_i %>% dplyr::mutate(!!paste0("N_", j) := NA)
      }
    }

    table_i <-
      table_i %>%
      setNames(ifelse(header[1,] != header[2,], paste(header[1,], header[2,], header[3,], sep = "_")
                      , paste(header[1,], header[3,], sep = "_"))) %>%  # Merge the top rows to create a header
      janitor::clean_names() %>% # Resulting names are unique and consist only of the _ character, numbers, and letter
      dplyr::mutate(dplyr::across(c(na_5, na_6, na_7), as.character))

  } else {
    header <-
      table_i %>%
      dplyr::slice(1:3) %>%
      dplyr::mutate(dplyr::across(everything(), ~replace(., . == " A F E C T A C I Ó N", "AFECTACIÓN")))

    n <- length(header) - length(table_i)
    if (n > 0) {
      for (j in 1:n) {
        table_i <- table_i %>% dplyr::mutate(!!paste0("N_", j) := NA)
      }
    }

    table_i <-
      table_i %>%
      setNames(paste(header[1,], header[2,], sep = "_")) %>%  # Merge the top two rows to create a header
      janitor::clean_names() # Resulting names are unique and consist only of the _ character, numbers, and letter
  }

  co_report <- dplyr::bind_rows(co_report, table_i)

}

# removing the temp object used in the loop (for)
rm(list = c("table_i", "header_start_row", "header"))
glimpse(co_report)

geo_id_co <- municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::filter(country == "Colombia") %>% dplyr::pull(geo_id)
disaster_amazonia_bioma_co <-
  co_report %>%
  dplyr::select(-starts_with("na")) %>%
  dplyr::filter(
    nchar(descripcion_y_ubicacion_fecha) == 5
    & descripcion_y_ubicacion_fecha != "FECHA"
  ) %>%
  dplyr::mutate(
    date = as.Date(as.numeric(descripcion_y_ubicacion_fecha), origin = "1899-12-30")
    , geo_id = descripcion_y_ubicacion_codificacion_segun_divipola
    , year = as.character(lubridate::year(date))
    , disaster = case_when(
      descripcion_y_ubicacion_evento %in% c(
        "AVALANCHA"
        , "AVALANCHA "
        , "DELIZAMIENTO"
        , "DESLIZAMIENTO"
        , "DESLIZAMIENTO "
        , "REMOCION EN MASA"
        , "REMOCI√ìN EN MASA"
        , "MOVIMIENTO EN MASA"
        , "REPRESAMIENTO"
      ) ~ "Landslides"
      , descripcion_y_ubicacion_evento %in% c(
        "INUNDACIÓN"
        , "INUNDACI√ìN"
        , "INUNDACIoN"
        , "INUNDACION "
        , "INUNDACION"
        , "INMERSIoN"
        , "INMERSION"
        , "IMERSION"
        , "CRECIENTE SUBITA"
        , "CRECIENTE SUBITA "
        , "CRECIENTE S√öBITA"
        , "AVENIDA TORRENCIAL"
      )  ~ "Floods"
      , descripcion_y_ubicacion_evento %in% c(
        "VENDAVAL"
        , "VENDAVAL "
        , "TEMPORAL"
        , "TORMENTA ELECTRICA"
        , "LLUVIAS"
      ) ~ "Storms"
      , descripcion_y_ubicacion_evento %in% c(
        "SEQUIA"
        , "SEQUIA "
      ) ~ "Droughts"
      , descripcion_y_ubicacion_evento %in% c(
        "INCENDIO FORESTAL"
        , "INCENDIO FORESTAL "
        , "INCENDIO fORESTAL"
        , "INCENDIO DE COBERTURA VEGETAL"
        , "INCENDIO DE RESIDUO VEGETAL"
      ) ~ "Wildfires"
      , TRUE ~ "NA"
    )
  ) %>%
  dplyr::filter(
    year %in% as.character(2013:2023)
    & disaster != "NA"
    & geo_id %in% geo_id_co
  )

# verifying
glimpse(disaster_amazonia_bioma_co)
table(disaster_amazonia_bioma_co$disaster)
table(disaster_amazonia_bioma_co$year)

# grouping events per municipalities
disaster_amz_freq_mun_co <-
  disaster_amazonia_bioma_co %>%
  dplyr::group_by(disaster, year, descripcion_y_ubicacion_departamento, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = descripcion_y_ubicacion_departamento)


# _ bolivia --------------------------------------------------------------

readxl::excel_sheets("R/input/bolivia/BD_2002_2022_BD_2023.xlsx")

bo_report <-
  readxl::read_xlsx(
    "R/input/bolivia/BD_2002_2022_BD_2023.xlsx"
    , sheet = "BD 2002 2022"
    , col_names = TRUE
    , guess_max = 15000
  ) %>%
  janitor::clean_names() %>%
  dplyr::bind_rows(
    readxl::read_xlsx(
      "R/input/bolivia/BD_2002_2022_BD_2023.xlsx"
      , sheet = "BD 2023"
      , skip = 25
      , col_names = TRUE
      , guess_max = 15000
    ) %>%
      janitor::clean_names() %>%
      dplyr::rename(
        "fecha_evento" = "fecha_del_evento"
        , "cod_ine_municipal" = "cod_ine"
        , "departamento" = "dpto"
        , "provincia" = "provincia"
        , "municipio" = "municipio"
        , "no_comunidades" = "n_com"
        , "tipo_de_evento" = "evento"
        , "fam_afec" = "fam_afec"
        , "fam_damn" = "fam_dam"
        , "viv_afect_no" = "viv_afec"
        , "viv_dest_no" = "viv_destr"
        , "has_afec" = "has_afec"
        , "has_perd" = "has_perd"
        , "gan_afec" = "gan_afec"
        , "gan_muer" = "gan_perd"
        , "obs" = "obs"
        , "fallecidos" = "fallec"
        , "desaparecidos" = "desap"
        , "evacuados" = "evac"
        , "declaratorias_de_desastre" = "decl_desastre"
        , "decl_emergencia" = "decl_emergencia"
        , "tipo_d_documento" = "tipo_de_documento"
      ) %>%
      dplyr::filter(!str_detect(departamento, "SUB TOTAL|TOTAL GENERAL|	Fuente:")) %>%
      dplyr::mutate(
        ano = 2023
        , no_comunidades = as.numeric(no_comunidades)
        , across(c(viv_afect_no, viv_dest_no, has_afec, has_perd, gan_afec, gan_muer), as.character)
      )
  )

geo_id_bo <- municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::filter(country == "Bolivia") %>% dplyr::pull(geo_id)
disaster_amazonia_bioma_bo <-
  bo_report %>%
  tidyr::separate_rows(tipo_de_evento, sep = " Y ") %>%
  tidyr::separate_rows(tipo_de_evento, sep = " E ") %>%
  tidyr::separate_rows(tipo_de_evento, sep = "-") %>%
  dplyr::mutate(
    geo_id = if_else(nchar(cod_ine_municipal) == 5, paste0("0", cod_ine_municipal), cod_ine_municipal)
    , year = as.character(ano)
    , disaster = case_when(
      tipo_de_evento %in% c(
        "Deslizamiento"
        , "Derrumbe"
        , "DESLIZAMIENTO"
        , "MAZAMORRA"
        , "Mazamorra"
      ) ~ "Landslides"
      , tipo_de_evento %in% c(
        "Riada"
        , "Inundación"
        , "Desborde"
        , "inundación"
        , "DESBORDE"
        , "DESBORDES"
        , "INUNDACIÓN"
        , " INUNDACIÓN"
        , "inundación"
        , "RIADA"
        , "INUNDACIÓN, RIADA"
      ) ~ "Floods"
      , tipo_de_evento %in% c(
        "Lluvias"
        , "Tormenta Eléctrica"
        , "Tormenta eléctrica"
        , "TORMENTAS ELÉCTRICAS"
        , "TORMENTA"
        , "INTENSAS LLUVIAS"
      ) ~ "Storms"
      , tipo_de_evento %in% c(
        "Sequia Hidrológica"
        , "Sequía"
        , "SEQUIA"
        , "SEQUÍA"
        , "SEQUIA "
        , "SEQUIA-HELADA"
        , "SEQUI"
      ) ~ "Droughts"
      , tipo_de_evento %in% c(
        "Incendio forestal"
        , "Incendio"
        , "INCENDIO"
        , "INCENDIOS"
      ) ~ "Wildfires"
      , TRUE ~ "NA"
    )) %>%
  dplyr::filter(
    year %in% as.character(2013:2023)
    & disaster != "NA"
    & geo_id %in% geo_id_bo
  )


table(disaster_amazonia_bioma_bo$disaster)
table(disaster_amazonia_bioma_bo$year)

# grouping events per municipalities
disaster_amz_freq_mun_bo <-
  disaster_amazonia_bioma_bo %>%
  dplyr::group_by(disaster, year, departamento, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = departamento)


# _ ecuador ---------------------------------------------------------------

readxl::excel_sheets("R/input/ecuador/Base_Eventos_2010_2024_eventos_catálogo.xlsx")

ec_report <-
  readxl::read_xlsx(
    "R/input/ecuador/Base_Eventos_2010_2024_eventos_catálogo.xlsx"
    , sheet = "Base20102022"
    , guess_max = 65000
  )

geo_id_ec <- municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::filter(country == "Ecuador") %>% dplyr::pull(geo_id)
disaster_amazonia_bioma_ec <-
  ec_report %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    geo_id = gsub("\\.0", "", codificacion_cantonal)
    , year = substr(fecha_del_evento, 1, 4)
    , disaster = case_when(
      evento %in% c(
        "AVALANCHA"
        , "Avalancha"
        , "Aluvión"
        , "ALUVIÓN"
        , "Deslizamiento"
        , "DESLIZAMIENTO"
        , "Hundimiento"
        , "HUNDIMIENTO"
        , "Socavamiento"
        , "SOCAVAMIENTO"
      ) ~ "Landslides"
      , evento %in% c(
        "Inundación"
        , "INUNDACIÓN"
      )  ~ "Floods"
      , evento %in% c(
        "VENDAVAL"
        , "Vendaval"
        , "TORMENTA ELÉCTRICA"
        , "Tormenta Eléctrica"
      ) ~ "Storms"
      , evento %in% c(
        "Déficit Hídrico"
        , "DÉFICIT HÍDRICO"
        , "Sequía hidrológica"
      ) ~ "Droughts"
      , evento %in% c(
        "INCENDIO FORESTAL"
        , "Incendio Forestal"
      ) ~ "Wildfires"
      , TRUE ~ "NA"
    )) %>%
  dplyr::filter(
    year %in% as.character(2013:2023)
    & disaster != "NA"
    & geo_id %in% geo_id_ec
  )

table(disaster_amazonia_bioma_ec$disaster)
table(disaster_amazonia_bioma_ec$year)

# grouping events per municipalities
disaster_amz_freq_mun_ec <-
  disaster_amazonia_bioma_ec %>%
  dplyr::group_by(disaster, year, provincia, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = provincia)


# _ peru ------------------------------------------------------------------

geo_id_pe <- municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::filter(country == "Peru") %>% dplyr::pull(geo_id)
pe_report <-
  readxl::read_xlsx("R/input/peru/BSINPAD_ 2013-2018.xlsx", sheet = "BD EVENTOS EN AMAZONIA", skip = 3, guess_max = 2700) %>%
  janitor::clean_names() %>%
  dplyr::select(-c(perdida_vacuno:afecta_de_animales_menores, has_cultivo_destruido, has_cultivo_afectado)) %>%
  dplyr::mutate(
    fecha_de_la_emer = as.character(fecha_de_la_emer)
    , ano = as.character(ano)
  ) %>%
  dplyr::bind_rows(
    readxl:::read_xlsx("R/input/peru/BD_SINPAD 2019-2022.xlsx", sheet = "BD EVENTOS EN AMAZONIA", skip = 3) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        across(c(ide_sinpad, camirural_destru, scamirural_afecta, anos), as.character)
      ) %>%
      dplyr::rename(
        "codigo_de_emergencia_sinpad" = "ide_sinpad"
        , "fecha_de_la_emer" = "fecha"
        , "ano" = "anos"
        , "cod_distrito" = "cod_ubigeo"
        , "dpto" = "dep"
        , "emergencia" = "emergencia"
        , "fallecidos" = "sfalle"
        , "desaparecidos" = "sdesa"
        , "lesionados" = "slesi"
        , "damnificados" = "sdamni"
        , "afectados" = "safecta"
        , "viviendas_destruidas" = "sdestruvivi"
        , "viviendas_afectadas" = "safectavivi"
        , "centros_educativos_destruidos" = "sdestru_aula"
        , "centros_salud_destruidos" = "sdestru_salud"
        , "centros_salud_afectados" = "safecta_salud"
        , "puente_destruido" = "spuente_destru"
        , "puente_afectado" = "spuente_afecta"
        , "carretera_destruida" = "scarre_destru"
        , "carretera_afectada" = "scarre_afecta"
        , "camnino_rural_destruido" = "camirural_destru"
        , "camino_rural_afectado" = "scamirural_afecta"
      )
  ) %>%
  dplyr::bind_rows(
    readxl::read_xlsx("R/input/peru/BD 2023 AMAZONIA PERUANA.xlsx", sheet = "BD 2023 AMAZONIA", skip = 4) %>%
      janitor::clean_names() %>%
      dplyr::rename(
        "fecha_de_la_emer" = "fecha"
        , "cod_distrito" = "cod_ubigeo"
        , "dpto" = "dep"
        , "prov" = "prov"
        , "dist" = "dist"
        , "emergencia" = "emergencia"
        , "fallecidos" = "sfalle"
        , "desaparecidos" = "sdesa"
        , "lesionados" = "slesi"
        , "damnificados" = "sdamni"
        , "afectados" = "safecta"
        , "viviendas_destruidas" = "sdestruvivi"
        , "viviendas_afectadas" = "safectavivi"
        , "centros_educativos_destruidos" = "sdestru_aula"
        , "centros_educativos_afectados" = "safec_aula"
        , "centros_salud_destruidos" = "sdestru_salud"
        , "centros_salud_afectados" = "safecta_salud"
        , "puente_destruido" = "spuente_destru"
        , "puente_afectado" = "spuente_afecta"
        , "carretera_destruida" = "scarre_destru"
        , "carretera_afectada" = "scarre_afecta"
        , "camnino_rural_destruido" = "camirural_destru"
        , "camino_rural_afectado" = "scamirural_afecta"
      ) %>%
      dplyr::mutate(
        ano = "2023"
        , across(c(camnino_rural_destruido, camino_rural_afectado), as.character)
        )
  ) %>%
  dplyr::filter(!is.na(ano))

unique(pe_report$emergencia)
unique(pe_report$ano)

disaster_amazonia_bioma_pe <-
  pe_report %>%
  dplyr::mutate(
    geo_id = cod_distrito
    , year = ano
    , disaster = case_when(
      emergencia == "INUNDACIÓN" ~ "Floods"
      , emergencia %in% c("DESLIZAMIENTO", "HUAYCO", "DERRUMBE DE CERRO", "ALUD") ~ "Landslides"
      , emergencia %in% c("LLUVIA INTENSA" , "TORMENTA ELÉCTRICA") ~ "Storms"
      , emergencia == "INCENDIO FORESTAL" ~ "Wildfires"
      , emergencia == "SEQUÍA" ~ "Droughts"
      , TRUE ~ "NA"
      )
  ) %>%
  dplyr::filter(
    year %in% as.character(2013:2023)
    & disaster != "NA"
    & geo_id %in% geo_id_pe
  )

table(disaster_amazonia_bioma_pe$disaster)
table(disaster_amazonia_bioma_pe$year)
length(unique(disaster_amazonia_bioma_pe$cod_distrito))

# grouping events per municipalities
disaster_amz_freq_mun_pe <-
  disaster_amazonia_bioma_pe %>%
  dplyr::group_by(disaster, year, dpto, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = dpto)



# __ all -------------------------------------------------------------------

# heatwave
unique(br_report$COBRADE) # "13310 - Onda de Calor"
sort(unique(co_report$descripcion_y_ubicacion_evento))
sort(unique(bo_report$tipo_de_evento)) #"Olas de calor"
sort(unique(ec_report$EVENTO))
sort(unique(pe_report$emergencia))
nrow(br_report %>% dplyr::filter(COBRADE == "13310 - Onda de Calor")) #102
nrow(bo_report %>% dplyr::filter(tipo_de_evento == "Olas de calor")) #3

glimpse(disaster_amz_freq_mun_br)
glimpse(disaster_amz_freq_mun_co)
glimpse(disaster_amz_freq_mun_bo)
glimpse(disaster_amz_freq_mun_ec)
glimpse(disaster_amz_freq_mun_pe)


reports <-
  rbind(
    disaster_amz_freq_mun_br %>% dplyr::mutate(country = "Brazil")
    , disaster_amz_freq_mun_co %>% dplyr::mutate(country = "Colombia")
    , disaster_amz_freq_mun_bo %>% dplyr::mutate(country = "Bolivia")
    , disaster_amz_freq_mun_ec %>% dplyr::mutate(country = "Ecuador")
    , disaster_amz_freq_mun_pe %>% dplyr::mutate(country = "Peru")
  ) %>%
  dplyr::left_join(
    municipios_amazonia_bioma %>% dplyr::select(geo_id, name_muni, geometry)
      , by = c("geo_id" = "geo_id")
  )

nrow(reports) #6059
sum(reports %>% dplyr::filter(country == "Bolivia" & disaster == "Wildfires") %>% dplyr::pull(n_report)) #357


main_disaster <-
  reports %>%
  dplyr::group_by(disaster, geometry, country, geo_id, name_muni) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
  sf::st_set_geometry("geometry")

glimpse(main_disaster)
summary(main_disaster)
class(main_disaster)
nrow(main_disaster) # 2153

# rm(disaster_amz_freq_mun_br, disaster_amz_freq_mun_co
#    , disaster_amz_freq_mun_bo, disaster_amz_freq_mun_ec
#    , disaster_amz_freq_mun_pe, list_files, geo_id_bo, geo_id_br
#    , geo_id_co, geo_id_ec, geo_id_pe
#    , bo_report, br_report, co_report, ec_report, pe_report)


# + IMPACT ----------------------------------------------------------------

# _ brazil ----------------------------------------------------------------

impact_br <-
  disaster_amazonia_bioma_br %>%
  dplyr::select(uf, geo_id, year, disaster, starts_with(c("dh_", "dm_"))) %>%
  dplyr::group_by(disaster, year, geo_id) %>%
  dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(!c(geo_id, disaster, year), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(impact_group = if_else(str_detect(variable, "^dh_"), "Human damage", "Material damage"))

human_impact_br <-
  impact_br %>%
  dplyr::filter(impact_group == "Human damage") %>%
  dplyr::mutate(variable_en = case_when(
    variable == "dh_mortos" ~ "Dead"
    , variable == "dh_feridos" ~ "Injured"
    , variable == "dh_enfermos" ~ "Sick"
    , variable == "dh_desabrigados" ~ "Evacuated"
    , variable == "dh_desalojados" ~ "Evacuated"
    , variable == "dh_desaparecidos" ~ "Disappeared"
    , variable == "dh_outros_afetados" ~ "Others"
    , TRUE ~ variable
  )) %>%
  dplyr::group_by(disaster, variable_en, year) %>%
  dplyr::summarise(total = sum(total), .groups = "drop")

infrastructure_impact_br <-
  impact_br %>%
  dplyr::filter(impact_group == "Material damage" & str_detect(variable, "_danificadas|_destruidas")) %>%
  dplyr::mutate(
    variable = gsub("_danificadas|_destruidas", "", variable)
    , variable_en = case_when(
      variable == "dm_unidades_habitacionais" ~ "Housing"
      , variable == "dm_instalacoes_publicas_de_saude" ~ "Health"
      , variable == "dm_instalacoes_publicas_de_ensino" ~ "Educational"
      , variable == "dm_instalacoes_publicas_prestadoras_de_outros_servicos" ~ "Public"
      , variable == "dm_instalacoes_publicas_de_uso_comunitario" ~ "Public"
      , variable == "dm_obras_de_infraestrutura_publica" ~ "Public"
      , TRUE ~ variable
    )) %>%
  dplyr::group_by(variable_en, year) %>%
  dplyr::summarise(total = sum(as.numeric(total), na.rm = TRUE), .groups = "drop")


# _ colombia --------------------------------------------------------------

human_impact_co <-
  disaster_amazonia_bioma_co %>%
  dplyr::select(year, disaster, afectacion_muertos, afectacion_heridos, afectacion_desapa) %>%
  dplyr::mutate(afectacion_muertos = as.numeric(afectacion_muertos)
               , afectacion_heridos = as.numeric(afectacion_heridos)
               , afectacion_desapa = as.numeric(afectacion_desapa)) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable == "afectacion_muertos" ~ "Dead"
      , variable == "afectacion_desapa" ~ "Disappeared"
      , variable == "afectacion_heridos" ~ "Injured"
    )) %>%
  dplyr::group_by(disaster, variable_en, year) %>%
  dplyr::summarise(total = sum(as.numeric(total), na.rm = TRUE), .groups = "drop")

infrastructure_impact_co <-
  disaster_amazonia_bioma_co %>%
  dplyr::select(year, disaster, afectacion_viv_destru:afectacion_c_comunit) %>%
  dplyr::mutate(across(afectacion_viv_destru:afectacion_c_comunit, as.numeric)) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable %in% c("afectacion_viv_aver", "afectacion_viv_destru") ~ "Housing"
        , variable == "afectacion_c_educat" ~ "Educational"
        , variable == "afectacion_c_salud" ~ "Health"
        , variable %in% c("afectacion_vias", "afectacion_ptes_vehic", "afectacion_ptes_peat"
                          , "afectacion_acued", "afectacion_alcant", "afectacion_c_comunit") ~ "Public"
    )) %>%
  dplyr::group_by(variable_en, year) %>%
  dplyr::summarise(total = sum(as.numeric(total), na.rm = TRUE), .groups = "drop")


# _ bolivia ---------------------------------------------------------------

human_impact_bo <-
  disaster_amazonia_bioma_bo %>%
  dplyr::select(year, disaster, fallecidos, desaparecidos, heridos, evacuados) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable == "fallecidos" ~ "Dead"
      , variable == "desaparecidos" ~ "Disappeared"
      , variable == "heridos" ~ "Injured"
      , variable == "evacuados" ~ "Evacuated"
    )) %>%
  dplyr::group_by(disaster, variable_en, year) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")


infrastructure_impact_bo <-
  disaster_amazonia_bioma_bo %>%
  dplyr::select(year, disaster, centros_educativos, viv_afect_no, viv_dest_no) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable == "centros_educativos" ~ "Educational"
      , variable %in% c("viv_afect_no", "viv_dest_no") ~ "Housing"
    )
    , total = as.numeric(total)
  ) %>%
  dplyr::group_by(variable_en, year) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")


# _ ecuador ---------------------------------------------------------------

human_impact_ec <-
  disaster_amazonia_bioma_ec %>%
  dplyr::select(year, disaster, fallecidas, heridas, personas_desaparecidas
                , personas_albergadas, personas_evacuadas, personas_en_proceso_de_evacuacion
                , personas_que_resisten_a_la_evacuacion , personas_damnificadas
                , personas_afectadas_directamente, personas_en_otros_medios) %>%
  dplyr::mutate(personas_afectadas_directamente = as.numeric(personas_afectadas_directamente)) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable == "fallecidas" ~ "Dead"
      , variable == "personas_desaparecidas" ~ "Disappeared"
      , variable == "heridas" ~ "Injured"
      , variable %in% c("personas_albergadas", "personas_evacuadas", "personas_en_proceso_de_evacuacion", "personas_que_resisten_a_la_evacuacion") ~ "Evacuated"
      , variable %in% c("personas_afectadas_directamente", "personas_damnificadas", "personas_en_otros_medios") ~ "Others" #"personas_impactadas"
    )) %>%
  dplyr::group_by(disaster, variable_en, year) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

infrastructure_impact_ec <-
  disaster_amazonia_bioma_ec %>%
  dplyr::select(year, disaster, viviendas_afectadas:bienes_privados_destruidos) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable %in% c("viviendas_afectadas", "viviendas_destruidas") ~ "Housing"
      , variable %in% c("establecimientos_educativos_afectados", "establecimientos_educativos_destruidos") ~ "Educational"
        , variable %in% c("centros_de_salud_afectados", "centros_de_salud_destruidos") ~ "Health"
        , variable %in% c("puentes_afectados", "puentes_destruidos", "bienes_publicos_afectados", "bienes_publicos_destruidos") ~ "Public"
        , variable %in% c("bienes_privados_afectados", "bienes_privados_destruidos") ~ "Private"
      )) %>%
  dplyr::group_by(variable_en, year) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")


# _ peru ------------------------------------------------------------------

human_impact_pe <-
  disaster_amazonia_bioma_pe %>%
  dplyr::select(year, disaster, fallecidos, desaparecidos, lesionados, damnificados, afectados) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable == "fallecidos" ~ "Dead"
      , variable == "desaparecidos" ~ "Disappeared"
        , variable == "lesionados" ~ "Injured"
        , variable %in% c("damnificados", "afectados") ~ "Others"
    )) %>%
  dplyr::group_by(disaster, variable_en, year) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")


infrastructure_impact_pe <-
  disaster_amazonia_bioma_pe %>%
  dplyr::select(
    year, disaster
    , viviendas_afectadas
    , viviendas_destruidas
    , centros_educativos_afectados
    , centros_educativos_destruidos
    , centros_salud_afectados
    , centros_salud_destruidos
    , puente_destruido
    , puente_afectado
    # , carretera_destruida        # the units seems to length, so it was not considered
    # , carretera_afectada
    # , camnino_rural_destruido
    # , camino_rural_afectado
  ) %>%
  tidyr::pivot_longer(!c(year, disaster), names_to = "variable", values_to = "total") %>%
  dplyr::mutate(
    variable_en = case_when(
      variable %in% c("viviendas_afectadas", "viviendas_destruidas") ~ "Housing"
      , variable %in% c("centros_educativos_afectados", "centros_educativos_destruidos") ~ "Educational"
      , variable %in% c("centros_salud_afectados", "centros_salud_destruidos") ~ "Health"
      , variable %in% c("puente_destruido"
                        , "puente_afectado"
                        # , "carretera_destruida"
                        # , "carretera_afectada"
                        # , "camnino_rural_destruido"
                        # , "camino_rural_afectado"
                        ) ~ "Public"
    )) %>%
  dplyr::group_by(variable_en, year) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")


# __ all -------------------------------------------------------------------

glimpse(human_impact_br)
glimpse(human_impact_co)
glimpse(human_impact_bo)
glimpse(human_impact_ec)
glimpse(human_impact_pe)

human_impact_all <-
  rbind(
    human_impact_br %>% dplyr::mutate(country = "Brazil")
    , human_impact_co %>% dplyr::mutate(country = "Colombia")
    , human_impact_bo %>% dplyr::mutate(country = "Bolivia")
    , human_impact_ec %>% dplyr::mutate(country = "Ecuador")
    , human_impact_pe %>% dplyr::mutate(country = "Peru")
  )

glimpse(infrastructure_impact_br)
glimpse(infrastructure_impact_co)
glimpse(infrastructure_impact_bo)
glimpse(infrastructure_impact_ec)
glimpse(infrastructure_impact_pe)

infrastructure_impact_all <-
  rbind(
    infrastructure_impact_br %>% dplyr::mutate(country = "Brazil")
    , infrastructure_impact_co %>% dplyr::mutate(country = "Colombia")
    , infrastructure_impact_bo %>% dplyr::mutate(country = "Bolivia")
    , infrastructure_impact_ec %>% dplyr::mutate(country = "Ecuador")
    , infrastructure_impact_pe %>% dplyr::mutate(country = "Peru")
  )

glimpse(human_impact_all)
glimpse(infrastructure_impact_all)

# rm(human_impact_br, human_impact_co, human_impact_bo, human_impact_ec, human_impact_pe
#    , infrastructure_impact_br, infrastructure_impact_co, infrastructure_impact_bo
#    , infrastructure_impact_ec, infrastructure_impact_pe)


# + INDEX -----------------------------------------------------------------

# _ wet / dry / fire -------------------------------------------------------

index_files <- list.files("R/input/_remote_sensing/climate_index", ".csv")

for (i in index_files) {

  index <- sub("_.*", "", i)
  print(index)

  index_i <-
    readr::read_csv(paste0("R/input/_remote_sensing/climate_index/", i), show_col_types = FALSE) %>%
    dplyr::mutate(
      year_index = gsub("_.*", "", `system:index`) %>% as.numeric()
      , year = 2013 + year_index
      , index = index
    )

  if (index != "hotpixel") {
    index_i <- index_i %>% dplyr::select(country, geo_id, max, mean, year, index)
  } else {
    index_i <- index_i %>% dplyr::select(country, geo_id, sum, year, index)
  }

  assign(
    paste0("index_", index)
    , dplyr::left_join(municipios_amazonia_bioma, index_i, by = c("country", "geo_id"), keep = FALSE)
  )
}

rm(index_i); rm(index)

index_hotpixel <-
  index_hotpixel %>%
  dplyr::mutate(perc_hotpixel = sum/(units::drop_units(amazon_area)/10000)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    threshold_p90 = quantile(perc_hotpixel, 0.90, na.rm = TRUE)
    , threshold_p95 = quantile(perc_hotpixel, 0.95, na.rm = TRUE)
    , perc_hotpixel_p90 = if_else(perc_hotpixel >= threshold_p90, perc_hotpixel, 0)
    , perc_hotpixel_p95 = if_else(perc_hotpixel >= threshold_p95, perc_hotpixel, 0)
  ) %>%
  dplyr::ungroup()


# _ population ------------------------------------------------------------

muni_amazonia_vect <-
  municipios_all_area %>%
  sf::st_transform(4326) %>%
  terra::vect()

pop_files <- list.files("R/input/_remote_sensing/gridded_population", "hdr.adf|*.tif$", recursive = TRUE)
pop_files <- pop_files[!str_detect(pop_files, "colorized")]
# population_stack <- NULL
gridded_population <- municipios_amazonia_bioma

for (i in pop_files) {
  year <- stringr::str_extract(i, "\\d+")
  print(year)

  pop_i <- terra::rast(paste0("R/input/_remote_sensing/gridded_population/", i))
  names(pop_i) <- paste0("pop_", year)

  zonal <-
    terra::zonal(pop_i, muni_amazonia_vect, fun = "sum", na.rm = TRUE, as.polygons = TRUE) %>%
    sf::st_as_sf() %>%
    sf::st_drop_geometry() %>%
    dplyr::select(country, geo_id, all_of(names(pop_i)))

  gridded_population <- dplyr::left_join(gridded_population, zonal, by = c("country", "geo_id"), keep = FALSE)

}

gridded_population_longer <-
  gridded_population %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("pop_")
    , names_to = "year"
    , values_to = "population"
  ) %>%
  dplyr::mutate(year = gsub("pop_", "", year))

gridded_population <-
  gridded_population %>%
  dplyr::mutate(pop_mean = round(base::rowMeans(dplyr::across(tidyr::starts_with("pop_")), na.rm = TRUE)))

rm(zonal, muni_amazonia_vect, pop_i)


# _ altitude / slope ----------------------------------------------------

elevation <-
  rbind(
    readr::read_csv("R/input/_remote_sensing/elevation/elevation_mean_max_per_municipality.csv", show_col_types = FALSE) %>%
      dplyr::mutate(index = "Elevation")
    , readr::read_csv("R/input/_remote_sensing/elevation/slope_mean_max_per_municipality.csv", show_col_types = FALSE) %>%
      dplyr::mutate(index = "Slope")
  ) %>%
  dplyr::left_join(municipios_amazonia_bioma, by = c("country", "geo_id"), keep = FALSE) %>%
  sf::st_set_geometry("geometry")


# + MODEL -----------------------------------------------------------------

glimpse(index_cdd)
glimpse(index_r20mm)
glimpse(index_r95p)
glimpse(index_rx1day)
glimpse(index_rx5day)
glimpse(gridded_population_longer)
glimpse(elevation)
glimpse(reports)

reports_wider <-
  reports %>%
  dplyr::select(-c(state, geometry)) %>%
  dplyr::group_by(year, geo_id, country, disaster) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = disaster
    , values_from = n_report
    , values_fill = 0
  )

model_dataset <-
  gridded_population_longer %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::left_join(
    index_cdd %>%
      sf::st_drop_geometry() %>%
      dplyr::select(country, geo_id, cdd_max = max, cdd_mean = mean, year)
    , by = c("country", "geo_id", "year"), keep = FALSE
  ) %>%
  dplyr::left_join(
    index_r20mm %>%
      sf::st_drop_geometry() %>%
      dplyr::select(country, geo_id, r20mm_max = max, r20mm_mean = mean, year)
    , by = c("country", "geo_id", "year"), keep = FALSE
  ) %>%
  dplyr::left_join(
    index_r95p %>%
      sf::st_drop_geometry() %>%
      dplyr::select(country, geo_id, r95p_max = max, r95p_mean = mean, year)
    , by = c("country", "geo_id", "year"), keep = FALSE
  ) %>%
  dplyr::left_join(
    index_rx1day %>%
      sf::st_drop_geometry() %>%
      dplyr::select(country, geo_id, rx1day_max = max, rx1day_mean = mean, year)
    , by = c("country", "geo_id", "year"), keep = FALSE
  ) %>%
  dplyr::left_join(
    index_rx5day %>%
      sf::st_drop_geometry() %>%
      dplyr::select(country, geo_id, rx5day_max = max, rx5day_mean = mean, year)
    , by = c("country", "geo_id", "year"), keep = FALSE
  ) %>%
  left_join(
    elevation %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(index == "Elevation") %>%
      dplyr::select(country, geo_id, elevation_max = max, elevation_mean = mean)
    , by = c("country", "geo_id"), keep = FALSE
  ) %>%
  dplyr::left_join(
    elevation %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(index == "Slope") %>%
      dplyr::select(country, geo_id, slope_max = max, slope_mean = mean)
    , by = c("country", "geo_id"), keep = FALSE
  ) %>%
  dplyr::left_join(
    index_hotpixel %>%
      sf::st_drop_geometry() %>%
      dplyr::select(country, geo_id, perc_hotpixel, perc_hotpixel_p90, perc_hotpixel_p95, year)
    , by = c("country", "geo_id", "year"), keep = FALSE
  ) %>%
  dplyr::left_join(
    reports_wider %>% dplyr::mutate(year = as.numeric(year))
    , by = c("country", "geo_id", "year"), keep = FALSE
  ) %>%
  # dplyr::select(-c(name_muni:perc_amazon_area)) %>%
  dplyr::filter(!country %in% c("French Guiana", "Guyana", "Suriname", "Venezuela")) %>%
  dplyr::mutate(dplyr::across(c(Droughts, Floods, Landslides, Storms, Wildfires), ~tidyr::replace_na(., 0)))

glimpse(model_dataset)
summary(dplyr::select(model_dataset, population, cdd_mean, r95p_mean, r20mm_mean,
                      rx1day_mean, rx5day_mean, elevation_mean, slope_mean))

model_dataset_clean <-
  na.omit(model_dataset) %>%
  mutate(year_num = year - min(year) + 1)

nrow(model_dataset) #11693
nrow(model_dataset_clean) #11690


# >> glm / nb ---------------------------------------------------------
options(scipen = 5)

# _ storms ----------------------------------------------------------------

model_glm_storms <-
  glm(
    Storms ~ country + year + log10(population) +
      r20mm_mean + r95p_mean + rx1day_mean
    , data = model_dataset_clean, family = poisson(link = "log")
  )

step_model_glm_storms <- step(model_glm_storms, direction = "both")
summary(model_glm_storms)
summary(step_model_glm_storms)

# residualPlots(step_model_glm_storms)
# par(mfrow = c(2,2))
# plot(step_model_glm_storms)

model_glm_nb_storms <-
  glm.nb(
    Storms ~ country + year_num + log10(population) +
      r20mm_mean + r95p_mean + rx1day_mean
    , data = model_dataset_clean
  )

summary(model_glm_nb_storms)

# png("R/output/model_glm_nb_storms.jpg", width = 500, height = 400)
# par(mfrow = c(2,2))
# plot(model_glm_nb_storms, col = "#ffd92f")
# dev.off()


# _ floods ----------------------------------------------------------------

model_glm_floods <-
  glm(
    Floods ~ country + year + log10(population) +
      r20mm_mean + r95p_mean + rx1day_mean
    , data = model_dataset_clean, family = poisson(link = "log")
  )

step_model_glm_floods <- step(model_glm_floods, direction = "both")
summary(model_glm_floods)
summary(step_model_glm_floods)


# residualPlots(step_model_glm_floods)
# plot(step_model_glm_floods)

model_glm_nb_floods <-
  glm.nb(
    Floods ~ country + year_num + log10(population) +
      r20mm_mean + r95p_mean + rx1day_mean
    , data = model_dataset_clean
  )

summary(model_glm_nb_floods)

# png("R/output/model_glm_nb_floods.jpg", width = 500, height = 400)
# par(mfrow = c(2,2))
# plot(model_glm_nb_floods, col = "#ffd92f")
# dev.off()


# _ landslides ------------------------------------------------------------

model_glm_landslides <-
  glm(
    Landslides ~ country + year + log10(population) + elevation_mean + slope_mean +
      r20mm_mean + r95p_mean + rx5day_mean
    , data = model_dataset_clean, family = poisson(link = "log")
  )

step_model_glm_landslides <- step(model_glm_landslides, direction = "both")
summary(model_glm_landslides)
summary(step_model_glm_landslides)


# residualPlots(step_model_glm_landslides)
# plot(step_model_glm_landslides)

model_glm_nb_landslides <-
  glm.nb(
    Landslides ~ country + year_num + log10(population) + elevation_mean +
      r20mm_mean + rx1day_mean + rx5day_mean
    , data = model_dataset_clean
  )

summary(model_glm_nb_landslides)

# png("R/output/model_glm_nb_landslides.jpg", width = 500, height = 400)
# par(mfrow = c(2,2))
# plot(model_glm_nb_landslides, col = "#ffd92f")
# dev.off()


# _ droughts --------------------------------------------------------------

model_glm_droughts <-
  glm(
    Droughts ~ country + year + log10(population) + cdd_mean
    , data = model_dataset_clean, family = poisson(link = "log")
  )

step_model_glm_droughts <- step(model_glm_droughts, direction = "both")
summary(model_glm_droughts)
summary(step_model_glm_droughts)

# residualPlots(step_model_glm_droughts)
# plot(step_model_glm_droughts)

model_glm_nb_droughts <-
  glm.nb(
    Droughts ~ country + year_num + log10(population) + cdd_mean #poly(cdd_mean, 2) # cdd_mean * country
    , data = model_dataset_clean
  )

# summary(glm.nb(Droughts ~ cdd_mean * factor(country), data = model_dataset_clean))
summary(model_glm_nb_droughts)

# png("R/output/model_glm_nb_droughts.jpg", width = 500, height = 400)
# par(mfrow = c(2,2))
# plot(model_glm_nb_droughts, col = "#ffd92f")
# dev.off()

# ggplot(model_dataset_clean, aes(x = cdd_mean, y = Droughts)) +
#   geom_point(alpha = 0.2) +
#   labs(x = "Consecutive Dry Days (cdd_mean | mean by municipality)"
#        , y = "Drought Events") +
#   theme_bw()
#
# summary(model_dataset_clean$cdd_mean)
# boxplot(model_dataset_clean$cdd_mean)


# _ wildfires -------------------------------------------------------------

model_glm_wildfires <-
  glm(
    Wildfires ~ country + year + log10(population) + perc_hotpixel + perc_hotpixel_p90 + perc_hotpixel_p95
    , data = model_dataset_clean, family = poisson(link = "log")
  )

step_model_glm_wildfires <- step(model_glm_wildfires, direction = "both")
summary(model_glm_wildfires)
summary(step_model_glm_wildfires)

# residualPlots(model_glm_wildfires)
# plot(step_model_glm_wildfires)

model_glm_nb_wildfires <-
  glm.nb(
    Wildfires ~ country + year_num + log10(population) + perc_hotpixel
    , data = model_dataset_clean
  )

summary(model_glm_nb_storms)
summary(model_glm_nb_floods)
summary(model_glm_nb_landslides)
summary(model_glm_nb_droughts)
summary(model_glm_nb_wildfires)


# >> inla ------------------------------------------------------------------

model_dataset_clean <-
  model_dataset_clean %>%
  dplyr::mutate(
    id_area = as.numeric(as.factor(paste(country, geo_id)))
    , id_time = (year - min(year) + 1)
    , country_time = interaction(country, id_time, drop = TRUE)
  ) %>%
  dplyr::left_join(
    municipios_amazonia_bioma %>% dplyr::select(country, geo_id, geometry)
    , by = c("country", "geo_id"), keep = FALSE
  ) %>%
  sf::st_set_geometry("geometry")

# queen_nb <- poly2nb(model_dataset_clean_geo) # take too long
# nb2INLA("R/model/map.adj", queen_nb)
amz_adj <- inla.read.graph(filename = "R/model/map.adj")

# storms
formula_1_storms <-
  Storms ~ 0 + 1 +
  log10(population) + r20mm_mean + r95p_mean + rx1day_mean +
  f(country, id_time, model = "iid") +
  f(id_area, model = "bym2", graph = amz_adj)

model_1_storms <-
  inla(
    formula = formula_1_storms
    , family  = "nbinomial"
    , data = model_dataset_clean
    , control.predictor = list(compute = TRUE)
    , control.compute   = list(cpo  = TRUE, dic  = TRUE, waic = TRUE)) # For model comparison


# floods
formula_1_floods <-
  Floods ~ 0 + 1 +
  log10(population) + r20mm_mean + r95p_mean + rx1day_mean +
  f(country, id_time, model = "iid") +
  f(id_area, model = "bym2", graph = amz_adj)

model_1_floods <-
  inla(
    formula = formula_1_floods
    , family  = "nbinomial"
    , data = model_dataset_clean
    , control.predictor = list(compute = TRUE)
    , control.compute   = list(cpo  = TRUE, dic  = TRUE, waic = TRUE)) # For model comparison


# landslides
formula_1_landslides <-
  Landslides ~ 0 + 1 +
  log10(population) + elevation_mean + r20mm_mean + rx5day_mean +
  f(country, id_time, model = "iid") +
  f(id_area, model = "bym2", graph = amz_adj)

model_1_landslides <-
  inla(
    formula = formula_1_landslides
    , family  = "nbinomial"
    , data = model_dataset_clean
    , control.predictor = list(compute = TRUE)
    , control.compute   = list(cpo  = TRUE, dic  = TRUE, waic = TRUE)) # For model comparison

# droughts
formula_1_droughts <-
  Droughts ~ 0 + 1 +
  log10(population) + cdd_mean +
  f(country, id_time, model = "iid") +
  f(id_area, model = "bym2", graph = amz_adj)

model_1_droughts <-
  inla(
    formula = formula_1_droughts
    , family  = "nbinomial"
    , data = model_dataset_clean
    , control.predictor = list(compute = TRUE)
    , control.compute   = list(cpo  = TRUE, dic  = TRUE, waic = TRUE)) # For model comparison


# wildfires
formula_1_wildfires <-
  Wildfires ~ 0 + 1 +
  log10(population) + perc_hotpixel +
  f(country, id_time, model = "iid") +
  f(id_area, model = "bym2", graph = amz_adj)

model_1_wildfires <-
  inla(
    formula = formula_1_wildfires
    , family  = "nbinomial"
    , data = model_dataset_clean
    , control.predictor = list(compute = TRUE)
    , control.compute   = list(cpo  = TRUE, dic  = TRUE, waic = TRUE)) # For model comparison

summary(model_1_storms)
summary(model_1_floods)
summary(model_1_landslides)
summary(model_1_droughts)
summary(model_1_wildfires)

rm(list = ls(pattern = "model_1"))


# _ trend ------------------------------------------------------------------

model_1_storms_summary_fixed <- model_1_storms$summary.fixed
model_1_floods_summary_fixed <- model_1_floods$summary.fixed
model_1_landslides_summary_fixed <- model_1_landslides$summary.fixed
model_1_droughts_summary_fixed <- model_1_droughts$summary.fixed
model_1_wildfires_summary_fixed <- model_1_wildfires$summary.fixed

model_1_storms_country <- model_1_storms$summary.random$country
model_1_floods_country <- model_1_floods$summary.random$country
model_1_landslides_country <- model_1_landslides$summary.random$country
model_1_droughts_country <- model_1_droughts$summary.random$country
model_1_wildfires_country <- model_1_wildfires$summary.random$country

rm(model_1_storms, model_1_floods, model_1_landslides, model_1_droughts, model_1_wildfires)
gc()

##calculate all the intercepts
dint <- exp(model_1_droughts_summary_fixed[1,1] + model_1_droughts_summary_fixed[2,1] * mean(log10(model_dataset_clean$population)) + model_1_droughts_summary_fixed[3,1] * mean(model_dataset_clean$cdd_mean))
fint <- exp(model_1_floods_summary_fixed[1,1] + model_1_floods_summary_fixed[2,1] * mean(log10(model_dataset_clean$population)) + model_1_floods_summary_fixed[3,1] * mean(model_dataset_clean$r20mm_mean) + model_1_floods_summary_fixed[4,1]*mean(model_dataset_clean$r95p_mean) + model_1_floods_summary_fixed[5,1]*mean(model_dataset_clean$rx1day_mean))
lint <- exp(model_1_landslides_summary_fixed[1,1] + model_1_landslides_summary_fixed[2,1] * mean(log10(model_dataset_clean$population)) + model_1_landslides_summary_fixed[3,1] * mean(model_dataset_clean$elevation_mean) + model_1_landslides_summary_fixed[4,1]*mean(model_dataset_clean$r20mm_mean) + model_1_landslides_summary_fixed[5,1]*mean(model_dataset_clean$rx5day_mean))
sint <- exp(model_1_storms_summary_fixed[1,1] + model_1_storms_summary_fixed[2,1] * mean(log10(model_dataset_clean$population)) + model_1_storms_summary_fixed[3,1] * mean(model_dataset_clean$r20mm_mean) + model_1_storms_summary_fixed[4,1]*mean(model_dataset_clean$r95p_mean) + model_1_storms_summary_fixed[5,1]*mean(model_dataset_clean$rx1day_mean))
wint <- exp(model_1_wildfires_summary_fixed[1,1] + model_1_wildfires_summary_fixed[2,1] * mean(log10(model_dataset_clean$population)) + model_1_wildfires_summary_fixed[3,1] * mean(model_dataset_clean$perc_hotpixel))

##based on https://www.reddit.com/r/rstats/comments/s96kc3/multiply_column_values_with_matching_values_from/
model_dataset_clean_info <-
  model_dataset_clean %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(model_1_droughts_country %>% dplyr::select(ID, mean_d = mean), by = c("country" = "ID")) %>%
  dplyr::left_join(model_1_floods_country %>% dplyr::select(ID, mean_f = mean), by = c("country" = "ID")) %>%
  dplyr::left_join(model_1_landslides_country %>% dplyr::select(ID, mean_l = mean), by = c("country" = "ID")) %>%
  dplyr::left_join(model_1_storms_country %>% dplyr::select(ID, mean_s = mean), by = c("country" = "ID")) %>%
  dplyr::left_join(model_1_wildfires_country %>% dplyr::select(ID, mean_w = mean), by = c("country" = "ID")) %>%
  dplyr::mutate(
    dbyt = exp(dint + id_time * mean_d)
    , fbyt = exp(fint + id_time * mean_f)
    , lbyt = exp(lint + id_time * mean_l)
    , sbyt = exp(sint + id_time * mean_s)
    , wbyt = exp(wint + id_time * mean_w)
  )

model_dataset_clean_info_long <-
  model_dataset_clean_info %>%
  dplyr::select(country, year, dbyt:wbyt) %>%
  tidyr::pivot_longer(
    cols = dbyt:wbyt
    , names_to = "events"
    , values_to = "values"
  ) %>%
  dplyr::mutate(
    events = case_when(
      events == "dbyt" ~ "Droughts"
      , events == "fbyt" ~ "Floods"
      , events == "lbyt" ~ "Landslides"
      , events == "sbyt" ~ "Storms"
      , events == "wbyt" ~ "Wildfires"
    )
    , plot = case_when(
      events == "Droughts" & country == "Ecuador" ~ 0
      , events == "Floods" & country != "Brazil" ~ 0
      , events == "Storms" & country == "Bolivia" ~ 0
      , events == "Landslides" & country %in% c("Brazil", "Peru") ~ 0
      , TRUE ~ 1
    )
  )

events_long <-
  model_dataset_clean %>%
  sf::st_drop_geometry() %>%
  dplyr::select(country, year, Storms:Wildfires) %>%
  tidyr::pivot_longer(
    cols = Storms:Wildfires
    , names_to = "events"
    , values_to = "values"
  ) %>%
  dplyr::group_by(country, year, events) %>%
  dplyr::summarise(values = sum(values), .groups = "drop")

events_long %>%
  ggplot(aes(x = year, y = values)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_smooth(data = model_dataset_clean_info_long %>% dplyr::filter(plot == 1), method = "loess", se = FALSE, color = "darkred") +
  facet_grid(events~country, scales = "free") +
  scale_x_continuous(breaks = seq(2013, 2023, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022, "")) +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Count") +
  theme(
    text = element_text(family = "Calibri", color = "gray21", size = 11)
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 11)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 11)
    , axis.title = element_text(family = "Calibri", color = "gray21", size = 11, hjust = 0)
    , panel.grid.minor.x = element_blank()
    , axis.line.x.bottom = element_line(color = "gray21")
    , axis.line.y.left = element_line(color = "gray21")
    , strip.background = element_rect(fill = "white")
    , panel.grid.minor.y = element_blank()
    , panel.grid.major.x = element_blank()
    , legend.position = "bottom"
  )

ggsave("R/output/inla_disaster_country.jpg", width = 9.8, height = 5.67, units = "in", dpi = 600)


rm(model_1_storms, model_1_floods, model_1_landslides, model_1_droughts, model_1_wildfires)


# + MAP -------------------------------------------------------------------
windows()

# _ location --------------------------------------------------------------

world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
world  <-
  world %>%
  sf::st_cast('MULTILINESTRING') %>%
  sf::st_cast('LINESTRING', do_split = TRUE) %>%
  dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE)) %>%
  sf::st_cast('POLYGON')

ortho <- paste0('+proj=ortho +lat_0=', -6, ' +lon_0=', -57, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

location <-
  ggplot() +
  geom_sf(data = world %>% st_transform(crs = ortho), fill = "gray80", color = NA) +
  geom_sf(data = amazonia %>% st_transform(crs = ortho), fill = "gray40", color = "gray40") +
  theme_minimal() +
  theme(
    legend.position = "none"
    , axis.text = element_blank()
    , plot.margin = margin(0, -2, 0, -2)
  )


# _ fig 1 ------------------------------------------------------------

disaster_agg <-
  main_disaster %>%
  dplyr::group_by(geometry, country, geo_id, name_muni) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
  dplyr::mutate(
    interval = cut(
      n_report
      , breaks = c(0, 5, 10, 50, 100, 150, max(n_report) + 1)
      , right = TRUE
    )
    , interval = factor(interval, levels = rev(levels(interval)))
  )

map_total_report <-
  map_layout_disaster_report_total(
    data = disaster_agg
    , variable = "interval"
    , class = levels(disaster_agg$interval)
    , legend_title = "Events"
    , title = "a) Weather disaster\n"
  ) +
  annotation_custom(
    grob = ggplotGrob(location)
    , ymin = -20
    , ymax = -10
    , xmin = -50
    , xmax = -42
  ) +
  ggspatial::annotation_scale(
    bar_cols = c("#737373", "white")
    , line_col = "#737373"
    , text_col = "#737373"
    , width_hint = .1
    , pad_y = unit(0.05, "cm")
    , pad_x = unit(0.05, "cm")
    , text_pad = unit(0.05, "cm")
    , style = "ticks"
    , location = "bl"
  ) +
  ggspatial::annotation_north_arrow(
    which_north = "true"
    , location = "bl"
    , height = unit(1, "cm")
    , width = unit(1, "cm")
    , pad_x = unit(-.1, "in")
    , pad_y = unit(0.3, "in")
    , style = ggspatial::north_arrow_minimal(
      text_size = 10
      , line_col = "#737373"
      , text_col = "#737373")
  )

create_interval_data <-
  function(data, disaster_type) {
    data %>%
      dplyr::filter(disaster == disaster_type) %>%
      dplyr::mutate(
        interval = cut(
          n_report
          , breaks = c(0, 1, 5, 10, 50, 100, max(n_report))
          , right = TRUE
        )
        , interval = factor(interval, levels = rev(levels(interval)))
      )
  }

storms_data <- create_interval_data(main_disaster, "Storms")
floods_data <- create_interval_data(main_disaster, "Floods")
landslides_data <- create_interval_data(main_disaster, "Landslides")
droughts_data <- create_interval_data(main_disaster, "Droughts")
wildfire_data <- create_interval_data(main_disaster, "Wildfires")

map_storms <-
  map_layout_disaster_report_event_class(
    data = storms_data
    , variable = "interval"
    , legend_title = "Events"
    , palette = c("#0B0405E6", "#081d58", "#08519c", "#1E90FF", "#87CEFA", "#9ecae1")
    , class = levels(storms_data$interval)
    , direction = 1
    , title = "b) Storm\n"
  )

map_floods <-
  map_layout_disaster_report_event_class(
    data = floods_data
    , variable = "interval"
    , legend_title = "Events"
    , palette = "G"
    , direction = 1
    , class = levels(floods_data$interval)
    , title = "c) Flood\n"
  )

map_landslides <-
  map_layout_disaster_report_event_class(
    data = landslides_data
    , variable = "interval"
    , legend_title = "Events"
    , palette = "beyonce::X16"
    , class = levels(landslides_data$interval)
    , direction = 1
    , title = "d) Landslide\n"
  )

map_droughts <-
  map_layout_disaster_report_event_class(
    data = droughts_data
    , variable = "interval"
    , legend_title = "Events"
    , palette = "YlOrBr"
    , class = levels(droughts_data$interval)
    , direction = -1
    , title = "e) Drought\n"
  )

map_wildfires <-
  map_layout_disaster_report_event_class(
    data = wildfire_data
    , variable = "interval"
    , legend_title = "Events"
    , palette = c("#3c0404", "#67000d", "#a50f15", "#ef3b2c", "#fc9272", "#F7C9AAFF")
    , class = levels(wildfire_data$interval)
    , direction = 1
    , title = "f) Wildfire\n"
  )

map_population <-
  gridded_population %>%
  ggplot() +
  geom_sf(aes(fill = log10(pop_mean)), alpha = .75, color = "gray50", linewidth = .05) +
  geom_sf(data = south_america, fill = NA, color = "gray35", linewidth = .3) +
  geom_sf(data = amazonia, fill = NA, color = "#7fbc41", linewidth = .4) +
  coord_sf(xlim  = c(-80, -43), ylim = c(-17.5, 10)) +
  theme_void() +
  scico::scale_fill_scico(palette = "acton", direction = -1, labels = scales::math_format(10^.x)) +
  labs(title = " g) Population\n") +
  theme(
    title = element_text(family = "Calibri", color = "gray21", size = 9)
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 10)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 10)
  ) +
  guides(fill = guide_colourbar(
    title = 'Pop'
    , title.position = 'top'
    , title.hjust = 0
    , ticks.colour = 'gray35'
    , ticks.linewidth = 1
    , barwidth = .8
    , barheight = 5))

g <- map_total_report + map_storms + map_floods + map_landslides + map_droughts + map_wildfires + map_population +  patchwork::plot_layout(nrow = 4)
ggsave("R/output/extreme_events.jpg", g, width = 8, height = 10, units = "in", dpi = 600)


# _ gt50 ------------------------------------------------------------------

top_mun_gt50 <-
  disaster_agg %>%
  dplyr::filter(n_report > 50)

windows()
map_total_report +
  geom_sf(data = top_mun_gt50, color = "darkred", fill = NA) +
  coord_sf(xlim  = c(-80, -43), ylim = c(-17.5, 10))

ggsave("R/output/extreme_events_gt50.jpg")


# _ index -----------------------------------------------------------------

plot_index_map <-
  function(data, fill_var, title, palette, type = NULL) {

    is_hotpixel_variant <- startsWith(fill_var, "perc_hotpixel") && identical(data, index_hotpixel)
    is_population_variant <- startsWith(fill_var, "population") && identical(data, gridded_population_longer)
    is_elevation_variant <- isTRUE(type == "elevation")

    scale_fill <-
      if (is_hotpixel_variant) {
        viridis::scale_fill_viridis(labels = scales::math_format(10^.x), option = palette, direction = -1, na.value = "white")
      } else if (is_population_variant) {
        scico::scale_fill_scico(palette = palette, direction = -1, labels = scales::math_format(10^.x))
      } else if (is_elevation_variant) {
        paletteer::scale_fill_paletteer_c(palette = palette, direction = -1, labels = scales::math_format(10^.x))
      } else {
        viridis::scale_fill_viridis(option = palette, direction = -1)
      }

    facet <- if (is_elevation_variant) {
      NULL
    } else {
      facet_wrap(~year, nrow = 3)
    }

    ggplot(data) +
      geom_sf(
        aes(fill = if (is_hotpixel_variant | is_population_variant | is_elevation_variant) {
          log10(.data[[fill_var]])
        } else { .data[[fill_var]] })
        , alpha = .75
        , color = "gray50", linewidth = .05
      ) +
      geom_sf(data = south_america, fill = NA, color = "gray35", linewidth = .3) +
      geom_sf(data = amazonia, fill = NA, color = "#7fbc41", linewidth = .4) +
      coord_sf(xlim  = c(-80, -43), ylim = c(-17.5, 10)) +
      facet +
      theme_void() +
      theme(
        text = element_text(family = "Calibri", color = "gray21", size = 10)
        , legend.position = "right"
        , legend.text = element_text(family = "Calibri", color = "gray21", size = 10)
        , legend.title = element_text(family = "Calibri", color = "gray21", size = 10)
        , strip.text.x = element_text(family = "Calibri", color = "gray21", size = 10)
      ) +
      scale_fill +
      guides(
        fill = guide_colourbar(
          title = title
          , title.position = 'top'
          , title.hjust = 0
          , ticks.colour = 'gray35'
          , ticks.linewidth = 1
          , barwidth = .8
          , barheight = 8
        ))
  }

map_hotpixel <- plot_index_map(index_hotpixel, "perc_hotpixel", "Hot pixel\nper ha", "F")
map_hotpixel_p90 <- plot_index_map(index_hotpixel, "perc_hotpixel_p90", "Hot pixel\nper ha\n(perc 90)", "F")
map_hotpixel_p95 <- plot_index_map(index_hotpixel, "perc_hotpixel_p95", "Hot pixel\nper ha\n(perc 95)", "F")

map_cdd <- plot_index_map(index_cdd, "mean", "CDD\n(mean)", "E")
map_r20mm <- plot_index_map(index_r20mm, "mean", "R20mm\n(mean)", "G")
map_r95p <- plot_index_map(index_r95p, "mean", "R95p\n(mean)", "G")
map_rx1day <- plot_index_map(index_rx1day, "mean", "Rx1day\n(mean)", "G")
map_rx5day <- plot_index_map(index_rx5day, "mean", "Rx5day\n(mean)", "G")

map_population <- plot_index_map(gridded_population_longer, "population", "Population", "acton")
map_altitude <- plot_index_map(elevation %>% dplyr::filter(index == "Elevation"), "mean", "", "ggthemes::Red-Green-White Diverging", type = "elevation")
map_slope <- plot_index_map(elevation %>% dplyr::filter(index == "Slope"), "mean", "", "ggthemes::Temperature Diverging", type = "elevation")

# map_elevation <- map_altitude + map_slope + patchwork::plot_layout(nrow = 1)

ggsave("R/output/hotpixel_municipality.jpg", map_hotpixel, width = 8, height = 4.6, units = "in", dpi = 600)
ggsave("R/output/hotpixelp90_municipality.jpg", map_hotpixel_p90, width = 8, height = 4.6, units = "in", dpi = 600)
ggsave("R/output/hotpixelp95_municipality.jpg", map_hotpixel_p95, width = 8, height = 4.6, units = "in", dpi = 600)

ggsave("R/output/cdd_municipality.jpg", map_cdd, width = 8, height = 4.6, units = "in", dpi = 600)
ggsave("R/output/r20mm_municipality.jpg", map_r20mm, width = 8, height = 4.6, units = "in", dpi = 600)
ggsave("R/output/r95p_municipality.jpg", map_r95p, width = 8, height = 4.6, units = "in", dpi = 600)
ggsave("R/output/rx1day_municipality.jpg", map_rx1day, width = 8, height = 4.6, units = "in", dpi = 600)
ggsave("R/output/rx5day_municipality.jpg", map_rx5day, width = 8, height = 4.6, units = "in", dpi = 600)

ggsave("R/output/population_municipality.jpg", map_population, width = 8, height = 4.6, units = "in", dpi = 600)
# ggsave("R/output/elevation_municipality.jpg", map_elevation, width = 4, height = 1.53, units = "in", dpi = 600)


# + NUMBER --------------------------------------------------------------------

top20_mun <-
  reports %>%
  dplyr::group_by(disaster, country, geo_id, name_muni, geometry) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
  dplyr::arrange(desc(n_report)) %>%
  dplyr::slice_head(n = 20) %>%
  dplyr::select(-geometry) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(
    gridded_population %>% sf::st_drop_geometry() %>% dplyr::select(country, geo_id, pop_mean)
    , by = c("country", "geo_id"), keep = NULL
  ) %>%
  dplyr::mutate(
    event_pop = n_report / pop_mean
  )

writexl::write_xlsx(top20_mun, "R/output/top20_mun_events.xlsx")

top10_mun <-
  reports %>%
  dplyr::group_by(country, geo_id, name_muni, geometry) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
  dplyr::arrange(desc(n_report)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::select(-geometry)

writexl::write_xlsx(top10_mun, "R/output/top10_mun_total_events.xlsx")
writexl::write_xlsx(
  top_mun_gt50 %>% sf::st_drop_geometry() %>% dplyr::arrange(desc(n_report))
  , "R/output/top_mun_gt50_total_events.xlsx"
)


# Total reported events
main_disaster %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(disaster) %>%
  dplyr::summarise(n_report = sum(n_report)) %>%
  dplyr::arrange(-n_report)
# 1 Floods         4233
# 2 Landslides     3089
# 3 Storms         2607
# 4 Wildfires      2016
# 5 Droughts        596

sum(main_disaster$n_report) #12541

# Total number of municipalities per category
table(disaster_agg$interval)
# (150,378] (100,150]  (50,100]   (10,50]    (5,10]     (0,5]
# 9         8        24       243       241       341

# + CHART -----------------------------------------------------------------

main_disaster_year_country <-
  reports %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(disaster, year, country) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop")


# _ event x country ------------------------------------------------------------------

main_disaster_year_country %>%
  dplyr::mutate(Disaster = disaster, Country = country) %>%
  ggplot(aes(x = as.numeric(year), y = n_report, color = Country, group = Country)) +
  geom_line() +
  geom_point(size = 1.5) +
  facet_wrap(~Disaster, nrow = 2, scales = "free_y") +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2013, 2023, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022, "")) +
  labs(y = "Number of events", x = "Year") +
  theme_bw() +
  theme(
    legend.position = c(.74, .3)
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 10)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 10)
    , legend.key.width = unit(.4, "cm")
    , legend.key.height = unit(.4, "cm")
    , legend.key.size = unit(1, 'lines')
    , legend.margin = margin(-.65, 0, 0, 0, unit = "cm")
    , strip.background = element_rect(fill = "white")
    , plot.margin = unit(c(2, 1, 2, 1), "mm")
    , title = element_text(family = "Calibri", color = "gray21", size = 9)
    , axis.title = element_text(family = "Calibri", color = "gray21", size = 11, hjust = 0)
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , panel.grid.major.x = element_blank()
  )

ggsave("R/output/reports_per_year_event.jpg", width = 9.8, height = 5.67, units = "in", dpi = 600)


# _ event x people ---------------------------------------------------------

event_impact <-
  human_impact_all %>%
  dplyr::group_by(year, country) %>%
  dplyr::summarise(people_impacted = sum(total, na.rm = TRUE), .groups = "drop") %>%
  dplyr::full_join(
    main_disaster_year_country %>%
      dplyr::group_by(year, country) %>%
      dplyr::summarise(events = sum(n_report, na.rm = TRUE), .groups = "drop")
    , by = c("year", "country")
  ) %>%
  dplyr::mutate(
    factor = case_when(
      country == "Bolivia" ~ 1
      , country == "Brazil" ~ 5000
      , country == "Colombia" ~ 2.5
      , country == "Ecuador" ~ 40
      , country == "Peru" ~ 500
    )
    , limit_max = case_when(
      country == "Bolivia" ~ 400
      , country == "Brazil" ~ 400
      , country == "Colombia" ~ 600
      , country == "Ecuador" ~ 1000
      , country == "Peru" ~ 400
    )
    , break_by = if_else(country == "Ecuador", 250, 100)
    , year = as.numeric(year)
  )


text_event <- tribble(~year, ~country, ~events, 2018, "Bolivia", 155)
text_impact <- tribble(~year, ~country, ~people_impacted, 2021.5, "Bolivia", 57) # 45

event_impact %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = people_impacted / factor), fill = "#D4AF37", alpha = 0.7) + #color = "#D4AF37",
  geom_line(aes(y = events, group = 1), color = "#DD513AFF") +
  geom_point(aes(y = events), color = "#DD513AFF", size = 3) +
  geom_text(data = text_event, aes(y = events), label = "Weather-related\nreports", colour = "#DD513AFF", fontface = "bold", lineheight = .8) +
  geom_text(data = text_impact, aes(y = people_impacted), label = "People\nimpacted", color = "#D4AF37", fontface = "bold", lineheight = .8) +
  facet_wrap(~ country, nrow = 2, scales = "free_y") +
  ggh4x::facetted_pos_scales(
    y = list(
      country == "Bolivia" ~ scale_y_continuous(
        limits = c(0, 300)
        , sec.axis = sec_axis(~ . * 1, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
      , country == "Brazil" ~ scale_y_continuous(
        limits = c(0, 700)
        , sec.axis = sec_axis(~ . * 5000, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
      , country == "Colombia" ~ scale_y_continuous(
        limits = c(0, 400)
       , sec.axis = sec_axis(~ . * 2.5, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
      , country == "Ecuador" ~ scale_y_continuous(
        limits = c(0, 900)
        , sec.axis = sec_axis(~ . * 40, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
      , country == "Peru" ~ scale_y_continuous(
        sec.axis = sec_axis(~ . * 500, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
    )
  ) +
  scale_x_continuous(
    breaks = seq(2013, 2023, 1)
    , limits = c(2012.5, 2023.5)
    , labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022, "")
  ) +
  labs(x = "Year") +
  theme_bw() +
  theme(
    text = element_text(family = "Calibri", color = "gray21", size = 12)
    , title = element_text(family = "Calibri", size = 12)
    , axis.text.x = element_text(colour = "gray21", family = "Calibri", size = 12)
    , axis.title.x = element_text(colour = "gray21", family = "Calibri", size = 12)
    , axis.text.y = element_text(colour = "#DD513AFF", family = "Calibri", size = 12)
    , axis.text.y.right = element_text(color = "#D4AF37", family = "Calibri", size = 12)
    , axis.title.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , axis.line.y = element_blank()
    , plot.margin = margin(.5, .2, .2, .2, unit = "cm")
    , plot.title = element_text(hjust = 0.5)
    , strip.background = element_rect(fill = "white")
    , strip.text = element_text(family = "Calibri", color = "gray21", size = 12)
    , panel.grid.minor.y = element_blank()
    , panel.grid.major.x = element_blank()
  )

ggsave("R/output/events_people_impacted_facet_col.jpg", width = 10.6, height = 6.46, units = "in", dpi = 600)


# __ by country -----------------------------------------------------------

human_impact_all %>%
  ggplot(aes(x = as.numeric(year), y = total, color = variable_en)) + #, shape = variable_en
  geom_line() + #, color = "gray21", linewidth = .1
  geom_point(size = 1) + #, color = "gray21", linewidth = .1
  facet_grid(country~disaster, scales = "free_y") + #
  scale_y_log10(labels = scales::label_log(digits = 2)) +
  scale_x_continuous(breaks = seq(2013, 2023, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022, "")) +
  labs(y = "Number of people impacted", x = "Year", color = "") + #, shape = "People impacted: "
  scale_color_manual(values = c("#FFDF91", "#D4AF37", "#C6782CFF", "#803C15FF", "#9F2519FF", "#000000FF")) +
  theme_bw() +
  theme(
    text = element_text(family = "Calibri", color = "gray21", size = 12)
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 11)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 11)
    , axis.title = element_text(family = "Calibri", color = "gray21", size = 11, hjust = 0)
    , legend.position = "bottom"
    , legend.key.width = unit(.4, "cm")
    , legend.key.height = unit(.4, "cm")
    , panel.grid.minor.x = element_blank()
    , plot.margin = unit(c(.5, 1, .5, .5), "lines")
    , axis.line.x.bottom = element_line(color = "gray21")
    , axis.line.y.left = element_line(color = "gray21")
    , strip.background = element_rect(fill = "white")
    , panel.grid.minor.y = element_blank()
    , panel.grid.major.x = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1))

ggsave("R/output/population_impact_country.jpg", width = 9.8, height = 5.67, units = "in", dpi = 600)

infra_impact_all_country <-
  infrastructure_impact_all %>%
  dplyr::group_by(variable_en, year, country) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(variable_en) %>%
  dplyr::mutate(
    total_period = sum(total)
    , log10_total = log10(total)
    , label_unit = case_when(
      total_period < 10^3 ~ paste0(total_period)
      , total_period < 10^6 & total_period > 10^3 ~ paste0(round(total_period/10^3, 0), "k")
      , total_period > 10^6 ~  paste0(round(total_period/10^6, 2), "m")
    )
  )

factor <-
  infra_impact_all_country %>%
  dplyr::group_by(variable_en) %>%
  dplyr::summarise(total = sum(total), .groups = "drop") %>%
  arrange(total)

infra_impact_all_country$variable_en <- factor(infra_impact_all_country$variable_en, levels = factor %>% pull(variable_en))


infra_impact_all_country %>%
  ggplot(aes(x = as.numeric(year), y = total, color = variable_en)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~country) +
  scale_y_log10(labels = scales::label_log(digits = 2)) +
  scale_x_continuous(breaks = seq(2013, 2023, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022, "")) +
  labs(y = "Number of infrastructure impacted", x = "Year", color = "Type infrastructure") +
  scale_color_manual(values = c("#D4AF37", "#C6782CFF", "#803C15FF", "#9F2519FF", "#000000FF")) +
  theme_bw() +
  theme(
    text = element_text(family = "Calibri", color = "gray21", size = 12)
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 11)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 11)
    , axis.title = element_text(family = "Calibri", color = "gray21", size = 11, hjust = 0)
    , legend.position = c(.8, .3)
    , legend.margin = margin(-1, 0, 0, 0, unit = "cm")
    , legend.key.width = unit(.4, "cm")
    , legend.key.height = unit(.4, "cm")
    , panel.grid.minor.x = element_blank()
    , plot.margin = unit(c(.5, 1, .5, .5), "lines")
    , axis.line.x.bottom = element_line(color = "gray21")
    , axis.line.y.left = element_line(color = "gray21")
    , strip.background = element_rect(fill = "white")
    , panel.grid.minor.y = element_blank()
    , panel.grid.major.x = element_blank()
  )

ggsave("R/output/infrastructure_impact_country.jpg", width = 9.8, height = 5.67, units = "in", dpi = 600)


# _ event x pop ----------------------------------------------------

main_disaster %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(
    gridded_population %>% sf::st_drop_geometry() %>% dplyr::select(country, geo_id, pop_mean)
    , by = c("country", "geo_id"), keep = NULL
  ) %>%
  dplyr::mutate(
    event_pop = n_report / pop_mean
  ) %>%
  ggplot(aes(x = pop_mean, y = n_report)) +
  geom_hex(bins = 15) +
  facet_grid(disaster ~ ., scales = "free_y") +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_bw(base_size = 14) +
  labs(x = "Population", y = "Total of events") +
  scale_x_log10(labels = scales::label_log(digits = 2)) +
  theme(
    text = element_text(family = "Calibri", color = "gray21", size = 11)
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 11)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 11)
    , axis.title = element_text(family = "Calibri", color = "gray21", size = 11, hjust = 0)
    ) +
  geom_smooth(se = FALSE, color = "gray21")

ggsave("R/output/event_pop.jpg", width = 3.08, height = 6.07, units = "in", dpi = 600)


#--- FOOTER --------------------------------------------------------------------
save.image("R/model/workspace_climate_reports.RData")
#rm(list = ls())
