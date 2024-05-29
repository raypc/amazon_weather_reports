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
#
# Extract code
source("R/model/layouts.R")
# load("R/model/workspace_amazon_climate_reports.RData")
#
#
# + ADMINISTRATIVE BOUNDARY -----------------------------------------------

south_america <-
  sf::read_sf("R/input/_geo/south_america/South_America.shp") %>%
  sf::st_transform(4674)

amazonia <-
  sf::read_sf("R/input/_geo/amazonia/amazonia_boundary.shp") %>%
  sf::st_transform(4674)


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
  sf::read_sf("R/input/bolivia/RECURRENCIA 2002-2022/Shapefile/RECURRENCIA EPSG 4326.shp") %>%
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

municipios_amazonia_bioma <-
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
  dplyr::mutate(total_area = sf::st_area(.)) %>%
  sf::st_intersection(amazonia %>% select(geometry)) %>%
  dplyr::mutate(
    amazon_area = sf::st_area(.)
    , perc_amazon_area = amazon_area/total_area
    , perc_amazon_area = units::drop_units(perc_amazon_area)
  ) %>%
  dplyr::filter(perc_amazon_area >= 0.5)


write_sf(municipios_amazonia_bioma, "R/input/_geo/amazonia_municipalities_50p.gpkg")

windows()
plot(st_geometry(municipios_amazonia_bioma), border = "gray")
plot(st_geometry(amazonia), add = TRUE, border = "green")


others <-
  rbind(municipios_fg, municipios_su, municipios_gu, municipios_ve) %>%
  sf::st_intersection(amazonia)


# + CLIMATE DISASTER REPORTS ----------------------------------------------

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

## Data wrangling: tidying and transfrom
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
    year %in% as.character(2013:2022)
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
  dplyr::group_by(disaster, year, uf, municipio, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = uf, municipality = municipio)


# _ colombia -------------------------------------------------------------

## these files needed to be crated by hand on account of pwd protection
list_files <- list.files("R/input/colombia", "*.xlsx$")
readxl::excel_sheets(paste0("R/input/colombia/", list_files[1]))

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

  header <-
    table_i %>%
    dplyr::slice(1:2) %>%
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
        , "INUNDACION"
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
    year %in% 2013:2022
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
  dplyr::group_by(disaster, year, descripcion_y_ubicacion_departamento, descripcion_y_ubicacion_municipio, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = descripcion_y_ubicacion_departamento, municipality = descripcion_y_ubicacion_municipio)


# _ bolivia --------------------------------------------------------------

readxl::excel_sheets("R/input/bolivia/BD_2002_2022_BD_2023.xlsx")

bo_report <-
  readxl::read_xlsx(
    "R/input/bolivia/BD_2002_2022_BD_2023.xlsx"
    , sheet = "BD 2002 2022"
    , col_names = TRUE
    , guess_max = 15000
  )

geo_id_bo <- municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::filter(country == "Bolivia") %>% dplyr::pull(geo_id)
disaster_amazonia_bioma_bo <-
  bo_report %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    geo_id = if_else(nchar(cod_ine_municipal) == 5, paste0("0", cod_ine_municipal), cod_ine_municipal)
    , year = as.character(ano)
    , disaster = case_when(
      tipo_de_evento %in% c(
        "Deslizamiento"
        , "Derrumbe"
        , "DESLIZAMIENTO"
        , "RIADA Y DESLIZAMIENTO"
      ) ~ "Landslides"
      , tipo_de_evento %in% c(
        "Riada"
        , "Inundación"
        , "Desborde"
        , "inundación"
        , "DESBORDE"
        , "INUNDACIÓN"
        , "RIADA"
        , "INUNDACIÓN Y RIADA"
        , "INUNDACIÓN, RIADA Y DESLIZAMIENTO"
      ) ~ "Floods"
      , tipo_de_evento %in% c(
        "Lluvias"
        , "Tormenta Eléctrica"
        , "TORMENTA"
      ) ~ "Storms"
      , tipo_de_evento %in% c(
        "Sequia Hidrológica"
        , "Sequía"
        , "SEQUIA"
        , "SEQUIA-HELADA"
        , "SEQUI-INCENDIOS"
        , "SEQUIA-INCENDIO"
        , "SEQUIA-INCENDIOS"
        , "SEQUIA E INCENDIO"
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
    year %in% 2013:2022
    & disaster != "NA"
    & geo_id %in% geo_id_bo
  )


table(disaster_amazonia_bioma_bo$disaster)
table(disaster_amazonia_bioma_bo$year)

# grouping events per municipalities
disaster_amz_freq_mun_bo <-
  disaster_amazonia_bioma_bo %>%
  dplyr::group_by(disaster, year, departamento, municipio, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = departamento, municipality = municipio)


# _ ecuador ---------------------------------------------------------------

readxl::excel_sheets("R/input/ecuador/Base_Eventos_2010_2024_eventos_catálogo.xlsx")

ec_report <- readxl::read_xlsx(
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
        , "Aluvión"
        , "ALUVIÓN"
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
    year %in% as.character(2013:2022)
    & disaster != "NA"
    & geo_id %in% geo_id_ec
  )

table(disaster_amazonia_bioma_ec$disaster)
table(disaster_amazonia_bioma_ec$year)

# grouping events per municipalities
disaster_amz_freq_mun_ec <-
  disaster_amazonia_bioma_ec %>%
  dplyr::group_by(disaster, year, provincia, canton, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = provincia, municipality = canton)


# _ peru ------------------------------------------------------------------

geo_id_pe <- municipios_amazonia_bioma %>% sf::st_drop_geometry() %>% dplyr::filter(country == "Peru") %>% dplyr::pull(geo_id)
pe_report <-
  readxl:::read_xlsx("R/input/peru/BSINPAD_ 2013-2018.xlsx", sheet = "BD EVENTOS EN AMAZONIA", skip = 3, guess_max = 2700) %>%
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
        ide_sinpad = as.character(ide_sinpad)
        , camirural_destru = as.character(camirural_destru)
        , scamirural_afecta = as.character(scamirural_afecta)
      ) %>%
      dplyr::select(
        "codigo_de_emergencia_sinpad" = "ide_sinpad"
        , "fecha_de_la_emer" = "fecha"
        , "ano" = "anos"
        , "mes"
        , "cod_distrito" = "cod_ubigeo"
        , "dpto" = "dep"
        , "prov"
        , "dist"
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
  )

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
    year %in% as.character(2013:2022)
    & disaster != "NA"
    & geo_id %in% geo_id_pe
  )

table(disaster_amazonia_bioma_pe$disaster)
table(disaster_amazonia_bioma_pe$year)
length(unique(disaster_amazonia_bioma_pe$cod_distrito))

# grouping events per municipalities
disaster_amz_freq_mun_pe <-
  disaster_amazonia_bioma_pe %>%
  dplyr::group_by(disaster, year, dpto, dist, geo_id) %>%
  dplyr::summarise(n_report = n(), .groups = "drop") %>%
  dplyr::rename(state = dpto, municipality = dist)



# __ all -------------------------------------------------------------------

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
    , disaster_amz_freq_mun_pe %>% mutate(country = "Peru")
  ) %>%
  dplyr::left_join(
    municipios_amazonia_bioma %>% select(geo_id, geometry)
      , by = c("geo_id" = "geo_id")
  )

sum(reports %>% dplyr::filter(country == "Bolivia" & disaster == "Wildfires") %>% dplyr::pull(n_report))
table(reports$disaster)

main_disaster <-
  reports %>%
  dplyr::group_by(disaster, geometry) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
  sf::st_set_geometry("geometry")

glimpse(main_disaster)
summary(main_disaster)
class(main_disaster)


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
  dplyr::group_by(variable_en, year) %>%
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
  dplyr::group_by(variable_en, year) %>%
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
  dplyr::group_by(variable_en, year) %>%
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
  dplyr::group_by(variable_en, year) %>%
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
  dplyr::group_by(variable_en, year) %>%
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

#
infrastructure_impact_pe <-
  disaster_amazonia_bioma_pe %>%
  dplyr::select(year, disaster
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


# + REMOTE SENSING --------------------------------------------------------


# _ reproject -------------------------------------------------------------

proj_albers_sirgas <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

raster_total_precipitation <-
  terra::rast("R/input/_remote_sensing/precipitation_anomalies_mean_plus_2std_clip.tif") %>%
  terra::project(proj_albers_sirgas, method = 'near')

raster_days_without_precipitation <-
  terra::rast("R/input/_remote_sensing/drought_anomalies_mean_plus_2std_clip.tif") %>%
  terra::project(proj_albers_sirgas, method = 'near')

raster_day_land_surface_temperature <-
  terra::rast("R/input/_remote_sensing/temperature_anamolies_mean_plus_2std_clip.tif") %>%
  terra::project(proj_albers_sirgas, method = 'near')


# _ zonal -----------------------------------------------------------------

total_precipitation <-
  sf::read_sf("R/input/_remote_sensing/zonal.gpkg", layer = "total_precipitation_year") %>%
  dplyr::mutate(
    variable_area = (`_count` * (res(raster_total_precipitation)[1] * res(raster_total_precipitation)[2]))
    , variable_proportion = variable_area/amazon_area
    , interval = cut(
      variable_proportion
      , breaks = c(0, .2, .4, .6, .8, max(variable_proportion) + .1)
      , labels = c("0 - 20", "20 - 40", "40 - 60", "60 - 80", "80 - 100")
      , right = FALSE
    )
  ) %>%
  st_transform(4674)


days_without_precipitation <-
  sf::read_sf("R/input/_remote_sensing/zonal.gpkg", layer = "days_without_precipitation_year") %>%
  mutate(
    variable_area_ha = (`_count` * (res(raster_days_without_precipitation)[1] * res(raster_days_without_precipitation)[2]))
    , variable_proportion = variable_area_ha/amazon_area
    , interval = cut(
      variable_proportion
      , breaks = c(0, .2, .4, .6, .8, max(variable_proportion) + .1)
      , labels = c("0 - 20", "20 - 40", "40 - 60", "60 - 80", "80 - 100")
      , right = FALSE
    )
  ) %>%
  st_transform(4674)


day_land_surface_temperature <-
  sf::read_sf("R/input/_remote_sensing/zonal.gpkg", layer = "temperature_anomalies") %>%
  mutate(
    `_mean` = if_else(is.na(`_mean`), 0, `_mean`)
    , interval = cut(
      `_mean`,
      breaks = c(0, 5, 10, 15, 20, max(`_mean`) + .1),
      labels = c("0 - 5", "5 - 10", "10 - 15", "15 - 20", "20 - 31"),
      right = FALSE
    )
  ) %>%
  st_transform(4674)


write_sf(total_precipitation, "R/input/_remote_sensing/climate_anomalies.gpkg", layer = "total_precipitation")
write_sf(days_without_precipitation, "R/input/_remote_sensing/climate_anomalies.gpkg", layer = "days_without_precipitation")
write_sf(day_land_surface_temperature, "R/input/_remote_sensing/climate_anomalies.gpkg", layer = "day_land_surface_temperature")

# day_land_surface_temperature <-
#   day_land_surface_temperature %>%
#   dplyr::mutate(
#     order = case_when(
#       interval == "0" ~ 1
#       , interval == "0 - 5" ~ 2
#       , interval == "5 - 10" ~ 3
#       , interval == "10 - 15" ~ 4
#       , interval == "15 - 20" ~ 5
#       , interval == "20 - 31" ~ 6
#     )
#   )
#
# day_land_surface_temperature$interval <-
#   with(day_land_surface_temperature, reorder(interval, order))


# + MAP -------------------------------------------------------------------
windows()

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


# _ 1 figure ------------------------------------------------------------

map_total_report <-
  map_layout_disaster_report_total(
    data = main_disaster %>%
      dplyr::group_by(geometry) %>%
      dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
      dplyr::mutate(
        interval = cut(
          n_report
          , breaks = c(0, 5, 10, 50, 100, 150, max(n_report) + 1)
          , labels = c("≤ 5", "5 - 10", "10 - 50", "50 - 100", "100 - 150", paste0("150 - ", max(n_report)))
          , right = TRUE
        )
      )
    , variable = "interval"
    , legend_title = "Events"
    , title = "a) Climate disaster reports\n"
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


map_total_precipitation <-
  map_layout_anomalies(
    data = total_precipitation
    , variable = "interval"
    , legend_title = "%"
    , color = c("#bdd7e7", "#6baed6", "#225ea8", "#081d58", "#000000FF")
    , title = "b) Precipitation index\n"
  )

map_days_without_precipitation <-
  map_layout_anomalies(
    data = days_without_precipitation
    , variable = "interval"
    , legend_title = "%"
    , color = c("#FCFFA4FF", "#FCA50AFF", "#DD513AFF", "#932667FF", "#49006a")
    , title = "c) Drought index\n"
  )

map_day_land_surface_temperature <-
  map_layout_anomalies(
    data = day_land_surface_temperature
    , variable = "interval"
    , legend_title = "Days" #
    , color = c("#FBF2B5FF", "#FFC87AFF", "#F65620FF", "#9F2519FF", "#800026")
    , title = "d) Temperature index\n"
  )

g <-
  grid.arrange(
    arrangeGrob(map_total_report, ncol = 1)
    , arrangeGrob(map_total_precipitation, map_days_without_precipitation, map_day_land_surface_temperature, ncol = 3)  # Three small maps in a column on the right
    , ncol = 1, heights = c(2.5, 1)
  )
ggsave("R/output/total_climate_reports.jpg", g, width = 18.4, height = 14, units = "cm")


# __ n --------------------------------------------------------------------

# Total reported events
main_disaster %>% st_drop_geometry() %>% dplyr::group_by(disaster) %>% dplyr::summarise(n_report = sum(n_report)) %>% arrange(-n_report)
# disaster   n_report
# Floods         3908
# Landslides     2778
# Storms         2143
# Wildfires      1431
# Droughts        274


# Total number of municipalities per category
total_class <-
  main_disaster %>%
  dplyr::group_by(geometry) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop") %>%
  dplyr::mutate(
    interval = cut(
      n_report
      , breaks = c(0, 5, 10, 50, 100, 150, max(n_report) + 1)
      , labels = c("≤ 5", "5 - 10", "10 - 50", "50 - 100", "100 - 150", paste0("150 - ", max(n_report)))
      , right = TRUE
    )
  )


table(total_class$interval)
# ≤ 5    5 - 10   10 - 50  50 - 100 100 - 150 150 - 367
# 368       213       195        20         8         6


# _ SM reports -------------------------------------------------------------

map_storms <-
  map_layout_disaster_report_event_class(
    data = main_disaster %>%
      filter(disaster == "Storms") %>%
      dplyr::mutate(
        interval = cut(
          n_report
          , breaks = c(0, 1, 5, 10, 50, 100, max(n_report))
          , right = TRUE
        ))
    , variable = "interval"
    , legend_title = "Events"
    , palette = c("#9ecae1", "#87CEFA", "#1E90FF", "#08519c", "#081d58", "#0B0405E6")
    , direction = 1
    , title = "a) Storm reports\n"
  )

map_floods <-
  map_layout_disaster_report_event_class(
    data = main_disaster %>%
      filter(disaster == "Floods") %>%
      dplyr::mutate(
        interval = cut(
          n_report
          , breaks = c(0, 1, 5, 10, 50, 100, max(n_report))
          , right = TRUE
        ))
    , variable = "interval"
    , legend_title = "Events"
    , palette = "G"
    , title = "b) Flood reports\n"
  )

map_landslides <-
  map_layout_disaster_report_event_class(
    data = main_disaster %>%
      filter(disaster == "Landslides") %>%
      dplyr::mutate(
        interval = cut(
          n_report
          , breaks = c(0, 1, 5, 10, 50, 100, max(n_report))
          , right = TRUE
        ))
    , variable = "interval"
    , legend_title = "Events"
    , palette = "rcartocolor::Sunset"
    , direction = 1
    , title = "c) Landslide reports\n"
  )

map_droughts <-
  map_layout_disaster_report_event_class(
    data = main_disaster %>%
      filter(disaster == "Droughts") %>%
      dplyr::mutate(
        interval = cut(
          n_report
          , breaks = c(0, 1, max(n_report))
          , right = TRUE
        ))
    , variable = "interval"
    , legend_title = "Events"
    , palette = c("#ffffba", "#ffdfba") # "fishualize::Ostracion_cubicus"
    , direction = 1
    , title = "d) Drought reports\n"
  )

map_wildfires <-
  map_layout_disaster_report_event_class(
    data = main_disaster %>%
      filter(disaster == "Wildfires") %>%
      dplyr::mutate(
        interval = cut(
          n_report
          , breaks = c(0, 1, 5, 10, 50, 100, max(n_report))
          , right = TRUE
        ))
    , variable = "interval"
    , legend_title = "Events"
    , palette = c("#F7C9AAFF", "#fc9272", "#ef3b2c", "#a50f15", "#67000d", "#3c0404")
    , direction = 1
    , title = "d) Wildfire reports\n"
  )

# windows()
g <- grid.arrange(map_storms, map_floods, map_landslides, map_droughts, map_wildfires, nrow = 3)
ggsave("R/output/extreme_events.jpg", g, width = 6.92, height = 6.8, units = "in")



# + CHART -----------------------------------------------------------------


# _ 2 figure ------------------------------------------------------------------

# __ others ---------------------------------------------------------------


main_disaster_year <-
  reports %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(disaster, year) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop")

main_disaster_year %>%
  dplyr::mutate(Disaster = disaster) %>%
  ggplot(aes(x = year, y = n_report, color = Disaster, group = Disaster)) +
  geom_line() +
  geom_point(aes(shape = Disaster)) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dotted", linewidth = 0.5) +
  scale_color_manual(values = c("#ffd92f", "#9ecae1", "#a6761d", "#0c2c84", "#e41a1c")) +
  labs(y = "Number of events", x = "Year") +
  theme_bw() +
  theme(
    legend.position = "right"
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 10)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 10)
    , legend.key.width = unit(.4, "cm")
    , legend.key.height = unit(.4, "cm")
    , legend.key.size = unit(1, 'lines')
    , legend.margin = margin(-.65, 0, 0, 0, unit = "cm")
    , plot.margin = unit(c(2, 1, 2, 1), "mm")
    , title = element_text(family = "Calibri", color = "gray21", size = 9)
    , axis.title = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 11
      , hjust = .05
    )
  )

ggsave("R/output/reports_per_year.jpg", width = 9.8, height = 5.67, units = "in")


main_disaster_year_country <-
  reports %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(disaster, year, country) %>%
  dplyr::summarise(n_report = sum(n_report), .groups = "drop")

main_disaster_year_country %>% mutate(Disaster = disaster) %>%
  ggplot(aes(x = as.numeric(year), y = n_report, color = Disaster, group = Disaster)) +
  geom_line() + #linewidth = .8
  geom_point(aes(shape = Disaster), size = 1.5) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dotted", show.legend = FALSE) + #, size = .8
  facet_wrap(~country, nrow = 2, scales = "free_y") +
  scale_color_manual(values = c("#ffd92f", "#9ecae1", "#a6761d", "#0c2c84", "#e41a1c")) +
  scale_x_continuous(breaks = seq(2013, 2022, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022)) +
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
    , panel.grid.minor.x = element_blank()
    , plot.margin = unit(c(2, 1, 2, 1), "mm")
    , title = element_text(family = "Calibri", color = "gray21", size = 9)
    , axis.title = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 11
      , hjust = 0
    )
  )

ggsave("R/output/reports_per_year_country.jpg", width = 9.8, height = 5.67, units = "in")

main_disaster_year_country %>% mutate(Disaster = disaster, Country = country) %>%
  ggplot(aes(x = as.numeric(year), y = n_report, color = Country, group = Country)) +
  geom_line() + #linewidth = .8
  geom_point(aes(shape = Country), size = 1.5) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dotted", show.legend = FALSE) + #, size = .8
  facet_wrap(~Disaster, nrow = 2, scales = "free_y") +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2013, 2022, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022)) +
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
    , axis.title = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 11
      , hjust = 0
    )
    , panel.grid.minor.x = element_blank()
  )

ggsave("R/output/reports_per_year_event.jpg", width = 9.8, height = 5.67, units = "in")



# __ event people ---------------------------------------------------------

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
      , country == "Brazil" ~ 10000
      , country == "Colombia" ~ 2.5
      , country == "Ecuador" ~ 40
      , country == "Peru" ~ 1000
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


text_event <- tribble(~year, ~country, ~events, 2015, "Brazil", 270)
text_impact <- tribble(~year, ~country, ~people_impacted, 2020, "Brazil", 210) # 45
text_correlation <-
  event_impact %>%
  group_by(country) %>%
  dplyr::summarise(
    correlation = round(cor(events, people_impacted, method = "pearson"), 2)
    , r_squared = round(correlation^2, 2)
  ) %>%
  mutate(
    year = case_when(
      country %in% c("Peru", "Bolivia") ~ 2018
      , country %in% c("Brazil", "Colombia") ~ 2015
      , country == "Ecuador" ~ 2021
    )
    , events = case_when(
      country == "Bolivia" ~ 250
      , country == "Brazil" ~ 350
      , country == "Colombia" ~ 450
      , country == "Ecuador" ~ 750
      , country == "Peru" ~ 350
    )
  )

event_impact %>%
  ggplot(aes(x = year)) +
  # geom_line(aes(y = people_impacted / factor, group = 1), color = "#D4AF37") +
  # geom_point(aes(y = people_impacted / factor), color = "#D4AF37", size = 3) +
  geom_col(aes(y = people_impacted / factor), fill = "#D4AF37", alpha = 0.7) + #color = "#D4AF37",
  geom_line(aes(y = events, group = 1), color = "#DD513AFF") +
  geom_point(aes(y = events), color = "#DD513AFF", size = 3) +
  geom_smooth(aes(y = events), color = "#DD513AFF", method = "lm", se = FALSE, linetype = "dashed", linewidth = .5) +
  geom_smooth(aes(y = people_impacted / factor), color = "#D4AF37", method = "lm", se = FALSE, linetype = "dashed", linewidth = .5) +
  geom_text(data = text_event, aes(y = events), label = "Climate reports", colour = "#DD513AFF", fontface = "bold") +
  geom_text(data = text_impact, aes(y = people_impacted), label = "People impacted", color = "#D4AF37", fontface = "bold") +
  # geom_text(data = text_correlation, aes(y = events, label = paste("Pearson r = ", correlation, "\nR² = ", r_squared)), colour = "gray21", size = 3) +
  facet_wrap(~ country, nrow = 2, scales = "free_y") +
  ggh4x::facetted_pos_scales(
    y = list(
      country == "Bolivia" ~ scale_y_continuous(
        limits = c(0, 300)
        , sec.axis = sec_axis(~ . * 1, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
      , country == "Brazil" ~ scale_y_continuous(
        limits = c(0, 350)
        , sec.axis = sec_axis(~ . * 10000, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
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
        sec.axis = sec_axis(~ . * 1000, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
    )
  ) +
  scale_x_continuous(
    breaks = seq(2013, 2022, 1)
    , limits = c(2012.5, 2022.5)
    , labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022)
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
  )

ggsave("R/output/events_people_impacted_facet_col.jpg", width = 10.6, height = 6.46, units = "in")


# __ people ---------------------------------------------------------------

human_impact_all_year <-
  human_impact_all %>%
  dplyr::group_by(variable_en, year) %>%
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
  human_impact_all_year %>%
  dplyr::group_by(variable_en) %>%
  dplyr::summarise(total = sum(total), .groups = "drop") %>%
  arrange(total)

human_impact_all_year$variable_en <- factor(human_impact_all_year$variable_en, levels = factor %>% pull(variable_en))


# __ infrastructure -------------------------------------------------------

infrastructure_impact_all_year <-
  infrastructure_impact_all %>%
  dplyr::group_by(variable_en, year) %>%
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
  infrastructure_impact_all_year %>%
  dplyr::group_by(variable_en) %>%
  dplyr::summarise(total = sum(total), .groups = "drop") %>%
  arrange(total)

infrastructure_impact_all_year$variable_en <- factor(infrastructure_impact_all_year$variable_en, levels = factor %>% pull(variable_en))


# >> by country -----------------------------------------------------------

human_impact_all_country <-
  human_impact_all %>%
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
  human_impact_all_country %>%
  dplyr::group_by(variable_en) %>%
  dplyr::summarise(total = sum(total), .groups = "drop") %>%
  arrange(total)

human_impact_all_country$variable_en <- factor(human_impact_all_country$variable_en, levels = factor %>% pull(variable_en))


human_impact_all_country %>%
  ggplot(aes(x = as.numeric(year), y = total, color = variable_en, shape = variable_en)) +
  geom_line() + #, color = "gray21", linewidth = .1
  geom_point(size = 1.5) + #, color = "gray21", linewidth = .1
  facet_wrap(~country) + #, scales = "free_y"
  scale_y_log10(labels = scales::label_log(digits = 2)) +
  scale_x_continuous(breaks = seq(2013, 2022, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022)) +
  labs(y = "Log10", x = "Year", color = "People impacted", shape = "People impacted") +
  scale_color_manual(values = c("#FFDF91", "#D4AF37", "#C6782CFF", "#803C15FF", "#9F2519FF", "#000000FF")) +
  theme_bw() +
  theme(
    text = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 12
    )
    , legend.text = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 9
    )
    , legend.title = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 9
    )
    , axis.title = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 11
      , hjust = 0
    )
    , legend.position = c(.74, .3)
    , legend.margin = margin(-1, 0, 0, 0, unit = "cm")
    , legend.key.width = unit(.4, "cm")
    , legend.key.height = unit(.4, "cm")
    , panel.grid.minor.x = element_blank()
    , plot.margin = unit(c(.5, 1, .5, .5), "lines")
    , axis.line.x.bottom = element_line(color = "gray21")
    , axis.line.y.left = element_line(color = "gray21")
    , strip.background = element_rect(fill = "white")
  )

ggsave("R/output/population_impact_country.jpg", width = 9.8, height = 5.67, units = "in")

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
  ggplot(aes(x = as.numeric(year), y = total, color = variable_en, shape = variable_en)) +
  geom_line() + #, color = "gray21", linewidth = .1
  geom_point(size = 1.5) + #, color = "gray21", linewidth = .1
  facet_wrap(~country) + #, scales = "free_y"
  scale_y_log10(labels = scales::label_log(digits = 2)) +
  scale_x_continuous(breaks = seq(2013, 2022, 1), labels = c("", 2014, "", 2016, "", 2018, "", 2020, "", 2022)) +
  labs(y = "Log10", x = "Year", color = "Infrastructure impacted", shape = "Infrastructure impacted") +
  scale_color_manual(values = c("#D4AF37", "#C6782CFF", "#803C15FF", "#9F2519FF", "#000000FF")) +
  theme_bw() +
  theme(
    text = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 12
    )
    , legend.text = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 9
    )
    , legend.title = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 9
    )
    , axis.title = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 11
      , hjust = 0
    )
    , legend.position = c(.74, .3)
    , legend.margin = margin(-1, 0, 0, 0, unit = "cm")
    , legend.key.width = unit(.4, "cm")
    , legend.key.height = unit(.4, "cm")
    , panel.grid.minor.x = element_blank()
    , plot.margin = unit(c(.5, 1, .5, .5), "lines")
    , axis.line.x.bottom = element_line(color = "gray21")
    , axis.line.y.left = element_line(color = "gray21")
    , strip.background = element_rect(fill = "white")
  )

ggsave("R/output/infrastructure_impact_country.jpg", width = 9.8, height = 5.67, units = "in")



# ++ 2023 -----------------------------------------------------------------

disaster_amazonia_bioma_br_2023 <-
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
    year %in% as.character(2023)
    & disaster != "NA"
    & geo_id %in% geo_id_br
  )

disaster_amazonia_bioma_bo_2023 <-
  bo_report %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    geo_id = if_else(nchar(cod_ine_municipal) == 5, paste0("0", cod_ine_municipal), cod_ine_municipal)
    , year = ano
    , disaster = case_when(
      tipo_de_evento %in% c(
        "Deslizamiento"
        , "Derrumbe"
        , "DESLIZAMIENTO"
        , "RIADA Y DESLIZAMIENTO"
      ) ~ "Landslides"
      , tipo_de_evento %in% c(
        "Riada"
        , "Inundación"
        , "Desborde"
        , "inundación"
        , "DESBORDE"
        , "INUNDACIÓN"
        , "RIADA"
        , "INUNDACIÓN Y RIADA"
        , "INUNDACIÓN, RIADA Y DESLIZAMIENTO"
      ) ~ "Floods"
      , tipo_de_evento %in% c(
        "Lluvias"
        , "Tormenta Eléctrica"
        , "TORMENTA"
      ) ~ "Storms"
      , tipo_de_evento %in% c(
        "Sequia Hidrológica"
        , "Sequía"
        , "SEQUIA"
        , "SEQUIA-HELADA"
        , "SEQUI-INCENDIOS"
        , "SEQUIA-INCENDIO"
        , "SEQUIA-INCENDIOS"
        , "SEQUIA E INCENDIO"
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
    year %in% 2023
    & disaster != "NA"
    & geo_id %in% geo_id_bo
  )

table(disaster_amazonia_bioma_br_2023$disaster)
table(disaster_amazonia_bioma_bo_2023$disaster)


disaster_amazonia_bioma_2023 <-
  rbind(
    disaster_amazonia_bioma_br_2023 %>% mutate(country = "Brazil") %>% select(country, disaster)
    , disaster_amazonia_bioma_bo_2023 %>% mutate(country = "Bolivia") %>% select(country, disaster)
  ) %>%
  dplyr::group_by(country, disaster) %>%
  dplyr::summarise(n = n(), .groups = "drop")

ggplot(disaster_amazonia_bioma_2023) +
  geom_col(aes(x = n, y = reorder(disaster, n, sum), fill = country)) +
  scale_fill_manual(values = c("#e6c79c", "#cbbeb5")) +
  labs(x = "Number of events") +
  theme_minimal() +
  theme(
    legend.position = "right"
    , legend.text = element_text(family = "Calibri", color = "gray21", size = 10)
    , legend.title = element_text(family = "Calibri", color = "gray21", size = 10)
    , legend.key.width = unit(.4, "cm")
    , legend.key.height = unit(.4, "cm")
    , legend.key.size = unit(1, 'lines')
    , legend.margin = margin(-.65, 0, 0, 0, unit = "cm")
    , plot.margin = unit(c(2, 1, 2, 1), "mm")
    , title = element_text(family = "Calibri", color = "gray21", size = 9)
    , axis.title.y = element_blank()
    , axis.title.x = element_text(
      family = "Calibri"
      , color = "gray21"
      , size = 11
      , hjust = .05
    )
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal"
      , ncol = 1
      , label.position = "right"
      , title = "Country"
      , title.position = "top"
      , title.hjust = 0
      , order = 1
    )
    , shape = guide_legend(order = 2, "")
    , color = guide_legend(order = 3, "")
  )

ggsave("R/output/reports_2023.jpg", width = 5.77, height = 2.67, units = "in")


#--- FOOTER --------------------------------------------------------------------
save.image("R/model/workspace_amazon_climate_reports.RData")
#rm(list = ls())
