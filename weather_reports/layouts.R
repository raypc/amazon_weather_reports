map_layout_disaster_report_total <-
  function(data, variable = NULL, legend_title, title = NULL, class, color = NULL){
    ggplot(data = data) +
      geom_sf(data = municipios_amazonia_bioma, aes(shape = "NE"), fill = "gray90", color = "gray50", linewidth = .05) +
      geom_sf(aes(fill = .data[[variable]]), alpha = .75, color = "gray50", linewidth = .05) +
      geom_sf(data = others, fill = "gray70", aes(color = "NA"), linewidth = .1) +
      geom_sf(data = south_america, fill = NA, color = "gray35", linewidth = .3) +
      geom_sf(data = amazonia, fill = NA, color = "#7fbc41", linewidth = .4) +
      geom_sf_text(
        data = south_america %>% dplyr::filter(COUNTRY == "Guyana")
        , aes(label = iso_code)
        , size = 3
        , colour = "gray10"
        , nudge_y = 4
        , nudge_x = 1.5
      ) +
      geom_sf_text(
        data = south_america %>% dplyr::filter(COUNTRY == "Ecuador")
        , aes(label = iso_code)
        , size = 3
        , colour = "gray10"
        , nudge_y = 1
      ) +
      geom_sf_text(
        data = south_america %>% dplyr::filter(COUNTRY %in% c("Colombia", "Venezuela"))
        , aes(label = iso_code)
        , size = 3
        , colour = "gray10"
        , nudge_y = -1.5
        , nudge_x = 1
      ) +
      geom_sf_text(
        data = south_america %>% dplyr::filter(COUNTRY %in% c("Peru", "Bolivia"))
        , aes(label = iso_code)
        , size = 3
        , colour = "gray10"
        , nudge_y = 2
      ) +
      geom_sf_text(
        data = south_america %>% dplyr::filter(COUNTRY == "Brazil")
        , aes(label = iso_code)
        , size = 3
        , colour = "gray10"
        , nudge_y = 8
        , nudge_x = -4
      ) +
      geom_sf_text(
        data = south_america %>% dplyr::filter(COUNTRY == "French Guiana (France)")
        , aes(label = iso_code)
        , size = 3
        , colour = "gray10"
        , nudge_y = 1.7
        , nudge_x = 2.4
      ) +
      geom_sf_text(
        data = south_america %>% dplyr::filter(COUNTRY == "Suriname")
        , aes(label = iso_code)
        , size = 3
        , colour = "gray10"
        , nudge_y = 3
        , nudge_x = 1.5
      ) +
      coord_sf(xlim  = c(-80, -43), ylim = c(-17.5, 10)) +
      scale_color_manual(values = "gray50") +
      scale_fill_manual(
        values = c("#A059A0FF", "#CE6693FF" , "#EB7F86FF", "#F8A07EFF", "#FAC484FF", "#F3E79BFF")
        , breaks = class
        ) +
      labs(title = title) +
      theme_void() +
      theme(
        text = element_text(family = "Calibri", color = "gray21", size = 10)
        , legend.position = "right"
        , legend.text = element_text(family = "Calibri", color = "gray21", size = 10)
        , legend.title = element_text(family = "Calibri", color = "gray21", size = 10)
        , legend.key.width = unit(.4, "cm")
        , legend.key.height = unit(.4, "cm")
        , legend.key.size = unit(1, 'lines')
        , legend.margin = margin(-.65, 0, 0, 0, unit = "cm")
        , plot.margin = unit(c(2, 1, 2, 1), "mm")
        , title = element_text(family = "Calibri", color = "gray21", size = 9)
      ) +
      guides(
        fill = guide_legend(
          title = legend_title
          , title.position = "top"
          , title.hjust = 0
          , order = 1
        )
        , shape = guide_legend(order = 2, "")
        , color = guide_legend(order = 3, "")
      )
  }



map_layout_disaster_report_event_class <-
  function(data, variable = NULL, legend_title, title = NULL, palette, class, direction){

    if (str_detect(title, "Storm|Wildfire|Humam")) {
      scale_function <- scale_fill_manual(values = palette, breaks = class)
    } else if (str_detect(title, "Flood")) {
      scale_function <- viridis::scale_fill_viridis(option = palette, direction = direction, discrete = TRUE)
    } else if (str_detect(title, "Drought")) {
      scale_function <- scale_fill_brewer(palette = "YlOrBr", direction = direction)
    } else {
      scale_function <- paletteer::scale_fill_paletteer_d(palette = palette, direction = direction)
    }

    ggplot(data = data) +
      geom_sf(data = municipios_amazonia_bioma, aes(shape = "NE"), fill = "gray90", color = "gray50", linewidth = .05) +
      geom_sf(aes(fill = .data[[variable]]), alpha = .75, color = "gray50", linewidth = .05) +
      geom_sf(data = others, fill = "gray70", aes(color = "NA"), linewidth = .1) +
      geom_sf(data = south_america, fill = NA, color = "gray35", linewidth = .3) +
      geom_sf(data = amazonia, fill = NA, color = "#7fbc41", linewidth = .4) +
      coord_sf(xlim  = c(-80, -43), ylim = c(-17.5, 10)) +
      scale_color_manual(values = "gray50") +
      labs(title = title) +
      scale_function +
      theme_void() +
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
      ) +
      guides(
        fill = guide_legend(
          title = legend_title
          , title.position = "top"
          , title.hjust = 0
          , order = 1
        )
        , shape = guide_legend(order = 2, "")
        , color = guide_legend(order = 3, "")
      )
  }

