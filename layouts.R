map_layout_disaster_report_total <-
  function(data, variable = NULL, legend_title, title = NULL, color = NULL){
    ggplot(data = data) +
      # geom_sf(data = south_america %>% filter(COUNTRY %in% c("Guyana", "French Guiana (France)", "Suriname")), fill = "gray90", linewidth = .1) +
      geom_sf(data = municipios_amazonia_bioma, aes(fill = "NE"), color = "gray50", linewidth = .1) +
      geom_sf(aes(fill = .data[[variable]]), alpha = .75, color = "gray50", linewidth = .1) +
      geom_sf(data = others, aes(fill = "NA"), linewidth = .1) +
      geom_sf(data = south_america, fill = NA, color = "gray35", linewidth = .3) +
      geom_sf(data = amazonia, fill = NA, color = "gray21", linewidth = .4) +
      geom_sf_text(
        data = south_america %>% filter(COUNTRY == "Guyana")
        , aes(label = COUNTRY)
        , size = 3
        , colour = "gray21"
        , nudge_y = 4
        , nudge_x = 1.5
        , fontface = "bold"
      ) +
      geom_sf_text(
        data = south_america %>% filter(COUNTRY == "Ecuador")
        , aes(label = COUNTRY)
        , size = 3
        , colour = "gray21"
        , nudge_y = 1
        , fontface = "bold"
      ) +
      geom_sf_text(
        data = south_america %>% filter(COUNTRY %in% c("Colombia", "Venezuela"))
        , aes(label = COUNTRY)
        , size = 3
        , colour = "gray21"
        , nudge_y = -1.5
        , nudge_x = 1
        , fontface = "bold"
      ) +
      geom_sf_text(
        data = south_america %>% filter(COUNTRY %in% c("Peru", "Bolivia"))
        , aes(label = COUNTRY)
        , size = 3
        , colour = "gray21"
        , nudge_y = 2
        , fontface = "bold"
      ) +
      geom_sf_text(
        data = south_america %>% filter(COUNTRY == "Brazil")
        , aes(label = COUNTRY)
        , size = 3
        , colour = "gray21"
        , nudge_y = 8
        , nudge_x = -4
        , fontface = "bold"
      ) +
      geom_sf_text(
        data = south_america %>% filter(COUNTRY == "French Guiana (France)")
        , aes(label = "French Guiana")
        , size = 3
        , colour = "gray21"
        , nudge_y = 1.9
        , nudge_x = 4.2
        , fontface = "bold"
      ) +
      geom_sf_text(
        data = south_america %>% filter(COUNTRY == "Suriname")
        , aes(label = COUNTRY)
        , size = 3
        , colour = "gray21"
        , nudge_y = 3
        , nudge_x = 1
        , fontface = "bold"
      ) +
      coord_sf(xlim  = c(-80, -43), ylim = c(-17.5, 10)) +
      scale_color_manual(values = "gray50") +
      scale_fill_manual(
        values = c("#FFDF91", "#f7b538", "#C6782CFF", "#803C15FF", "#39080f", "#0f0909" , "gray90", "gray70") #, "#4D0011" "#D4AF37",
        # values = c("#FFDF91", "#f7b538", "#C6782CFF", "#803C15FF", "#4D0011", "#2C0C00FF" , "gray90", "gray80") #, "#4D0011" "#D4AF37",
        , breaks = c("≤ 5", "5 - 10", "10 - 50", "50 - 100", "100 - 150", "150 - 367", "NE", "NA")
        ) +
      labs(title = title) +
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
          direction = "horizontal"
          , ncol = 1
          , label.position = "right"
          , title = legend_title
          , title.position = "top"
          , title.hjust = 0
          , order = 1
        )
      )
  }


map_layout_disaster_report_event_class <-
  function(data, variable = NULL, legend_title, title = NULL, palette, direction = -1){ #color = NULL, alpha = NULL

    if (str_detect(title, "Storm|Wildfire|Drought|Humam")) {
      scale_function <- scale_fill_manual(values = palette)
    } else if (str_detect(title, "Flood")) {
      scale_function <- viridis::scale_fill_viridis(option = palette, direction = direction, discrete = TRUE)
    } else {
      scale_function <- paletteer::scale_fill_paletteer_d(palette = palette, direction = direction)
    }

    ggplot(data = data) +
      # geom_sf(data = south_america %>% filter(COUNTRY %in% c("Guyana", "French Guiana (France)", "Suriname")), aes(color = "NA"), fill = "gray85", linewidth = .05) +
      geom_sf(data = municipios_amazonia_bioma, aes(shape = "NE"), fill = "gray90", color = "gray50", linewidth = .05) +
      geom_sf(aes(fill = .data[[variable]]), alpha = .75, color = "gray50", linewidth = .05) +
      geom_sf(data = others, fill = "gray70", aes(color = "NA"), linewidth = .1) +
      geom_sf(data = south_america, fill = NA, color = "gray35", linewidth = .3) +
      geom_sf(data = amazonia, fill = NA, color = "gray21", linewidth = .2) +
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
          direction = "horizontal"
          , ncol = 1
          , label.position = "right"
          , title = legend_title
          , title.position = "top"
          , title.hjust = 0
          , order = 1
        )
        , shape = guide_legend(order = 2, "")
        , color = guide_legend(order = 3, "")
      )
  }


map_layout_anomalies <-
  function(data, variable = NULL, legend_title, title = NULL, color = NULL){
    ggplot(data = data) +
      geom_sf(aes(fill = .data[[variable]]), alpha = .75, color = "gray50", linewidth = .05) +
      geom_sf(data = south_america, fill = NA, color = "gray35", linewidth = .2) +
      geom_sf(data = amazonia, fill = NA, color = "gray21", linewidth = .2) +
      coord_sf(xlim  = c(-80, -43), ylim = c(-17.5, 10)) +
      scale_color_manual(values = "gray50") +
      labs(title = title) +
      theme_void() +
      scale_fill_manual(values = color) +
      # scale_function +
      theme(
        legend.position = "right"
        , legend.text = element_text(family = "Calibri", color = "gray21", size = 10)
        , legend.title = element_text(family = "Calibri", color = "gray21", size = 10)
        , legend.key.width = unit(.4, "cm")
        , legend.key.height = unit(.4, "cm")
        , legend.key.size = unit(1, 'lines')
        , legend.margin = margin(-.65, 0, 0, 0, unit = "cm")
        , plot.margin = unit(c(2, 1, 2, 1), "mm")
        , title = element_text(family = "Calibri", color = "gray21", size = 8)
      ) +
      guides(
        fill = guide_legend(
          direction = "horizontal"
          , ncol = 1
          , label.position = "right"
          , title = legend_title
          , title.position = "top"
          , title.hjust = 0
          , order = 1
        )
      )
  }
