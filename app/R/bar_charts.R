

prov_colors <- c("#FFC000","#1F497D", rep("#4F81BD",9))
names(prov_colors) = c("CA", "BC", "NL", "PEI", "NS", "NB", "QC", "ON", "MN", "SK", "AB")

province_bar_chart <- function(df, x_axis_type = "number") {
  
  # Set x-axis range based on the data
  if (x_axis_type == "number") {
    # For changes, allow extra space on both sides of zero
    x_min <- min(df$value, na.rm = TRUE)
    x_max <- max(df$value, na.rm = TRUE)
    
    # Give labels a little room beyond the largest value
    padding <- max(abs(c(x_min, x_max))) * 0.15
    
    x_range <- c(
      min(0, x_min) - padding,
      max(0, x_max) + padding
    )
    
  } else {
    # For percentages, start at zero
    x_max <- max(df$value, na.rm = TRUE)
    x_range <- c(0, x_max * 1.15)
  }
  
  plot_ly(
    df,
    x = ~value,
    y = ~fct_rev(province),
    type = "bar",
    orientation = "h",
    marker = list(
      color = ~color
    ),
    text = ~label,
    textposition = "outside",
    textfont = list(
      size = 14,
      color = "#111111"
    ),
    cliponaxis = FALSE,
    hoverinfo = "none"
  ) %>%
    layout(
      xaxis = list(
        range = x_range,
        title = "",
        showgrid = FALSE,
        showticklabels = FALSE,
        zeroline = TRUE,
        zerolinecolor = "#D9D9D9",
        zerolinewidth = 1,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        showgrid = FALSE,
        ticks="outside", 
        ticklen=15,          # Controls the size of the gap (in pixels)
        tickcolor="rgba(0,0,0,0)",  # Makes the tick lines completely transparent
        fixedrange = TRUE,
        tickfont = list(
          size = 15,
          color = "#44546A"
        )
      ),
      margin = list(
        l = 45,
        r = 45,
        t = 30,
        b = 0
      ),
      showlegend = FALSE,
      bargap = 0.25
    ) %>%
    plotly_config()
}


## Monthly Employment Change for Canada and Provinces
bar_chart_1 <- function(){
  
  ##From Table: 14-10-0287-01 (formerly CANSIM 282-0087)
  vector_list <- tribble(
    ~province, ~vector,
    "CA", "v2062811",
    "NL", "v2063000",
    "PEI", "v2063189",
    "NS", "v2063378",
    "NB", "v2063567",
    "QC", "v2063756"                                                                                                                                             ,
    "ON", "v2063945",
    "MN", "v2064134",
    "SK", "v2064323",
    "AB", "v2064512",
    "BC", "v2064701"
  )
  
  df <- left_join(
    vector_list,
    get_cansim_vector_for_latest_periods(
      vectors = vector_list$vector,
      periods = 2) %>%
      clean_names(),
    by = "vector"
  ) %>%
    mutate(date = ifelse(ref_date == max(ref_date), "current", "previous")) %>%
    select(province, date, val_norm) %>%
    pivot_wider(names_from = "date", values_from = "val_norm") %>%
    mutate(value = current - previous,
           label = label_comma(style_positive = "plus")(value),
           color = prov_colors[province]) %>%
    arrange(desc(value)) %>%
    mutate(province = fct_inorder(province))
  
  province_bar_chart(df)
}

## Unemployment rate for Canada and Provinces (15 years and over; seasonally adjusted)
bar_chart_2 <- function(){

  vector_list <- tribble(
    ~province, ~vector,
    "CA", "v2062815",
    "NL", "v2063004",
    "PEI", "v2063193",
    "NS", "v2063382",
    "NB", "v2063571",
    "QC", "v2063760"                                                                                                                                             ,
    "ON", "v2063949",
    "MN", "v2064138",
    "SK", "v2064327",
    "AB", "v2064516",
    "BC", "v2064705"
  )
  
  df <- left_join(
    vector_list,
    get_cansim_vector_for_latest_periods(
      vectors = vector_list$vector,
      periods = 1) %>%
      clean_names(),
    by = "vector"
  ) %>%
    select(province, value) %>%
    mutate(label = label_percent(accuracy = 0.1)(value/100),
           color = prov_colors[province]) %>%
    arrange(value, province) %>%
    mutate(province = fct_inorder(province))
  
  province_bar_chart(df, x_axis_type = "percent")
  
}

bar_chart_3 <- function(indicator = "Employment", 
                        ## for only gender use age_groups = "All Ages"
                        age_groups = c("15 to 24 years", "25 to 54 years", "55 years and over"), 
                        ## for only age_group, use genders = "Total"
                        genders = c("Men+", "Women+"),
                        comparator_type = "current"){ ## comparator_type can be "current", "mom", or "yoy"
  
  vector_list <- tribble(
      ~indicator_label,          ~age_group,  ~gender,    ~vector,
          "Employment",          "All Ages",  "Total", "v2064701",
          "Employment",    "15 to 24 years",  "Total", "v2064728",
          "Employment",    "25 to 54 years",  "Total", "v2064836",
          "Employment", "55 years and over",  "Total", "v2064863",
          "Employment",          "All Ages",   "Men+", "v2064710",
          "Employment",    "15 to 24 years",   "Men+", "v2064737",
          "Employment",    "25 to 54 years",   "Men+", "v2064845",
          "Employment", "55 years and over",   "Men+", "v2064872",
          "Employment",          "All Ages", "Women+", "v2064719",
          "Employment",    "15 to 24 years", "Women+", "v2064746",
          "Employment",    "25 to 54 years", "Women+", "v2064854",
          "Employment", "55 years and over", "Women+", "v2064881",
   "Unemployment rate",          "All Ages",  "Total", "v2064705",
   "Unemployment rate",    "15 to 24 years",  "Total", "v2064732",
   "Unemployment rate",    "25 to 54 years",  "Total", "v2064840",
   "Unemployment rate", "55 years and over",  "Total", "v2064867",
   "Unemployment rate",          "All Ages",   "Men+", "v2064714",
   "Unemployment rate",    "15 to 24 years",   "Men+", "v2064741",
   "Unemployment rate",    "25 to 54 years",   "Men+", "v2064849",
   "Unemployment rate", "55 years and over",   "Men+", "v2064876",
   "Unemployment rate",          "All Ages", "Women+", "v2064723",
   "Unemployment rate",    "15 to 24 years", "Women+", "v2064750",
   "Unemployment rate",    "25 to 54 years", "Women+", "v2064858",
   "Unemployment rate", "55 years and over", "Women+", "v2064885",
  "Participation rate",          "All Ages",  "Total", "v2064706",
  "Participation rate",    "15 to 24 years",  "Total", "v2064733",
  "Participation rate",    "25 to 54 years",  "Total", "v2064841",
  "Participation rate", "55 years and over",  "Total", "v2064868",
  "Participation rate",          "All Ages",   "Men+", "v2064715",
  "Participation rate",    "15 to 24 years",   "Men+", "v2064742",
  "Participation rate",    "25 to 54 years",   "Men+", "v2064850",
  "Participation rate", "55 years and over",   "Men+", "v2064877",
  "Participation rate",          "All Ages", "Women+", "v2064724",
  "Participation rate",    "15 to 24 years", "Women+", "v2064751",
  "Participation rate",    "25 to 54 years", "Women+", "v2064859",
  "Participation rate", "55 years and over", "Women+", "v2064886"
  )
  
  vector_list_filtered <- vector_list %>%
    filter(indicator_label == indicator,
           age_group %in% age_groups,
           gender %in% genders)

  df <- left_join(
    vector_list_filtered,
    get_cansim_vector(
      vectors = vector_list_filtered$vector,
      start_time = prev_year) %>%
      clean_names() %>%
      select(vector, ref_date, value),
    by = "vector") %>%
    mutate(ref_date = ymd(ref_date),
           date = case_when(ref_date == curr_date ~ "current",
                            ref_date == prev_month ~ "previous_month",
                            ref_date == prev_year ~ "previous_year"),
           age_group = fct_inorder(age_group),
           gender = fct_inorder(gender)) %>%
    filter(!is.na(date))
  
  df <- df %>%
    select(-ref_date) %>%
    pivot_wider(names_from = "date", values_from = "value") %>%
    mutate(mom = current - previous_month,
           yoy = current - previous_year) %>%
    select(-previous_month, -previous_year) %>%
    pivot_longer(cols = c(current, mom, yoy), names_to = "comparator", values_to = "value") %>%
    mutate(label = label_comma()(value)) %>%
    filter(comparator == comparator_type)
  
  age_group_bar_chart(df)

}

age_group_bar_chart <- function(df) {
  
  
  # Dynamic y-axis range
  y_min <- min(df$value, na.rm = TRUE)
  y_max <- max(df$value, na.rm = TRUE)
  
  padding <- max(abs(c(y_min, y_max))) * 0.15
  
  y_range <- c(
    min(0, y_min) - padding,
    max(0, y_max) + padding
  )
  
  # Colours
  colors <- c(
    "Men+" = "#63ABD0",
    "Women+" = "#2E86B9",
    "Total" = "#B7D3E3"
  )
  
  plot_ly(df) %>% add_bars(
    width = 0.25,
   # df,
    x = ~age_group,
    y = ~value,
    color = ~gender,
    colors = colors,
  #  type = "bar",
    text = ~label,
  textangle = 0,
    textposition = ~ifelse(value == 0, "outside","auto"), #~ifelse(value >= 0, "outside", "inside"),
    textfont = list(
      size = 18,
      color = "#111111"
    ),
    hoverinfo = "none"
  ) %>%
    layout(
      xaxis = list(
        title = "",
        showgrid = FALSE,
        gridcolor = "#E6E6E6",
        zeroline = FALSE,
        tickfont = list(
          size = 15,
          color = "#595959"
        ),
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        range = y_range,
        showgrid = TRUE,
        gridcolor = "#E6E6E6",
        zeroline = TRUE,
        zerolinecolor = "#111111",
        zerolinewidth = 1,
        tickfont = list(
          size = 14,
          color = "#595959"
        ),
        fixedrange = TRUE
      ),
      legend = list(title = list(text = "Legend")),
      # legend = list(
      #   orientation = "h",
      #   x = 1,
      #   xanchor = "right",
      #   y = -0.15,
      #   yanchor = "top",
      #   font = list(
      #     size = 15,
      #     color = "#333333"
      #   )
      # ),
      margin = list(
        l = 45,
        r = 15,
        t = 30,
        b = 0
      ),
      bargap = 0.5
    ) %>%
    plotly_config()
}

plotly_config <- function(plot) {
  plot %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      responsive = TRUE,
      modeBarButtonsToRemove = c(
        "zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale",
        "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        # 2D Shape Drawing: drawline, drawopenpath, drawclosedpath, drawcircle, drawrect, eraseshape
        # 3D: zoom3d, pan3d, orbitRotation, tableRotation, handleDrag3d, resetCameraDefault3d, resetCameraLastSave3d, hoverClosest3d
        "hoverClosestCartesian", "hoverCompareCartesian",
        # Geo: zoomInGeo, zoomOutGeo, resetGeo, hoverClosestGeo
        "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "sendDataToCloud", "toggleSpikelines", "resetViewMapbox"
      )
    )
}

