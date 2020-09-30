library(DT)
library(RColorBrewer)
library(sparkline)

# datatable styler --------------------------------------------------------

build_df_heatmap <- function(color_targets = -1,
                          scrollY = 600,
                          thresholds = seq(from = 2400, length.out = 7, by = 100),
                          color_pal  = "OrRd"){
  
  the_fun <- . %>%
    datatable(extensions = c('FixedColumns',"Scroller"), 
              rownames = FALSE, 
              selection = list(mode = 'single', target = 'cell'),
              options = list(dom = 't', pageLength = 99, scrollX = TRUE, 
                             scrollY = scrollY, scroller = TRUE,
                             fixedColumns = list(leftColumns = 1))) %>%
    formatStyle(color_targets, backgroundColor = styleInterval(
      thresholds, brewer.pal(length(thresholds) + 2, color_pal)[-(length(thresholds)+2)]))  
  
  return(the_fun)
}

build_df_scrollY <- function(center_targets = 0:4, hide_targets = 0, scrollY = 399){
  the_fun <- . %>% datatable(
    escape = FALSE, selection = "single", extensions = 'Scroller', filter = "bottom",
    options = list(columnDefs = list(list(className = 'dt-center', targets = center_targets),
                                     list(visible = FALSE, targets = hide_targets)),
                   deferRender = TRUE, scrollY = scrollY, scroller = TRUE, 
                   dom = "t"))  
  return(the_fun)
}

# build_df_scrollY(0:4, 600)(mtcars)
# build_df_heat(-1, 600)(mtcars)

build_df_spark <- function(center_targets = 0, hide_targets = 0:1){
  the_fun <- . %>% datatable(
    escape = FALSE, selection = "single", 
    extensions = c('RowGroup'),
    options = list(columnDefs = list(list(className = 'dt-center', targets = center_targets),
                                     list(width = "30%",   targets = 3:5),
                                     list(width = "10%",   targets = 2 ),
                                     list(visible = FALSE, targets = hide_targets)),
                   rowGroup = list(dataSrc = 1),
                   fnDrawCallback = htmlwidgets::JS('function(){  HTMLWidgets.staticRender();}'),
                   dom = "t")) %>%
    spk_add_deps()
  return(the_fun)  
}


