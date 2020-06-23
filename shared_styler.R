library(DT)
library(RColorBrewer)

# datatable styler --------------------------------------------------------

build_df_heatmap <- function(color_targets = -1,
                          scrollY = 600,
                          thresholds = seq(from = 2400, length.out = 8, by = 100),
                          color_pal  = "Oranges"){
  
  the_fun <- . %>%
    datatable(extensions = c('FixedColumns',"Scroller"), 
              rownames = FALSE, 
              selection = list(mode = 'single', target = 'cell'),
              options = list(dom = 't', pageLength = 99, scrollX = TRUE, 
                             scrollY = scrollY, scroller = TRUE,
                             fixedColumns = list(leftColumns = 1))) %>%
    formatStyle(color_targets, backgroundColor = styleInterval(
      thresholds, brewer.pal(length(thresholds) + 1, color_pal)))  
  
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
