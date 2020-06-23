library(shinymaterial)
library(shinyjs)
library(tidyverse)
library(DT) 
source("support.R")
source("shared_styler.R")

# UI ----------------------------------------------------------------------

ui <- material_page(

  # UI post loading css -----------------------------------------------------
  useShinyjs(),
  tags$head(tags$style(HTML(css_content)),
            tags$title("Dashboard - Server Status and Cron-job Logs")),
  nav_bar_color = "cyan lighten-1",
  nav_bar_fixed = FALSE,


  # UI side nav -------------------------------------------------------------
  material_side_nav(
    fixed = FALSE,
    image_source = "theme.png",
    material_side_nav_tabs(
      side_nav_tabs = c("Logs & Jobs"   = "tab_main", 
                        "System Status" = "tab_sys",
                        "R-Shiny Logs"  = "tab_rlog"),
      icons = c("local_activity", "assessment", "sync_problem") #https://materializecss.com/icons.html
    ),
    tags$hr(),
    tags$div(class = "container", 
             tags$p("Log Display Options"),
             material_checkbox("log_rev", "Reversed Order", initial_value = TRUE),
             material_radio_button("log_sel", "Filters", choices = print_log_opts ))
  ),

  material_side_nav_tab_content(
    side_nav_tab_id = "tab_main",
    material_tabs(tabs = c("Dashboard" = "tab_dash", "Charts" = "tab_plot",
                           "Logs" = "tab_logs", "Utilities" = "tab_util")),
  

  # UI side1 dashboard ------------------------------------------------------
  material_tab_content(
    tab_id = "tab_dash",
    material_card(
      "Find the best price",
      tags$br(),
      material_row(
        material_column(width = 9,
                        dataTableOutput("fltpivot", width = "99%"),
                        verbatimTextOutput("debug")),
        material_column(width = 3,
                        dataTableOutput("fltsel"))
      ),
      tags$p("Use", tags$a(href = gds_main, " DataStudio "), "to explore pricing history and more."),
      tags$p("To search for a route, use ", actionLink("linkModal", "Table View"), " here."),
      material_modal("show_dt", "", "",
                     dataTableOutput("fltdt"),
                     button_icon = "featured_play_list", button_depth = 1, button_color = "cyan",
                     floating_button    = TRUE,
                     close_button_label = "Close", 
                     display_button     = TRUE))
  ),
  material_tab_content(
    tab_id = "tab_plot",
    material_row(
      material_card("Fligth by destinations (colored by departure cities)",
                    tags$img(src = "fltplot.png", width = "100%"),
                    tags$a(href = gds_main, class = "waves-effect waves-light btn", tags$i(class = "material-icons left", "filter_center_focus"), "Interactive"))
    ),
    material_row(
      material_card("Accomondatoin by destinations and nights (colored by rate type)",
                    tags$img(src = "mrtplot.png", width = "100%"),
                    tags$a(href = gds_main, class = "waves-effect waves-light btn", tags$i(class = "material-icons left", "filter_center_focus"), "Interactive"))
    ),
    material_row(
      material_column(width = 6,
                      tags$div(class = "wrapss", uiOutput("iframe0"))),
      material_column(width = 6,
                      tags$div(class = "wrap18",  uiOutput("iframe2")),
                      material_checkbox("loadFrames", "Show secondary dashboard (massive data, full control)", initial_value = FALSE),
                      tags$p(HTML(html_test_code), tags$i(" | "),
                             tags$a(href=ihg_link1, "IHG 3nts"), tags$i("  |  "), 
                             tags$a(href=ihg_link2, "IHG 4nts")))
    )
  ),
  
  # UI side1 log display ----------------------------------------------------
    material_tab_content(
      tab_id = "tab_logs",
      material_modal("show_hotels", "Legends", "Hotel Names",
                     tags$p("1 St.Regis Bora Bora"),
                     tags$p("2 Le Meridien Bora Bora"),
                     tags$p("3 Le Meridien Ile des Pins"),
                     tags$p("4 Fiji Marriot Resort Momi Bay"),
                     tags$p("5 Sheraton Resort & Spa, Tokoriki Island, Fiji"),
                     button_icon = "format_list_numbered", button_depth = 1, button_color = "cyan",
                     floating_button    = TRUE,
                     close_button_label = "Close", 
                     display_button     = TRUE),
      material_card(title = "Logs", verbatimTextOutput("logprint"))
    ),
  ),

  # UI side1 flight ---------------------------------------------------------
  material_tab_content(
    tab_id = "tab_util",
    
    # hidden pre-defined routes
    material_modal( 
      "show_predefined", "Presets", "Pre-defined Routes", # pre-defines
      material_radio_button("inRoute", "Routes with 2 or 3 Segments", choices = c(flight_2segs, flight_3segs)),
      tags$p("Other destinations include: Amsterdam (AMS) | Barcelona (BCN) | Berlin (TXL) | Brussels (BRU) | Budapest (BUD) | Copenhagen (CPH) | Frankfurt (FRA) | Geneva (GVA)  | Gothenburg (GOT) | Helsinki (HEL) | Lisbon (LIS) | Madrid (MAD) | Malaga (AGP) | Munich (MUC) | Oslo (OSL) | Prague (PRG) | Stockholm (ARN) | Vienna (VIE) | Warsaw (WAW) | Zurich (ZRH)"),
      button_icon = "assistant_photo", button_depth = 0, button_color = "grey",
      floating_button = FALSE, display_button = FALSE),

    material_row(
      material_column(
        width = 8, offset = 2, 
        material_card(
          title = "",
          tags$div( # main input, build with complex LABEL
            class = "row",
            tags$div(class = "col s12 m2", # fligth Cabin
                     material_dropdown("inCabin",  HTML("<i class='material-icons'>airline_seat_recline_extra</i> Cabin"), 
                                       choices = c("Business"="B", "Economy"="E"), selected = "E")
                     
            ),
            tags$div(class = "col s12 m5", # flight Dates
                     material_text_box(
                       input_id = "inDates",  
                       tagList(HTML("<i class='material-icons'>date_range</i> Dates"),
                               tags$div(class = "right",
                                        actionLink("inPlus1day", "+1 day"), " | ", actionLink("inMinus1day", "-1 day"), " | ",
                                        actionLink("inPlus7day", "+7 day"), " | ", actionLink("inMinus7day", "-7 day")))),

            ),
            tags$div(class = "col s12 m5",  # flight Segments
                     material_text_box(
                       input_id = "inDests",
                       tagList(HTML("<i class='material-icons'>flight</i> Routes"),
                               tags$div(class = "right", actionLink("inOpenPre", "Show Pre-defined Routes"))))
            )
          ),
          tags$div( # output
            class = "row center-align",
            htmlOutput("outFltLinks", inline = TRUE)
          )
        )
      )
    )
  ),

  # UI side2 sys ------------------------------------------------------------

  material_side_nav_tab_content(
    side_nav_tab_id = "tab_sys",
    material_card(title = "Jobs",       verbatimTextOutput("jobprint")),
    material_card(title = "Memory",     verbatimTextOutput("memprint")),
    material_card(title = "Processes",  verbatimTextOutput("proprint")),
    material_card(title = "System Path",verbatimTextOutput("pthprint")),
    tags$p("Excluding process for `root` and `shiny`.")
  ),


  # UI side3 rlog -----------------------------------------------------------
  material_side_nav_tab_content(
    side_nav_tab_id = "tab_rlog",
    material_card(title = "Log from Shiny Apps", verbatimTextOutput("rlogprint")),
    tags$p("Only latest logs are displayed")
  ),

)

# Server ------------------------------------------------------------------


server <- function(session, input, output) {
  
  output$jobprint <- renderPrint(print_cron_jobs())
  output$memprint <- renderPrint(print_mem())
  output$proprint <- renderPrint(print_process())
  output$rlogprint<- renderPrint(print_shiny_log())
  output$pthprint <- renderPrint(print_path())
  

  # getIt output ------------------------------------------------------------

  flt <- readRDS("/home/yanpan/getIt/results/sharing.rds")
  output$fltdt    <- renderDT(build_df_scrollY(scrollY = 390)(flt$df_best))
  output$fltpivot <- renderDT(build_df_heatmap(scrollY = 550)(flt$df_pivot))
  output$fltsel   <- renderDT(build_df_scrollY(scrollY = 550, hide_targets = c(0:8))(
    tryCatch(flt$df_combo %>% 
               filter(ddate == flt$df_pivot$ddate[input$fltpivot_cells_selected[1,1]], 
                      rdate == colnames(flt$df_pivot)[input$fltpivot_cells_selected[1,2] + 1]) %>%
               rowwise() %>% 
               mutate(href = flight_url_qatar_by_data(route, ddate, rdate),
                      `Outbound<br/>Inbound` = route %>% gsub("\\|", "<br/>",.),
                      `Best(link)<br/>EUR` = paste0("<a target='_blank' href = '", href, "'>", ceiling(eur), " €</a><br />(", ceiling(eur1), "+", ceiling(eur2), ")")) %>%
               arrange(eur),
             error = function(e) data.frame(0,1,2,3,4,5,6,7, #toString(e),
                                            Tip = "Select Dates in Calendar First<br /> For all data, click the button at right bottom")
    )
  ))
  observeEvent(input$linkModal, open_material_modal(session, "show_dt"))
  
  
  output$debug <- renderPrint({" "})
  

  # print cron log with AutoRefresher ---------------------------------------
  autoRefresher <- reactiveTimer(19000)
  output$logprint <- renderPrint({
    autoRefresher()
    print_log_all(input$log_sel, input$log_rev)
  })

  

  # DataStudio iFrames ------------------------------------------------------

  # output$iframe0  <- renderUI(tags$iframe(src = gds_link0))
  observeEvent(input$loadFrames, {
      output$iframe2 <- renderUI(tags$iframe(src = gds_link2))
      toggle(selector = '.wrap18')
  })


  # Flight URL tools --------------------------------------------------------

  observeEvent(input$inPlus1day,  {update_material_text_box(session, "inDates", mod_date(input$inDates, 1))})
  observeEvent(input$inPlus7day,  {update_material_text_box(session, "inDates", mod_date(input$inDates, 7))})
  observeEvent(input$inMinus1day, {update_material_text_box(session, "inDates", mod_date(input$inDates,-1))})
  observeEvent(input$inMinus7day, {update_material_text_box(session, "inDates", mod_date(input$inDates,-7))})

  observeEvent(input$inRoute, {
    if(nchar(input$inRoute) < 18) {
      update_material_text_box(session, "inDates",  "2021-03-29 2021-04-17")
    } else {
      update_material_text_box(session, "inDates",  "2021-03-29 2021-04-17 2021-04-21")
    }
    update_material_text_box(session, "inDests", input$inRoute)
  })
  
  observeEvent(list(input$inDates, input$inDests), {
    output$outFltLinks <- renderUI(HTML(get_flt_UI(input$inDates, input$inDests, input$inCabin)))
  })
  
  observeEvent(input$inOpenPre, {
    open_material_modal(session, "show_predefined")
  })
}



# start the app -----------------------------------------------------------

shinyApp(ui = ui, server = server)

