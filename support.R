source("/home/yanpan/getIt/shared_url_builder.R")

# global vars -------------------------------------------------------------

flt <- readRDS("/home/yanpan/getIt/results/sharing.rds")
htl <- readRDS("/home/yanpan/getIt/results/sharing_mrt.rds")
month_choices  <- flt$df_combo %>% names %>% sort(decreasing = TRUE)
gds_qr01       <- "https://datastudio.google.com/u/1/reporting/1lWsOIgy6lHabGl02NY7ksTE77JemG1Q1/page/qrNUB"
gds_mrt01      <- ""
print_log_opts <- c("Today Brief", "Today All", "Summary", "All (max 1000 rows)")
flight_3segs   <- c("HEL-KIX, SYD-HKG, HKG-HEL",
                    "HEL-TYO, SYD-HKG, HKG-HEL",
                    "HEL-KIX, NAN-LAX, LAX-HEL",
                    "HEL-NOU, NAN-LAX, LAX-HEL",
                    "HEL-NAN, NAN-LAX, LAX-HEL",
                    "HEL-KIX, PPT-LAX, LAX-HEL",
                    "HEL-NOU, PPT-LAX, LAX-HEL",
                    "HEL-NAN, PPT-LAX, LAX-HEL",
                    "HEL-PPT, PPT-LAX, LAX-HEL")
flight_2segs   <- c("HEL-NAN, SYD-HEL",
                    "OSL-NAN, SYD-OSL",
                    "OSL-CBR, CBR-OSL",
                    "OSL-SYD, CBR-OSL",
                    "HEL-HND, SYD-HEL",
                    "HEL-SYD, SYD-HEL",
                    "HEL-MEL, MEL-HEL",
                    "HEL-PPT, PPT-HEL")

html_test_code <- '<a href="#" data-target="slide-out" class="sidenav-trigger">Options/Views</a>'
css_content    <- "
      .wrap18{position: relative; padding-bottom: 136%;}
      .wrap11{position: relative; padding-bottom: 103%;}
      .wrapss{position: relative; padding-bottom:  85%;}
      iframe {position: absolute; top: 0; left: 0; width: 100%; height: 100%;}
      pre    {overflow: auto; font-size: 0.8em;}
      .col .row{margin-left: 1%; width: 98%;}
      td.dt-center {vertical-align: text-top !important;}
"


# Support Functions -------------------------------------------------------

print_shiny_log <- function(){
  
  log_files <- list.files('/home/yanpan/R/', pattern = ".log", full.names = TRUE)
  linesep <- "========= ========= =========\n"
  for(log_file in log_files) {
    log_content <- readLines(log_file)
    cat(linesep, "=== File:", sub("/home/yanpan", "", log_file), " ===\n", linesep, "\n")
    cat(log_content, sep = "\n")
  }
}

print_log <- function(log_file, opt = "Reversed Today", reversed = TRUE){
  
  log_content <- readLines(log_file)
  
  id <- max(length(log_content)-1000, 1):length(log_content)
  if(grepl("Today", opt)) id <- min(grep(Sys.Date(),  log_content), 1):length(log_content)
  if(id[1] == Inf) {cat("no log to display\n"); return;}
  if(grepl("Brief", opt)) id <- grep(paste0(Sys.Date(), "|===|-failed-|halted"),  log_content)
  if(grepl("Summa", opt)) id <- grep(substr(Sys.Date(), 1, 7), log_content)
  
  id <- sort(unique(id), decreasing = reversed)
  
  cat("===\n=== File:", sub("/home/yanpan", "", log_file), " ===\n===\n\n")
  cat(log_content[id], sep = "\n")
}

print_log_all <- function(opt, reversed){
  log_files <- list.files('/home/yanpan/getIt/', pattern = ".log", full.names = TRUE)
  for(log in log_files) print_log(log, opt, reversed)
}

print_cron_jobs <- function(){
  in_file <- readLines("/home/yanpan/dashboard/list_cron.R")
  in_target<- max(grep("XXXXXXXXX", in_file)) + 1
  out_print<- in_file[in_target:length(in_file)]
  sysinfo  <- paste("Running", R.version.string, "on", R.version$platform)
  cat(gsub("# ", "", out_print), sysinfo, sep = "\n")
}

print_mem <- function(){
  cat(system("free -m", intern = TRUE), sep = "\n")
}

print_path <- function(){
  cat("symbolic links (R versions):",
      "ln -s /usr/bin/R /usr/bin/R3",
      "ln -s /usr/bin/Rscript /usr/bin/Rscript3",
      "whereis:",
      system("whereis R", intern = TRUE), 
      system("whereis R3", intern = TRUE), 
      system("whereis R3", intern = TRUE), 
      sep = "\n")
}


print_process <- function(){
  system("ps -ef", intern = T) -> sysout
  sysout <- sysout[which(substr(sysout, 1, 4) != "root")]
  sysout <- sysout[which(substr(sysout, 1, 4) != "shin")]
  # poscmd <- gregexpr('00:00', sysout[2])[[1]][1] + 8
  # sysout <- paste0(substr(sysout, 1, poscmd), "\n\t", substr(sysout, poscmd, 999))
  cat(sysout, sep = "\n\n")
}

mod_date <- function(date_str, value = 1){
  dates <- strsplit(date_str, "[ \t,]+") %>% unlist() %>% as.Date()
  out   <- format.Date(dates + value, "%Y-%m-%d")
  return(paste(out, collapse = " "))
}

get_flt_UI <- function(date_str, city_str, cabin = "E"){
  
  dates <- strsplit(date_str, "[ \t,]+")  %>% unlist() %>% as.Date()
  dests <- strsplit(city_str, "[ \t,-]+") %>% unlist() %>% toupper()
  destx <- c("TLL", dests[-1])
  
  days <-  dates %>% weekdays() %>% paste(collapse = " ")
  
  if(length(dests) / length(dates) != 2) return("Invalid input.")
  
  if(length(dates) == 2){
    return(  c("(Departs on", days , ") <br />",
               flight_url_qatar_2legs(dates, dests, cabin) %>% html_markup("QR"),  " | ",
               flight_url_finnair_any(dates, dests, cabin) %>% html_markup("AY"),  " | ",
               flight_url_singapore_return(dates, dests, cabin) %>% html_markup("SQ"),  " | ",
               flight_url_google_any (dates, dests) %>% html_markup("GFL"), " | ",
               flight_url_qatar_2legs(dates, destx) %>% html_markup("QR*"), " | ",
               flight_url_finnair_any(dates, destx) %>% html_markup("AY*"), " | ",
               flight_url_google_any (dates, destx) %>% html_markup("GFL*")))
  }
  
  if(length(dates) == 3){
    return(  c("(Departs on", days , ") <br />",
               flight_url_cwt_3legs  (dates, dests) %>% html_markup("CWT"), " | ",
               flight_url_finnair_any(dates, dests, cabin) %>% html_markup("AY"),  " | ",
               flight_url_google_any (dates, dests) %>% html_markup("GFL"), " | ",
               flight_url_google_any (dates, destx) %>% html_markup("GFL*")))
  }

}




# date_str = "2021-01-04 2021-01-07"; city_str = "HEL SYD PER HEL"
