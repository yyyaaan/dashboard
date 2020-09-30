# get cron job list -------------------------------------------------------
cronjobs <- system("crontab -l", intern =TRUE)
cronjobs <- paste0("# ", cronjobs)

# write to myself ---------------------------------------------------------
in_file  <- readLines("/home/yanpan/dashboard/list_cron.R")
in_target<- max(grep("XXXXXXXXX", in_file))
out_file <- c(in_file[1:in_target], 
              paste("# As of", Sys.time()), "# ",cronjobs)
writeLines(out_file, "/home/yanpan/dashboard/list_cron.R")


# Additional CLEANUP Job --------------------------------------------------

if(weekdays(Sys.Date()) == weekdays(as.Date("2020-06-04"))){
  # runs on Thursdays (2020-06-03 is Thursday)
  
  setwd("/home/yanpan/get/")
  log_files <- list.files(pattern = ".log")
  # move to folder, all exisiting will be replaced
  system("mv z_*.html   z_keeps")
  system("mv *.log      z_keeps")
  system("mv z_*.png    z_keeps")
  system("mv z_*.*.rds  z_keeps")
  # empty logs
  log_reset <- paste("Log rest @", Sys.time())
  for(the_file in log_files) writeLines(log_reset, the_file)
}


# do not change below -----------------------------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# As of 2020-09-30 00:01:02
# 
# ## cronR job
# ## id:   job_cron
# ## tags: DailyTask
# ## desc: get cron job list
# 1 0 * * * /usr/lib/R/bin/Rscript '/home/yanpan/dashboard/list_cron.R'  >> '/home/yanpan/dashboard/list_cron.log' 2>&1
# 
# ## cronR job
# ## id:   job_getthem
# ## tags: job_getThem
# ## desc: I execute things
# 3 0 * * * /usr/lib/R/bin/Rscript '/home/yanpan/getIt/scheduled.R'  >> '/home/yanpan/getIt/scheduled.log' 2>&1
# 
