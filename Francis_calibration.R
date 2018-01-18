# Set up ------------------------------------------------------------------
source('nsga_org.R')
source("file_process_functions.R")

SWMM_input_file_name <- "composite_sep_Francis.inp"

no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)

# Data --------------------------------------------------------------------
variables <- as.list(c("runoff"))
obs <- vector("list", length(variables))
obs[[1]] <- read_calibration("Francis_outflow.csv")

# RMSE and -NSE for metric
metric_function <- metric_factory(c(4, 9), 2)

find_metrics <- function(path = getwd()) {
  sims <- read_output(path, c("underdrain", "runoff"))
  sims <- sims %>%
    mutate(runoff = runoff + underdrain) %>%
    select(-underdrain)
  
  metrics <- vector("list", length(variables))
  
  for (i in seq_along(variables)) {
    sim <- sims[, c(1, 1 + i)]
    names(sim)[2] <- "value"
    
    metrics[[i]] <- calculate_metric(sim, obs[[i]], metric_function)
  }
  
  return(list(metrics = unlist(metrics),
              sims = sims))
}

# Functions ---------------------------------------------------------------
write_input <- function(x, wd = getwd(), SWMM_input_file_name = "composite_sep_Francis.inp"){
  org_wd <- getwd()
  setwd(wd)
  
  input_template <- readLines(SWMM_input_file_name)
  line <- input_template
  
  x <- data.frame(matrix(x, ncol = 9)) # change to match input number
  
  for (k in 1:nrow(x)){
    temp <- x[k,]
    
    berm_height <- 10 + 300*temp[1] # [20, 350]
    
    # field_capcity <- 0.1 + 0.3*temp[2] # [0.1, 0.4]
    # conductivity <- 1 + 200*temp[3] # [1,201]
    conductivity_slope <- 1 + 100*temp[3] # [1, 101]
    suction_head <- 2 + 200*temp[4] # [2, 202]
    
    storge_void <- 0.4 + 0.4*temp[5] # [0.4, 0.8]
    seepage <- 0.1 + 20*temp[6] # [0.1, 20.1]
    
    flow_coe <- 0.1 + 200*temp[7] # [0.1, 200.1]
    flow_exp <- 0.1 + 0.8*temp[8] # [0.1, 0.9]
    offset_height <- 1 + 40*temp[9] # [1, 51]
    
    line_num = 81
    word_2b_inserted <- as.character(c(berm_height))
    
    line[line_num] = change_word(input_template, 
                                 word_2b_inserted = word_2b_inserted,
                                 line_num = line_num,
                                 ind_sta = c(29), ind_end = c(31))
    
    line_num = 82
    word_2b_inserted <- as.character(c(conductivity_slope,
                                       suction_head))
    
    line[line_num] = change_word(input_template, 
                                 word_2b_inserted = word_2b_inserted,
                                 line_num = line_num,
                                 ind_sta = c(84,95), ind_end = c(87,97))
    
    line_num = 83
    word_2b_inserted <- as.character(c(storge_void, seepage))
    
    line[line_num] = change_word(input_template, 
                                 word_2b_inserted = word_2b_inserted,
                                 line_num = line_num,
                                 ind_sta = c(40,51), ind_end = c(43,53))
    
    line_num = 84
    word_2b_inserted <- as.character(c(flow_coe, flow_exp, offset_height))
    
    line[line_num] = change_word(input_template, 
                                 word_2b_inserted = word_2b_inserted,
                                 line_num = line_num,
                                 ind_sta = c(29,40,51), ind_end = c(29,42,53))
    
    fileConn<-file(paste0("test",k,".inp")) # creat file and write by line
    writeLines(line, fileConn)
    close(fileConn)
  }
}

remain_files <- c("swmm5.exe", "Francis_rain.csv")
clean <- clean_after_run(remain_files)

fn <- function(x, no_cores = detectCores() - 2, return_series = F){
  org_wd <- getwd()
  write_input(x, org_wd)
  
  paths <- as.list(str_c(getwd(),"/cluster",c(1:no_cores)))
  x <- data.frame(x)
  le <- ceiling(nrow(x)/no_cores)
  
  metrics <- vector("list", le) # metrics stores metrics for each par simulatuion
  sims <- vector("list", le) # sims stores the time series for each par simulatuion
  
  for (j in 1:le){
    ind <- (j - 1)*no_cores + 1:no_cores   # move SWMM input file to cluster
    file_2_copy <- as.list(paste0("test",ind,".inp")) 
    folder_suffix <- as.list(1:no_cores)
    file_suffix <- as.list(ind)
    arguments <- list(file_2_copy,folder_suffix,file_suffix) 
    arguments %>%
      pmap(move_file,"cluster","test")
    
    # run
    parLapply(cl, paths, run_swmm) 
    
    # read output and calculate matrix
    sims_par <- vector("list", no_cores)
    metrics_par <- vector("list", no_cores)
    for (kk in 1:no_cores) {
      temp <- find_metrics(paths[[kk]])
      sims_par[[kk]] <- temp$sims
      metrics_par[[kk]] <- temp$metrics
    }
    
    metrics[[j]] <- metrics_par
    sims[[j]] <- sims_par
    
    # clean cluster for new cases
    lapply(paths, clean)
  }
  
  # clean cluster
  delete_file(list.files(pattern = '^test.*\\.inp'))
  
  if (return_series){
    temp <- flattenlist(sims)
  } else {
    temp <- matrix(unname(unlist(metrics)), byrow = T, ncol = length(metrics[[1]][[1]]))
  }
  return(temp)
}

# Calibration -------------------------------------------------------------
results_org <- nsga_org(fn = fn, varNo = 9, objDim = 2, 
                        lowerBounds = rep(0, 9), 
                        upperBounds = rep(1, 9), 
                        popSize = 18, tourSize = 2, generations = 15, cprob = 0.9, XoverDistIdx = 20, 
                        mprob = 0.1, MuDistIdx = 3, save_every = 2)

save(results_org, file = "results_org.Rda")
write_input(results_org$parameters)

pairs(results_org$parameters)

# Post-processing ---------------------------------------------------------
pareto_ranks_kept <- 1:2
pareto_ranks_kept <- results_org$paretoFrontRank %in% pareto_ranks_kept

sims <- fn(results_org$parameters, return_series = T)

le <- sum(pareto_ranks_kept)
pareto_series <- vector("list", le)
for (i in 1:sum(pareto_ranks_kept)){
  pareto_series[[i]] <- sims[[i]]
}

# Time series plots
sim <- pareto_series[[2]]
sim$cases <- "simulation"

ob <- obs[[1]]
names(ob)[2] <- "runoff"
ob$cases <- "observaton"

joint <- rbind(sim, ob) %>%
  gather(type, value, -time, -cases)

sta_time <- parse_datetime("2014-03-30 00:00:30") 
end_time <- parse_datetime("2014-06-20 13:59:30")

ggplot(joint, aes(time, value, colour = cases)) +
  geom_line() +
  labs(xlab = "time [min]",
       y = "dischrage [mm/h]",
       title=" Simulated vs. monitored ouflow rate at St. Francis rain gardens (Upper)") +
  scale_x_datetime(limits = c(sta_time, end_time), date_breaks = "4 days") +
  scale_y_continuous(limits = c(0,120)) +
  theme(legend.position = "top")

# Test year 2016 ----------------------------------------------------------
com <- "swmm5.exe test1.inp test.rpt test1.out"
system(com)

sims <- read_output(getwd(), c("underdrain", "runoff"))
sims <- sims %>%
  mutate(runoff = runoff + underdrain) %>%
  select(-underdrain)

sim <- sims
sim$cases <- "simulation"

obs <- vector("list", 1)
obs[[1]] <- read_calibration("Griggs_outflow.csv")
ob <- obs[[1]] 
names(ob) <- c("time", "runoff")
ob$cases <- "observaton"

joint <- rbind(sim, ob) %>%
  gather(type, value, -time, -cases)

sta_time <- parse_datetime("2014-03-01 00:00:30") 
end_time <- parse_datetime("2014-04-30 13:59:30")

ggplot(joint, aes(time, value, colour = cases)) +
  geom_line() +
  labs(xlab = "time [min]",
       y = "dischrage [mm/h]",
       title=" Simulated vs. monitored ouflow rate at Griggs Reservoir Rain Garden") +
  scale_x_datetime(limits = c(sta_time, end_time), date_breaks = "1 month") +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(0,100))









