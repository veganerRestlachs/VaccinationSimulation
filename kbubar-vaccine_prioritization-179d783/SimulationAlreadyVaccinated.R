source("run_sim.R")
source("helper_functions.R")
country <- "DEU"
source("setup.R")

# _____________________________________________________________________
# FIGURE 1: Dynamics curves ####
# _____________________________________________________________________
this_C <- C/scale_15
ptm <- proc.time()  

cores=detectCores()
cl <- makeCluster(cores[1]-1) # to not overload your computer
registerDoParallel(cl)

for (i in seq(0, 50, by = 1)){
  j <- i/100
  list_all[[paste0(i)]] <- run_sim(this_C, j, "all", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_kids[[paste0(i)]] <- run_sim(this_C, j, "kids", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_adults[[paste0(i)]] <- run_sim(this_C, j, "adults", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_elderly[[paste0(i)]] <- run_sim(this_C, j, "elderly", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_twentyplus[[paste0(i)]] <- run_sim(this_C, j, "twentyplus", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
} 
stopCluster(cl)

proc.time() - ptm

p_mort <- plot_over_vax_avail("deaths")
p_infect <- plot_over_vax_avail("cases")

t_one <- 11
infect_10 <- plot_strat_overtime("I", list_all[[1]], list_all[[t_one]], list_adults[[t_one]], 
                                 list_kids[[t_one]], list_twentyplus[[t_one]], list_elderly[[t_one]], 0.1/num_perday) +
  onlyy_theme + 
  ggtitle("10% vaccine supply") + 
  theme(plot.title = element_text(color = "black"))
t_two <- 41
infect_30 <- plot_strat_overtime("I", list_all[[1]], list_all[[t_two]], list_adults[[t_two]], 
                                 list_kids[[t_two]], list_twentyplus[[t_two]], list_elderly[[t_two]], 0.3/num_perday) + 
  nolabels_theme +
  ggtitle("30% vaccine supply") + 
  theme(plot.title = element_text(color = "black"))

mort_10 <- plot_strat_overtime("D", list_all[[1]], list_all[[t_one]], list_adults[[t_one]], 
                               list_kids[[t_one]], list_twentyplus[[t_one]], list_elderly[[t_one]], 0.1/num_perday) 

mort_30 <- plot_strat_overtime("D", list_all[[1]], list_all[[t_two]], list_adults[[t_two]], 
                               list_kids[[t_two]], list_twentyplus[[t_two]], list_elderly[[t_two]], 0.3/num_perday) + 
  onlyx_theme

strategy_panel <- plot_vaxdist_hist()

sub_panel2 <- ggarrange(infect_10, infect_30, p_infect + onlyy_theme,
                        mort_10, mort_30, p_mort + xlab("Total vaccine supply\n(% of population)"),
                        nrow = 2)

# export as 9.5x4   
grid.arrange(strategy_panel, sub_panel2,
             widths = c(2, 7.5))
