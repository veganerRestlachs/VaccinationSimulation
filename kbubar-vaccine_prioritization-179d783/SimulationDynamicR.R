source("run_sim.R")
source("helper_functions.R")
country <- "DEU"
source("setup.R")
library(roll)
#_____________________________________________________________________
# Calculate timedependent C
#_____________________________________________________________________

startDate =  as.POSIXct(as.Date("1.2.2021", "%d.%m.%Y"))

RValues <-getRValues(startDate)
vaccinationValues = getVaccinationValues(firstVaccination = FALSE,startDate)
## Merge Data to last common date
endDate = min(RValues[nrow(RValues),"Date"], vaccinationValues[nrow(vaccinationValues),"Date"])
RValues<-RValues[RValues$Date <= endDate,]
vaccinationValues<-vaccinationValues[vaccinationValues$Date <=endDate,]

assertthat::are_equal(nrow(RValues),nrow(vaccinationValues))

# Scale R such that R_t becomes R_0
RValues$R<-RValues$R/(1.-vaccinationValues$fraction)
#create Dynamic C
dynamicC <-vector(mode = "list")
for (i in 1:nrow(RValues)){
  tmp_scale = scale_u_for_R0(u_var,C,RValues[i,"R"])
  dynamicC[[i]] <-C/tmp_scale
}

#create Dynamic numPerDay
dynamicNPD <-vector(mode = "list")
for (i in 1:nrow(vaccinationValues)){
  dynamicNPD[[i]] <-vaccinationValues[i,"Shots"]/pop_total
}
TestingFactor = 1.2
fractionInfected = RValues$Infectious[[1]]*TestingFactor    ### Original Value 0.0025
# _____________________________________________________________________
# FIGURE 1: Dynamics curves ####
# _____________________________________________________________________

ptm <- proc.time()  


for (i in seq(0, 50, by = 1)){
  j <- i/100
  list_all[[paste0(i)]] <- run_simDynamic(dynamicC, j, "all", dynamicNPD, v_e_type, this_v_e,fractionInfected = fractionInfected, vaccinated = 0.0)
  list_kids[[paste0(i)]] <- run_simDynamic(dynamicC, j, "kids", dynamicNPD, v_e_type, this_v_e,fractionInfected = fractionInfected, vaccinated = 0.0)
  list_adults[[paste0(i)]] <- run_simDynamic(dynamicC, j, "adults", dynamicNPD, v_e_type, this_v_e,fractionInfected = fractionInfected, vaccinated = 0.0)
  list_elderly[[paste0(i)]] <- run_simDynamic(dynamicC, j, "elderly", dynamicNPD, v_e_type, this_v_e, fractionInfected = fractionInfected, vaccinated = 0.0)
  list_twentyplus[[paste0(i)]] <- run_simDynamic(dynamicC, j, "twentyplus", dynamicNPD, v_e_type, this_v_e,fractionInfected = fractionInfected, vaccinated = 0.0)
} 


proc.time() - ptm

p_mort <- plot_over_vax_avail("deaths")
  p_infect <- plot_over_vax_avail("cases")

t_one <- "10"
t_zero <-"0"
RValues$percent = RValues$Infectious*100
infect_10 <- plot_strat_overtimeDyn("I", list_all[[t_zero]], list_all[[t_one]], list_adults[[t_one]], 
                                 list_kids[[t_one]], list_twentyplus[[t_one]], list_elderly[[t_one]], RValues, 0.1/num_perday) +
  onlyy_theme + 
  ggtitle("10% vaccine supply") + 
  theme(plot.title = element_text(color = "black"))
t_two <- "30"
infect_30 <- plot_strat_overtimeDyn("I", list_all[[t_zero]], list_all[[t_two]], list_adults[[t_two]], 
                                 list_kids[[t_two]], list_twentyplus[[t_two]], list_elderly[[t_two]], RValues, 0.3/num_perday) + 
  nolabels_theme +
  ggtitle("30% vaccine supply") + 
  theme(plot.title = element_text(color = "black"))

mort_10 <- plot_strat_overtime("D", list_all[[t_zero]], list_all[[t_one]], list_adults[[t_one]], 
                               list_kids[[t_one]], list_twentyplus[[t_one]], list_elderly[[t_one]], 0.1/num_perday) 

mort_30 <- plot_strat_overtime("D", list_all[[t_zero]], list_all[[t_two]], list_adults[[t_two]], 
                               list_kids[[t_two]], list_twentyplus[[t_two]], list_elderly[[t_two]], 0.3/num_perday) + 
  onlyx_theme

strategy_panel <- plot_vaxdist_hist()

sub_panel2 <- ggarrange(infect_10, infect_30, p_infect + onlyy_theme,
                        mort_10, mort_30, p_mort + xlab("Total vaccine supply\n(% of population)"),
                        nrow = 2)

# export as 9.5x4   
grid.arrange(strategy_panel, sub_panel2,
             widths = c(2, 7.5))

