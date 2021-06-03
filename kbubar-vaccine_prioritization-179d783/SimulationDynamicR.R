source("run_sim.R")
source("helper_functions.R")
country <- "DEU"
source("setup.R")

#_____________________________________________________________________
# Calculate timedependent C
#_____________________________________________________________________
#read from csv file
RValues<-read.csv("../Nowcasting_Zahlen_csv.csv", sep = ";")[,c("Datum","Schätzer_Reproduktionszahl_R")]

startDate =  as.POSIXct(as.Date("1.2.2021", "%d.%m.%Y"))
#change column names to english
names(RValues)<-c("Date","R")
# Format date type to posixct, because everything else didn't work
RValues$Date<-as.POSIXct(as.Date(RValues$Date,"%d.%m.%Y"))
# kick out NAs
RValues<-na.omit(RValues)
# kick out Dates after Startdate
RValues<-RValues[RValues$Date > startDate,]
# reindex
rownames(RValues) <- 0:(nrow(RValues)-1)

#create Dynamic C
dynamicC <-vector(mode = "list")
for (i in 1:nrow(RValues)){
  tmp_scale = scale_u_for_R0(u_var,C,RValues[i,"R"])
  dynamicC[[i]] <-C/tmp_scale
}


# _____________________________________________________________________
# FIGURE 1: Dynamics curves ####
# _____________________________________________________________________

ptm <- proc.time()  


for (i in seq(0, 50, by = 1)){
  j <- i/100
  list_all[[paste0(i)]] <- run_simDynamic(dynamicC, j, "all", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_kids[[paste0(i)]] <- run_simDynamic(dynamicC, j, "kids", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_adults[[paste0(i)]] <- run_simDynamic(dynamicC, j, "adults", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_elderly[[paste0(i)]] <- run_simDynamic(dynamicC, j, "elderly", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
  list_twentyplus[[paste0(i)]] <- run_simDynamic(dynamicC, j, "twentyplus", num_perday, v_e_type, this_v_e, vaccinated = 0.2)
} 


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
