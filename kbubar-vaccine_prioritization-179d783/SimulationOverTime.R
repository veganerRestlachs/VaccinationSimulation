source("run_sim.R")
source("helper_functions.R")
country <- "DEU"
source("setup.R")
library(roll)
#_____________________________________________________________________
# Calculate timedependent C
#_____________________________________________________________________

startDate =  as.POSIXct(as.Date("1.2.2021", "%d.%m.%Y"))
ConstantRValue = 0
TestingFactorStart = 3
TestingFactorEnd = 1.3
firstVaccinationImmunity = FALSE
secPerDay = 60*60*24
ImmunityShift =0
initialSero = 2225000 ## For 1.2.
initSero = rep(initialSero/pop_total,num_groups)
RValues <-getRValues(startDate)
if(ConstantRValue){
  RValues$R = ConstantRValue
}
if(firstVaccinationImmunity){
  vaccinationValues = getVaccinationValues(firstVaccination = firstVaccinationImmunity,startDate-ImmunityShift*secPerDay)
  vaccinationValues$Date<-vaccinationValues$Date+ImmunityShift*secPerDay
}else{
  vaccinationValues = getVaccinationValues(firstVaccination = firstVaccinationImmunity,startDate-ImmunityShift*secPerDay)
  vaccinationValues$Date<-vaccinationValues$Date+ImmunityShift*secPerDay  
}
## Merge Data to last common date
endDate = min(RValues[nrow(RValues),"Date"], vaccinationValues[nrow(vaccinationValues),"Date"])
RValues<-RValues[RValues$Date <= endDate,]
vaccinationValues<-vaccinationValues[vaccinationValues$Date <=endDate,]

assertthat::are_equal(nrow(RValues),nrow(vaccinationValues))
TestingFactor = seq(TestingFactorStart,TestingFactorEnd, length.out = nrow(RValues))
# Scale R such that R_t becomes R_0
if(ConstantRValue <1e-5){
  RValues$R<-RValues$R/(1.-vaccinationValues$fraction)
}
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
RValues$Infectious<-RValues$Infectious*TestingFactor
fractionInfected = RValues$Infectious[[1]]    ### Original Value 0.0025
initVaccLevel = vaccinationValues$fraction[1]
# _____________________________________________________________________
# FIGURE 1: Dynamics curves ####
# _____________________________________________________________________

ptm <- proc.time()  

noVax_all <- run_simDynamic(dynamicC, 0., "all", dynamicNPD, v_e_type, this_v_e,sero = initSero,fractionInfected = fractionInfected, vaccinated = 0.0)
#noVax_kids <- run_simDynamic(dynamicC, 0., "kids", dynamicNPD, v_e_type, this_v_e,fractionInfected = fractionInfected, vaccinated = 0.0)
#noVax_adults <- run_simDynamic(dynamicC, 0., "adults", dynamicNPD, v_e_type, this_v_e,fractionInfected = fractionInfected, vaccinated = 0.0)
#noVax_elderly <- run_simDynamic(dynamicC, 0., "elderly", dynamicNPD, v_e_type, this_v_e, fractionInfected = fractionInfected, vaccinated = 0.0)
#noVax_twentyplus <- run_simDynamic(dynamicC, 0., "twentyplus", dynamicNPD, v_e_type, this_v_e,fractionInfected = fractionInfected, vaccinated = 0.0)


vaxAvailability  =0.5
all <- run_simDynamic(dynamicC, vaxAvailability, "all", dynamicNPD, v_e_type, this_v_e,sero = initSero,fractionInfected = fractionInfected, vaccinated = initVaccLevel)
kids <- run_simDynamic(dynamicC, vaxAvailability, "kids", dynamicNPD, v_e_type, this_v_e,sero = initSero,fractionInfected = fractionInfected, vaccinated = initVaccLevel)
adults <- run_simDynamic(dynamicC, vaxAvailability, "adults", dynamicNPD, v_e_type, this_v_e,sero = initSero,fractionInfected = fractionInfected, vaccinated = initVaccLevel)
elderly <- run_simDynamic(dynamicC, vaxAvailability, "elderly", dynamicNPD, v_e_type, this_v_e,sero = initSero, fractionInfected = fractionInfected, vaccinated = initVaccLevel)
twentyplus <- run_simDynamic(dynamicC, vaxAvailability, "twentyplus", dynamicNPD, v_e_type, this_v_e,sero = initSero,fractionInfected = fractionInfected, vaccinated = initVaccLevel)



proc.time() - ptm

#p_mort <- plot_over_vax_avail("deaths")
#p_infect <- plot_over_vax_avail("cases")

RValues$percent = RValues$Infectious*100
infections <- plot_strat_overtimeDyn("I", noVax_all,all, adults,kids,twentyplus,elderly, RValues, 0.1/num_perday) +
  ggtitle("Simulation with actual rollout speed R = 1.15") + 
  theme(plot.title = element_text(color = "black"))

infections

mort <- plot_strat_overtimeDyn("D", noVax_all,all, adults,kids,twentyplus,elderly, RValues, 0.1/num_perday) +
  theme(plot.title = element_text(color = "black"))

mort

