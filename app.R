library(shiny) # get the things to make the shiny shiny
library(bslib) # library of themes to use
library(ggplot2) # need ggplot for final plot
library(dplyr) # need for final plot

# set of instructions in html file

# function to run -- note this is both coho and chinook, but calling coho for now
coho_grow <- function(temp_vect, prey_index_vect, prop_indigest_vect, days) {

  # alternate river types?
  
  # species parameters
  # consumption
  CA <- 0.303; CB <- -0.275 
  CQ <- 5; CTO <- 15; CTM <- 18; CTL <- 24
  CK1 <- 0.36; CK4 <- 0.01
  # respiration
  RA <- 0.00264; RB <- -0.217; RQ <- 0.06818; RTO <- 0.0234
  RK4 <- 0.13 # RTM <- 0; RTL <- 25; RK1 <- 1
  ACT <- 9.7; BACT <- 0.0405 # SDA <- 0.172
  # excretion
  FA <- 0.212; FB <- -0.222; FG <- 0.631
  UA <- 0.0314; UB <- 0.58; UG <- -0.299
  W <- 0.1
  W_list <- W
  
  for (i in 1:days) {
    
      # deal with prey index -- equation 3
      c_max <- CA*(W^CB)*prey_index_vect[i] # *prey_index
      G1 <- (1/(CTO-CQ))*log((0.98*(1-CK1)/(CK1*0.02)))
      L1 <- exp(G1*(temp_vect[i]-CQ))
      KA <- (CK1*L1)/(1+CK1*(L1-1))
      G2 <- (1/(CTL-CTM))*log((0.98*(1-CK4)/(CK4*0.02)))
      L2 <- exp(G2*(CTL-temp_vect[i]))
      KB <- (CK4*L2)/(1+CK4*(L2-1))
      consumption_temp <- KA*KB
      consumption <- c_max*consumption_temp # p = prey_index/tot_prey is propotion of max consumption...
      # consumption
      
      # deal with respiration -- equation 1
      respiration_temp <- exp(RQ*temp_vect[i]) # f(T)
      vel <- ifelse(temp_vect[i] > RTL, RK1*(W^RK4), ACT*(W^RK4)*exp(BACT*temp_vect[i]))
      respiration_activity <- exp(RTO*vel) # ACTIVITY, RTO = coefficient of swimming speed
      respiration <- RA*(W^RB)*respiration_temp*respiration_activity
      # respiration 
      
      # deal with losses -- equation 3
      PE <- FA*(temp_vect[i]^FB)*exp(FG) # ignoring p?
      PF <- ((PE-0.1)/0.9)*(1-prop_indigest_vect[i]) + prop_indigest_vect[i] # assume with prey index that all food is digestable
      egestion <- PF*consumption # egestion
      excretion <- UA*(temp_vect[i]^UB)*exp(UG)*(consumption-egestion) # excretion, ignoring p?
      waste <- egestion + excretion
      # waste 
      
      # now growth!
      daily_growth <- consumption - respiration - waste
      W <- W + daily_growth
      W_list <- c(W_list, W)
      
  }
  
  return(W_list)
  
}

# test it
day_list <- 1:365
temp_vect_test <- 0.5 + 5*sin(day_list/120) + rnorm(365, 0, 0.2)
prey_index_test <- rep(0.5, 365) + rnorm(365, 0, 1)
prop_indg_test <- rbinom(365, 100, 0.2)/100
test_grow <- coho_grow(temp_vect_test, prey_index_test, prop_indg_test, 365)
plot(test_grow)
# yay!

#' okay so what will the shiny app have.... probably like:
#' proportion of glacial & rain streams 
#' ---> so then modeling growth in each plus combined possible growth
#' ---> but need to think through how the combined will look (e.g., proportion of each is time spent in each?)
#' max temp in each
#' food in each + dial for amount of digestible food

# glacier temps flatter, around 4 degrees year round
# rain temps more seasonal, from 0-10 degrees
max_rain_temp <- 10; sin_max_rain_temp <- max_rain_temp/2
rain_temps <- sin_max_rain_temp + 1 + sin_max_rain_temp*sin(day_list/67) + rnorm(365, 0, 1)
rain_temps[which(rain_temps < 0)] <- 0.001 # make sure all above 0
max_glcr_temp <- 5; sin_max_glcr_temp <- max_glcr_temp/2
glcr_temps <- sin_max_glcr_temp + 1 + sin_max_glcr_temp*sin(day_list/80 - 99) + rnorm(365, 0, 1)
glcr_temps[which(glcr_temps < 0)] <- 0.001 # make sure all above 0

# both have same amount of food generally, but glacier food comes earlier
# glacier food peaks in april-may, rain food peaks june-august
max_prey_rain <- 0.1
prey_index_rain <- max_prey_rain + 0.5 + max_prey_rain*sin(day_list/100+0.1) + rnorm(365, 0, 0.2)
prey_index_rain[which(prey_index_rain < 0)] <- 0.001 # make sure all above 0
prop_indg_rain <- rbinom(365, 100, 0.6)/100
max_prey_glcr <- 0.1
prey_index_glcr <- max_prey_glcr + 0.5 + max_prey_glcr*sin(day_list/100+1) + rnorm(365, 0, 0.2)
prey_index_glcr[which(prey_index_glcr < 0)] <- 0.001 # make sure all above 0
prop_indg_glcr <- rbinom(365, 100, 0.1)/100

#' now add how many streams are glacier v. rain
#' will be way more complex.... will need to change function to have p(in glacier/rain), and return three growth curves
#' also need to think about preference v. number of streams: 
#' e.g., if better to be in glacier, should be more likely to be in glacier, but limited by percent of streams that are 

# okay so that's seperate growth
# now need one that's combined
move_grow <- function(temp_vect_r, temp_vect_g, prey_index_vect_r, prey_index_vect_g, prop_indigest_vect_r, prop_indigest_vect_g, prop_g, days) {
  
  # alternate river types?
  
  # species parameters
  # consumption
  CA <- 0.303; CB <- -0.275 
  CQ <- 5; CTO <- 15; CTM <- 18; CTL <- 24
  CK1 <- 0.36; CK4 <- 0.01
  # respiration
  RA <- 0.00264; RB <- -0.217; RQ <- 0.06818; RTO <- 0.0234
  RK4 <- 0.13 # RTM <- 0; RTL <- 25; RK1 <- 1
  ACT <- 9.7; BACT <- 0.0405 # SDA <- 0.172
  # excretion
  FA <- 0.212; FB <- -0.222; FG <- 0.631
  UA <- 0.0314; UB <- 0.58; UG <- -0.299
  W <- 0.1
  W_list <- W
  
  for (i in 1:days) {
    
    # deal with prey index -- equation 3
    # rain
    c_max <- CA*(W^CB)*prey_index_vect_r[i] # *prey_index
    G1 <- (1/(CTO-CQ))*log((0.98*(1-CK1)/(CK1*0.02)))
    L1 <- exp(G1*(temp_vect_r[i]-CQ))
    KA <- (CK1*L1)/(1+CK1*(L1-1))
    G2 <- (1/(CTL-CTM))*log((0.98*(1-CK4)/(CK4*0.02)))
    L2 <- exp(G2*(CTL-temp_vect_r[i]))
    KB <- (CK4*L2)/(1+CK4*(L2-1))
    consumption_temp <- KA*KB
    consumption_r <- c_max*consumption_temp # p = prey_index/tot_prey is propotion of max consumption...
    # glcr
    c_max <- CA*(W^CB)*prey_index_vect_g[i] # *prey_index
    G1 <- (1/(CTO-CQ))*log((0.98*(1-CK1)/(CK1*0.02)))
    L1 <- exp(G1*(temp_vect_g[i]-CQ))
    KA <- (CK1*L1)/(1+CK1*(L1-1))
    G2 <- (1/(CTL-CTM))*log((0.98*(1-CK4)/(CK4*0.02)))
    L2 <- exp(G2*(CTL-temp_vect_g[i]))
    KB <- (CK4*L2)/(1+CK4*(L2-1))
    consumption_temp <- KA*KB
    consumption_g <- c_max*consumption_temp
    
    # deal with respiration -- equation 1
    # rain
    respiration_temp <- exp(RQ*temp_vect_r[i]) # f(T)
    vel <- ifelse(temp_vect_r[i] > RTL, RK1*(W^RK4), ACT*(W^RK4)*exp(BACT*temp_vect_r[i]))
    respiration_activity <- exp(RTO*vel) # ACTIVITY, RTO = coefficient of swimming speed
    respiration_r <- RA*(W^RB)*respiration_temp*respiration_activity
    # glcr 
    respiration_temp <- exp(RQ*temp_vect_g[i]) # f(T)
    vel <- ifelse(temp_vect_g[i] > RTL, RK1*(W^RK4), ACT*(W^RK4)*exp(BACT*temp_vect_g[i]))
    respiration_activity <- exp(RTO*vel) # ACTIVITY, RTO = coefficient of swimming speed
    respiration_g <- RA*(W^RB)*respiration_temp*respiration_activity
    
    # deal with losses -- equation 3
    # rain
    PE <- FA*(temp_vect_r[i]^FB)*exp(FG) # ignoring p?
    PF <- ((PE-0.1)/0.9)*(1-prop_indigest_vect_r[i]) + prop_indigest_vect_r[i] # assume with prey index that all food is digestable
    egestion <- PF*consumption_r # egestion
    excretion <- UA*(temp_vect_r[i]^UB)*exp(UG)*(consumption_r-egestion) # excretion, ignoring p?
    waste_r <- egestion + excretion
    # glcr 
    PE <- FA*(temp_vect_g[i]^FB)*exp(FG) # ignoring p?
    PF <- ((PE-0.1)/0.9)*(1-prop_indigest_vect_g[i]) + prop_indigest_vect_g[i] # assume with prey index that all food is digestable
    egestion <- PF*consumption_g # egestion
    excretion <- UA*(temp_vect_g[i]^UB)*exp(UG)*(consumption_g-egestion) # excretion, ignoring p?
    waste_g <- egestion + excretion
    
    # now growth!
    daily_growth_r <- consumption_r - respiration_r - waste_r
    daily_growth_g <- consumption_g - respiration_g - waste_g
    # choose a river
    prob_in_glacier <- rbinom(1, 1, prop_g)
    # use the difference to flip if they go into the preferred river or not
    river_comp <- daily_growth_g/(daily_growth_r + daily_growth_g)
    river_comp <- ifelse(river_comp < 0, 0, river_comp); river_comp <- ifelse(river_comp > 1, 1, river_comp)
    pref_glacier <- rbinom(1, 1, river_comp)
    glacier_res <- prob_in_glacier + pref_glacier
    # if wants to be in glacier and can be, grow at glacier rate
    # if mixed, flip a coin
    # if wants to be in river and can be, grow at river rate
    ifelse(glacier_res == 2 | prop_g == 1, {daily_growth_m <- daily_growth_g}, 
           ifelse(glacier_res == 1 & prop_g != 0, {daily_growth_m <- ifelse(rbinom(1, 1, 0.5) == 1, daily_growth_g, daily_growth_r)}, 
                  {daily_growth_m <- daily_growth_r}))
    W <- W + daily_growth_m
    W_list <- c(W_list, W)
    
  }
  
  return(W_list)
  
}

# so run the model twice
rain_grow <- coho_grow(rain_temps, prey_index_rain, prop_indg_rain, 365)
glcr_grow <- coho_grow(glcr_temps, prey_index_glcr, prop_indg_glcr, 365)
# testing plot
plot(rain_grow, col = "lightgreen", type = "l", lwd = 2); lines(glcr_grow, col = "lightblue", lwd = 2)
# add movement growth
max_grow <- move_grow(rain_temps, glcr_temps, prey_index_rain, prey_index_glcr, prop_indg_rain, prop_indg_glcr, 0.2, 365)
lines(max_grow, col = "gray", lwd = 2)
# together, you get differnt growth than alone
# there's a few combos where together can be more (e.g., rain has crappy food and there are some glacier streams)
# but generally, better to be in rain, unless the rain is hot (e.g. above 16 degrees)
# also need to convert input temp into F for students

#' okay... so now need to shiny this mess
#' will basically run the two functions and then plot
#' possible sliders:
#' ---> temp increase for rain and glacier (2)
#' ---> prey for rain and glacier (amount and then also the quality) (4)
#' ---> proportion of streams that are glacier (1)
#' what are some possible takeaways?
#' ---> prey amount and quality changes salmon growth (looking at one river with fixed temperature regime)
#' ---> salmon can grow the most when they can move between streams, and they don't grow if it gets too hot (looking at two rivers with fixed prey inputs, but can alter number of streams)
#' ---> prey amount, quality, and temperature have non-linear affects on salmon growth (one river, but now can change all three things about it)
#' ---> food web mosaics can benefit salmon (full set up, with lots of guidance)
