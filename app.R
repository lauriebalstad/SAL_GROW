library(shiny) # get the things to make the shiny shiny
library(bslib) # library of themes to use
library(ggplot2) # need ggplot for final plot
library(dplyr) # need for final plot
library(lubridate) # need for some date plotting

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
  RK4 <- 0.13; RTM <- 0; RTL <- 25; RK1 <- 1
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
  RK4 <- 0.13; RTM <- 0; RTL <- 25; RK1 <- 1
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
    daily_growth_m <- NA
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

#' okay... so now need to shiny this mess
#' will basically run the two functions and then plot
#' possible sliders:
#' ---> temp increase for rain and glacier (2)
#' ---> prey for rain and glacier (amount and then also the quality) (4)
#' ---> proportion of streams that are glacier (1)
#' what are some possible takeaways?
#' ---> prey amount and quality changes salmon growth (looking at one river with fixed temperature regime)
#' ---> salmon can grow the most when they can move between streams, and they don't grow if it gets too hot (looking at two rivers with fixed prey inputs, but can alter temps & number of streams)
#' ---> prey amount, quality, and temperature have non-linear affects on salmon growth (one river, but now can change all three things about it)
#' ---> food web mosaics can benefit salmon (full set up, with lots of guidance) -- SKIPPING

cards <- list(
  # one river output
  card(# full_screen = TRUE,
       card_header("Coho salmon growth"), 
       plotOutput(outputId = "prey_river_plot"), 
       height = "500px"),
  card(# full_screen = TRUE,
       card_header("Coho salmon growth"), 
       plotOutput(outputId = "one_river_plot"), 
       height = "500px"),
  # two river output
  card(# full_screen = TRUE,
       card_header("Coho salmon growth"), 
       plotOutput(outputId = "two_river_plot"))
)

# for ui/server: https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/

# sliders
{prey_amount <- sliderInput(
  inputId = "prey_amount", 
  label = "Prey amount (qualitative):",
  min = 1, max = 100,
  value = 48, step = 0.5
)

prey_quality <- sliderInput(
  inputId = "prey_quality", 
  label = "Prey quality (percent digestable):",
  min = 0, max = 100,
  value = 26, step = 0.5
)

rain_temp <- sliderInput(
  inputId = "rain_temp", 
  label = "Rain river temperature:",
  min = 46, max = 72,
  value = 53, step = 0.5
)

glcr_temp <- sliderInput(
  inputId = "glcr_temp", 
  label = "Glacier river temperature:",
  min = 35, max = 61,
  value = 39, step = 0.5
)

prop_glcr <- sliderInput(
  inputId = "prop_glcr", 
  label = "Percent glacial streams:",
  min = 0, max = 100,
  value = 27, step = 0.5
)}

ui = tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    theme = shinythemes::shinytheme("yeti"),
    "Understanding SEAK Coho salmon growth",
    
    tabPanel("About",
             htmlOutput(outputId = "about_text") # drop in text
    ),
    tabPanel("Lesson plans",
             htmlOutput(outputId = "lesson_text") # drop in text
    ),
    tabPanel("Prey quality",
             sidebarPanel(
               prey_amount,
               prey_quality
             ),
             mainPanel(layout_columns(cards[[1]], 
                                      card(card_header("Prey avalibility"), 
                                           plotOutput(outputId = "prey_plot_1"), 
                                           height = "500px")),
                       htmlOutput(outputId = "prey_quality_text"))
    ),
    tabPanel("Temperature & consumption",
             sidebarPanel(
               rain_temp,
               prey_amount
             ),
             mainPanel(layout_columns(cards[[2]], 
                                      layout_columns(card(card_header("Prey avalibility"), 
                                                          plotOutput(outputId = "prey_plot_2"), 
                                                          height = "250px"), 
                                                     card(card_header("River temperature"), 
                                                          plotOutput(outputId = "temp_plot_2"), 
                                                          height = "250px"), 
                                                     col_widths = c(9, 12))
                                      ),
                       htmlOutput(outputId = "temp_consump_text"))
    ),
    tabPanel("A warming world",
             sidebarPanel(
               rain_temp, 
               glcr_temp,
               prop_glcr
             ),
             mainPanel(layout_columns(cards[[3]], 
                                      layout_columns(card(card_header("Prey avalibility"), 
                                                          plotOutput(outputId = "prey_plot_3"), 
                                                          height = "250px"), 
                                                     card(card_header("River temperature"), 
                                                          plotOutput(outputId = "temp_plot_3"), 
                                                          height = "250px"), 
                                                     col_widths = c(9, 12))
             ),
             htmlOutput(outputId = "warming_world_text"))
    )
))

server <- function(input, output, session) {
  
  # run simulation with reactive data
  reactive_data <- reactive({
    
    day_list <- 1:365
    
    rain_cels <- (input$rain_temp-32)*5/9
    glcr_cels <- (input$glcr_temp-32)*5/9
    
    # make time series
    # glacier temps flatter, around 4 degrees year round
    # rain temps more seasonal, from 0-10 degrees
    max_rain_temp <- rain_cels; sin_max_rain_temp <- max_rain_temp/2
    rain_temps <- sin_max_rain_temp + 1 + sin_max_rain_temp*sin(day_list/67) + rnorm(365, 0, 0.5)
    rain_temps[which(rain_temps < 0)] <- 0.001 # make sure all above 0
    max_glcr_temp <- glcr_cels; sin_max_glcr_temp <- max_glcr_temp/2
    glcr_temps <- sin_max_glcr_temp + 1 + sin_max_glcr_temp*sin(day_list/80 - 99) + rnorm(365, 0, 0.5)
    glcr_temps[which(glcr_temps < 0)] <- 0.001 # make sure all above 0
    
    # both have same amount of food generally, but glacier food comes earlier
    # glacier food peaks in april-may, rain food peaks june-august
    # rain things
    max_prey_rain <- input$prey_amount/500 + 0.001
    prey_index_rain <- max_prey_rain + 0.5 + max_prey_rain*sin(day_list/100+0.1) + rnorm(365, 0, 0.015)
    prey_index_rain[which(prey_index_rain < 0)] <- 0.001 # make sure all above 0
    prop_indg_rain <- rbinom(365, 100, (100-input$prey_quality)/100)/100
    # glacier things -- same amount, diff period, but better quality food
    max_prey_glcr <- input$prey_amount/500 + 0.001
    prey_index_glcr <- max_prey_glcr + 0.5 + max_prey_glcr*sin(day_list/100+1) + rnorm(365, 0, 0.015)
    prey_index_glcr[which(prey_index_glcr < 0)] <- 0.001 # make sure all above 0
    prop_indg_glcr <- rbinom(365, 100, 0.1)/100
    
    # run simulation
    # rain river
    sim_dat_rain <- coho_grow(rain_temps, prey_index_rain, prop_indg_rain, 365) 
    
    # glcr river
    sim_dat_glcr <- coho_grow(glcr_temps, prey_index_glcr, prop_indg_glcr, 365) 
    
    # mixed river
    sim_dat_combo <- move_grow(rain_temps, glcr_temps, 
                               prey_index_rain, prey_index_glcr, 
                               prop_indg_rain, prop_indg_glcr, 
                               input$prop_glcr/100, 
                               365)
    
    dat_out <- data.frame(day = rep(1:length(sim_dat_combo), 3), 
                          sal_size = c(sim_dat_rain, sim_dat_glcr, sim_dat_combo), 
                          river_type = c(rep("Rain river", length(sim_dat_combo)), 
                                   rep("Glacier river", length(sim_dat_combo)),
                                   rep("Between river\nmovement", length(sim_dat_combo)))
                          )
    dat_out$calendar_date <- as.Date(dat_out$day, origin = as.Date("2020-04-01"))
    
    rain_cond <- data.frame(river_type = "Rain river", 
                            temp_F = rain_temps*9/5 + 32,
                            prey = 100*(prey_index_rain-0.001), 
                            day = 1:365)
    rain_cond$calendar_date <- as.Date(rain_cond$day, origin = as.Date("2020-04-01"))
    glcr_cond <- data.frame(river_type = "Glacier river", 
                            temp_F = glcr_temps*9/5 + 32,
                            prey = 100*(prey_index_glcr-0.001), 
                            day = 1:365)
    glcr_cond$calendar_date <- as.Date(glcr_cond$day, origin = as.Date("2020-04-01"))
    all_cond <- rbind(rain_cond, glcr_cond)
    
    return(list(dat_out, rain_cond, all_cond))
    
  })
  
  output$about_text <- renderText(includeHTML("about_text.html"))
  output$lesson_text <- renderText(includeHTML("lesson_text.html"))
  
  output$prey_river_plot <- renderPlot({
    
    tmp_dat <- reactive_data()[[1]]
    
    plotData <- tmp_dat %>% filter(river_type == "Rain river")
    
    ggplot(plotData) + 
      geom_line(aes_string(x="calendar_date", y="sal_size", col="river_type")) + 
      labs(col="River type", x="Date", y="Coho size (grams)") + 
      scale_color_manual(values = c("#95d840")) + 
      scale_linewidth_manual(values = c(1.5)) + 
      coord_cartesian(ylim = c(0, 24)) + # force fixed axis
      theme_classic() + theme(text = element_text(size = 14), 
                              legend.position = "bottom")
    
  })
  output$prey_quality_text <- renderText(includeHTML("prey_quality_text.html"))
  
  output$one_river_plot <- renderPlot({
    
      tmp_dat <- reactive_data()[[1]]
    
      plotData <- tmp_dat %>% filter(river_type == "Rain river")
    
      ggplot(plotData) + 
        geom_line(aes_string(x="calendar_date", y="sal_size", col="river_type")) + 
        labs(col="River type", x="Date", y="Coho size (grams)") + 
        scale_color_manual(values = c("#95d840")) + 
        scale_linewidth_manual(values = c(1.5)) + 
        coord_cartesian(ylim = c(0, 18)) + # force fixed axis
        theme_classic() + theme(text = element_text(size = 14), 
                                legend.position = "bottom")
    
  })
  output$temp_consump_text <- renderText(includeHTML("temp_consump_text.html"))
  
  output$two_river_plot <- renderPlot({
    
    ggplot(reactive_data()[[1]]) + 
      geom_line(aes_string(x="calendar_date", y="sal_size", col="river_type")) + 
      labs(col="River type", x="Date", y="Coho size (grams)") + 
      scale_color_manual(values = c("gray75", "#33638d", "#95d840")) + 
      scale_linewidth_manual(values = c(1.5)) + 
      coord_cartesian(ylim = c(0, 18)) + # force fixed axis
      theme_classic() + theme(text = element_text(size = 14), 
                              legend.position = "bottom")
    
  })
  output$warming_world_text <- renderText(includeHTML("warming_world_text.html"))
  
  output$prey_plot_1 <- renderPlot({
    
    ggplot(reactive_data()[[2]]) + 
      geom_line(aes_string(x="calendar_date", y="prey", col="river_type")) + 
      labs(col="River type", x="Date", y="Prey amount") + 
      scale_color_manual(values = c("#95d840")) + 
      scale_linewidth_manual(values = c(1.5)) + 
      coord_cartesian(ylim = c(40, 100)) + 
      theme_classic() + theme(text = element_text(size = 14), 
                              legend.position = "bottom")
    
  })
  
  output$prey_plot_2 <- renderPlot({
    
    ggplot(reactive_data()[[2]]) + 
      geom_line(aes_string(x="calendar_date", y="prey", col="river_type")) + 
      labs(col="River type", x="Date", y="Prey amount") + 
      scale_color_manual(values = c("#95d840")) + 
      scale_linewidth_manual(values = c(1.5)) + 
      coord_cartesian(ylim = c(40, 100)) + 
      theme_classic() + theme(text = element_text(size = 10), 
                              legend.position = "bottom")
    
  })

  output$temp_plot_2 <- renderPlot({
    
    ggplot(reactive_data()[[2]]) + 
      geom_hline(aes(yintercept = 32), col = "lightgray", lty = 2) + 
      geom_line(aes_string(x="calendar_date", y="temp_F", col="river_type")) + 
      labs(col="River type", x="Date", y="River temperature (degrees F)") + 
      scale_color_manual(values = c("#95d840")) + 
      scale_linewidth_manual(values = c(1.5)) + 
      coord_cartesian(ylim = c(30, 75)) + 
      theme_classic() + theme(text = element_text(size = 10), 
                              legend.position = "bottom")
    
  })
  
  output$prey_plot_3 <- renderPlot({
    
    ggplot(reactive_data()[[3]]) + 
      geom_line(aes_string(x="calendar_date", y="prey", col="river_type")) + 
      labs(col="River type", x="Date", y="Prey amount") + 
      scale_color_manual(values = c("#33638d", "#95d840")) + 
      scale_linewidth_manual(values = c(1.5)) + 
      coord_cartesian(ylim = c(40, 100)) + 
      theme_classic() + theme(text = element_text(size = 10), 
                              legend.position = "bottom")
    
  })
  
  output$temp_plot_3 <- renderPlot({
    
    ggplot(reactive_data()[[3]]) + 
      geom_hline(aes(yintercept = 32), col = "lightgray", lty = 2) + 
      geom_line(aes_string(x="calendar_date", y="temp_F", col="river_type")) + 
      labs(col="River type", x="Date", y="River temperature (degrees F)") + 
      scale_color_manual(values = c("#33638d", "#95d840")) + 
      scale_linewidth_manual(values = c(1.5)) + 
      coord_cartesian(ylim = c(30, 75)) + 
      theme_classic() + theme(text = element_text(size = 10), 
                              legend.position = "bottom")
    
  })
  
} 

shinyApp(ui, server) # run the shiny
