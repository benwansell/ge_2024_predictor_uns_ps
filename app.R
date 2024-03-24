library(tidyverse)
library(shiny)
library(rlang)
library(sf)
library(htmlwidgets)
library(cartogram)
library(leaflet)
# library(rgeos)
# library(rgdal)


mps_simple <- read.csv("ge_19_hp_all.csv")
mps_simple <- mps_simple %>% 
  filter(region!="Northern Ireland") %>% 
  filter(constituency_name!="Chorley") %>% 
  filter(row_number()!=539)

mps_simple_new <- read.csv("ge_19_new_boundaries.csv")
mps_simple_new <- mps_simple_new %>%
  filter(region_name!="NI") %>%
  filter(bc_name!="Chorley")

new_demogs <- read.csv("new_const_demog_ons.csv")

mps_simple_new <- mps_simple_new %>% 
  left_join(new_demogs, by = "ons_code")

hex_map <- st_read("map_ge24.shp")

hex_map <- hex_map %>%
  rename(ons_const_id = ONSCnID)

# hex_map_new <- st_read("uk-wpc-hex-constitcode-v4-nov-2023.shp")
#
# hex_map_new <- st_transform(hex_map_new, 4326)
#
# hex_map_new <- hex_map_new %>%
#   rename(ons_code = GSScode) %>%
#   filter(Country!="Northern Ireland")

hex_map_new<-st_read("new_boundaries.shp")






ui <- fluidPage(
  titlePanel("2024 General Election Predictor (UNS and PS)"),
  h5("By Ben Ansell, Nuffield College, University of Oxford,  @benwansell, www.benwansell.com,  benansell.substack.com"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose polling averages"),
      
      numericInput("cons_surv", 
                   h6("Conservative Party"), 
                   value = 44.8) , 
      numericInput("lab_surv", 
                   h6("Labour Party"), 
                   value = 33) , 
      numericInput("lib_surv", 
                   h6("Liberal Democrats"), 
                   value = 11.9) , 
      numericInput("ref_surv", 
                   h6("Reform UK"), 
                   value = 2.1 ) ,
      numericInput("green_surv", 
                   h6("Green Party"), 
                   value = 2.8 ) ,
      numericInput("snp_surv", 
                   h6("SNP"), 
                   value = 4.0 ) ,
      numericInput("pc_surv", 
                   h6("Plaid Cymru"), 
                   value = 0.48 ), 
      sliderInput("tac_param_lab", h5("Tactical Voting by Labour voters"),
                  min = 0, max = 1, value = 0, step = 0.01),
      sliderInput("tac_param_lib", h5("Tactical Voting by Liberal voters"),
                  min = 0, max = 1, value = 0, step = 0.01),
      sliderInput("tac_param_green", h5("Tactical Voting by Green voters"),
                  min = 0, max = 1, value = 0, step = 0.01),
      sliderInput("tac_param_ref", h5("Reform Voters Going Con"),
                  min = 0, max = 1, value = 0, step = 0.01),
      width=3),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  

                  
                  tabPanel("Seats (UNS)", h4("Predicted Seat Counts"),
                           plotOutput("hist_new", height="400px", width="600px")),
                  # 
                  tabPanel("Seats (PS)", h4("Predicted Seat Counts"),
                           plotOutput("hist_ps", height="400px", width="600px")),
                  # 
                  tabPanel("Plot (UNS)",
                           h4("Predicted Seats Won: (click points to see details)"),
                           plotOutput("scatter_new", click = "plot_click", height = "500px", width="700px"),
                           h5(" Constituency (from figure)" ),
                           textOutput("info_new"),
                           p("")),

                  tabPanel("Plot (PS)",
                            h4("Predicted Seats Won: (click points to see details)"),
                            plotOutput("scatter_ps", click = "plot_click", height = "500px", width="700px"),
                            h5(" Constituency (from figure)" ),
                            textOutput("info_ps"),
                            p("")),

                  # 
                  tabPanel("Demographics (UNS)",
                           h4("Demographics:"),
                           selectInput(inputId = "x_demog",
                                       label = "X-axis",
                                       choices = c("Own House: Outright" = "house_own",
                                                   "Own House: Mortgage" = "house_mort",
                                                   "Private Renter" = "house_rent",
                                                   "Social Renter" = "house_soc",
                                                   "Other Housing Tenure" = "house_other",
                                                   "Age Under 15" = "age_under_15",
                                                   "Age 16 to 24" = "age_16_24",
                                                   "Age 25 to 34" = "age_25_34",
                                                   "Age 35 to 49" = "age_35_49",
                                                   "Age 50 to 64" = "age_50_64",
                                                   "Age Over 65" = "age_over_65",
                                                   "Born in UK" = "born_uk",
                                                   "Born in Ireland" = "born_ire",
                                                   "Born in Europe" = "born_europe",
                                                   "Born in Africa" = "born_africa",
                                                   "Born in Asia or Mideast" = "born_asia",
                                                   "Born in Americas" = "born_americas",
                                                   "Born in Australasia" = "born_aust",
                                                   "White British" = "white_british",
                                                   "Other White" = "other_white",
                                                   "Asian" = "asian",
                                                   "Black" = "black",
                                                   "Mixed Ethnicity" = "mixed",
                                                   "Other Ethnicity" = "other_ethnicity",
                                                   "Buddhist" = "rel_bud",
                                                   "Christian" = "rel_christ",
                                                   "Hindu" = "rel_hindu",
                                                   "Jewish"= "rel_jewish",
                                                   "Muslim" = "rel_muslim",
                                                   "Sikh" = "rel_sikh",
                                                   "Other Religion" = "rel_other",
                                                   "Not Religious" = "rel_non"
                                                   ),
                                       selected = "house_own"),
                           selectInput(inputId = "y_demog",
                                       label = "Y-axis",
                                       choices = c("Predicted Conservative Vote" = "Con",
                                                   "Predicted Labour Vote" = "Lab",
                                                   "Predicted Liberal Vote" = "Lib",
                                                   "Predicted Reform Vote" = "Ref",
                                                   "Predicted Green Vote" = "Green",
                                                   "Predicted SNP Vote" = "SNP",
                                                   "Predicted Plaid Vote" = "PC"
                                                   ),
                                       selected = "Con"),
                           plotOutput("scatter_new_demo", click = "plot_click", height = "500px", width="700px"),
                           p(""),
                  h5(" Constituency (from figure)" ),
                  textOutput("info_new_demo")),
                  
                  tabPanel("Demographics (PS)",
                           h4("Demographics:"),
                           selectInput(inputId = "x_demog",
                                       label = "X-axis",
                                       choices = c("Own House: Outright" = "house_own",
                                                   "Own House: Mortgage" = "house_mort",
                                                   "Private Renter" = "house_rent",
                                                   "Social Renter" = "house_soc",
                                                   "Other Housing Tenure" = "house_other",
                                                   "Age Under 15" = "age_under_15",
                                                   "Age 16 to 24" = "age_16_24",
                                                   "Age 25 to 34" = "age_25_34",
                                                   "Age 35 to 49" = "age_35_49",
                                                   "Age 50 to 64" = "age_50_64",
                                                   "Age Over 65" = "age_over_65",
                                                   "Born in UK" = "born_uk",
                                                   "Born in Ireland" = "born_ire",
                                                   "Born in Europe" = "born_europe",
                                                   "Born in Africa" = "born_africa",
                                                   "Born in Asia or Mideast" = "born_asia",
                                                   "Born in Americas" = "born_americas",
                                                   "Born in Australasia" = "born_aust",
                                                   "White British" = "white_british",
                                                   "Other White" = "other_white",
                                                   "Asian" = "asian",
                                                   "Black" = "black",
                                                   "Mixed Ethnicity" = "mixed",
                                                   "Other Ethnicity" = "other_ethnicity",
                                                   "Buddhist" = "rel_bud",
                                                   "Christian" = "rel_christ",
                                                   "Hindu" = "rel_hindu",
                                                   "Jewish"= "rel_jewish",
                                                   "Muslim" = "rel_muslim",
                                                   "Sikh" = "rel_sikh",
                                                   "Other Religion" = "rel_other",
                                                   "Not Religious" = "rel_non"
                                       ),
                                       selected = "house_own"),
                           selectInput(inputId = "y_demog",
                                       label = "Y-axis",
                                       choices = c("Predicted Conservative Vote" = "Con",
                                                   "Predicted Labour Vote" = "Lab",
                                                   "Predicted Liberal Vote" = "Lib",
                                                   "Predicted Reform Vote" = "Ref",
                                                   "Predicted Green Vote" = "Green",
                                                   "Predicted SNP Vote" = "SNP",
                                                   "Predicted Plaid Vote" = "PC"
                                       ),
                                       selected = "Con"),
                           plotOutput("scatter_new_demo_ps", click = "plot_click", height = "500px", width="700px"),
                           p(""),
                           h5(" Constituency (from figure)" ),
                           textOutput("info_new_demo_ps")),


                  tabPanel("Map (UNS)",
                           h4("Map"),
                           leafletOutput("mymap_new", height = "700px")),
                  
                  tabPanel("Map (PS)",
                           h4("Map"),
                           leafletOutput("mymap_ps", height = "700px")),

# 
                  tabPanel("PR",
                           h4("Westminster with Proportional Representation"),
                           plotOutput("pr", height = "400px", width = "600px"))
        ),
      


      downloadButton("downloadData_new", "Download Data (UNS)"),
      downloadButton("downloadData_new_ps", "Download Data (PS)"),
      hr(),
    h5("Difference in Seats between UNS and PS (and with PR)"),
    tableOutput("difference"),
      # 
      # hr(),

      h5("Details for Particular Constituencies (UNS)"),
      selectInput("constituency_new", h6("Choose a Constituency"), mps_simple_new$bc_name),
      textOutput("info2_new"),
      div(style='height:25px'),
      
      hr(),

    h5("Details for Particular Constituencies (PS)"),
    selectInput("constituency_new", h6("Choose a Constituency"), mps_simple_new$bc_name),
    textOutput("info2_new_ps"),
    div(style='height:25px'),

    hr(),

  

      h5("Top Fifty Marginals (UNS)"),
      div(style='height:300px; overflow-y: scroll',
          tableOutput("margin_new")
      ),
    hr(),

    h5("Top Fifty Marginals (PS)"),
    div(style='height:300px; overflow-y: scroll',
         tableOutput("margin_new_ps")
    ),




      hr(),
      p(""),
      p(""),
      h5("Information:"),
      p("Predictions all use Uniform National Swing (UNS) or Proportionate Swing (PS) from GE 2019 on new constituency boundaries. Proportionate Swing only used for Conservatives and Labour in PS model. Small parties always use UNS to prevent very large shifts and this means numbers won't always add to one hundred for PS."),
      p("Dotted line is 325 seats (pure majority). Working majority closer to 320 because of Speakers and Sinn Fein. Tactical voting shifts a proportion of Labour, Liberal,
        and Green voters to the party among these who did best in GE 19. Shifting Reform voters to Conservative only moves Reform voters in that direction. Constituencies are weighted by 2019 absolute turnout. Northern Ireland and Chorley (Speaker) excluded."),
      p("2019 vote share for new boundaries taken from estimates compiled by Professors Colin Rallings and Michael Thrasher on behalf of BBC News, ITV News, Sky News and the Press Association. Calculations for Scotland done by Professor David Denver,
        those for Northern Ireland by Nicholas Whyte. Brexit vote estimates for new boundaries from Chris Hanretty. New boundaries hex map from Philip Brown and Alasdair Rae, Automatic Knowledge Ltd. Demographics are from House of Commons library and ONS - NB Scotland only has age profile."),
      p("All code produced by Ben Ansell. Note: this is an updated version of my previous GE24 calculator which used the old constituency boundaries. It can be found here: https://livedataoxford.shinyapps.io/GE24Simulator/. Articles written about it can be found at https://benansell.substack.com/p/tactical-coping and https://benansell.substack.com/p/tactical-coping-a-postscript")
    )
  )
)

server <- function(input, output) {
  
  
  cons_surv<-reactive({cons_surv <- input$cons_surv})
  lab_surv<-reactive({lab_surv <- input$lab_surv})
  lib_surv<-reactive({lib_surv <- input$lib_surv})
  ref_surv<-reactive({ref_surv <- input$ref_surv})
  green_surv<-reactive({green_surv <- input$green_surv})
  snp_surv<-reactive({snp_surv <- input$snp_surv})
  pc_surv<-reactive({pc_surv <- input$pc_surv})
  
  tac_param_lab <- reactive({tac_param_lab <- input$tac_param_lab})
  tac_param_lib <- reactive({tac_param_lib <- input$tac_param_lib})
  tac_param_green <- reactive({tac_param_green <- input$tac_param_green})
  tac_param_ref <- reactive({tac_param_ref <- input$tac_param_ref})
  
  

  
  # On new boundaries UNS
  
  mps_reactive_new <- reactive({
    
    
    final_mps_new <- mps_simple_new %>%
      mutate ( han_est_norm = leave_hanretty-mean(leave_hanretty),
               lab_min_con_19 = lab_19-con_19,
               con_min_lab_19 = con_19-lab_19,
               weight = (total_votes/mean(total_votes, na.rm=TRUE)),
               cons_adj = con_19 + weight*cons_surv()-weight*(mean(weight*con_19, na.rm=TRUE)),
               cons_adj = case_when(cons_adj<0 ~ 0,
                                    cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                                    cons_adj > 100 ~ 100),
               cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n()),
               lab_adj = lab_19 + weight*lab_surv() - weight*(mean(weight*lab_19, na.rm=TRUE)),
               lab_adj = case_when(lab_adj<0 ~ 0,
                                   lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                                   lab_adj > 100 ~ 100),
               lab_prob = lab_adj, #rnorm(mean = lab_adj, sd=0.03, n()),
               lib_adj = lib_19 + weight*lib_surv() - weight*(mean(weight*lib_19, na.rm=TRUE)),
               lib_adj = case_when(lib_adj<0 ~ 0,
                                   lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                                   lib_adj > 100 ~ 100),
               lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               ref_adj = bxp_19+ weight*ref_surv() - weight*(mean(weight*bxp_19, na.rm=TRUE)),
               ref_adj = case_when(ref_adj<0 ~ 0,
                                   ref_adj>=0 &ref_adj<=100 ~ ref_adj,
                                   ref_adj > 100 ~ 100),
               ref_prob = ref_adj, #rnorm(mean = ref_adj, sd=0.03, n()),
               green_adj = green_19 + weight*green_surv() - weight*(mean(weight*green_19, na.rm=TRUE)), #- green_param() * han_est_norm,
               green_adj = case_when(green_adj<0 ~ 0,
                                     green_adj>=0 &green_adj<=100 ~ green_adj,
                                     green_adj > 100 ~ 100),
               green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               snp_adj = snp_19 + (631/59)*(weight*snp_surv() - weight*(mean(weight*snp_19, na.rm=TRUE))), #{{pc_param}} * han_est_norm,
               snp_adj = case_when(snp_adj<0 ~ 0,
                                   country_name!="Scotland" ~0,
                                   snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                                   snp_adj > 100 ~ 100),
               snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               pc_adj = pc_19 + (631/40)*(weight*pc_surv() - weight*(mean(weight*pc_19, na.rm=TRUE))), #{{pc_param}} * han_est_norm,
               pc_adj = case_when(pc_adj<0 ~ 0,
                                  country_name!="Wales" ~0,
                                  pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                                  pc_adj > 100 ~ 100),
               pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               lab_prob = case_when(lab_19 >= lib_19 & lab_19 >= green_19 ~ tac_param_lib()*(lib_adj)+tac_param_green()*green_adj+lab_adj , 
                                    lab_19 < lib_19 | lab_19 < green_19 ~ (1-tac_param_lab())*lab_adj,
                                    TRUE ~ lab_adj),
               lib_prob = case_when(lib_19 > lab_19 & lib_19 > green_19 ~ tac_param_lab()*(lab_adj)+tac_param_green()*green_adj+lib_adj , 
                                    lib_19 <= lab_19 | lib_19 <= green_19   ~ (1-tac_param_lib())*lib_adj,
                                    TRUE ~ lib_adj),
               green_prob = case_when(green_19 > lab_19 & green_19 > lib_19 ~ tac_param_lab()*(lab_adj)+tac_param_lib()*lib_adj+green_adj , 
                                      green_19 <= lab_19 | green_19 <= lib_19 ~ (1-tac_param_green())*green_adj,
                                      TRUE ~ green_adj),
               cons_prob =  tac_param_ref()*(ref_adj)+cons_adj,
               ref_prob = (1-tac_param_ref())*ref_adj,
               Con = case_when(cons_prob>=0 & cons_prob<=100 ~ round(cons_prob, digits=2),
                               cons_prob>100 ~ 100,
                               cons_prob<0 ~ 0),
               Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ round(lab_prob, digits=2),
                               lab_prob>100 ~ 100,
                               lab_prob<0 ~ 0),
               Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ round(lib_prob, digits=2),
                               lib_prob>100 ~ 100,
                               lib_prob<0 ~ 0),
               Ref = case_when(ref_prob>=0 & ref_prob<=100 ~ round(ref_prob, digits=2),
                               ref_prob>100 ~ 100,
                               ref_prob<0 ~ 0),
               Green =  case_when(green_prob>=0 & green_prob<=100 ~ round(green_prob, digits=2),
                                  green_prob>100 ~ 100,
                                  green_prob<0 ~ 0),
               SNP =  case_when(snp_prob>=0 & snp_prob<=100 ~ round(snp_prob, digits=2),
                                snp_prob>100 ~ 100,
                                snp_prob<0 ~ 0),
               PC = case_when(pc_prob>=0 & pc_prob<=100 ~  round(pc_prob, digits=2),
                              pc_prob>100 ~ 100,
                              pc_prob<0 ~ 0,
                              country_name!="Wales" ~0)
      )
    
    final_mps_w_new <- final_mps_new %>%
      mutate(winner = colnames(final_mps_new[, c("Con", "Lab", "Lib", "Ref", "Green", "SNP", "PC")] )
             [max.col(final_mps_new[, c("Con", "Lab", "Lib", "Ref", "Green", "SNP", "PC")] ,ties.method="first")] )
    
    margin_data_new <- final_mps_w_new %>%
      pivot_longer(cols= c(Con, Lab, Lib, Ref, Green, SNP, PC), names_to ="votes") %>%
      group_by(bc_name) %>%
      top_n(2, value) %>%
      summarise(margin = max(value)-min(value))
    
    final_mps_w_new <- final_mps_w_new %>%
      left_join(margin_data_new, by="bc_name")
    
    final_mps_w_new
  })
  
  # New boundaries Prop Swing for Con and Lab
  
  mps_reactive_new_prop <- reactive({
    
    
    final_mps_new_prop <- mps_simple_new %>%
      mutate ( han_est_norm = leave_hanretty-mean(leave_hanretty),
               lab_min_con_19 = lab_19-con_19,
               con_min_lab_19 = con_19-lab_19,
               weight = (total_votes/mean(total_votes, na.rm=TRUE)),
               cons_adj = con_19*cons_surv()/44.8,
               cons_adj = case_when(cons_adj<0 ~ 0,
                                    cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                                    cons_adj > 100 ~ 100),
               cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n()),
               lab_adj = lab_19*lab_surv()/33,
               lab_adj = case_when(lab_adj<0 ~ 0,
                                   lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                                   lab_adj > 100 ~ 100),
               lab_prob = lab_adj, #rnorm(mean = lab_adj, sd=0.03, n()),
               lib_adj = lib_19 + weight*lib_surv() - weight*(mean(weight*lib_19, na.rm=TRUE)),
               lib_adj = case_when(lib_adj<0 ~ 0,
                                   lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                                   lib_adj > 100 ~ 100),
               lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               ref_adj = bxp_19+ weight*ref_surv() - weight*(mean(weight*bxp_19, na.rm=TRUE)),
               ref_adj = case_when(ref_adj<0 ~ 0,
                                   ref_adj>=0 &ref_adj<=100 ~ ref_adj,
                                   ref_adj > 100 ~ 100),
               ref_prob = ref_adj, #rnorm(mean = ref_adj, sd=0.03, n()),
               green_adj = green_19 + weight*green_surv() - weight*(mean(weight*green_19, na.rm=TRUE)), #- green_param() * han_est_norm,
               green_adj = case_when(green_adj<0 ~ 0,
                                     green_adj>=0 &green_adj<=100 ~ green_adj,
                                     green_adj > 100 ~ 100),
               green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               snp_adj = snp_19 + (631/59)*(weight*snp_surv() - weight*(mean(weight*snp_19, na.rm=TRUE))), #{{pc_param}} * han_est_norm,
               snp_adj = case_when(snp_adj<0 ~ 0,
                                   country_name!="Scotland" ~0,
                                   snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                                   snp_adj > 100 ~ 100),
               snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               pc_adj = pc_19 + (631/40)*(weight*pc_surv() - weight*(mean(weight*pc_19, na.rm=TRUE))), #{{pc_param}} * han_est_norm,
               pc_adj = case_when(pc_adj<0 ~ 0,
                                  country_name!="Wales" ~0,
                                  pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                                  pc_adj > 100 ~ 100),
               pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               lab_prob = case_when(lab_19 >= lib_19 & lab_19 >= green_19 ~ tac_param_lib()*(lib_adj)+tac_param_green()*green_adj+lab_adj , 
                                    lab_19 < lib_19 | lab_19 < green_19 ~ (1-tac_param_lab())*lab_adj,
                                    TRUE ~ lab_adj),
               lib_prob = case_when(lib_19 > lab_19 & lib_19 > green_19 ~ tac_param_lab()*(lab_adj)+tac_param_green()*green_adj+lib_adj , 
                                    lib_19 <= lab_19 | lib_19 <= green_19   ~ (1-tac_param_lib())*lib_adj,
                                    TRUE ~ lib_adj),
               green_prob = case_when(green_19 > lab_19 & green_19 > lib_19 ~ tac_param_lab()*(lab_adj)+tac_param_lib()*lib_adj+green_adj , 
                                      green_19 <= lab_19 | green_19 <= lib_19 ~ (1-tac_param_green())*green_adj,
                                      TRUE ~ green_adj),
               cons_prob =  tac_param_ref()*(ref_adj)+cons_adj,
               ref_prob = (1-tac_param_ref())*ref_adj,
               Con = case_when(cons_prob>=0 & cons_prob<=100 ~ round(cons_prob, digits=2),
                               cons_prob>100 ~ 100,
                               cons_prob<0 ~ 0),
               Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ round(lab_prob, digits=2),
                               lab_prob>100 ~ 100,
                               lab_prob<0 ~ 0),
               Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ round(lib_prob, digits=2),
                               lib_prob>100 ~ 100,
                               lib_prob<0 ~ 0),
               Ref = case_when(ref_prob>=0 & ref_prob<=100 ~ round(ref_prob, digits=2),
                               ref_prob>100 ~ 100,
                               ref_prob<0 ~ 0),
               Green =  case_when(green_prob>=0 & green_prob<=100 ~ round(green_prob, digits=2),
                                  green_prob>100 ~ 100,
                                  green_prob<0 ~ 0),
               SNP =  case_when(snp_prob>=0 & snp_prob<=100 ~ round(snp_prob, digits=2),
                                snp_prob>100 ~ 100,
                                snp_prob<0 ~ 0),
               PC = case_when(pc_prob>=0 & pc_prob<=100 ~  round(pc_prob, digits=2),
                              pc_prob>100 ~ 100,
                              pc_prob<0 ~ 0,
                              country_name!="Wales" ~0)
      )
    
    final_mps_w_new_prop <- final_mps_new_prop %>%
      mutate(winner = colnames(final_mps_new_prop[, c("Con", "Lab", "Lib", "Ref", "Green", "SNP", "PC")] )
             [max.col(final_mps_new_prop[, c("Con", "Lab", "Lib", "Ref", "Green", "SNP", "PC")] ,ties.method="first")] )
    
    margin_data_new_prop <- final_mps_w_new_prop %>%
      pivot_longer(cols= c(Con, Lab, Lib, Ref, Green, SNP, PC), names_to ="votes") %>%
      group_by(bc_name) %>%
      top_n(2, value) %>%
      summarise(margin = max(value)-min(value))
    
    final_mps_w_new_prop <- final_mps_w_new_prop %>%
      left_join(margin_data_new_prop, by="bc_name")
    
    final_mps_w_new_prop
  })

  
  # New Boundaries histogram UNS
  

  output$hist_new<- renderPlot({

    no_parties<- length(unique(mps_reactive_new()$winner))

    if (no_parties==7){

    mps_reactive_new() %>%
      ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats", fill="Winner")+
        geom_hline(yintercept = 325, linetype = "dotted")+
      scale_fill_manual(limits = c("Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                        values=c("blue", "green", "red", "orange", "#40E0D0", "gold", "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
      #  ylim(c(0, 632))
    }
    else {
      mps_reactive_new() %>%
        ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats", fill="Winner")+
        geom_hline(yintercept = 325, linetype = "dotted")+
        scale_fill_manual(limits = c("Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                          values=c("blue", "green", "red", "orange", "#40E0D0", "gold", "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
       # ylim(c(0, 632))
    }

  })
  
  output$hist_ps<- renderPlot({
    
    no_parties<- length(unique(mps_reactive_new_prop()$winner))
    
    if (no_parties==7){
      
      mps_reactive_new_prop() %>%
        ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats", fill="Winner")+
        geom_hline(yintercept = 325, linetype = "dotted")+
        scale_fill_manual(limits = c("Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                          values=c("blue", "green", "red", "orange", "#40E0D0", "gold", "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
      #  ylim(c(0, 632))
    }
    else {
      mps_reactive_new_prop() %>%
        ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats", fill="Winner")+
        geom_hline(yintercept = 325, linetype = "dotted")+
        scale_fill_manual(limits = c("Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                          values=c("blue", "green", "red", "orange", "#40E0D0", "gold", "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
      # ylim(c(0, 632))
    }
    
  })
  


  # New boundaries scatter UNS

  output$scatter_new <- renderPlot({
    no_parties<- length(unique(mps_reactive_new()$winner))

    if (no_parties==7){
      mps_reactive_new()  %>%
        ggplot(aes(x=con_min_lab_19, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                          values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+theme_classic()+
        xlab("Conservative Lead over Labour 2019") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none")
    }
    else{
      mps_reactive_new()  %>%
        ggplot(aes(x=con_min_lab_19, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                           values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+theme_classic()+
        xlab("Conservative Lead over Labour 2019") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none")
    }
  })
  
  output$scatter_ps <- renderPlot({
    no_parties<- length(unique(mps_reactive_new_prop()$winner))
    
    if (no_parties==7){
      mps_reactive_new_prop()  %>%
        ggplot(aes(x=con_min_lab_19, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                           values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+theme_classic()+
        xlab("Conservative Lead over Labour 2019") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none")
    }
    else{
      mps_reactive_new_prop()  %>%
        ggplot(aes(x=con_min_lab_19, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                           values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+theme_classic()+
        xlab("Conservative Lead over Labour 2019") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none")
    }
  })
  
  # New boundaries demog scatter UNS
  
  output$scatter_new_demo <- renderPlot({
    
    no_parties<- length(unique(mps_reactive_new()$winner))
  
    if (no_parties==7){
    mps_reactive_new()  %>%
          ggplot(aes_string(x=input$x_demog, y = input$y_demog))+geom_text(aes(label = winner, color = winner, alpha=0.5))+
          geom_smooth(method = "lm", alpha = 0.2, color = "grey")+
          scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                             values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+
          theme_classic()+
          xlab(paste0("Demographic: ", input$x_demog)) +ylab(paste0("Predicted Vote for ", input$y_demog))+theme(legend.position = "none")
    }
    
    else{ mps_reactive_new()  %>%
        ggplot(aes_string(x=input$x_demog, y = input$y_demog))+geom_text(aes(label = winner, color = winner, alpha=0.5))+
        geom_smooth(method="lm", alpha = 0.2, color = "grey")+
        scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                           values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+
        theme_classic()+
        xlab(paste0("Demographic: ", input$x_demog)) +ylab(paste0("Predicted Vote for ", input$y_demog))+theme(legend.position = "none")
    }
    

  })
  
  output$scatter_new_demo_ps <- renderPlot({
    
    no_parties<- length(unique(mps_reactive_new_prop()$winner))
    
    if (no_parties==7){
      mps_reactive_new_prop()  %>%
        ggplot(aes_string(x=input$x_demog, y = input$y_demog))+geom_text(aes(label = winner, color = winner, alpha=0.5))+
        geom_smooth(method = "lm", alpha = 0.2, color = "grey")+
        scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                           values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+
        theme_classic()+
        xlab(paste0("Demographic: ", input$x_demog)) +ylab(paste0("Predicted Vote for ", input$y_demog))+theme(legend.position = "none")
    }
    
    else{ mps_reactive_new_prop()  %>%
        ggplot(aes_string(x=input$x_demog, y = input$y_demog))+geom_text(aes(label = winner, color = winner, alpha=0.5))+
        geom_smooth(method="lm", alpha = 0.2, color = "grey")+
        scale_color_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                           values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+
        theme_classic()+
        xlab(paste0("Demographic: ", input$x_demog)) +ylab(paste0("Predicted Vote for ", input$y_demog))+theme(legend.position = "none")
    }
    
    
  })
  

  output$pr <- renderPlot({
    
    cons_surv<-reactive({cons_surv <- input$cons_surv})
    lab_surv<-reactive({lab_surv <- input$lab_surv})
    lib_surv<-reactive({lib_surv <- input$lib_surv})
    ref_surv<-reactive({ref_surv <- input$ref_surv})
    green_surv<-reactive({green_surv <- input$green_surv})
    snp_surv<-reactive({snp_surv <- input$snp_surv})
    pc_surv<-reactive({pc_surv <- input$pc_surv})
    
    pr <- data.frame(Party = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                     PR_Seats  =c(round(cons_surv()*6.31), round(green_surv()*6.31), round(lab_surv()*6.31), 
                                  round(lib_surv()*6.31), round(ref_surv()*6.31), 
                                  round(snp_surv()*6.31), round(pc_surv()*6.31)))
    
    maj <- mps_reactive_new() %>% 
      group_by(winner) %>% 
      count() %>% 
      rename(Party = winner)
    
    #aes(x = Party, y = PR_Seats, fill = Party, label = PR_Seats)
    pr %>% 
      ggplot()+
      geom_col(data = pr, aes(x = Party, y = PR_Seats, fill = Party))+
      geom_col(data = maj, aes(x=Party, y = n, color = "black", fill = Party), alpha=0.1)+
      geom_hline(yintercept = 325, linetype = "dotted")+
      geom_text(data = pr, vjust=-.5, aes(x = Party, y = PR_Seats, label = paste("PR:", PR_Seats)))+
      geom_text(data = maj, vjust = +1.5, aes(x=Party, y = 0, label = paste("FPTP:", n)))+
      scale_fill_manual(limits = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                        values=c("blue", "green", "red", "orange", "#40E0D0", "gold" , "dark green"))+
      labs(y="Seats Under PR")+
      scale_colour_discrete(labels =c("FPTP (UNS)"), name = "")+
      theme_classic()
    
  })
  


  output$mymap_new <- renderLeaflet({

    winner_sf_new <- hex_map_new %>%
      left_join(mps_reactive_new() , by = "ons_code") %>%
      mutate(winner_color =  case_when(winner=="Con" ~ "blue",
                                       winner=="Green"  ~ "green",
                                       winner=="Lab" ~ "red",
                                       winner=="Lib" ~ "orange",
                                       winner=="Ref" ~ "#40E0D0",
                                       winner=="SNP" ~ "gold",
                                       winner=="PC" ~ "dark green",
                                       TRUE ~ "purple"))


    labels <- paste0(
      "Constituency: ", hex_map_new$bc_name, "</br>",
      "Winner in GE19: ", hex_map_new$winner_19
    ) %>% lapply(htmltools::HTML)

    leaflet(options=leafletOptions(
      dragging = T, zoomControl = F, tap = T,
      minZoom = 5.4, maxZoom = 7, maxBounds = list(list(1,-7.5),list(58.5,50.0)),
      attributionControl = T),
      winner_sf_new) %>%
      addPolygons(
        color = "grey",
        weight=0.75,
        opacity = 0.5,
        fillOpacity = 1,
        fillColor = ~winner_color,
        label = paste0(
          "Constituency: ", winner_sf_new$BCName, "</br>",
          "Winner in GE19: ", winner_sf_new$winner_19, "</br>",
          "Winner in GE24: ", winner_sf_new$winner
        ) %>% lapply(htmltools::HTML)
      )%>%

      htmlwidgets::onRender(
        "function(x, y) {
        var myMap = this;
        myMap._container.style['background'] = '#fff';
    }")%>%
      mapOptions(zoomToLimits = "first")
  })
  
  
  output$mymap_ps <- renderLeaflet({
    
    winner_sf_new <- hex_map_new %>%
      left_join(mps_reactive_new_prop() , by = "ons_code") %>%
      mutate(winner_color =  case_when(winner=="Con" ~ "blue",
                                       winner=="Green"  ~ "green",
                                       winner=="Lab" ~ "red",
                                       winner=="Lib" ~ "orange",
                                       winner=="Ref" ~ "#40E0D0",
                                       winner=="SNP" ~ "gold",
                                       winner=="PC" ~ "dark green",
                                       TRUE ~ "purple"))
    
    
    labels <- paste0(
      "Constituency: ", hex_map_new$bc_name, "</br>",
      "Winner in GE19: ", hex_map_new$winner_19
    ) %>% lapply(htmltools::HTML)
    
    leaflet(options=leafletOptions(
      dragging = T, zoomControl = F, tap = T,
      minZoom = 5.4, maxZoom = 7, maxBounds = list(list(1,-7.5),list(58.5,50.0)),
      attributionControl = T),
      winner_sf_new) %>%
      addPolygons(
        color = "grey",
        weight=0.75,
        opacity = 0.5,
        fillOpacity = 1,
        fillColor = ~winner_color,
        label = paste0(
          "Constituency: ", winner_sf_new$BCName, "</br>",
          "Winner in GE19: ", winner_sf_new$winner_19, "</br>",
          "Winner in GE24: ", winner_sf_new$winner
        ) %>% lapply(htmltools::HTML)
      )%>%
      
      htmlwidgets::onRender(
        "function(x, y) {
        var myMap = this;
        myMap._container.style['background'] = '#fff';
    }")%>%
      mapOptions(zoomToLimits = "first")
  })
  
  # 

  output$info_new <- renderPrint({
    nearData<-nearPoints(mps_reactive_new(), input$plot_click)
    c<-paste( nearData$bc_name, ",", sep="", " C:", nearData$Con,
              " L:", nearData$Lab, " LD:", nearData$Lib, " Ref:", nearData$Ref, " G:", nearData$Green, " SNP:", nearData$SNP, " PC:", nearData$PC)
    c
  })
  
  output$info_ps <- renderPrint({
    nearData<-nearPoints(mps_reactive_new_prop(), input$plot_click)
    c<-paste( nearData$bc_name, ",", sep="", " C:", nearData$Con,
              " L:", nearData$Lab, " LD:", nearData$Lib, " Ref:", nearData$Ref, " G:", nearData$Green, " SNP:", nearData$SNP, " PC:", nearData$PC)
    c
  })
  
  output$info_new_demo <- renderPrint({
    nearData<-nearPoints(mps_reactive_new(), input$plot_click)
    c<-paste( nearData$bc_name, ",", sep="", " C:", nearData$Con,
              " L:", nearData$Lab, " LD:", nearData$Lib, " Ref:", nearData$Ref, " G:", nearData$Green, " SNP:", nearData$SNP, " PC:", nearData$PC)
    c
  })
  
  output$info_new_demo_ps <- renderPrint({
    nearData<-nearPoints(mps_reactive_new_prop(), input$plot_click)
    c<-paste( nearData$bc_name, ",", sep="", " C:", nearData$Con,
              " L:", nearData$Lab, " LD:", nearData$Lib, " Ref:", nearData$Ref, " G:", nearData$Green, " SNP:", nearData$SNP, " PC:", nearData$PC)
    c
  })
  
  
  

  

  output$info2_new <- renderText({
    d<-mps_reactive_new() %>%
      filter(bc_name==input$constituency_new) %>%
      select(margin, Con, Lab, Lib, Ref, Green, SNP, PC)

    #d<-d[,c("display_as_19", "margin", "Con")]

    paste( "Margin:", d$margin, " C:", d$Con, " L:", d$Lab, " LD:", d$Lib,
          " Ref:", d$Ref, " G:", d$Green, " SNP:", d$SNP, " PC:", d$PC, sep="")

  })
  
  output$info2_new_ps <- renderText({
    d<-mps_reactive_new_prop() %>%
      filter(bc_name==input$constituency_new) %>%
      select(margin, Con, Lab, Lib, Ref, Green, SNP, PC)
    
    #d<-d[,c("display_as_19", "margin", "Con")]
    
    paste( "Margin:", d$margin, " C:", d$Con, " L:", d$Lab, " LD:", d$Lib,
           " Ref:", d$Ref, " G:", d$Green, " SNP:", d$SNP, " PC:", d$PC, sep="")
    
  })
  
  
  # 
  
  
  
  
  output$downloadData_new <- downloadHandler(
    filename = "uk_2024_new_estimates.csv",
    content = function(file) {
      write.csv(mps_reactive_new() , file, row.names = FALSE)
    }
  )
  
  output$downloadData_new_ps <- downloadHandler(
    filename = "uk_2024_new_estimates.csv",
    content = function(file) {
      write.csv(mps_reactive_new_prop() , file, row.names = FALSE)
    }
  )

  
  # 
  output$margin_new <- renderTable({
    tab <- mps_reactive_new() %>%
      arrange(margin) %>%
      top_n(-50, margin) %>%
      select(bc_name, Win19, winner, margin) %>%
      rename(Constituency = bc_name, `GE 19 Party` = Win19, `GE 24 Winner` = winner, `Margin of Victory` = margin)
    # %>%
    #   arrange(margin) %>%
    #   top_n(5, margin) %>%
    #   select(constituency_name, display_as, margin)

    tab
  })
  
  output$margin_new_ps <- renderTable({
    tab <- mps_reactive_new_prop() %>%
      arrange(margin) %>%
      top_n(-50, margin) %>%
      select(bc_name, Win19, winner, margin) %>%
      rename(Constituency = bc_name, `GE 19 Party` = Win19, `GE 24 Winner` = winner, `Margin of Victory` = margin)
    # %>%
    #   arrange(margin) %>%
    #   top_n(5, margin) %>%
    #   select(constituency_name, display_as, margin)
    
    tab
  })
  
  output$difference <- renderTable({
    uns_bounds <- mps_reactive_new() %>%
      count(winner) %>%
      rename(UNS = n)
    ps_bounds <- mps_reactive_new_prop() %>%
      count(winner) %>%
      rename(PS = n)
    combined <- left_join(uns_bounds, ps_bounds) %>%
      mutate(`PS-UNS` = PS-UNS) %>%
      rename(Winner = winner)
    
    cons_surv<-reactive({cons_surv <- input$cons_surv})
    lab_surv<-reactive({lab_surv <- input$lab_surv})
    lib_surv<-reactive({lib_surv <- input$lib_surv})
    ref_surv<-reactive({ref_surv <- input$ref_surv})
    green_surv<-reactive({green_surv <- input$green_surv})
    snp_surv<-reactive({snp_surv <- input$snp_surv})
    pc_surv<-reactive({pc_surv <- input$pc_surv})
    
    pr <- data.frame(Winner = c( "Con", "Green", "Lab", "Lib", "Ref", "SNP", "PC"),
                     PR_Seats  =c(round(cons_surv()*6.31), round(green_surv()*6.31), round(lab_surv()*6.31),
                                  round(lib_surv()*6.31), round(ref_surv()*6.31),
                                  round(snp_surv()*6.31), round(pc_surv()*6.31)))
    
    combined <- left_join(combined, pr, by="Winner") %>%
      mutate(PR_Seats = as.integer(PR_Seats),
             uns_min_PR = UNS - PR_Seats,
             ps_min_PR = PS - PR_Seats) %>%
      rename(`PR Seats` = PR_Seats,
             `UNS - PR` = uns_min_PR,
             `PS - PR` = ps_min_PR)
    
    combined
    
  })
  
  
  
}

shinyApp(ui, server)

