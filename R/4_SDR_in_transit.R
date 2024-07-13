##Clear environment, if needed
rm(list = ls())

#### Load packages

#install
# install.packages('visdat')
# install.packages("remotes")
# install.packages("DiagrammeR")
# install.packages('dplyr')
# install.packages("tidyverse")
# install.packages("srvyr")
# install.packages("writexl")
# install.packages("janitor")
# install.packages("robotoolbox")

##load
library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(robotoolbox)
library(labelled)
library(remotes)
library(dm)
library(janitor)
library(visdat)
library(dplyr)
library(writexl)



###Once you enter, you will receive your token from KoBO which you need to insert as below
kobo_setup(url = "https://kobo.unhcr.org", token = "5c3dbc61503ba4cedbc0e39b87589950eb2d31c4")


###Run the script below to see list of your surveys
asset_list <- kobo_asset_list()
asset_list

##Find the survey you want to analyse and enter the name as below

uid <- filter(asset_list, name == "Monitoreo de movimientos mixtos") |> ## change the survey name accordingly as shown in KoBo
  pull(uid)

###You will see the number of submissions and name.

asset <- kobo_asset(uid)
asset

###Your data frame will be displayed here without the need for you to download it from KoBo
df_mmm <- kobo_data(asset)

###Filtering for correct period
# 1 abril - 17 de junio las cifras del MMM para en transito

main_mmm <- pull_tbl(df_mmm, main, keyed = TRUE)%>% 
  filter(A03_date >= "2024-04-01" & end <= "2024-06-17" ,
         A07_consent == "yes")%>% 
  mutate(R4V_transit_pop = 
           ifelse( B03_nationality == "VEN",
                   "Venezuela",
                   "Other nationality"))
ind_mmm <- pull_tbl(df_mmm, family_status_countries, keyed = TRUE)

#Select the ones with consent

###Clean dataset

#confirm by using the script below to see if there are any duplicates, if there are, please delete

duplicated(main_mmm) # Check if there are any duplicates
sum(duplicated(main_mmm)) # Number of duplicates
get_dupes(main_mmm)

duplicated(ind_mmm) # Check if there are any duplicates
sum(duplicated(ind_mmm)) # Number of duplicates
get_dupes(ind_mmm)

vis_dat(main_mmm) # Example of a missing ind heatmap using the `visdat` package
vis_dat(ind_mmm) # Example of a missing ind heatmap using the `visdat` package


#Venezuelan population
#Indicators
main_mmm <- main_mmm %>%
  filter(R4V_transit_pop == "Venezuela") %>%
  mutate(
    # Education
    IND_EDU_T1 = ifelse(EDU_T_Q1 == "none" & !is.na(EDU_T_Q1), 1, 0),
    
    # Integration
    IND_INT_T1 = ifelse(INT_T_Q1 %in% c("none", "charity", "humanitarian_assistance") & !is.na(INT_T_Q1), 1, 0),
    
    # Health
    IND_HE = ifelse(HE_T_Q1 == "yes" & HE_T_Q2 == "no" & !is.na(HE_T_Q1) & !is.na(HE_T_Q2), 1, 0),
    
    # Food Security
    FS_T2 = ifelse(F03_Nmeals %in% c("none", "one") & !is.na(F03_Nmeals), 1, 0),
    FS_T3 = ifelse(F04_Food_Sec != "no_difficulties" & !is.na(F04_Food_Sec), 1, 0),
    IND_FS = ifelse(FS_T2 == 1 | FS_T3 == 1, 1, 0),
    
    # Shelter
    IND_SHE_T1 = ifelse(SHE_T_Q1 %in% c("street_squat", "daily_payment", "shelter_temporary", "colective_center", "informal_setting", "transport", "dont_know", "prefer_not_answer") & !is.na(SHE_T_Q1), 1, 0),
    IND_SHE_T2 = ifelse(SHE_T_Q2 != "none" & !is.na(SHE_T_Q2), 1, 0),
    
    # WASH
    IND_WA_T1 = ifelse(!WASH_T_Q2 %in% c("tap_water_free", "shelter") & !is.na(WASH_T_Q2), 1, 0),
    IND_WA_T2 = ifelse(!WASH_T_Q3 %in% c("establishment_unpaid", "shelter") & !is.na(WASH_T_Q3), 1, 0),
    WA_T4 = ifelse(WASH_T_Q4 != "shelter" & !is.na(WASH_T_Q4), 1, 0),
    WA_T5 = ifelse(WASH_T_Q5 %in% c("no", "pads", "cloth", "toilet_paper", "not_applicable", "other") & !is.na(WASH_T_Q5), 1, 0),
    IND_WA_T3 = ifelse(WA_T4 == 1 | WA_T5 == 1, 1, 0),
    
    # Nutrition
    IND_NUT_T1 = ifelse((B13_pregnant == "yes" | (B14_lactating == "yes" & H02_breastfed_problems == "yes")) & !is.na(B13_pregnant) & !is.na(B14_lactating) & !is.na(H02_breastfed_problems), 1, 0),
    NUT_T4 = ifelse(H03_humanitarian_services_weight_check == 0 | H03_humanitarian_services_nutrition_counseling == 0 | H03_humanitarian_services_health_counseling == 0 | H03_humanitarian_services_breastfeed_counseling == 0 | H03_humanitarian_services_none == 1, 1, 0),
    NUT_T5 = ifelse(H01_breastfed == "yes" & H02_breastfed_exclusively == "none_excl" & !is.na(H01_breastfed) & !is.na(H02_breastfed_exclusively), 0, 1),
    NUT_T8_6_23 = ifelse(II03_humanitarian_services_parent_counseling == 0 & II03_humanitarian_services_supplement == 0 & II03_humanitarian_services_weight_check == 0 & II03_humanitarian_services_nutrition_counseling == 0, 1, 0),
    NUT_T8_24_59 = ifelse(I03_humanitarian_services_weight_check3 == 0 & I03_humanitarian_services_nutrition_counseling3 == 0 & I03_humanitarian_services_supplement3 == 0, 1, 0),
    NUT_T8 = ifelse(NUT_T8_6_23 == 1 | NUT_T8_24_59 == 1, 1, 0),
    NUT_T10_6_23 = ifelse(II01_foodpoverty_leche_materna + II01_foodpoverty_granos_raices + II01_foodpoverty_legumbres_frijoles + II01_foodpoverty_lacteos + II01_foodpoverty_carnes_pescado + II01_foodpoverty_huevo + II01_foodpoverty_verduras + II01_foodpoverty_otras_verduras < 5 & !is.na(II01_foodpoverty_leche_materna) & !is.na(II01_foodpoverty_granos_raices) & !is.na(II01_foodpoverty_legumbres_frijoles) & !is.na(II01_foodpoverty_lacteos) & !is.na(II01_foodpoverty_carnes_pescado) & !is.na(II01_foodpoverty_huevo) & !is.na(II01_foodpoverty_verduras) & !is.na(II01_foodpoverty_otras_verduras), 1, 0),
    NUT_T10_24_59 = ifelse(II01_foodpoverty_leche_materna + I01_foodpoverty_granos_raices + I01_foodpoverty_legumbres_frijoles + I01_foodpoverty_lacteos + I01_foodpoverty_carnes_pescado + I01_foodpoverty_huevo + I01_foodpoverty_verduras + I01_foodpoverty_otras_verduras < 5 & !is.na(II01_foodpoverty_leche_materna) & !is.na(I01_foodpoverty_granos_raices) & !is.na(I01_foodpoverty_legumbres_frijoles) & !is.na(I01_foodpoverty_lacteos) & !is.na(I01_foodpoverty_carnes_pescado) & !is.na(I01_foodpoverty_huevo) & !is.na(I01_foodpoverty_verduras) & !is.na(I01_foodpoverty_otras_verduras), 1, 0),
    NUT_T10 = ifelse(NUT_T10_6_23 == 1 | NUT_T10_24_59 == 1, 1, 0),
    IND_NUT_T2 = ifelse(NUT_T4 == 1 | NUT_T8 == 1, 1, 0),
    IND_NUT_T3 = ifelse(NUT_T10 == 1 | NUT_T5 == 1, 1, 0),
    
    # Protection General
    IND_PRO_T1 = ifelse(E02_incidents != "none" & !is.na(E02_incidents), 1, 0),
    PRO_T3 = ifelse(F06_Mainneeds_information == 1 & !is.na(F06_Mainneeds_information), 1, 0),
    PRO_T4 = ifelse(E07_risk_return == "yes" & (E04_reasons_not_return_discrimination == 1 | E04_reasons_not_return_pol_instability == 1) & !is.na(E07_risk_return) & !is.na(E04_reasons_not_return_discrimination) & !is.na(E04_reasons_not_return_pol_instability), 1, 0),
    IND_PRO_T2 = ifelse(PRO_T3 == 1 | PRO_T4 == 1, 1, 0),
    
    # Child Protection
    CP_T1 = ifelse(`F08_Mainneeds_6months-17years_psych` == 1 & !is.na(`F08_Mainneeds_6months-17years_psych`) | `F08_Mainneeds_6months-17years_safe_spaces` == 1 & !is.na(`F08_Mainneeds_6months-17years_psych`), 1, 0),
    CP_T2 = ifelse(B12_children_separated == "yes" & !is.na(B12_children_separated), 1, 0),
    IND_PRO_CP = ifelse(CP_T1 == 1 | CP_T2 == 1, 1, 0),
    
    # Human Trafficking and Smuggling
    HTS_T1 = ifelse(E02_incidents_abductionORkidnapping == 1 | E02_incidents_arrestORdetention == 1 | E02_incidents_fraud == 1 & !is.na(E02_incidents_abductionORkidnapping) & !is.na(E02_incidents_arrestORdetention) & !is.na(E02_incidents_fraud), 1, 0),
    HTS_T2 = ifelse(E02_incidents_Exploitationwork == 1 & !is.na(E02_incidents_Exploitationwork), 1, 0),
    HTS_T3 = ifelse(E02_incidents_threat == 1 | E02_incidents_physicalAssault == 1 | E02_incidents_abductionORkidnapping == 1 | E02_incidents_extorsion == 1 & !is.na(E02_incidents_threat) & !is.na(E02_incidents_physicalAssault) & !is.na(E02_incidents_abductionORkidnapping) & !is.na(E02_incidents_extorsion), 1, 0),
    IND_HTS = ifelse(HTS_T1 == 1 | HTS_T2 == 1 | HTS_T3 == 1, 1, 0),
    
    # GBV
    IND_PRO_GBV = ifelse(G01_Spec_needs_woman_at_risk == 1 | G01_Spec_needs_victimsurvivor == 1 & !is.na(G01_Spec_needs_woman_at_risk) & !is.na(G01_Spec_needs_victimsurvivor), 1, 0)
  )

               #calculate PiN in transit -MPI intersector and sectoral PiNs

pins_indicator_ven <- main_mmm %>%
  select(c(B06_travel_with_children, 
           B13_pregnant, 
           B14_lactating, 
           #Education
           IND_EDU_T1,
           #Integration
           IND_INT_T1,
           #Health
           IND_HE,
           #Food Security
           IND_FS,
           #Shelter
           IND_SHE_T1,
           IND_SHE_T2, 
           #WASH
           IND_WA_T1,
           IND_WA_T2,
           IND_WA_T3,
           #Nutrition
           IND_NUT_T1,
           IND_NUT_T2,
           IND_NUT_T3,
           #Protection General
           IND_PRO_T1,
           IND_PRO_T2,
           #Child Protection
           IND_PRO_CP,
           #Human Trafficking and Smuggling
           IND_HTS,
           #GBV
           IND_PRO_GBV))

  #Multiply each column of pins_indicator by the respective weight in df_weights
  
  weighted_pins_indicator_ven <- pins_indicator_ven %>%
  mutate(across(c(IND_FS,
                  IND_HE,
                  IND_HTS,
                  IND_NUT_T1,
                  IND_NUT_T2,
                  IND_NUT_T3,
                  IND_PRO_T1,
                  IND_PRO_T2,
                  IND_PRO_CP,
                  IND_PRO_GBV,
                  IND_SHE_T1,
                  IND_SHE_T2,
                  IND_WA_T1,
                  IND_WA_T2,
                  IND_WA_T3,
                  IND_EDU_T1,
                  IND_INT_T1), ~ . * 
                  df_weights$ind_weight_per_sector[match(cur_column(), df_weights$ind_per_sector)]))

#Calculate sectorial deprivation score ds suming weighted indicators per sector

ds_per_sector_ven <- weighted_pins_indicator_ven %>%
  mutate(
    Food_security_ds = IND_FS,
    Health_ds =  IND_HE,
    Integration_ds = rowSums(weighted_pins_indicator_ven[, c("IND_INT_T1")], na.rm = TRUE),
    Nutrition_ds = rowSums(weighted_pins_indicator_ven[, c("IND_NUT_T1", "IND_NUT_T2", "IND_NUT_T3" )], na.rm = TRUE),
    Nutrition_ds_women = rowSums(weighted_pins_indicator_ven[, c("IND_NUT_T1" )], na.rm = TRUE),
    Nutrition_ds_baby = rowSums(weighted_pins_indicator_ven[, c("IND_NUT_T2", "IND_NUT_T3" )], na.rm = TRUE),
    Protection_ds = rowSums(weighted_pins_indicator_ven[, c("IND_PRO_T1", "IND_PRO_T2", "IND_PRO_CP", "IND_PRO_GBV", "IND_HTS")], na.rm = TRUE),
    Child_protection_ds = IND_PRO_CP,
    Gender_based_violence_ds = IND_PRO_GBV,
    HT_S_ds = IND_HTS,
    Shelter_ds = rowSums(weighted_pins_indicator_ven[, c("IND_SHE_T1", "IND_SHE_T2")], na.rm = TRUE),
    Wash_ds = rowSums(weighted_pins_indicator_ven[, c("IND_WA_T1", "IND_WA_T2", "IND_WA_T3" )], na.rm = TRUE),
    Education_ds = rowSums(weighted_pins_indicator_ven[, c("IND_EDU_T1")], na.rm = TRUE)
  )%>%
  rename()

### Step 2: Calculate the intersectorial MPI using the scores from each sector 
#and using the 33.3% cutoff to identify those to the right of the distribution.
#Exclude subsectors - CP, HT and GBV (only calculate intersectoral MPI with 9 dimensions)

df_pin_ven <- ds_per_sector_ven %>%
  mutate(intersector_ds = rowSums(select(., c(
    "Food_security_ds",
    "Health_ds",
    "Integration_ds",
    "Nutrition_ds",
    "Protection_ds",
    "Shelter_ds",
    "Wash_ds",
    "Education_ds"
  )), na.rm = TRUE)) %>%
  # Calculate if included in intersector_pin with the threshold of 33.3% of intersector ds
  mutate(intersector_pin = ifelse(intersector_ds > 0.333, 1, 0))
# ggplot of intersector_ds to see distribution

ggplot(df_pin_ven, aes(x = intersector_ds)) +
  geom_density(fill = "blue", alpha = 0.9) +
  labs(title = "Density Plot of Intersector IPM Distribution",
       x = "Intersector ds",
       y = "Density") +
  theme_minimal()

pin_intersector_ven <- sum(df_pin_ven$"intersector_pin") / nrow(df_pin_ven)

### Step 3: Identify individuals with deprivations in each sector. Now, having the figure of the Intersectoral MPI, we can identify who is part of the MPI of each sector. This ensures that, although a person may have all the deprivations in a sector, they will only be part of the MPI if they have more than 33.3% of the total deprivations.


### Step 3: Identify individuals with deprivations in each sector. 
#Now, having the figure of the Intersectoral MPI, we can identify who is part of the MPI of each sector. 
#This ensures that, although a person may have all the deprivations in a sector, they will
#only be part of the MPI if they have more than 33.3% of the total deprivations.

df_sectoral_pin_ven <- df_pin_ven %>%
  select(-intersector_ds) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Food_security_ds",
      "Health_ds",
      "Integration_ds",
      "Protection_ds",
      "Shelter_ds",
      "Wash_ds"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  rename_with(~ str_replace(., "_ds$", "_pin"),
              c(
                "Food_security_ds",
                "Health_ds",
                "Integration_ds",
                "Protection_ds",
                "Shelter_ds",
                "Wash_ds")) %>%
  mutate(across(
    c(
      "Gender_based_violence_ds",
      "HT_S_ds"
    ),
    ~ ifelse(Protection_pin == 1 & . > 0, 1, 0)
  ))|>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin")) 


#Education

df_sectoral_pin_edu_ven <- df_pin_ven %>%
  filter(B06_travel_with_children=="yes")%>%
  select(-intersector_ds, Education_ds) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Education_ds"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin")) 

#final Education pin
pins_final_edu_ven <- data.frame(  
  Sectors = c(
    "Education"
  ),
  pins = round(colSums(select(df_sectoral_pin_edu_ven, Education_pin)) / nrow(df_sectoral_pin_edu_ven) *
                 100, 1)
)

#Nutrition women
df_sectoral_pin_nut_w_ven <- df_pin_ven %>%
  filter(B13_pregnant=="yes"| B14_lactating=="yes")%>%
  select(-intersector_ds, Nutrition_ds_women) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Nutrition_ds_women"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds_", "_pin_")) 

#final Nutrition women pin
pins_final_nut_w_ven <- data.frame(  
  Sectors = c(
    "Nutrition Woman"
  ),
  pins = round(colSums(select(df_sectoral_pin_nut_w_ven, Nutrition_pin_women)) / nrow(df_sectoral_pin_nut_w_ven) *
                 100, 1)
)


#Nutrition  households with children
df_sectoral_pin_nut_b_ven <- df_pin_ven %>%
  filter(B06_travel_with_children=="yes")%>%
  select(-intersector_ds, Nutrition_ds_baby) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Nutrition_ds_baby"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds_", "_pin_")) 

#final Nutrition baby pin
pins_final_nut_b_ven <- data.frame(  
  Sectors = c(
    "Nutrition Baby"
  ),
  pins = round(colSums(select(df_sectoral_pin_nut_b_ven, Nutrition_pin_baby)) / nrow(df_sectoral_pin_nut_b_ven) *
                 100, 1)
)

#Child Protection
df_sectoral_pin_cp_ven <- df_sectoral_pin_ven%>%
  filter(B06_travel_with_children=="yes")%>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Child_protection_pin"
    ),
    ~ ifelse(Protection_pin == 1 & . > 0, 1, 0)
  )) %>%
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin")) 

#final child protection
pins_final_cp_ven <- data.frame(  
  Sectors = c(
    "Child Protection"
  ),
  pins = round(colSums(select(df_sectoral_pin_cp_ven, Child_protection_pin)) / nrow(df_sectoral_pin_cp_ven) *
                 100, 1)
)


#final PIN table
pins_final_ven <- data.frame(  
  Sectors = c(
    "Food_security",
    "Health",
    "Integration",
    "Protection",
    "Gender_based_violence",
    "Human_Traficking_&_Smuggling",
    "Shelter",
    "Wash",
    "Intersector"
  ),
  pins = round(colSums(select(df_sectoral_pin_ven, Food_security_pin, Health_pin, 
                              Integration_pin,
                              Protection_pin, Gender_based_violence_pin,
                              HT_S_pin,
                              Shelter_pin, Wash_pin, intersector_pin)) / nrow(df_sectoral_pin_ven) *
                 100, 1)
)

#merge EDU, NUT, CP and final

#merge

pins_final_final_ven <- rbind(pins_final_ven, pins_final_edu_ven, pins_final_nut_w_ven, pins_final_nut_b_ven, pins_final_cp_ven)

write_xlsx(pins_final_final_ven, "pins_final_final_ven.xlsx")

    
    
    
    


