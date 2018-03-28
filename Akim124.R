library("tidyverse") 
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
tb1= read_csv("eddypro.csv",skip = 1,na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tb1=tb1[-1,] 
tb1 
tb1=tb1[tb1$DOY > 60 & tb1$DOY < 150,] 
glimpse(tb1) 
tb1 = select(tb1, -(roll)) 
tb1 = tb1 %>% mutate_if(is.character, factor) 
names(tb1) = str_replace_all(names(tb1), "[!]","_emph_") 
names(tb1) = names(tb1) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tb1) 
sapply(tb1,is.numeric) 
tb1_numeric = tb1 [,sapply (tb1,is.numeric) ] 
tb1_non_numeric = tb1[,!sapply(tb1,is.numeric) ] 
cor_tb = cor(drop_na(tb1_numeric)) 
cor_tb 
cor_tb1 = cor(drop_na(tb1_numeric)) %>% as.data.frame %>% select(co2_flux) 
cor_tb1 
vars = row.names(cor_tb1)[cor_tb1$co2_flux^2 > .2] %>% na.exclude 
vars
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula 
mod1=lm(co2_flux ~rand_err_Tau + H + LE + rand_err_LE + co2_flux + h2o_flux+         
          rand_err_h2o_flux + co2_molar_density + co2_mole_fraction
        + co2_mixing_ratio + sonic_temperature + air_temperature +
          u_star_ + T_star_ + un_H + un_LE + un_co2_flux + un_h2o_flux + h2o_var + w_div_ts_cov + w_div_co2_cov   
        + w_div_h2o_cov + co2 + co2_1 + flowrate, data = tb1) 
summary(mod1)
anova(mod1) 
mod2=lm(co2_flux ~(rand_err_Tau + H + LE + rand_err_LE + co2_flux + h2o_flux+         
                     rand_err_h2o_flux + co2_molar_density + co2_mole_fraction
                   + co2_mixing_ratio + sonic_temperature + air_temperature +
                     u_star_ + T_star_ + un_H + un_LE + un_co2_flux + un_h2o_flux)^2, data = tb1) 
summary(mod2) 
anova(mod2) 

mod3=lm(co2_flux ~(rand_err_Tau + H + LE + rand_err_LE + co2_flux + h2o_flux+         
                     rand_err_h2o_flux + co2_molar_density + co2_mole_fraction
                   + co2_mixing_ratio + sonic_temperature + air_temperature + 
                     u_star_ + T_star_ + un_H + un_LE + un_co2_flux + un_h2o_flux)^2  - co2_molar_density:u_star_
        - co2_molar_density:un_h2o_flux - co2_mole_fraction:un_LE - co2_mixing_ratio:T_star_  
        -sonic_temperature:u_star_  - sonic_temperature:un_LE - air_temperature:u_star_ - 
          air_temperature:T_star_ - u_star_:T_star_ - u_star_:un_H - u_star_:un_LE - u_star_:un_h2o_flux  
        - T_star_:un_LE - T_star_:un_h2o_flux  - un_H:un_LE - un_H:un_h2o_flux, data = tb1)
summary(mod3) 
anova(mod3)
mod4=lm(co2_flux ~(rand_err_Tau + H + LE + rand_err_LE + co2_flux + h2o_flux+         
                     rand_err_h2o_flux + co2_molar_density + co2_mole_fraction
                   + co2_mixing_ratio + sonic_temperature + air_temperature + 
                     u_star_ + T_star_ + un_H + un_LE + un_co2_flux + un_h2o_flux)^2  - co2_molar_density:u_star_
        - co2_molar_density:un_h2o_flux - co2_mole_fraction:un_LE - co2_mixing_ratio:T_star_  
        -sonic_temperature:u_star_  - sonic_temperature:un_LE - air_temperature:u_star_ - 
          air_temperature:T_star_ - u_star_:T_star_ - u_star_:un_H - u_star_:un_LE - u_star_:un_h2o_flux  
        - T_star_:un_LE - T_star_:un_h2o_flux  - un_H:un_LE - un_H:un_h2o_flux - air_temperature:un_LE - un_LE:un_h2o_flux, data = tb1)
summary(mod4) 
anova(mod4)

