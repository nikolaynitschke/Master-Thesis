install.packages("stargazer")
install.packages("readxl")
install.packages("vdemdata")
install.packages("plm")
install.packages("anytime")
install.packages("car")
library(tidyverse)
library(stargazer)
library(dplyr)
library(plm)
library(readxl)
library(car)
library(anytime)




####third party intervetion####
milc_10 <- read_excel("milc-10.xls")

colnames(milc_10) <- c("conflict.name", "country.ID", "Country.ID", "region.ID", "govermnet", "territory", "side.A",
                       "side.B", "dyad.1", "dyad.2", "dyad.3", "dyad.4", "dyad.5", "year", "active.year", "start.year", "end.year",
                       "source", "singel.state", "Group.of.states", "singel.IGO", "group.of.IGO", "other.third",
                       "P5", "neighbor", "UN", "IGO", "Third.party.1.COW", "Third.party.1.GW", "Third.party.2.COW","Third.party.2.GW",
                       "Third.party.3.COW", "Third.party.3.GW", "Third.party.4.COW", "Third.party.4.GW", "Third.party.5.COW", "Third.party.5.GW",
                       "Third.party.6.COW", "Third.party.6.GW", "Third.party.7.COW", "Third.party.7.GW", "Third.party.8.COW", "Third.party.8.GW",
                       "Third.party.9.COW", "Third.party.9.GW", "Third.party.10.COW", "Third.party.10.GW",
                       "Third.party.11.COW", "Third.party.11.GW", "Third.party.12.COW","Third.party.12.GW", "Third.party.13.COW",
                       "Third.party.13.GW", "Third.party.14.COW", "Third.party.14.GW", "Third.party.15.COW", "Third.party.15.GW",
                       "Third.party.16.COW", "Third.party.16.GW", "Third.party.17.COW", "Third.party.17.GW", "Third.party.18.COW", 
                       "Third.party.18.GW", "Third.party.19.COW", "Third.party.19.GW", "Third.party.20.COW", "Third.party.20.GW", 
                       "Third.party.21.COW", "Third.party.21.GW", "Third.party.22.COW", "Third.party.22.GW", "Third.party.23.COW", 
                       "Third.party.23.GW", "Third.party.24.COW", "Third.party.24.GW", "Third.party.25.COW", "Third.party.25.GW", 
                       "Third.party.26.COW", "Third.party.26.GW", "Third.party.27.COW", "Third.party.27.GW", "total.number.third.parties",
                       "warring.parties", "indirect.talk.1", "indirect.talk.2", "direct.talk.1", "direct.talk.2",
                       "unclear.talks.1", "unclear.talks.2", "bilateral.talks.1", "bilateral.talks.2", "arbitration",
                       "good.office", "fact.finding", "permanent.observer", "peacekeeping", "WAR", "WAR.inclusion",
                       "year.of.war", "comment.on.war")

milc_10 <- milc_10 %>% 
  mutate(s_year = as.Date(as.numeric(milc_10$start.year), origin = "1899-12-31"),
         s_year = format(s_year, format = "%Y"),s_year = as.numeric(s_year),
         e_year = as.Date(as.numeric(milc_10$end.year), origin = "1899-12-31"),
         e_year = format(e_year, format = "%Y"), e_year = as.numeric(e_year), 
         s_year = case_when(str_detect(start.year, "\\?") ~ as.numeric(str_extract(start.year, "\\d{4}")),
                            TRUE ~ s_year), e_year = case_when(str_detect(end.year, "\\?") ~
                                                                 as.numeric(str_extract(end.year, "\\d{4}")), TRUE ~ e_year))



civil.war.in <- milc_10 %>% select(conflict.name, side.A, side.B, year, s_year, e_year, singel.state, singel.IGO, group.of.IGO,
                                  Group.of.states, other.third, indirect.talk.1, indirect.talk.2, direct.talk.1, direct.talk.2,
                                  unclear.talks.1, unclear.talks.2, bilateral.talks.1, bilateral.talks.2, arbitration,
                                  good.office, fact.finding, permanent.observer, peacekeeping, WAR, WAR.inclusion, warring.parties)

civil.war.in <- civil.war.in [-c(1,2,3),]

civil.war.in$conflict.name [civil.war.in$conflict.name == "Bosnia and Herzegovina (Bihacka Krajina)"] <- "Bosnia-Herzegovina"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Bosnia and Herzegovina (Croat)"] <- "Bosnia-Herzegovina"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Cambodia"] <- "Cambodia (Kampuchea)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Democratic Republic of Congo / Zaire"] <- "DR Congo (Zaire)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Angola (Cabinda)"] <- "Angola"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Comoros (Anjouan)"] <-"Comoros"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Croatia (Serb)"] <-"Croatia"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Ethiopia (Ogaden)"] <- "Ethiopia"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Ethiopia (Oromiya)"] <- "Ethiopia"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Georgia (South Ossetia)"] <- "Georgia"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Georgia (Abkhazia)"] <- "Georgia"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Guinea Bissau"] <- "Guinea-Bissau"
civil.war.in$conflict.name [civil.war.in$conflict.name == "India (Tripura)"] <- "India"
civil.war.in$conflict.name [civil.war.in$conflict.name == "India (Nagaland)"] <- "India"
civil.war.in$conflict.name [civil.war.in$conflict.name == "India (Manipur)"] <- "India"
civil.war.in$conflict.name [civil.war.in$conflict.name == "India (Bodoland)"] <- "India"
civil.war.in$conflict.name [civil.war.in$conflict.name == "India (Assam)"] <- "India"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Indonesia (Aceh)"] <- "Indonesia"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Israel (Palestine)"] <- "Israel"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Macedonia"] <- "North Macedonia"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Mali (Azawad)"] <- "Mali"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Myanmar"] <- "Myanmar (Burma)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Myanmar (Karen)"] <- "Myanmar (Burma)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Myanmar (Karenni)"] <- "Myanmar (Burma)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Myanmar (Mon)"] <- "Myanmar (Burma)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Myanmar (Shan)"] <- "Myanmar (Burma)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Niger (Air and Azawad)"] <- "Niger"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Niger (Eastern Niger)"] <- "Niger"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Nigeria (Niger Delta)"] <- "Nigeria"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Nigeria (Northern Nigeria)"] <- "Nigeria"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Papua New Guinea (Bougainville)"] <- "Papua New Guinea"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Philippines (Mindanao)"] <- "Philippines"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Russia"] <- "Russia (Soviet Union)"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Senegal (Casamance)"] <- "Senegal"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Sudan (Southern Sudan)"] <- "Sudan"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Thailand (Patani)"] <- "Thailand"
civil.war.in$conflict.name [civil.war.in$conflict.name == "UK (Northern Ireland)"] <- "United Kingdom"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Yemen (South Yemen)"] <- "	South Yemen"
civil.war.in$conflict.name [civil.war.in$conflict.name == "Yugoslavia (Kosovo)"] <- "Serbia (Yugoslavia)"




####Rule of law####

rule.of.law <-`V-Dem-CY-Full+Others-v13`  %>% select(v2x_rule, year, country_name, e_gdppc, e_p_polity, v2x_polyarchy)

rule.of.law$country_name <- sub("Democratic Republic of the Congo", " DR Congo (Zaire)"  , rule.of.law$country_name)

rule.of.law$country_name <- sub("Republic of the Congo", "Congo"  , rule.of.law$country_name)

rule.of.law$country_name <- sub("Democratic Republic of the Congo", " DR Congo (Zaire) "  , rule.of.law$country_name)

rule.of.law$country_name <- sub("Zimbabwe", " Zimbabwe (Rhodesia) "  , rule.of.law$country_name)

rule.of.law$country_name <- sub("Bosnia and Herzegovina", " Bosnia-Herzegovina "  , rule.of.law$country_name)

rule.of.law$country_name <- sub("Cambodia", " Cambodia (Kampuchea)"  , rule.of.law$country_name)

rule.of.law$country_name <- sub("The Gambia", "Gambia"  , rule.of.law$country_name)

rule.of.law$country_name <- sub("Madagascar", "Madagascar (Malagasy)", rule.of.law$country_name)

rule.of.law$country_name <- sub("Burma/Myanmar", "Myanmar (Burma)", rule.of.law$country_name)

rule.of.law$country_name <- sub("Russia", "Russia (Soviet Union)", rule.of.law$country_name)

rule.of.law$country_name <- sub("Serbia", "Serbia (Yugoslavia)", rule.of.law$country_name)

rule.of.law$country_name <- sub("Republic of Vietnam", "South Vietnam", rule.of.law$country_name)

rule.of.law$country_name [rule.of.law$country_name == "Yemen (South Yemen)"] <- "	South Yemen"

rule.of.law$country_name <- str_trim(rule.of.law$country_name)

rule.of.law <- filter(rule.of.law, year > 1944)

rule.of.law <- rule.of.law %>% distinct()



####Civil war####

civil.war <- ucdp_prio_acd_221  %>% select(location, type_of_conflict, ep_end, ep_end_date, start_date, intensity_level) %>% 
  subset(type_of_conflict %in% c("3", "4"))

civil.war$ep_end_date <- substr(civil.war$ep_end_date, 1,4)

civil.war$start_date <- as.numeric(substr(civil.war$start_date, 1,4))

#civil.war <- filter(civil.war, start_date > 1944)

civil.war <- rename(civil.war, country_name = location)

civil.war.in <- rename(civil.war.in, country_name = conflict.name)

#civil.war <- filter(civil.war, start_date < 2015)

#civil.war <- civil.war %>%  filter(ep_end_date < 2015)

civil.war <- civil.war %>%
  filter(!country_name %in% c("Afghanistan", "Algeria", "Angola", "Burkina Faso", "Cameroon", "DR Congo (Zaire)", "Egypt",
                              "Ethiopia", "Israel", "Kenya", "Libya", "Mali", "Mozambique", "Myanmar (Burma)", "Niger",
                              "Nigeria", "Pakistan", "Philippines", "Rwanda", "Somalia", "South Sudan", "Syria", "Tanzania",
                              "Uganda", "Ukraine", "United States of America", "Yemen (North Yemen)"))
#the one om not sure about
civil.war <- civil.war %>%
  filter(!country_name %in% c("Bangladesh", "Central African Republic", "Chad", "India", "Indonesia", "Iran", "Lebanon",
                              "Russia (Soviet Union)", "Sierra Leone", "South Yemen", "Sudan", "Thailand", "Tunisia", "Venezuela"))
         
civil.war <- na.omit(civil.war)

####Mutate####
civil.war <- civil.war %>%
  filter(!(country_name == "Paraguay" & ep_end_date %in% c(1947, 1954)))


civil.war <- civil.war %>%
  filter(!(country_name == "Bolivia" & ep_end_date %in% c(1946, 1952)))


civil.war <- civil.war %>%
  filter(!(country_name == "Guatemala" & ep_end_date %in% c(1949, 1954, 1963)))


civil.war <- civil.war %>%
  filter(!(country_name == "China" & ep_end_date %in% c(1947, 1949, 1950, 1956, 1959)))


civil.war <- civil.war %>%
  filter(!(country_name == "Cuba" & ep_end_date %in% c(1953, 1958)))


civil.war <- civil.war %>%
  filter(!(country_name == "Azerbaijan" & ep_end_date %in% c(1993, 1994, 1995, 1998, 2005, 2008, 2012)))

civil.war <- civil.war %>%
  filter(!(country_name == "Argentina" & ep_end_date %in% c(1955, 1963)))

civil.war <- civil.war %>%
  filter(!(country_name == "Bosnia-Herzegovina" & ep_end_date == 1994))

civil.war <- civil.war %>%
  filter(!(country_name == "Burundi" & ep_end_date %in% c(1965, 1992, 2006, 2008)))

civil.war <- civil.war %>%
  filter(!(country_name == "Cambodia (Kampuchea)" & ep_end_date == 1975))

civil.war <- civil.war %>%
  filter(!(country_name == "Comoros" & ep_end_date == 1989))

civil.war <- civil.war %>%
  filter(!(country_name == "Congo" & ep_end_date %in% c(1993, 1999, 2002)))

civil.war <- civil.war %>%
  filter(!(country_name == "Croatia" & ep_end_date == 1993))

civil.war <- civil.war %>%
  filter(!(country_name == "Djibouti" & ep_end_date == 1994))

civil.war <- civil.war %>%
  filter(!(country_name == "El Salvador" & ep_end_date == 1972))

civil.war <- civil.war %>%
  filter(!(country_name == "Eritrea" & ep_end_date %in% c(1997, 1999)))

civil.war <- civil.war %>%
  filter(!(country_name == "Georgia" & ep_end_date %in% c(1992, 1993, 2004)))

civil.war <- civil.war %>%
  filter(!(country_name == "Ghana" & ep_end_date %in% c(1966, 1981)))

civil.war <- civil.war %>%
  filter(!(country_name == "Haiti" & ep_end_date %in% c(1989, 1991)))

civil.war <- civil.war %>%
  filter(!(country_name == "Iraq" & ep_end_date %in% c(1959, 1963, 1984, 1987, 1970, 1992)))

civil.war <- civil.war %>%
  filter(!(country_name == "Ivory Coast" & ep_end_date == 2004))

civil.war <- civil.war %>%
  filter(!(country_name == "Laos" & ep_end_date %in% c(1961, 1973)))

civil.war <- civil.war %>%
  filter(!(country_name == "Liberia" & ep_end_date %in% c(1980, 1990)))

civil.war <- civil.war %>%
  filter(!(country_name == "Malaysia" & ep_end_date %in% c(1960, 1966, 1975, 1981)))

civil.war <- civil.war %>%
  filter(!(country_name == "Mauritania" & ep_end_date == 1978))

civil.war <- civil.war %>%
  filter(!(country_name == "Mexico" & ep_end_date == 1994))

civil.war <- civil.war %>%
  filter(!(country_name == "Morocco" & ep_end_date == 1971))

civil.war <- civil.war %>%
  filter(!(country_name == "Nepal" & ep_end_date == 1962))

civil.war <- civil.war %>%
  filter(!(country_name == "Nicaragua" & ep_end_date == 1979))

civil.war <- civil.war %>%
  filter(!(country_name == "Oman" & ep_end_date == 1957))

civil.war <- civil.war %>%
  filter(!(country_name == "Papua New Guinea" & ep_end_date == 1990))

civil.war <- civil.war %>%
  filter(!(country_name == "Peru" & ep_end_date %in% c(1965, 1999)))

civil.war <- civil.war %>%
  filter(!(country_name == "Senegal" & ep_end_date %in% c(1990, 1993, 1995, 1998, 2001, 2003)))

civil.war <- civil.war %>%
  filter(!(country_name == "Serbia (Yugoslavia)" & ep_end_date %in% c(1991, 1992)))

civil.war <- civil.war %>%
  filter(!(country_name == "South Africa" & ep_end_date == 1983))

civil.war <- civil.war %>%
  filter(!(country_name == "Spain" & ep_end_date %in% c(1982, 1987)))

civil.war <- civil.war %>%
  filter(!(country_name == "Sri Lanka" & ep_end_date %in% c(1990, 1971, 2001, 2003)))

civil.war <- civil.war %>%
  filter(!(country_name == "Tajikistan" & ep_end_date %in% c(1998, 2000)))

civil.war <- civil.war %>%
  filter(!(country_name == "Turkey" & ep_end_date %in% c(1993, 2005, 2000)))

civil.war <- civil.war %>%
  filter(!(country_name == "United Kingdom" & ep_end_date == 1991))

civil.war <- civil.war %>%
  filter(!(country_name == "Uzbekistan" & ep_end_date == 2000))

civil.war <- civil.war %>%
  filter(!(country_name == "Zimbabwe (Rhodesia)" & ep_end_date == 1968))

civil.war2 <- civil.war %>% 
  rowwise() %>% 
  mutate(coverage = ifelse(is.na(ep_end_date), list(seq(as.numeric(start_date), 2020,1)),
                                                    list(seq(as.numeric(start_date), as.numeric(ep_end_date), 1))))


rule.of.law2 <- data.frame(rule.of.law, civ_war = NA)

for(i in 1:nrow(rule.of.law2)){
  c_name <- rule.of.law$country_name[i]
  year <- rule.of.law$year[i]
  
  tmp <- civil.war2 %>% 
    filter(country_name == c_name)
  
  for(j in 1:nrow(tmp)){
    cov_list <- unlist(tmp$coverage[j])
    
    if(year %in% cov_list){
      rule.of.law2$civ_war[i] <- 1
      break
    }else{
      
      if(j == nrow(tmp)){
        rule.of.law2$civ_war[i] <- 0
        break
      }else{
        next
      }
    } 
  }
}

df_intervention <- data.frame(unique(civil.war.in[, c("country_name", "year", "s_year", "e_year", "warring.parties")])) %>% 
  mutate(intervention = 1,
         year = as.numeric(year))

civ_set_full <- rule.of.law2 %>%  
  left_join(df_intervention, by = c("country_name", "year")) %>% 
  mutate(intervention = ifelse(is.na(intervention), 0, intervention))


test2 <- civ_set_full %>%
  group_by(country_name) %>%
  mutate(civ_war_start = min(year[civ_war == 1]),
         civ_war_end = max(year[civ_war == 1]),
         imp_val = ifelse(year == (civ_war_start-1), v2x_rule, NA))

test2 <- test2 %>%
  filter(!country_name %in% c("Afghanistan", "Algeria", "Angola", "Burkina Faso", "Cameroon", "DR Congo (Zaire)", "Egypt",
                              "Ethiopia", "Israel", "Kenya", "Libya", "Mali", "Mozambique", "Myanmar (Burma)", "Niger",
                              "Nigeria", "Pakistan", "Philippines", "Rwanda", "Somalia", "South Sudan", "Syria", "Tanzania",
                              "Uganda", "Ukraine", "United States of America", "Yemen (North Yemen)"))
#the one om not sure about
test2 <- test2 %>%
  filter(!country_name %in% c("Bangladesh", "Central African Republic", "Chad", "India", "Indonesia", "Iran", "Lebanon",
                              "Russia (Soviet Union)", "Sierra Leone", "South Yemen", "Sudan", "Thailand", "Tunisia", "Venezuela"))


test2 <- test2 %>%
  group_by(country_name) %>%
  filter(!all(is.na(imp_val)))

before.civ <- test2 %>% 
  group_by(country_name)

before.civ <- before.civ %>% 
  filter(!is.na(imp_val) & year >= min(year[!is.na(imp_val)])) 

before.civ <- before.civ %>% 
  select(civ_war_minus_1_year = year, country_name, imp_val)

Cont_int <- test2 %>%
  select(year, country_name,intervention)

Cont_int <- Cont_int %>% 
  filter(intervention == 1)

Cont_int <- Cont_int %>% 
  group_by(country_name)

Cont_int <- Cont_int %>% 
  summarize(intervention = 1,
            intervention_period = paste0(min(year), "-", max(year))) %>% 
  select(country_name, intervention, intervention_period)


data_lagged <- test2 %>% 
  select(country_name, civ_war_end) %>% 
  mutate(lagged_year_3 = civ_war_end + 3, 
         lagged_year_5 = civ_war_end + 5, 
         lagged_year_10 = civ_war_end + 10) %>% 
  distinct(country_name, .keep_all = TRUE)


# Selecting relevant columns from test2 dataset
test2_select <- test2 %>% 
  select(country_name, year, v2x_rule)

# Joining with lagged year variables by country_name
final_data <- left_join(data_lagged, test2_select, by = "country_name")

# Selecting only the necessary columns
final_data <- final_data %>% 
  select(country_name, lagged_year_3, lagged_year_5, lagged_year_10, v2x_rule) %>% 
  distinct(country_name, .keep_all = TRUE)

#lowest score during the civil war
civ_war_min <- test2 %>%
  filter(civ_war == 1) %>%
  group_by(country_name) %>%
  summarise(civ_war_min = min(v2x_rule, na.rm = TRUE),
            year = year[which.min(v2x_rule)])


civ_middel <- test2 %>%
  group_by(country_name) %>%
  summarise(min_v2x_rule_civ_war = min(v2x_rule[civ_war == 1], na.rm = TRUE),
            civ_war_years = paste(year[civ_war == 1], collapse = ", "))

 warring.p <- test2 %>% 
   select(country_name, year, warring.parties)
 
 # Pivot the data into long format
 warring.p_long <- pivot_longer(warring.p, cols = -c(year, country_name),
                                names_to = "variable", values_to = "value")
 
 # Filter for rows where the value is 1
 warring.p_long <- filter(warring.p_long, value == 1)
 
 # Rename the variable column and warring.parties value
 warring.p_long$variable <- "warring.parties"
 warring.p_long$value <- 1
 
 # Remove countries that are mentioned multiple times
 warring.p_long <- distinct(warring.p_long, country_name, .keep_all = TRUE)
 
 # Pivot the data back into wide format
 warring.p <- pivot_wider(warring.p_long, names_from = "variable", values_from = "value")
 
 v_dem <- test2 %>% 
   select(country_name, year, v2x_polyarchy)
 
 

civ1 <- data_lagged %>%
  left_join(civ_war_min, by = c("country_name")) %>% 
  left_join(civ_middel, by = c("country_name"))

test3 <- test2 %>% 
  select(v2x_rule, country_name, year, e_gdppc, e_p_polity, v2x_polyarchy)

civ_j <- data_lagged %>% 
  pivot_longer(-c(country_name), names_to = "variable", values_to = "year") %>% 
  left_join(test3, by = c("year", "country_name")) %>% 
  pivot_wider(names_from = "variable", values_from = c("v2x_rule", "year", "e_gdppc", "e_p_polity", "v2x_polyarchy")) %>% 
  left_join(before.civ, by = c("country_name")) %>% 
  left_join(civ_middel, by = c("country_name")) %>% 
  left_join(Cont_int, by = c("country_name")) %>% 
  left_join(warring.p, by = c("country_name"))

civ_j <- rename(civ_j, middel_of_war_rule = min_v2x_rule_civ_war) 
civ_j <-   rename(civ_j, end_of_war = v2x_rule_civ_war_end) 
civ_j <-  rename(civ_j, rule_lagged_3 = v2x_rule_lagged_year_3)
civ_j <-   rename(civ_j, rule_lagged_5 = v2x_rule_lagged_year_5) 
civ_j <-  rename(civ_j, rule_lagged_10 = v2x_rule_lagged_year_10)
civ_j <-  rename(civ_j, year_3 = year_lagged_year_3)
civ_j <-  rename(civ_j, year_5 = year_lagged_year_5)
civ_j <-  rename(civ_j, year_10 = year_lagged_year_10)
civ_j <-  rename(civ_j, e_gdppc_3 = e_gdppc_lagged_year_3)
civ_j <-   rename(civ_j, e_gdppc_5 = e_gdppc_lagged_year_5) 
civ_j <-  rename(civ_j, e_gdppc_10 = e_gdppc_lagged_year_10)
civ_j <-  rename(civ_j, e_p_polity_3 = e_p_polity_lagged_year_3)
civ_j <-   rename(civ_j, e_p_polity_5 = e_p_polity_lagged_year_5) 
civ_j <-  rename(civ_j, e_p_polity_10 = e_p_polity_lagged_year_10)
civ_j <- rename(civ_j, before_rule_lagged_1 = imp_val)
civ_j <- rename(civ_j, v_dem_3 = v2x_polyarchy_lagged_year_3)
civ_j <- rename(civ_j, v_dem_5 = v2x_polyarchy_lagged_year_5)
civ_j <- rename(civ_j, v_dem_10 = v2x_polyarchy_lagged_year_10)

civ_j$intervention <- ifelse(is.na(civ_j$intervention), 0, civ_j$intervention)

civ_j$warring.parties <- ifelse(is.na(civ_j$warring.parties ), 0, civ_j$warring.parties)
####the analysis####

# Perform OLS regression
civ_j$before_rule_lagged_1 <- as.numeric(civ_j$before_rule_lagged_1)
civ_j$middel_of_war_rule <- as.numeric(civ_j$middel_of_war_rule)
civ_j$rule_lagged_3 <- as.numeric(civ_j$rule_lagged_3)
civ_j$rule_lagged_5 <- as.numeric(civ_j$rule_lagged_5)
civ_j$rule_lagged_10 <- as.numeric(civ_j$rule_lagged_10)
civ_j$e_gdppc_3 <- as.numeric(civ_j$e_gdppc_3)
civ_j$e_gdppc_5 <- as.numeric(civ_j$e_gdppc_5)
civ_j$e_gdppc_10 <- as.numeric(civ_j$e_gdppc_10)
civ_j$e_p_polity_3 <- as.numeric(civ_j$e_p_polity_3)
civ_j$e_p_polity_5 <- as.numeric(civ_j$e_p_polity_5)
civ_j$e_p_polity_10 <- as.numeric(civ_j$e_p_polity_10)
civ_j$v_dem_3 <- as.numeric(civ_j$v_dem_3)
civ_j$v_dem_5 <- as.numeric(civ_j$v_dem_5)
civ_j$v_dem_10 <- as.numeric(civ_j$v_dem_10)

#model <- lm(`before_rule_lagged_1` + `middel_of_war_rule` + `rule_lagged_3` + `rule_lagged_5` + `rule_lagged_10` ~ intervention + e_gdppc_3 + e_gdppc_5 + e_gdppc_10 + e_p_polity_3 + e_p_polity_5 + e_p_polity_10 + warring.parties, data = civ_j)


model.before <- lm(before_rule_lagged_1 ~ intervention + e_gdppc_3 + e_p_polity_3 + + v_dem_3 + warring.parties, data = civ_j)

model.middel <- lm(middel_of_war_rule ~ intervention + e_gdppc_3 + e_p_polity_3 + + v_dem_3 + warring.parties, data = civ_j)

model_lag_3 <- lm(rule_lagged_3 ~ intervention + e_gdppc_3 + e_p_polity_3 + v_dem_3 + warring.parties, data = civ_j)

model_lag_5 <- lm(rule_lagged_5 ~ intervention + e_gdppc_5 + e_p_polity_5 + v_dem_5 + warring.parties, data = civ_j)

model_lag_10 <- lm(rule_lagged_10 ~ intervention + e_gdppc_10 + e_p_polity_10 + v_dem_10, data = civ_j)

model_lag_3_bm <- lm(before_rule_lagged_1 + middel_of_war_rule + rule_lagged_3 ~ intervention + e_gdppc_3 + e_p_polity_3 + + v_dem_3 + warring.parties, data = civ_j)

model_lag_5_bm <- lm(before_rule_lagged_1 + middel_of_war_rule + rule_lagged_5 ~ intervention + e_gdppc_5 + e_p_polity_5 + v_dem_5 + warring.parties, data = civ_j)

model_lag_10_bm <- lm(before_rule_lagged_1 + middel_of_war_rule + rule_lagged_10 ~ intervention + e_gdppc_10 + e_p_polity_10 + v_dem_10 + warring.parties, data = civ_j)

durbinWatsonTest(model.before)
durbinWatsonTest(model.middel)
durbinWatsonTest(model_lag_3)
durbinWatsonTest(model_lag_5)
durbinWatsonTest(model_lag_10)
durbinWatsonTest(model_lag_3_bm)
durbinWatsonTest(model_lag_5_bm)
durbinWatsonTest(model_lag_10_bm)

summary(model.before)
summary(model.middel)
summary(model_lag_3)
summary(model_lag_5)
summary(model_lag_10)
summary(model_lag_3_bm)
summary(model_lag_5_bm)
summary(model_lag_10_bm)

plot.ts(model_lag_10)

#### tester ####


