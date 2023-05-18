## Final Project For R
library(tidyverse)
library(modelr)
library(moments)

##SET UP
setwd('C:/Users/alexa/Desktop/R_Final')
getwd()
arrestsbefore <- read_csv('Adult_arrests.csv')
arrests<- arrestsbefore%>%
  select(!`Felony Total`)%>%
  select(!Total)%>%
  pivot_longer(cols = `Drug Felony`:`Other Misdemeanor`, names_to = 'crimes', values_to = 'count_per_crime')
arrestswithtotal<- arrestsbefore%>%
  select(!`Felony Total`)%>%
  pivot_longer(cols = `Drug Felony`:`Other Misdemeanor`, names_to = 'crimes', values_to = 'count_per_crime')
arrestswithtotaljoin<- arrestswithtotal%>%
  inner_join(gov, by = c("Year" = "year"))
census <- read_csv('census_counties.csv')
census<- census%>%
  select(c(pop2010,pop2023,name))
census<- census%>%
  pivot_longer(cols = !name, names_to = 'year',values_to = 'population')
gov <- read_csv('ny_governor.csv')
gov<- gov%>%
  pivot_longer(cols = !c(Name:Party, end_year), names_to = 'Number_of_year_in_office',values_to = 'year')
gov<-gov%>%
  filter(!is.na(year))
gov$Number_of_year_in_office <- as.numeric(gov$Number_of_year_in_office)
gov<- gov%>%
  select(Name:year)
gov_arrests <- arrests %>%
  inner_join(gov, by = c("Year" = "year"))

## SECTION 1
## Graph 1
arrests %>%
  filter(crimes != "Misdemeanor Total") %>%
  ggplot() +
  geom_point(aes(Year, count_per_crime,color = count_per_crime,alpha = count_per_crime)) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, 15)) +
  facet_wrap(~ crimes)+
  labs(title = 'The Count of crimes based on year per different crime types',
       x = 'Year',
       y = 'Count of Crime per Committed',
       color = 'Count of Crime',
       alpha = 'Count of Crime',
       caption = 'The Alpha shows just how many points sit by 0')

## Graph 2
arrests %>%
  filter(crimes != "Misdemeanor Total") %>%
  filter(County == c("Bronx","Unknown NYC county","Queens"))%>%
  ggplot() +
  geom_point(aes(Year, count_per_crime,color = count_per_crime)) +
  facet_wrap(~ crimes)+
  labs(title = 'The Count of crimes based on year per different crime types',
       x = 'Year',
       y = 'Count of Crime per Committed',
       color = 'Count of Crime',
       subtitle = 'Lets take a closer look into just the NYC counties')

##Graph 3
arrestsmutate<- arrests%>%
  group_by(crimes,Year)%>%
  mutate(avgpercrimes = mean(count_per_crime))
arrestsmutate%>%
  filter(crimes != "Misdemeanor Total")%>%
  ggplot()+
  geom_line(aes(Year,avgpercrimes,color = avgpercrimes))+
  facet_wrap(~crimes)+
  labs(title = 'Average crime counts based on year per crime',
       x = 'Year',
       y = 'Average count of crime committed',
       color = 'Average count of\ncrime committed')

## Graph 4
arrests%>%
  filter(crimes == c("Drug Misdemeanor","Drug Felony"))%>%
  group_by(Year)%>%
  mutate(drug_user_caught = sum(count_per_crime))%>%
  ungroup()%>%
  ggplot()+
  geom_line(aes(Year,drug_user_caught,color = drug_user_caught))+
  scale_y_continuous(labels = scales::comma_format())+
  labs(title = 'Did Drugs Win the "War on Drugs"?',
       x = 'Year',
       y = 'Count of Drug related Crimes',
       color = 'Count of Drug \nrelated Crimes')


##Graph 5
gov_arrests %>%
  filter(crimes != "Misdemeanor Total") %>%
  group_by(crimes, Year) %>%
  mutate(avgcrime = mean(count_per_crime)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(Year, count_per_crime,color = Party))+
  geom_line(aes(Year, avgcrime), color = 'Black', size = 1)+
  facet_wrap(~ crimes) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, 15)) +
  scale_color_manual(values = c('red', 'blue'), 
                     labels = c('Republican', 'Democrat')) +
  labs(title = "Does the Governor's Political Affiliation impact crimes?",
       x = 'Year',
       y = 'Average Crime Counts',
       color = 'Political Party')



## Graph 6
gov_arrests %>%
  filter(crimes == "Violent Felony") %>%
  mutate(avgcrime = mean(count_per_crime)) %>%
  ggplot() +
  geom_point(aes(Year, count_per_crime,color = Party))+
  geom_line(aes(Year, avgcrime), color = 'Black', size = 1)+
  scale_x_continuous(limits = c(1970, 2020)) +
  scale_color_manual(values = c('red', 'blue'), 
                     labels = c('Republican', 'Democrat')) +
  labs(title = "Does the Governor's Political Affiliation impact crimes?",
       x = 'Year',
       y = 'Violent Crime Counts',
       color = 'Political Party')

##SECTION 2
#GRAPH 7 - LBF
sd<- sd(arrestsbefore$`Drug Misdemeanor`)
prop_drug_mis_lbf<- lm(`Drug Misdemeanor`~`Property Misdemeanor`,data = arrestsbefore)
arrestsbefore%>%
  add_residuals(prop_drug_mis_lbf)%>%
  ggplot()+
  geom_point(aes(`Property Misdemeanor`,`Drug Misdemeanor`,color = abs(resid)))+
  geom_abline(aes(intercept = prop_drug_mis_lbf[[1]][[1]], slope = prop_drug_mis_lbf[[1]][[2]]),color = 'red',size = 1.25)+
  labs(title = "The Correlation between Drug Misdemeanor's Based \non Property Misdemeanor's",
       x= "Property Misdemeanor's",
       y = "Drug Misdemeanor's",
       color = "Strength holding the \nLine of Best Fit")

##GRAPH 7.2
arrestsbefore%>%
  add_residuals(prop_drug_mis_lbf)%>%
  ggplot()+
  geom_point(aes(`Property Misdemeanor`,resid,color = abs(resid)))+
  geom_ref_line(h=0)+
  labs(title = 'Residual Plot of the Last line of best fit',
       subtitle = 'Linear Model',
       x = 'Property Misdemeanor',
       y = 'Residual',
       color = 'Strength holding the \nLine of Best Fit')

## GRAPH 7.3
arrestsbefore %>%
  add_residuals(prop_drug_mis_lbf) %>%
  filter(resid <= 0 + sd + sd + sd) %>%
  filter(resid >= 0 - sd - sd - sd) %>%
  ggplot() +
  geom_histogram(aes(resid, fill = cut(resid, breaks = seq(-3*sd, 3*sd, by = sd/5))), binwidth = sd/5, color = 'black', show.legend = FALSE) +
  labs(title = "Histogram of The Correlation between Drug Misdemeanor's \nBased on Property Misdemeanor's",
       x= "Residuals",
       y = "Count of Residuals per Bin")


##GRAPH 8 - a spin on 8 using error bars
arrestsbefore
arrestsbefore%>%
  group_by(Year)%>%
  mutate(avgcount_of_crimes = mean(`Felony Total`+`Misdemeanor Total`))%>%
  mutate(sdofcount_of_crimes = sd(`Felony Total`+`Misdemeanor Total`))%>%
  ungroup()%>%
  mutate(upper = avgcount_of_crimes + qnorm(p=.975)*sdofcount_of_crimes/sqrt(3))%>%
  mutate(lower = avgcount_of_crimes + qnorm(p = .025)*sdofcount_of_crimes/sqrt(3))%>%
  ggplot()+
  geom_point(aes(Year,`Felony Total`+`Misdemeanor Total`))+
  geom_point(aes(Year,avgcount_of_crimes),color = 'red',size = 1.5)+
  geom_errorbar(aes(x = Year , y = avgcount_of_crimes , ymin = lower , ymax = upper),color = 'red',size = 1)+
  labs(title = 'Has the average Crime count per county gone up over time?',
       x = 'Year',
       y = 'Total Crime Count per County')

##GRAPH 8.R Replace?

arrestswithtotaljoin%>%
  group_by(Party)%>%
  mutate(avg_tot_count = mean(Total))%>%
  mutate(sd_tot_count = mean(Total))%>%
  ungroup()%>%
  mutate(upper = avg_tot_count + qnorm(p=.975)*sd_tot_count/sqrt(3))%>%
  mutate(lower = avg_tot_count + qnorm(p = .025)*sd_tot_count/sqrt(3))%>%
  ggplot()+
  geom_point(aes(Party,Total))+
  geom_point(aes(Party,avg_tot_count),color = 'red',size = 1.5)+
  geom_errorbar(aes(x = Party , y = avg_tot_count , ymin = lower , ymax = upper),color = 'red',size = 1)+
  labs(title = 'Has Political Party of the Govenor impacted Crime Counts?',
       x = "Political Party of the Govenor",
       y = "Count Of Crimes")
##Graph8.R.2
arrestswithtotaljoin%>%
  filter(County == c("Bronx","Unknown NYC county","Queens"))%>%
  group_by(Party)%>%
  mutate(avg_tot_count = mean(Total))%>%
  mutate(sd_tot_count = mean(Total))%>%
  ungroup()%>%
  mutate(upper = avg_tot_count + qnorm(p=.975)*sd_tot_count/sqrt(3))%>%
  mutate(lower = avg_tot_count + qnorm(p = .025)*sd_tot_count/sqrt(3))%>%
  ggplot()+
  geom_point(aes(Party,Total))+
  geom_point(aes(Party,avg_tot_count),color = 'red',size = 1.5)+
  geom_errorbar(aes(x = Party , y = avg_tot_count , ymin = lower , ymax = upper),color = 'red',size = 1)+
  labs(title = 'Has Political Party of the Govenor impacted Crime Counts?',
       subtitle = "This was filtered by only NYC counties",
       x = "Political Party of the Govenor",
       y = "Count Of Crimes")

##SECTION 3
##BEST FIGURE EVER
##GRAPH 9
##MY MAKING LBF
arrestsbefore_new <- arrestsbefore %>%
  group_by(Year) %>%
  mutate(avgdrug_fel = mean(`Drug Felony`)) %>%
  mutate(avgviolent_fel = mean(`Violent Felony`)) %>%
  mutate(avgdwi_fel = mean(`DWI Felony`)) %>%
  mutate(avgother_fel = mean(`Other Felony`)) %>%
  mutate(avgdrug_mis = mean(`Drug Misdemeanor`)) %>%
  mutate(avgdwi_mis = mean(`DWI Misdemeanor`)) %>%
  mutate(avgprop_mis = mean(`Property Misdemeanor`)) %>%
  mutate(avgother_mis = mean(`Other Misdemeanor`)) %>%
  ungroup()

##make line of best fits
avgdrug_fellbf<-lm(avgdrug_fel~I(Year^2)+Year,data = arrestsbefore_new)
avgviolent_fellbf<-lm(avgviolent_fel~I(Year^2)+Year,data = arrestsbefore_new)
avgdwi_fellbf<-lm(avgdwi_fel~I(Year^2)+Year,data = arrestsbefore_new)
avgother_fellbf<-lm(avgother_fel~I(Year^2)+Year,data = arrestsbefore_new)
avgdrug_mislbf<-lm(avgdrug_mis~I(Year^2)+Year,data = arrestsbefore_new)
avgdwi_mislbf<-lm(avgdwi_mis~I(Year^2)+Year,data = arrestsbefore_new)
avgprop_mislbf<-lm(avgprop_mis~I(Year^2)+Year,data = arrestsbefore_new)
avgother_mislbf<-lm(avgother_mis~I(Year^2)+Year,data = arrestsbefore_new)

##add predictions to data frame
arrestsbfe <- arrestsbefore_new %>%
  add_predictions(avgdrug_fellbf) %>%
  mutate(avgdrug_fellbfcol = pred) %>%
  add_predictions(avgviolent_fellbf) %>%
  mutate(avgviolent_fellbfcol = pred)%>%
  add_predictions(avgdwi_fellbf)%>%
  mutate(avgdwi_fellbfcol = pred)%>%
  add_predictions(avgother_fellbf)%>%
  mutate(avgother_fellbfcol = pred)%>%
  add_predictions(avgdrug_mislbf)%>%
  mutate(avgdrug_mislbfcol = pred)%>%
  add_predictions(avgdwi_mislbf)%>%
  mutate(avgdwi_mislbfcol = pred)%>%
  add_predictions(avgprop_mislbf)%>%
  mutate(avgprop_mislbfcol = pred)%>%
  add_predictions(avgother_mislbf)%>%
  mutate(avgother_mislbfcol = pred)

##GRAPH
arrestsbfe%>%
  ggplot() +
  geom_line(aes(Year, avgdrug_fel, color = "Drug Felony"),size = 1) +
  geom_line(aes(Year, avgdrug_fellbfcol,color = "Drug Felony"),linetype = "dashed",size = .5)+
  geom_line(aes(Year, avgviolent_fel, color = "Violent Felony"),size = 1) +
  geom_line(aes(Year, avgviolent_fellbfcol,color = "Violent Felony"),linetype = "dashed",size = .5)+
  geom_line(aes(Year, avgdwi_fel, color = "DWI Felony"),size = 1) +
  geom_line(aes(Year, avgdwi_fellbfcol,color = "DWI Felony"),linetype = "dashed",size = .5)+
  geom_line(aes(Year, avgother_fel, color = "Other Felony"),size = 1) +
  geom_line(aes(Year, avgother_fellbfcol,color = "Other Felony"),linetype = "dashed",size = .5)+
  geom_line(aes(Year, avgdrug_mis, color = "Drug Misdemeanor"),size = 1) +
  geom_line(aes(Year, avgdrug_mislbfcol,color = "Drug Misdemeanor"),linetype = "dashed",size = .5)+
  geom_line(aes(Year, avgdwi_mis, color = "DWI Misdemeanor"),size = 1) +
  geom_line(aes(Year, avgdwi_mislbfcol,color = "DWI Misdemeanor"),linetype = "dashed",size = .5)+
  geom_line(aes(Year, avgprop_mis, color = "Property Misdemeanor"),size = 1) +
  geom_line(aes(Year, avgprop_mislbfcol,color = "Property Misdemeanor"),linetype = "dashed",size = .5)+
  geom_line(aes(Year, avgother_mis, color = "Other Misdemeanor"),size = 1) +
  geom_line(aes(Year, avgother_mislbfcol,color = "Other Misdemeanor"),linetype = "dashed",size = .5)+
  labs(title = "Average Number of Arrests by Crime Type",
       x = "Year",
       y = "Average Number of Arrests",
       color = "Crime Type",
       caption = "The solid lines are our Average lines for each crime. \nWe can see that the Dotted lines are our lines of best fit.\nThey are set to be a second order polynomial. \nWe can see that early in the 1990s some of the \nsmaller crimes would peek and then fall around in the 2000s. \nThis can be due policing,political or socio-economic trends. \nFor the most part most of the crimes would rise then fall. \nThis could be due to the pandemic and losening of some laws and lack of police officers.") +
  scale_color_manual(name = "Crime Type",
                     values = c(
                       "Drug Felony" = "red",
                       "Violent Felony" = "blue",
                       "DWI Felony" = "purple",
                       "Other Felony" = "orange",
                       "Drug Misdemeanor" = "gold",
                       "DWI Misdemeanor" = "green",
                       "Property Misdemeanor" = "lightblue",
                       "Other Misdemeanor" = "navy"))+
  theme(plot.caption = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "White"),
        panel.grid.major = element_line(color = 'grey'))

