
#Les inn data. Merk at NHANES er et innebygd datasett i R.


library(dslabs)
library(NHANES)

data(NHANES)



#Inspeksjon

str(NHANES)
head(NHANES)
glimpse(NHANES)


library(dplyr)


#Lager dataramme for kvinner i 20-29 ??rs alder <- tab  


tab <- NHANES %>%
  filter(Gender == "female", AgeDecade == "20-29")


#Gj??r kvinner i 20-29 ??rs alder til referansegruppe.  


ref <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(
    average = mean (BPSysAve, na.rm = TRUE),
  ) %>%
    pull(average)


#Sjekker min og max verdi p?? systolisk blodtrykk hos referansegruppen. 
 

NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(
    minbp = min(BPSysAve, na.rm = TRUE),
    maxbp = max(BPSysAve, na.rm = TRUE)
  )
 

#Sjekker sentralm??l ved systolisk  blodtrykk blant kvinner for??vrig.  


NHANES %>%
   filter(Gender == "female") %>%
   group_by(AgeDecade) %>%
   summarize(
     average = mean(BPSysAve, na.rm = TRUE),
     standard_deviation = sd(BPSysAve, na.rm = TRUE)
   )
 

#Sjekker sentralm??l ved systolisk blodtrykk blant menn. 


 NHANES %>%
   filter(Gender == "male" & !is.na(AgeDecade)) %>%
   group_by(AgeDecade) %>%
   summarize(
     average = mean(BPSysAve, na.rm = TRUE),
     standard_deviation = sd(BPSysAve, na.rm = TRUE)
   )
 

  #Kj??nnssammenligning ved systolisk blodtrykk blant kvinner og menn, 
 
 
 NHANES %>%
   filter(!is.na(AgeDecade)) %>%
   group_by(AgeDecade, Gender) %>%
   summarize(average = mean(BPSysAve, na.rm = TRUE), 
             standard_deviation = sd(BPSysAve, na.rm = TRUE)
   )
 
 
 #Sjekker interne gruppeforskjeller i systolisk blodtrykk hos menn- rase.
 
 
 NHANES %>%
   filter(Gender == "male" & AgeDecade == " 40-49") %>%
   group_by(Race1) %>%
   summarize(
     average = mean(BPSysAve, na.rm = TRUE),
     standard_deviation = sd(BPSysAve, na.rm = TRUE)
   ) %>%
   arrange(average)

 
 
 #Modelldata- gjennomsnittlig forekomst av systolisk blodtrykk etter aldersgrupper. 
 
 library(ggplot2)
 
 NHANES %>%
   filter(!is.na(AgeDecade)) %>%
   group_by(AgeDecade, Gender) %>%
   summarize(
     average = mean(BPSysAve, na.rm = TRUE)
   ) %>%
   
  
   
   ggplot(aes(x = AgeDecade, y = average, color = Gender, group = Gender)) +
   geom_line() +
   geom_point() +
   labs(
     title = "Average Systolic Blood Pressure by Age and Gender",
     x = "Age Group",
     y = "Average Blood Pressure"
   ) +
   theme_minimal()
 