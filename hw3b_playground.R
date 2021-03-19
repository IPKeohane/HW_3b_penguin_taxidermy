library(tidyverse)
library(palmerpenguins)
library(ggiraph)
library(ggiraphExtra)

glimpse(penguins)
summary(penguins)


data_male <- penguins %>% filter(sex=="male",
                                 !is.na(bill_depth_mm),
                                !is.na(bill_length_mm),
                                !is.na(flipper_length_mm),
                                !is.na(body_mass_g))

data_female <- penguins %>% filter(sex=="female",
                                   !is.na(bill_depth_mm),
                                   !is.na(bill_length_mm),
                                   !is.na(flipper_length_mm),
                                   !is.na(body_mass_g))

ggplot(aes(x=body_mass_g), data=data_male) +
  geom_histogram() +
  facet_wrap(island~species)

ggplot(aes(x=body_mass_g), data=data_female) +
  geom_histogram() +
  facet_wrap(island~species)


dummy <- penguins %>% filter(island=="Gentoo",species=="Chinstrap")

penguins %>% filter(species=="Gentoo") %>%
  t_test(bill_depth_mm ~ sex) 

penguins %>% filter(species=="Adelie") %>%
  t_test(body_mass_g ~ island) 

penguins %>% filter(species=="Adelie") %>%
  t_test(flipper_length_mm ~ island) 



data_lm =  penguins %>% drop_na() %>% select(-c(year,island))


lm_multi = lm(body_mass_g ~ bill_length_mm + bill_depth_mm +
                flipper_length_mm + species + sex, data=data_lm)
                
summary(lm_multi)

lm_multi_predictions = predict(lm_multi) 
head(lm_multi_predictions)s

dummy = lm_multi %>% augment(se_fit=TRUE) %>% 
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit, pred=.fitted)
  
  







x_cols=c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")

for(col in x_cols){
  ggplot(data=data_lm,aes_string(x=col)) +
    geom_smooth(aes(y=body_mass_g),method = "lm",se = FALSE) +
    geom_smooth(aes(y=pred),method = "lm",color="pink",se = FALSE) +
    geom_point(aes(y=body_mass_g)) +
    geom_errorbar(aes(ymin=lwr, ymax=upr),color="orange") +
    geom_point(aes(y = pred), color="red", size = 1) +
    facet_wrap(~species,ncol=1,scale="free")
}
    
    
    
  


dummy = data_lm %>% expand(body_mass_g)


geom_point(aes(x = body_mass_g, y = bill_depth_mm), data=gentoo) + # original data
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = body_mass_g), alpha = .15, data=lm_gentoo_3_predict) +
  geom_line(data=lm_gentoo_3_predict, aes(y = .fitted, x = body_mass_g), size = 1) 








