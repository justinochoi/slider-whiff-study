library(tidyverse)
library(lme4)
library(mgcv)
library(ggeffects) 
library(scales)

# first step: simplest model possible
logistic <- glm(is_whiff ~ z_diff, data = main_df, family = binomial)
summary(logistic)

logistic_preds <- predict_response(logistic, terms = 'z_diff [all]') 
plot(logistic_preds) + 
  labs(
    x = 'Vertical Location Delta (ft.)', 
    y = 'Fitted Whiff Probability', 
    title = 'You Gotta Start Somewhere', 
    subtitle = 'Decent, but we can improve'
  ) + 
  theme(plot.title = element_text(face='bold'))

simple_gam <- gam(is_whiff ~ s(z_diff), data = main_df, family = binomial)
summary(simple_gam) 

simple_preds <- predict_response(simple_gam, terms = 'z_diff [all]') 
plot(simple_preds) + 
  labs(
    x = 'Vertical Location Delta (ft.)', 
    y = 'Fitted Whiff Probability', 
    title = 'Simple Man With a Simple GAM', 
    subtitle = 'Better!'
  ) + 
  theme(plot.title = element_text(face='bold'))

complex_gam <- gam(is_whiff ~ release_speed + count + s(pfx_x) + s(pfx_z) + 
                     s(plate_x) + s(plate_z) + s(z_diff), data = main_df, family = binomial) 
summary(complex_gam)

complex_preds <- predict_response(complex_gam, terms = 'z_diff [all]')
plot(complex_preds) + 
  labs(
    x = 'Vertical Location Delta (ft.)', 
    y = 'Fitted Whiff Probability', 
    title = 'What If We Control For Other Variables?', 
    subtitle = 'Not good!'
  ) + 
  theme(plot.title = element_text(face='bold'))
  

interaction_gam <- gam(is_whiff ~ release_speed + count + s(pfx_x) + s(pfx_z) + s(plate_x) + 
                         s(plate_z, z_prev), data = main_df, family = binomial) 
summary(interaction_gam) 

z_height <- c(3.5, 2.55, 1.6) 
interaction_preds <- predict_response(interaction_gam, terms = c("plate_z", "z_prev [z_height]"))

plot(interaction_preds) + 
  scale_color_brewer(palette = 'Set1', labels = c('Bottom','Middle','Top')) +
  scale_fill_brewer(palette = 'Set1') + 
  labs(
    x = 'Vertical Location of Slider (ft.)', 
    y = 'Fitted Whiff Probability', 
    title = 'How Does Prior FF Location Impact Whiff Probability?', 
    subtitle = 'FF locations are relative to strike zone', 
    color = 'Prior FF\nLocation') + 
  theme(plot.title = element_text(face='bold'))


  
  
