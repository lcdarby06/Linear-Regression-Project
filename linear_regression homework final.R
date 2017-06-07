## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?


states_rds <- readRDS("states.rds")


## 1.Examine/plot data before fitting model
str(states_rds)
plot(states_rds$metro, states_rds$energy)
cor(states_rds$metro, states_rds$energy, use = "complete.obs")

## 2. Print and interpret model 'summary'

energy_mod <- lm(states_rds$energy ~ states_rds$metro, data = states_rds)
summary(energy_mod)

## Based on the summary, the model explains about 11% of the variation of energy from its mean. The metro variable is significant at the 5% level. 
## For each percentage point increase in pct. of residents in a state living in a metro area, the model predicts a 2.29 decrease in per capita energy in a state.

## 3. 'plot' the model to look for deviations from modeling assumptions

plot(energy_mod)

## Repeating steps 1-3 for additional variable(s)

cor(states_rds$energy, as.numeric(states_rds$region), use = "complete.obs")

## After running pairwise correlations for the dependent variable (energy) and possible explanatory factors in the dataset, the ones which had a correlation 
## greater than or equal to the metro variable were as follows: area, toxic, green, house, senate

## New model including all of these additional variables
energy_mod2 <- lm(states_rds$energy ~ states_rds$metro + states_rds$area + states_rds$toxic + states_rds$green + states_rds$house + states_rds$senate, data = states_rds)
summary(energy_mod2)

## This model was found to explain about 78% of the variation around the mean (R^2), and 75% accounting for loss of degrees of freedom.  
## However, only the toxic and green variables were found to be significant approaching the 0% level, and the metro area is no longer significant. 
## The area variable was marginally significant at the 10% level.
## Based on this info, the senate variable will be dropped first as it is the least significant.

energy_mod3 <- lm(states_rds$energy ~ states_rds$metro + states_rds$area + states_rds$toxic + states_rds$green + states_rds$house, data = states_rds)
summary(energy_mod3)

## This model is essentially the same with an R^2 of 78% and adjusted R^2 of 76%. 
## All previously significant variables are still significant, but the house variable will now be dropped since it's the least significant.

energy_mod4 <- lm(states_rds$energy ~ states_rds$metro + states_rds$area + states_rds$toxic + states_rds$green, data = states_rds)
summary(energy_mod4)

## This model is essentially the same as the previous models with a slightly better adjusted R^2 (over 76%). 
## The metro variable in the model is still not significant, so it will be dropped.

energy_mod5 <- lm(states_rds$energy ~ states_rds$area + states_rds$toxic + states_rds$green, data = states_rds)
summary(energy_mod5)

## The adjusted R^2 is slightly better at 77%. The area variable is only marginally significant (<10%), so it will be dropped.

energy_mod6 <- lm(states_rds$energy ~ states_rds$toxic + states_rds$green, data = states_rds)
summary(energy_mod6)

## This model is slightly worse at adjusted R^2 of 75% and R^2 of 76%, so it seems that the best model includes the marginally significant area variable. 
## Additionally, this model is significantly better than the original model which only included the metro variable.


## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

## 1. Add on to regression equation by generating interaction term and testing interaction.
states_rds$area_density <- states_rds$area*states_rds$density
energy_mod7 <- lm(states_rds$energy ~ states_rds$area + states_rds$toxic + states_rds$green + states_rds$area_density, data = states_rds)
summary(energy_mod7)

## For the model above, it was hypothesized that the (population) density of the area of the state would impact the effect of area on energy consumed per capita.
## However, based on a p-value of 0.8 for this interaction term, this appears to not be the case.

energy_mod8 <- lm(states_rds$energy ~ states_rds$area + states_rds$toxic + states_rds$green + states_rds$region, data = states_rds)
summary(energy_mod8)

## The base level is considered the West since that category is omitted from the output. 
## Based on the results, a state being in a region other than the West is predicted to have a higher energy consumption per capita, holding other included variables constant.
## These estimates are 0.036, 0.056, and 0.026 higher units of energy consumption per capita for the N. East, South, and Midwest regions, respectively.
## However, these estimates are imprecisely measured for the N. East and Midwest regions, as indicated by a p-value > 0.1.
