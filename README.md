# pollen_predictor

Result of the Environmental Intelligence Grand Challenges event with the University of Exeter and the Met Office. We were tasked with creating a statistical model which would use weather data to predict grass pollen grain density at different sites across the UK. Using compound poisson generalised linear models we were able to effectively model daily pollen density. In the model we included variables for total daily precipitation, if the day of the year had previously had a pollen density of 30 g/m^3, the daily mean relative humidity, mean temperature in the first quarter, a second order polynomial of daily maximum temperature and a second order polynomial of the cumulative temperature of the year.

The results below show how the performance of the model varies between sites, with performance being very good at some sites e.g. Ipswich and less good at others e.g. King's College London.

![alt text](https://github.com/spoonerf/pollen_predictor/blob/master/pollen_results.png?raw=true)

The model can be interacted with here:

https://spoonerf.shinyapps.io/pollen_predict/

Location specific grass pollen predictions for today and the following three days can be found here: 

https://spoonerf.shinyapps.io/pollen_forecast/
