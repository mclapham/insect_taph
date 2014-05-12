Scripts for Insect Taphonomy
===========

Data files and R scripts for data processing and figures for Karr and Clapham (2014) insect taphonomy paper.

IMPORTANT: Run data_acq.R script first to acquire data and filter based on criteria described in paper.

1) act_prd_comparison.R: compares actual proportion articulated against predicted articulation based only on the environment, mean size, and proportion of beetles in each 10 Myr bin. Plots figure 7.

2) fig1_plot.R: calculates proportion of articulated specimens per time interval; plots figure 1
 
3) glm_all_env.R: performs logistic regression models (articulation as a function of age and age+environment) for all insects; plots figure 3

4) glm_all_param.R: performs logistic regression with all parameters (age, size, environment, is_beetle) for all insects and predicts articulation while holding each one constant (excluding age). Parameters converted to binomial using Cenozoic and Carboniferous averages as end points and simulating smooth variation between. Plots figure 6.

5) glm_all_size.R: performs logistic regression model (articulation as a function of log10 wing element length) for all insects; plots figure 4

6) glm_orders_env.R: performs logistic regression models (articulation as a function of age and age+environment) for six suprafamilil groups; plots figure 2

7) glm_orders_size.R: performs logistic regression model (articulation as a function of log10 wing element length) for six suprafamilial groups; plots figure 5
