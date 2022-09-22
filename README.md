# nflPredictions
Mutliple regression models to predict outcomes of NFL games using statistics.
Currently predicting the outcomes of spreads and total points using various statistics on a 10 game rolling average. Attempting to improve the accuracy of the model, currently achieving about 60% accuracy against Vegas odds in optimal conditions. 
Data gathered using nflverse package and stathead.com, information can be found here: https://www.nflfastr.com/index.html and https://stathead.com/all/

Run 'runModel.R' to generate the model, then use the function found in 'makePredictions.R' to make prediction for the desired season and week. If desired, parameters can be changed, and 'testing function.R' can be used to quickly backtest the model, getting a basic idea of its performance. 
