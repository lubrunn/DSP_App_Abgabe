###1. Default
The default model contains fixed hyperparamter. By opening the custom tab one can
inspect the default values.
<br>
###2. Custom
The custom model allows for an individual choice of hyperparameter.
Be aware!
The following hyperparameter increase the time of computation:
``tree_depth`` ``trees`` ``mtry``
<br>
###3. Hyperparameter tuning
This model allows for basic hyperparamter tuning. In this case
the model is tuned via grid search. The following variables are tuned
over the samples of the data:
``min_n`` ``tree_depth`` ``learn_rate``
``loss_reduction``.
Please note that the computation time increases substantilly by increasing
the size of the grid.
