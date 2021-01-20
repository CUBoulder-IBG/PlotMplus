# PlotMplus
R package to plot loading, correlation matrix, scree plot, etc. from MPLUS output. 

Functions added so far

* plot_esem
* plot_cor
* plot_loading
* plot_scree

### `plot_cor`
MPLUS report contains the correlation matrix in a lower triangular format. The `plot_cor` converts it to a full square matrix and use the `corrplot` package to output a plot of correlation matrix.
It uses only the basic features of corrplot but advanced features will be added. 

### `plot_loading`
Plots the loading matrix. The loadings are extracted from MPLUS output from the parameters section. Any row with 'BY' is considered as loading. An example plot:



