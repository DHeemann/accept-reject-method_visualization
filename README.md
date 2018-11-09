# accept-reject-method_visualization
R Shiny app to visualize the acceptance-rejection method to sample from a user-specified probability density function

![accept_reject](https://user-images.githubusercontent.com/36103689/48264018-5e7ee780-e428-11e8-8551-cfa8b7cb5303.png)


The user can specify any univariate probability density function from which they want to sample from. 
The entered PDF has to follow R-syntax (e.g. |x| -> abs(x))

Current proposal distributions are the Uniform, Gamma & the Normal distribution. 
With the "blow-up" factor, one can assure that the proposal distribution is above the pdf one wants to sample from for any x.
