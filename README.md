# US COVID Mortality
This repository contains the code I used for a project to determine if the official COVID death counts provided by the US Government between 2020 & 2021.

I do this in 3 steps:
* Train & validate a SARIMA model on time-series usig data up to 2019
* Forecast death counts for 2020 & 2021 -- since the model was traind with pre-COVID data, this gives us _expected deaths_
* Compare expected deaths and official total death counts: assuming no other major event happened between 2019 & 2020, this gives us the calculated COVID related deaths
* Finally, compare the calculated deaths with the actual death counts from the US government to arrive at a conclusion


# Code & Visualizations
Do take a look at the explanatory blog post and visualization of the data at Notion [here](https://www.notion.so/akalank/Are-the-official-COVID-related-mortality-figures-in-the-US-accurate-A-forecasting-exercise-using-a--e81f4e29ccd047c78392c3d6b1445e1b). To see the same post without leaving this repository, view the markdown version of the post here: [Code & Visualization]([https://github.com/akalankjayakumar/US-COVID-Mortality/blob/main/Code%20%26%20Visualization.md](https://github.com/akp-j/US-COVID-Mortality/blob/main/Code%20%26%20Visualization.md)).
