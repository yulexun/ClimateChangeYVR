# Non-Linear Effects of Pressure, Precipitation, Wind, and Gust Speed on Temperature at Vancouver International Airport

## Overview
This study investigates how atmospheric factors influence temperature at Vancouver International Airport using a polynomial regression model.

The analysis shows that total precipitation has the strongest effect, significantly reducing mean temperature, while wind speed and atmospheric pressure exhibit non-linear and consistent negative impacts, respectively. 

By explaining 61% of the variability in temperature, the model demonstrates that precipitation, pressure, gust speed and wind speed are key drivers of regional climate patterns. 

Understanding these relationships helps inform climate adaptation strategies for important infrastructure, including airports, in a warming world.

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from Government of Canada.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models for candidate. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download, clean data and build models.

## Statement on LLM usage

The drafted outline, the color coded maps and the figure captions were written with the help of ChatGPT 4. The prose of sentences and grammar across the paper is checked and modified using Microsoft Editor. The entire chat history with ChatGPT 4 is available in other/llm_usage/usage.txt. There is no chat history with Microsoft Editor.

## Note

This R project is setup with [Positron](https://positron.posit.co/), the new IDE by Posit PBC. The properties of this project is stored in `/renv/settings.json`. We use renv for reproducibility and portability. With the metadata from the lockfile, other researchers can install exactly the same version of every package.
You can run 
```sh
renv::restore()
```
to restore the R project emvironment. We also included a .Rproj file for RStudio users. For more information, see [this Github Issue](https://github.com/posit-dev/positron/discussions/3967) and [renv](https://rstudio.github.io/renv/articles/renv.html).

