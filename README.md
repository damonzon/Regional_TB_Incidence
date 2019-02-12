# Regional_TB_Incidence

This repo creates barplots for wWHO worldwide regional data
for Tbercculosis Incidence (Cases/100,000) for 2000-2016.

The app.R Shiny app can be run by the following commands in Rstudio.

library(shiny)

library(shinythemes)

library(data.table)

library(plotly)

shiny::runGitHub("WHO_TB_Burden/Barplot", "damonzon")

