## Load packages
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(here)
library(glue)
library(stringr)
library(flowchart)
library(arrow)
library(kableExtra)
library(gtsummary)
library(gt)

## Link Big Query Data
library(bigrquery)

## Causal ML packages
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(xgboost)
library(dagitty)
library(EValue)

## Flowchart
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

## Color scheme for plots
pastel.blue   <- "lightblue"
blue   = "#7EC8E3"
pastel.orange <- "#FDB462"
orange = "#F6A44D"

teal   <- "#76B7B2"
green  <- "#59A14F"
orange <- "#FDB462"
red    <- "#E15759"
purple <- "#B07AA1"
gold   <- "#EDC948"
grey   <- "#9D9D9D"

color_pal <- c(blue,
               teal,
               green,
               orange,
               red,
               purple,
               gold,
               grey)  

