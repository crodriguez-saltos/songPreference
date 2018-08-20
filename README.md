---
title: "README"
author: "Carlos Antonio Rodríguez-Saltos"
date: "August 20, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

songPreference is a R package that permits analyzing the output of SingSparrow. It allows to filter data and produce basic reports.

## Install songPreference
songPreference can be installed directly from GitHub. The package `devtools` is required. If that package is not present in R, install it with the following line of code from the R console:

```{r}
install.packages("devtools")
```

Then install `songPreference` with the following code:

```{r}
install_github(repo= "songPreference", username= "crodriguez-saltos")
```
