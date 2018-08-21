`songPreference` is a R package that permits analyzing the output of SingSparrow. It allows to filter data and produce basic reports.

## Installing songPreference
`songPreference` can be installed directly from GitHub. The package `devtools` is required. If that package is not present in R, install it with the following line of code:

```{r}
install.packages("devtools")
```

Then install `songPreference` with the following code:

```{r}
install_github(repo= "crodriguez-saltos/songPreference", build_vignettes= TRUE)
```

## Getting started
The package comes with useful vignettes to get started. Once the package is installed, check the vignettes by using the following code:

```{r}
browseVignettes(package= "songPreference")
```
