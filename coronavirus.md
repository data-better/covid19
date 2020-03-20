---
title: "Introduction to the Coronavirus Dataset"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to the Coronavirus Dataset}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, 
                      message=FALSE, 
                      warning=FALSE, 
                      fig.height=5, 
                      fig.width=8,
                      collapse = TRUE,
                      comment = "#>")
```

### The coronavirus dataset

The `coronavirus` dataset provides a snapshot of the daily confirmed, recovered, and death cases of the Coronavirus (the 2019 Novel Coronavirus COVID-19) by geographic location (i.e., country/province). Let's load the dataset from the **coronavirus** package:

```{r }
#install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
data(coronavirus)
```

The dataset has the following fields:

* `date` - The date of the summary
* `Province.State` - The province or state, when applicable
* `Country.Region` - The country or region name
* `Lat` - Latitude point
* `Long`- Longitude point
* `cases` - the number of daily cases (corresponding to the case type)
* `type` - the type of case (i.e., confirmed, death, and recovered)

We can use the `head` and `str` functions to see the structure of the dataset:

```{r }
head(coronavirus)
str(coronavirus)
```

### Querying and analyzing the coronavirus dataset

We will use the **dplyr** and **tidyr** packages to query, transform, reshape, and keep the data tidy, the **plotly** package to plot the data and the **DT** package to view it:

```{r}
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
```

#### Cases summary

Let's start with summarizing the total number of cases by type as of `r max(coronavirus$date)` and then plot it: 

```{r}
total_cases <- coronavirus %>% 
  group_by(type) %>%
  summarise(cases = sum(cases)) %>%
  mutate(type = factor(type, levels = c("confirmed", "recovered", "death")))
total_cases
```


```{r}
plot_ly(data = total_cases, 
        x = ~ type, 
        y = ~cases, 
        type = 'bar',
        text = ~ paste(type, cases, sep = ": "),
    hoverinfo = 'text') %>%
  layout(title = "Coronavirus - Cases Distribution",
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Case Type"),
         hovermode = "compare")
```


We can learn from this table that, so far worldwide, the recovery rate is `r paste(round(100 *total_cases$cases[3] / total_cases$cases[1], 2), "%", sep = "")` and the death rate is `r paste(round(100 *total_cases$cases[2] / total_cases$cases[1], 2), "%", sep = "")`.

#### Top effected countries

The next table provides an overview of the ten countries with the highest confirmed cases. We will use the `datatable` function from the **DT** package to view the table:


```{r}
confirmed_country <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(Country.Region) %>%
  summarise(total_cases = sum(cases)) %>%
  mutate(perc = total_cases / sum(total_cases)) %>%
  arrange(-total_cases)
confirmed_country %>%
  head(10) %>%
  datatable(rownames = FALSE,
            colnames = c("Country", "Cases", "Perc of Total")) %>%
  formatPercentage("perc", 2)
```


As `r paste(round(100 * confirmed_country$perc[which(confirmed_country$Country.Region == "Mainland China")],1), "%", sep = "")` of the confirmed cases are in China, to get a better overview of the worldwide dist of the virus, let's exclude China and plot the rest of the world dist:

```{r}
coronavirus %>% 
  filter(type == "confirmed", 
         Country.Region != "Mainland China") %>%
  group_by(Country.Region) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(country = factor(Country.Region, levels = Country.Region)) %>%
  ungroup() %>%
  plot_ly(labels = ~ country, 
          values = ~ total_cases,
          type = "pie",
          textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(country, "<br />",
                     "Number of confirmed cases: ", total_cases, sep = "")) %>%
  layout(title = "Coronavirus - Confirmed Cases")
```

#### Recovery and death rates

Similarly, we can use the `pivot_wider` function from the **tidyr** package (in addition to the **dplyr** functions we used above) to get an overview of the three types of cases (confirmed, recovered, and death). We then will use it to derive the recovery and death rate by country. As for most of the countries, there is not enough information about the results of the confirmed cases, we will filter the data for countries with at least 25 confirmed cases and above:

```{r}
coronavirus %>% 
  filter(Country.Region != "Others") %>%
  group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(- confirmed) %>%
  filter(confirmed >= 25) %>%
  mutate(recover_rate = recovered / confirmed,
         death_rate = death / confirmed)  %>%
  datatable(rownames = FALSE,
            colnames = c("Country", "Confirmed", "Recovered", "Death", "Recovery Rate", "Death Rate")) %>%
   formatPercentage("recover_rate", 2) %>%
   formatPercentage("death_rate", 2) 
```

Note that it will be misleading to make any conclusion about the recovery and death rate. As there is no detail information about:

* There is no measurement between the time a case was confirmed and recovery or death. This is not an apple to apple comparison, as the outbreak did not start at the same time in all the affected countries.
* As age plays a critical role in the probability of survival from the virus, we cannot make a comparison between different cases without having more demographic information.

#### Diving into China

The following plot describes the overall distribution of the total confirmed cases in China by province:

```{r}
coronavirus %>% 
  filter(Country.Region == "China",
         type == "confirmed") %>%
  group_by(Province.State, type) %>%
  summarise(total_cases = sum(cases)) %>%  
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(- confirmed) %>%
  plot_ly(labels = ~ Province.State, 
                  values = ~confirmed, 
                  type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~ paste(Province.State, "<br />",
                                 "Number of confirmed cases: ", confirmed, sep = "")) %>%
  layout(title = "Total China Confirmed Cases Dist. by Province")
```
---
title: "Showing the Spatial Distribution of Covid-19 Confirmed Cases"
author: "Jarrett Byrnes"
date: "3/11/2020"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Put the title of your vignette here}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)
library(ggplot2)
coronavirus_spatial <- function(return_shape = c("point", "polygon"),
                                updated_data = FALSE,
                                returncols = c("all", "simple","reduced"),
                                ...){
  if(updated_data) coronavirus <- update_coronavirus()

  #get a world map
  worldmap <- rnaturalearth::ne_countries(returnclass = "sf", ...) %>%
    select(-type)

  #filter data to confirmed
  coronavirus_sf <- coronavirus %>%
    sf::st_as_sf(coords = c("Long", "Lat"),
             remove = FALSE,
             crs = sf::st_crs(worldmap))

  #get geospatial info about where the coronavirus is

  #if points join
  if(return_shape[1]=="point"){
    joined_corona <- sf::st_join(coronavirus_sf, worldmap)
  }else{
    joined_corona <- sf::st_join(worldmap,coronavirus_sf) %>%
    mutate(ifelse(is.na(cases), 0, NA)) #deal with countries with 0 cases so far
  }

  #select down to sane columns
  joined_corona <- switch(returncols[1],
                               "simple" = joined_corona %>% select(names(coronavirus_sf),
                                                                   admin, name_long, continent,
                                                                   region_un, subregion, region_wb),

                               "reduced" = joined_corona %>% select(name, name_long,
                                                                    names(coronavirus_sf),continent,
                                                                    region_un, subregion, region_wb,
                                                                    subunit, postal, formal_en,
                                                                    iso_a2, iso_a3, iso_n3,
                                                                    un_a3,
                                                                    pop_est, gdp_md_est),
                               joined_corona) #default

  joined_corona
}
```

## Updating the coronavirus data

To see the current distribution of cases around the world, we'll start by updating the coronavirus dataset.

```{r update, results='hide'}
library(coronavirus)
library(dplyr)
```

## Generating spatial data

The `coronavirus` package using the [rnaturalearth](https://cran.r-project.org/web/packages/rnaturalearth/) package for spatial information. [Natural Earth](https://www.naturalearthdata.com/) provides a wealth of spatial data easily accessed via R as [sf](https://cran.r-project.org/web/packages/sf/) objects. Using [sf](https://cran.r-project.org/web/packages/sf/), we can do a spatial join on the coronavirus data to get it synced up with the `ne_countries()` data from `rnaturalearth`.

Let's get the data as both points for plotting as well as polygons we can fill. We will filter to confirmed cases only.

```{r make_sf_objs}
library(sf)
library(rnaturalearth)
coronavirus_points <- coronavirus_spatial() %>%
                       filter(type == "confirmed")
                       
coronavirus_polys <- coronavirus_spatial(return_shape = "polygon")%>%
                       filter(type == "confirmed")
```

## Aggregating data

Let's aggregate so that we only look at total confirmed cases.

```{r aggregate}
library(dplyr)
coronavirus_points <- coronavirus_points %>%
  group_by(Province.State, Country.Region, name, continent) %>%
  summarize(cases = sum(cases))
coronavirus_polys <- coronavirus_polys %>%
  group_by(Country.Region, name, continent) %>%
  summarize(cases = sum(cases))
```

## Plotting

Great! We can now see what the spatial distribution looks like simply using ggplot. Note, the polys do not contain any records with nothing in them, so we'll need a baseline worldmap.

```{r plot1, fig.width=6}
library(ggplot2)
worldmap <- ne_countries(returnclass = "sf")
ggplot(data = worldmap) +
  geom_sf(fill = "white") +
  geom_sf(data = coronavirus_polys,
          mapping = aes(fill = log10(cases+1))) +
  geom_sf(data = coronavirus_points,
          mapping = aes(size = cases),
          alpha = 0.7, color = "black") +
  scale_fill_viridis_c(option = "D",
                       breaks = 0:4, labels = 10^c(0:4)) +
  scale_size_continuous(range = c(1, 8)) +
  labs(fill = "# of Cases", size = "# of Cases") +
  theme_minimal()
```
