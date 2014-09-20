---
title       : Welcome to the Energy VisualizeR!
subtitle    : User's Documentation
author      : 
job         : 
framework   : io2012      # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## User's documentation: Introduction

This **Shiny App** visualises profiles of the energy consumption and energy production (from photovoltaic panels) that are typical of central Europe. Measurements are available every 15 minutes for about 460 days. [note: as the original data is proprietary data, this data subset has been here deliberately quite heavily noised! ;-)] 


The data is a CSV file and is read at the launch of the Shiny App:


```r
data <- read.csv("data_coursera_noised.csv")
dim(data)
```

```
## [1] 44832     9
```

The data set contains 44832 entries and 7 energy meters (plus one column for the dates, and one empty column)


---

## User's documentation: Instructions



1. **Enter the power production (peak power, in kWp) of your photovoltaic installation (or the one you are planning to build on the roof of your house!) as well as your annual energy consumption in kWh (energy consumption without the energy for heating neither for the hot water).**
<br> If the user does not know these values, typical values are set by default. Modifying the peak power production and the energy consumption will rescale the profiles shown on the plot.



---

## User's documentation: Instructions (continued)




<ol start="2">
    <li>  <b>Click on the play button to start going through the historical data!</b> <br>
<i>Warning: historical values are not available for all energy meters all the time. This is due to the fact that these energy meters where not installed all at the same time, and some were decommissioned.</i>  </li>

    <li>   <b>Optional: change the load type and select another energy meter to get different representative energy consumption/production profiles!</b>  </li>
</ol>


The following loads types are available: 
- House (general energy consumption: lights, appliances, fridge, etc.)
- Heat pump: for heating the house
- Hot water heat pump: for heating the hot water
- photovoltaic production




---


## User's documentation: final notes

Going through the historical data, one can notice periodic behaviour or seasonal changes (e.g. lower photovoltaic production in winter, higher consumption of the heat pump in order to deliver the required heat, etc.)

Additionally to showing the profiles, the ***Supply Cover Factor*** (i.e. the proportion of the photovoltaic production that the user directly consumes at his own home [and not the part that is injected in the electricity network]) is computed for each day (written in the plot's title). A *Supply Cover Factor* of 100% means that 100% of the energy produced by the photovoltaic installation is consumed directly at home. A low *Supply Cover Factor* happens when it is very sunny or when the customer do not use much energy when the photovoltaic production is high. Additionally, if the photovoltaic installation is over sized, the *Supply Cover Factor* will be very low (the user of the App can easily test the effects of an over-sized PV installation by modifiying the input of the PV's peak power).



<style>
strong {
  font-weight: bold;
}
</style>


<style>
em {
  font-style: italic
}
</style>




        


---

