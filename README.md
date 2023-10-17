# An Interactive Clustering-Based Visualization Tool for Air Quality Data Analysis


Examining PM2.5 (atmospheric particulate matter with a maximum diameter of 2.5 micrometers), seasonal patterns is an important research area for environmental scientists. An improved understanding of PM2.5 seasonal patterns can help environmental protection agencies (EPAs) make decisions and develop complex models for controlling the concentration of PM2.5 in different regions. This work proposes an R Shiny App web-based interactive tool, namely a “model-based time series clustering” (MTSC) tool, for clustering PM2.5 time series using spatial and population variables and their temporal features, like seasonality. Our tool allows stakeholders to visualize important characteristics of PM2.5 time series, including temporal patterns and missing values, and cluster series by attribute groupings. We apply the MTSC tool to cluster Taiwan’s PM2.5 time series based on air quality zones and types of monitoring stations. The tool clusters the series into four clusters that reveal several phenomena, including an improvement in Taiwan's air quality since 2017 in all regions, although at varying rates, an increasing pattern of PM2.5 concentration when moving from northern towards southern regions, winter/summer seasonal patterns that are more pronounced in certain types of areas (e.g., industrial), and unusual behavior in the southernmost region. The tool provides cluster-specific quantitative figures, like seasonal variations in PM2.5 concentration in different air quality zones of Taiwan, and identifies, for example, an annual peak in early January and February (maximum value around 120  ). Our analysis identifies a region in southernmost Taiwan as different from other zones that are currently grouped together with it by Taiwan EPA (TEPA), and a northern region that behaves differently from its TEPA grouping. All these cluster-based insights help EPA experts implement short-term zone-specific air quality policies (e.g., fireworks and traffic regulations, school closures) as well as longer-term decision-making (e.g., transport control stations, fuel permits, old vehicle replacement, fuel type).


Users can upload the dataset they wish to explore using the 'Browse' button. This dataset should include the specific columns 'Series' (all the time series), 
'Date', 'cat.col' (column which categorizes the series) names, and 'latitude' and 'longitude' (the area of the colelcted series). 
It also needs to include other columns as splitting variables (domain-relevant attributes) 
with desired names. 
The frequency option helps the user choose the time series frequency that is currently 'Weekly' (for daily time series) or 'Monthly' 
(for monthly time series) is available for our tool.  

Our example dataset is monthly ```PM2.5-Taiwan.csv```, which shows the daily Taiwan PM2.5 index in different regions and counties from Jan 2006 to Nov 2019. 
Domain-relevant attributes applied to this dataset include TEPA zones (Taiwan air quality zones) (10 categories), station type (6 categories), metro (2 categories), population (numeric), and  administrative level (3 categories). 

For the purpose of comparing two sets of parameter settings, our tool creates two sets of results, presented in two tabs.
The app screenshot using MOB depth = 3 and 2, Prune option = 'AIC' with all the above domain-relevant attributes as splitting variables:

![alt text](<https://github.com/mahsaashouri/model-based-time-series-clustering-tool/blob/main/screenshot.png>)



Binder 
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/mahsaashouri/model-based-time-series-clustering-tool/main?urlpath=shiny)
