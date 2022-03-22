# model-based time series clustering tool

We propose a web-based interactive tool, called `model-based time series clustering tool', 
for visualizing the results of clustering large collections of time series with cross-sectional domain-relevant attributes. 
Such data often arise in Internet-of-Things (IoT) and sensor-based applications, where each time series is coupled with cross-sectional information.
While the clustering algorithm in the background is automated, our visualization tool allows users to modify various parameters that lead to different cluster 
definitions and numbers of clusters. We illustrate the tool by applying it to an air quality dataset (PM2.5 index) collected in different monitoring stations in Taiwan.
Our web-based tool, based on R's Shiny App, helps visualize various characteristics of time series, such as temporal patterns and missing values, as well as clustering 
attribute groupings.

Users can upload the dataset they wish to explore using the `Browse' button. This dataset should include the specific columns `Series' (all the time series), 
`Date', `cat.col' (column which categorizes the series) names, and `latitude'and `longitude' (the area of the colelcted series). 
It also needs to include other columns as splitting variables (domain-relevant attributes) 
with desired names. 
The frequency option helps the user choose the time series frequency that is currently `Weekly' (for daily time series) or `Monthly' 
(for monthly time series) is available for our tool.  

Our example dataset is monthly ```PM2.5-Taiwan.csv```, which shows the daily Taiwan PM2.5 index in different regions and counties from Jan 2006 to Nov 2019. 
Domain-relevant attributes applied to this dataset include geographical division, region (5 categories), county (22 categories), administrative (3 categories), 
population (numeric), metro (2 categories), and airport (2 categories). 

For the purpose of comparing two sets of parameter settings, our tool creates two sets of results, presented in two tabs.
The app screenshot using MOB depth = 3 and 2, Prune option = 'AIC' with all the above domain-relevant attributes as splitting variables:

![alt text](<https://github.com/mahsaashouri/Interactive-tool-for-time-series-visualization/blob/main/screenshot.png>)

![alt text](<https://github.com/mahsaashouri/Interactive-tool-for-time-series-visualization/blob/main/screenshot2.png>)