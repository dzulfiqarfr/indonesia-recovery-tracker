# Indonesia EconRecovery <img src="images/ier_hexsticker.png" align="right" height="140"/>

The Indonesia EconRecovery website publishes charts and tables on key economic indicators to track the country’s recovery from the COVID-19 pandemic. The website gets updated every day.

## Data

The website sources most datasets from Statistics Indonesia (BPS) and collects them through the agency’s [application programming interface (API)](https://webapi.bps.go.id/). In addition, the website also puts together datasets from [Bank Indonesia (BI)](https://www.bi.go.id/id/publikasi/laporan/default.aspx) and [Google](https://www.google.com/covid19/mobility/), all of which are available on their website. For the COVID-19 metrics, the website uses Our World in Data’s (OWID) coronavirus dataset, which you can find on [its repo](https://github.com/owid/covid-19-data/tree/master/public/data).

For key economic indicators, the website adds projections made by the International Monetary Fund (IMF), World Bank or the Organization for Economic Cooperation and Development (OECD). Where appropriate, the development targets for indicators such as the unemployment and poverty rates outlined by the government are also included.

Although the website gets updated regularly, in some cases there are delays in reporting. For example, although BPS updates the inflation data on the first day of every month, the inflation by component dataset does not get updated on the same day. The same is also true for other datasets that still come in what the agency called a static table.

As of the latest update released on April 4, 2021, the indicators included on the website are as follow:

Data | Source  
------ | ----------  
[Consumer confidence index](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/cci.html) | BI  
[Employment](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/employment.html) | BPS  
[Economic growth](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/gdp.html) | BPS  
[Inflation](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/inflation.html) | BPS  
[Mobility](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/mobility.html) | Google  
[Poverty](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/poverty.html) | BPS  
[Retail sales index](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/rsi.html) | BI  
[COVID-19](https://dzulfiqarfr.github.io/indonesia-recovery-tracker/index.html) | OWID  

The datasets are available in a tidy format in CSVs, which you can find in the [data 📁](/data).


## What's new

**April 9, 2021**:  
- Added the IMF’s April 2021 projections on economic growth, inflation and unemployment rates to the key econ indicator table;  
- Added a thousand separator to the COVID-19 case figure in the key COVID-19 indicator table;  
- Added a timestamp to the key indicator tables;  
- Minor improvements to inflation charts;  
- Fixed a minor bug in the filter button for the table on economic growth by expenditure;   
- Added a condition to overwrite the **ggplot2**-based charts;  
- Separated key economic and COVID-19 indicators into two tables;  
- Added "Latest" section to the homepage; and  
- Changed the direction of the color scale for inflation by expenditure table

**April 4, 2021**:  
- the name was changed to Indonesia EconRecovery;  
- the layout was updated and now used the [distill](https://rstudio.github.io/distill/) 📦;  
- added charts on the Consumer Confidence Index by income group and the Retail Sales Index by category;  
- added charts and tables on inflation, employment and poverty;  
- made the tables filterable and sortable by moving to the [reactable](https://glin.github.io/reactable/index.html) 📦 from the [gt](https://gt.rstudio.com/index.html) 📦;  
- added links to download datasets and charts;  
- improved chart tooltips; and  
- improved clarity of the key indicator table on the homepage  


## Acknowledgement

- The code to add a logo to the **ggplot2**-based charts is adopted from [Thomas Mock](https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/)  
- The function to create a diverging color scale for the tables is adopted from [Paul](https://stackoverflow.com/questions/64469714/set-asymmetric-midpoint-for-data-color-in-gt-table)  