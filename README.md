
# [**cpo**](https://github.com/anguswg-ucsb/cpo)

CPO data collection, aggregation, and analysis code repository

The primary function of this repository is to provide reproducible
scripts for collecting and summarizing water rights and climate data for
the state of Colorado.

<br>

## Data sources:

### Water Rights data sources:

1.  CDSS/DWR via [cdssr](https://github.com/anguswg-ucsb/cdssr)

<br>

### Climate data sources:

1.  SWE from NRCS SNOTEL sites

2.  NRCS streamflow forecasts

3.  EDDI (Evaporative Drought Demand Index)

The main script in this repository is the `get_everything()` script that
when run, will pull together all available data from the sources above
and summarize the data into an annual value for every point (water
right) in each district.

## Data Collection steps:

<br>

### Pick a district from district shapefile

Loop through each district shape

<br>

### Retrieve all river networks for the AOI

Use `NHDPlusTools`

<br>

### Subset to the largest/mainstem rivers

By stream level

<br>

### Determine most upstream point of river network

We’ll use this point to determine which water right we want to use/get
data for

<br>

### Get all water rights within AOI

Purpose is to get a unique water right identifier (or multiple) for each
district

<br>

### Match water rights to upstream flowlines

Subset the water rights by river and determine which water right should
be associated with each mainstems most upstream point.

<br>

**To recap:** For each district, we get the biggest river(s), find the
most upstream point(s), and then find a water right(s) that represents
the most upstream point(s) on each mainstem.

**Techniques used:** <br> - fuzzy string matching <br> - basic
distance/nearest neighbor calculation

### Get CDSS call data

Use `cdssr` to get water right call data for each of our selected water
rights in our district of interest/AOI

<br>

*MOST UPSTREAM WATER RIGHT AND PRESCRIBE THE MOST JUNIOR WATER RIGHT
IMAGINABLE (99999.00000)*

<br>

–\> Simulate what it would be like if you were at the top of the river
and you had no water rights (i.e. all downstream users get first dibs on
water)
