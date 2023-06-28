
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

### Area of Interest

For each water district in Colorado, we collect the same set of water
right and climate data indicators. Below is a plot showing the different
water district boundaries in Colorado.

![Colorado Water Districts colored by basin](img/districts_plot.png)

### Pick a district from district shapefile

We loop through each one of the water districts and apply the same data
collection process for each water district. For this example, we will
walk through the data collection process for a single district, district
6.

``` r
# subset to example district 6
aoi <-
  districts %>% 
  dplyr::filter(district == 6) %>% 
  dplyr::select(district, division, basin, name, geometry)

aoi
#> Simple feature collection with 1 feature and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -105.7008 ymin: 39.84716 xmax: -104.9851 ymax: 40.16224
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 5
#>   district division basin        name                                   geometry
#>      <int>    <int> <chr>        <chr>                             <POLYGON [°]>
#> 1        6        1 South Platte Boulder Creek ((-105.6478 40.05384, -105.6488 …
```

![Colorado Water district 6](img/aoi_plot.png)

<br>

### Retrieve all river networks for the AOI

Use `NHDPlusTools` First thing we do is we get NHDPlus flowlines for
each district like so:

``` r
flowlines <- nhdplusTools::get_nhdplus(AOI = aoi)
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar
#> Spherical geometry (s2) switched on

nrow(flowlines)
#> [1] 327
```

We’ve got 327 unique flowlines in district 6.

![NHDPlus flowlines in AOI](img/fline_plot.png)

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
