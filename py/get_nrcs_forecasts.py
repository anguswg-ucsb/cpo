# Credit: Dillon Ragar

import zeep
from zeep import helpers
import pandas as pd
from matplotlib import pyplot as plt

# NRCS SOAP API Client
NRCS_URL = 'https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL'
client   = zeep.Client(wsdl = NRCS_URL)

def nrcs_fx_date_request(
  station_code,
  element_code,
  forecast_period,
  begin_publication_date, 
  end_publication_date, 
  state_id  = "CO", 
  network   = "USGS", 
  site_name = None
  ):
    """
    Get forecast data from NRCS AWDB (SOAP API) for all dates within a range.
    Note: volume forecasts are in k_ac units (thousands of acre-feet).
    all query types: https://www.nrcs.usda.gov/wps/portal/wcc/home/dataAccessHelp/webService/webServiceReference
    
    :param (str) station_code: from NRCS AWDB, the station reference that beings the 
        "station triplet" formatting.
    :param (str) element_code: the element code, i.e. "SRVO" : stream volume adjusted. 
    :param (str) forecast_period: total forecasted stream volume during period
    :param (str) begin_publication_date: start date
    :param (str) end_publication_date: end date
    :param (str) state_id: state, default Colorado
    :param (str) network: entity responsible for "station" associated w/ forecast, default USGS. 
    :param (str) site_name: optional, name to include in DataFrame for locating site
    :return: (pd.DataFrame)
    """
    station_triplet = f"{station_code}:{state_id}:{network}"

    res = client.service.getForecastsByPubDate(
        station_triplet,
        element_code,
        forecast_period,
        begin_publication_date,
        end_publication_date
    )
    
    metadata = client.service.getStationMetadata(station_triplet)    
    
    # response is list of dicts containing lists
    out_list = helpers.serialize_object(res, dict)
    df = pd.concat([pd.DataFrame(i) for i in out_list])
    
    # make datetime index
    df.index = pd.to_datetime(df['publicationDate'])
    df = df.drop(columns=['publicationDate'])
    
    # foramt dtypes
    df['calculationDate'] = pd.to_datetime(df['calculationDate'])
    df['exceedenceValues'] = df['exceedenceValues'].astype(float)
    
    #kac to ac conversion
    #df['exceedenceValues'] *= 1000
    
    df['station_code'] = station_code
    df['latitude'] = float(metadata['latitude'])
    df['longitude'] = float(metadata['longitude'])
    
    if site_name is not None:
        df['site_name'] = site_name

    return df

# pull together list of all station codes in Colorado
stations = client.service.getStations(
    stateCds = "CO",
    networkCds = "USGS",
    logicalAnd = True
)


def get_forecast_data(station_codes, start_date, end_date, forecast_period, element_code, network):
    all_fcsts = []

    for loc in station_codes:
        print(f"Station triplet: {loc}")

        try:
            fcst_df = nrcs_fx_date_request(
                station_code=loc,
                element_code=element_code,
                forecast_period=forecast_period,
                begin_publication_date=start_date,
                end_publication_date=end_date,
                network = network
            )

            all_fcsts.append(fcst_df)
        except Exception as e:
            # Print the error message if an error occurs
            print(f"Error occurred for {loc}: {str(e)}")
            continue

    df = pd.concat(all_fcsts)

    return df

# get all forecasts for all sites in colorado 
forecasts = get_forecast_data(
    station_codes   = stations,
    element_code    = "SRVO", # stream volume
    forecast_period = "APR-JUL",
    start_date      = "1970-01-01",
    end_date        = "2023-10-01",
    network         = "USGS"
    )
# save out forecasts data
forecasts.to_csv("data/nrcs/nrcs_forecasts.csv")
