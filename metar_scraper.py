"""
Adapted from https://github.com/akrherz/iem/edit/master/scripts/asos/iem_scraper_example.py
Example script that scrapes data from the IEM ASOS download service
"""
from __future__ import print_function
import json
import time
import datetime
# Python 2 and 3: alternative 4
try:
    from urllib.request import urlopen
except ImportError:
    from urllib2 import urlopen

# Number of attempts to download data
MAX_ATTEMPTS = 6
# HTTPS here can be problematic for installs that don't have Lets Encrypt CA
SERVICE = "http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"

def download_data(uri):
    """Fetch the data from the IEM

    The IEM download service has some protections in place to keep the number
    of inbound requests in check.  This function implements an exponential
    backoff to keep individual downloads from erroring.

    Args:
      uri (string): URL to fetch

    Returns:
      string data
    """
    attempt = 0
    while attempt < MAX_ATTEMPTS:
        try:
            data = urlopen(uri, timeout=300).read().decode('utf-8')
            if data is not None and not data.startswith('ERROR'):
                return data
        except Exception as exp:
            print("download_data(%s) failed with %s" % (uri, exp))
            time.sleep(5)
        attempt += 1

    print("Exhausted attempts to download, returning empty data")
    return ""

def get_metar(stations = None):
    """
    Our main method
    stations is a DataFrame with airports as row index, and colums for start_time and end_time indicating the
    starting and ending time of the available notams for which we should get metars.
    Both writes out a .txt file for each station, and compiles the results into a DataFrame
    """
    import datetime as dt
    import pandas as pd
    
    service = SERVICE + "data=all&tz=Etc/UTC&format=comma&latlon=yes&"

    if stations is None:
        print('Provide DataFrame of stations and time range')
    else:
        
        df_all = [] # blank DataFrame to populate
        
        for index, row in stations.iterrows():
            
            startts = dt.datetime.strptime(row['start_time'], '%Y-%m-%d %H:%M:%S.%f')
            endts = dt.datetime.strptime(row['end_time'], '%Y-%m-%d %H:%M:%S.%f')

            station_service = service + startts.strftime('year1=%Y&month1=%m&day1=%d&')
            station_service += endts.strftime('year2=%Y&month2=%m&day2=%d&')

            uri = '%s&station=%s' % (station_service, index)
            print('Downloading: %s' % (index, ))
            data = download_data(uri)
            
            # Format data string as DataFrame, and keep just the data elements. Save as one large DataFrame
            df = pd.DataFrame([x.split(',') for x in data.split('\n')])
            df.columns = df.iloc[5] # Rename the columns with the values in the fifth row
            df = df[6:].reset_index() # Keep only values from the sixth row on, and reset the row index

            df_all.append(df)

            
            outfn = '%s_%s_%s.txt' % (index, startts.strftime("%Y%m%d%H%M"),
                                      endts.strftime("%Y%m%d%H%M"))
            out = open(outfn, 'w')
            out.write(data)
            out.close()
        
        # Once loop is finished, use concat 
        df_all = pd.concat(df_all)
        return df_all    

if __name__ == '__main__':
    get_metar()
