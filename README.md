# notam-metar
Integration of NOTAM facility condition and METAR weather condition messages

As part of OPSNET-R proof-of-concept project.
1.3 M notice to [NOTAMs](https://www.faa.gov/air_traffic/publications/notices/) are issued in the US each year, everything from runway slippery to a light is out. [METARs](https://www.aviationweather.gov/metar) are weather notices. This project explores the benefits of integrating these.

## Goals

- Create a template for a visual representation of METAR weather condition data, specifically Runway Visual Range (RVR) and Ceiling for cloud cover, and demonstrate how NOTAMs can be visualized when both RVR and Ceiling are low.
- Start with 1 day, 1 airport, and consider pipeline for scaling up
- Consider possible genearlized information, such as 'how often does bad weather and equipment outages co-occur for a specific airport?', 'Which airports have the most outages?', 'how is does this vary by season?'.
- Ultimately would like to have a tool for FAA to quickly assess where bad weather conditions intersect with limited equipment functionality.
