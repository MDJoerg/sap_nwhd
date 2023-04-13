# REST API

## Overview

The REST API is implemented in package ZNWHD_RSA (REST Service API). After installing NWHD the REST API is not active by default.
You have to configure a service via transaction SICF.

After configuring the SICF node the LDB data can be accessed by the NWHD REST API endpoint. Using this API you can connect via the Python connector and do Data Science stuff.

All SAP security features are available.


## Installation

Create a new node with transcaction SICF. The default name is "nwhd_rest". Use "NWHD REST API endpoint" for description.
Enter the class ZCL_NWHD_RSA_ROUTER as implementation class in the tab "HANDLER LIST".

After saving a package has to be assigned. Use "local" or "$TMP" if no transport request is required. Otherwise use a package with transport to production.

Activate and test the new SICF node. An error message should be displayed. Remember the path to the endpoint.

## REST API (V1)

All of the following methods accepts typical SAP parameters like sap-client. Depending on your system configuration these additional parameters could be required. 
The default syntax is ´https://saphost/api_path/version/method´

### Get available source systems

This method `sources`returns get all active source data systems with detail information. The source GUID of a system is required for other api methods.

#### Request

- HTTP: GET
- Syntax: https://saphost/api_path/v1/sources
- Example: http://saphost/nwhd_rest/v1/sources
#### Response

```json
[{
	"SRC_GUID": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
	"SOURCE_ID": "D01",
	"SOURCE_TYPE": "SAPNWABAP",
	"SOURCE_DESCRIPTION": "SAP Dev"
}, {
	"SRC_GUID": "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
	"SOURCE_ID": "P01",
	"SOURCE_TYPE": "SAPNWABAP",
	"SOURCE_DESCRIPTION": "SAP Prod"
}]
```

### Get available numeric value keys 

This method `numeric_available` returns all value keys and count for a given source system. 

#### Request

- HTTP: GET
- Syntax: https://saphost/api_path/v1/numeric_available/source
- Optional Parameters:
    - from_date - start date in SAP syntax - e.g. "20230101"
    - to_date - end date in SAP syntax - e.g. "20231231"
- Examples: 
    - http://saphost/nwhd_rest/v1/numeric_available/source
    - http://saphost/nwhd_rest/v1/numeric_available/source?from_date=20230101&to_date=20230131

#### Response

```json
[{
	"COLLECTOR": "BALog",
	"CATEGORY": "DB",
	"FIELD": "Count",
	"COUNT": 16728
}, {
	"COLLECTOR": "BALog",
	"CATEGORY": "Last24h",
	"FIELD": "AllTypeCount",
	"COUNT": 14729
}, {
	"COLLECTOR": "BALog",
	"CATEGORY": "Last24h",
	"FIELD": "AllTypeMsgAbort",
	"COUNT": 1
}, {
	"COLLECTOR": "BALog",
	"CATEGORY": "Last24h",
	"FIELD": "AllTypeMsgCount",
	"COUNT": 15046
}]
```

### Get numeric value timeseries

This method `numeric_timeseries` returns all values with timestamps for a given source system and value key. 

#### Request

- HTTP: GET
- Syntax: https://saphost/api_path/v1/numeric_available/source/collector/category/field
- Optional Parameters:
    - from_date - start date in SAP syntax - e.g. "20230101"
    - to_date - end date in SAP syntax - e.g. "20231231"
    - max_rows - max records for sap raw data selection - e.g. "10000"
- Examples: 
    - http://saphost/nwhd_rest/v1/numeric_available/source/WP/TypeDIA/Runtime
    - http://saphost/nwhd_rest/v1/numeric_available/source/WP/TypeDIA/Runtime?max_rows=1000000
    - http://saphost/nwhd_rest/v1/numeric_available/source/WP/TypeDIA/Runtime?max_rows=1000000&from_date=20230101&to_date=20230131


#### Response

```json
[{
	"VALUE": 0.0000000,
	"TIMESTAMP": "2022-12-28T10:11:52Z"
}, {
	"VALUE": 0.0000000,
	"TIMESTAMP": "2022-12-28T10:45:51Z"
}, {
	"VALUE": 4.0000000,
	"TIMESTAMP": "2022-12-28T10:50:51Z"
}, {
	"VALUE": 8.0000000,
	"TIMESTAMP": "2022-12-28T10:55:51Z"
}, {
	"VALUE": 0.0000000,
	"TIMESTAMP": "2022-12-28T11:00:51Z"
}]
```