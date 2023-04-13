# SAP NWHD - SAP ABAP NetWeaver Health Data

## Introduction
The main idea behind this project is to collect different health data from an SAP ABAP system and publish this information to different publishers like local or remote database storages, mqtt brokers, ... The SAP system acts as an sensor and distribute the data to a central space with a push message.

The project is born by practical requirements to support monitoring of external systems and collect, display and analyze "sensor data". 

The main development objects are:
- jobs - to call different collectors and send the data to different publishers
- collectors - collect data an prepare it like a typical sensor
- publisher - send the collected data to a central endpoint

Other development objects are planned to support different transfer channels, process detected exceptions, ..

All development object types support the interface pattern and can be enhanced by custom development. 

## Quick Start

1. Import the package as online or offline archive with [abapGit](https://abapgit.org/)
2. Open the area menu ZNWHD (enter ZNWHD into the SAPGUI OK code field)
3. Enter area menu "Administration -> Execute NWHD Job" (or transaction ZNWHD_JOB)
4. Execute the job with default option to see the result of all collectors in the report protocol
5. create a variant and choose one or more publishers (e.g. ZCL_NWHD_LDB_PUB for saving to local database)
6. plan a periodic job to collect the SAP NetWeaver health data 

## Use cases

### Single ABAP System

Plan a job to collect the data and save it to your local database.
Add a CDS based data model to the tables located in package ZNWHD_LDB to transform the data to time series style and work with it.

### One central ABAP System for storage and more than on satellite system in own data center

Plan a job in every satellite system and send the data via RFC or QRFC publisher to the central system. A RFC destination (SM59) and queue name is required.
If you want to watch the central system too you have to plan a job there and handle it like "Single ABAP System".

Within the central system the data will be stored in the local data base (LDB). See "Single ABAP System" for use it.

## Documentation

1. Installation and configuration
2. Development
3. Local Data Storage
4. API
    - [REST API](doc/rest/rest_api.md)
5. Python
    - [Python NWHD Connector Documentation](doc/pynwd/pynwd.md)
    - [Python Module pynwd.py](pynwd/pynwhd.py)
    - [Jupyter Notebook Demo - Connector with menu](pynwd/DemoNWHDConnectorMenu.ipynb)
    - [Jupyter Notebook Demo - Connector with scripting](pynwd/DemoNWHDConnectorJupyter.ipynb)


last update: 13.04.2023