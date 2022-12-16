# SAP ABAP NetWeaver Health Data

The main idea behind this project is to collect different health data from an SAP ABAP system and publish this information to different publishers like local or remote database storages, mqtt brokers, ... The SAP system acts as an sensor and distribute the data to a central space with a push message.

The project is born by practical requirements to support monitoring of external systems and collect, display and analyze "sensor data". 

The main development objects are:
- jobs - to call different collectors and send the data to different publishers
- collectors - collect data an prepare it like a typical sensor
- publisher - send the collected data to a central endpoint

Other development objects are planned to support different transfer channels, process detected exceptions, ..

All development object types support the interface pattern and can be enhanced by custom development. 

