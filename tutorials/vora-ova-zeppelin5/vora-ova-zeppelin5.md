---
title: Working with Time Series using Apache Zeppelin
description: SAP Vora 1.4: In-memory engine enables efficient analyses of time series data in distributed environments
primary_tag: products>sap-vora
tags: [  tutorial>beginner, topic>big-data, topic>sql, products>sap-vora ]
---

## Prerequisites  
 - [Working with Hierarchies using Apache Zeppelin](http://www.sap.com/developer/tutorials/vora-ova-zeppelin2.html)


## Next Steps
 - [Working with Document Store Engine in SAP HANA Vora](http://www.sap.com/developer/tutorials/vora-ova-zeppelin7.html)

## Details
### You will learn  
You will learn how to process time series data using SAP Vora 1.4 engine.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Time Series processing)]
The time series engine in SAP Vora 1.4 enhances the in-memory engine by enabling time series data to be efficiently analyzed in distributed environments. It supports highly compressed time series storage and time series analysis algorithms that work directly on top of the compressed data.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Running notebook 5_Time_Series)]
Select the `5_Time_Series` notebook.

Start off by creating the required partition function and scheme, then follow by creating a table and loading time-series data.
![Start time series](zep5_02_14.jpg)

Continue with the paragraphs as below, doing simple time-series analysis, calculate the advance from a specific point in time.
![Time series analysis](zep5_03_14.jpg)

Next determine the trend of sales over a time period, multi column analysis of Sales-Advertising and overall trend in sales and advertising.
![More time series analysis](zep5_04_14.jpg)

[DONE]
[ACCORDION-END]

## Next Steps
- [Working with Document Store Engine in SAP HANA Vora](http://www.sap.com/developer/tutorials/vora-ova-zeppelin7.html)
