---
title: Store sensor data in SAP Vora
description: Use SAP Vora to store sensor data by using SAP Data Hub, developer edition.
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - You have completed [Bundle data (via JavaScript)](https://www.sap.com/developer/tutorials/datahub-pipelines-bundledata.html)

## Details
### You will learn  
During this tutorial, you will store sensor data in SAP Vora. Thereto you will load the data which you have stored in HDFS to SAP Vora.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Add and configure SAP Vora HdfsLoader)]
Open the pipeline which you have created during the previous tutorials (`test.myFirstPipeline`) in the modelling environment `http://localhost:8090`.

Add a **`SAP Vora HdfsLoader`** operator to the pipeline by drag & drop. Then connect the `outFilename` port of the **HDFS Consumer** operator to the `inhdfsfilename` port of the **`SAP Vora HdfsLoader`**.

![picture_01](./datahub-pipelines-storeinvora_01.png)  

Configure the **`SAP Vora HdfsLoader`** operator. You need to maintain the following properties:

| Property &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;                     | Value                                |
| :------------------------------ | :------------------------------------ |
| `dsn`                          | `v2://vora:2202/?binary=true`        |
| `initStatements`               | `CREATE TABLE IF NOT EXISTS SENSORDATA (counter INTEGER, deviceid INTEGER, temperature DOUBLE, humidity DOUBLE, co2 DOUBLE, co DOUBLE, lpg DOUBLE, smoke DOUBLE, presence INTEGER, light DOUBLE, sound DOUBLE);`   |
| `tableName`                    | `SENSORDATA`                         |
| `hadoopNamenode`               | `hdfs:9000`                          |

Afterwards press the **Save** button.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Execute the data pipeline and check result in SAP Vora)]
Press the **Run** button to execute the pipeline.

When the **Status** tab indicates that the pipeline is running, wait a moment and then open the SAP Vora Tools `http://localhost:9225`.
Find table `SENSORDATA` on the **left (1)**. Then press the **Data Preview (2)**  button.

![picture_02](./datahub-pipelines-storeinvora_02.png)  

Stop the pipeline by pressing the **Stop** button.

**Attention**: If no data is stored in SAP Vora, you can add a **Terminal** operator to the pipeline and connect it with the `outresult` port of the **`SAP Vora HdfsLoader`** operator. If you see an error message like `could not handle api call, failure reason : execution of scheduler plan failed`, you might have incorrectly connected the **Kafka Consumer** operator and the **`ToString` Converter** operator. Please review the previous tutorial.

[ACCORDION-END]

---
