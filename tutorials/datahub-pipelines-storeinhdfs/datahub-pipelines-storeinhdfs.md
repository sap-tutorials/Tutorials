---
title: Store sensor data in HDFS in SAP Data Hub, developer edition 1.4
description: Use HDFS to store sensor data by using SAP Data Hub, developer edition 1.4.
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Prerequisites  

 - You have completed [Use a message broker](https://developers.sap.com/tutorials/datahub-pipelines-broker.html)

## Next Steps
 - [Bundle data (via JavaScript)](https://developers.sap.com/tutorials/datahub-pipelines-bundledata.html)

## Details
**This tutorial is obsolete and will be removed shortly (end of-November 2018).**
### You will learn  
During this tutorial, you will learn how to store sensor data in the Hadoop Distributed File System (HDFS). You will use an **HDFS Producer** operator for this.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Check Hadoop)]
The SAP Data Hub, developer edition per default comes with an Apache Hadoop installation. Open `http://localhost:50070` and check that HDFS is up and running.

![picture_01](./datahub-pipelines-storeinhdfs_01.png)  

Navigate to **Utilities** | **Browse the file system (1)** and display the `/tmp/hdfsManager` directory (when you install SAP Data Hub, developer edition initially, this directory is empty).

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add and configure HDFS Producer)]
Open the pipeline which you have created during the previous tutorials (`test.myFirstPipeline`) in the modelling environment `http://localhost:8090`. For this, select the **Graphs** tab in the tab bar on the left side and search for `test.myFirstPiepline`.

Remove the connection between the **`Kafka Consumer2`** operator and the **`ToString` Converter** operator.

Add a **HDFS Producer** operator to the pipeline by drag & drop. Then connect the `message` port of the **`Kafka Consumer2`** operator to the `inFile` port of the **HDFS Producer** operator.

![picture_02](./datahub-pipelines-storeinhdfs_02.png)  

Configure the **HDFS Producer** operator. You need to maintain the following properties:

| Property                       | Value                                 |
| ------------------------------ | ------------------------------------- |
| `append`                       | `false`                               |
| `path`                         | `/tmp/hdfsManager/test_<counter>.txt` |
| `hadoopNamenode`               | `hdfs:9000`                           |
| `hadoopUser`                   | `hdfs`                                |

The **HDFS Producer** will write the received data to files in the `/tmp/hdfsManager` directory. The files follow the scheme `test_<counter>.txt` (whereas `<counter>` is an incremental integer).

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add and configure HDFS Consumer)]
Add a **HDFS Consumer** operator to the pipeline by drag & drop. Then connect the `outFile` port of the **HDFS Consumer** operator to the `ininterface` port of the **`ToString` Converter** operator.

![picture_03](./datahub-pipelines-storeinhdfs_03.png)  

Configure the **HDFS Consumer** operator. You need to maintain the following properties:

| Property                       | Value                               |
| ------------------------------ | ----------------------------------- |
| `path`                         | `/tmp/hdfsManager/`                 |
| `hadoopNamenode`               | `hdfs:9000`                         |
| `onlyReadOnChange`             | `true`                              |
| `hadoopUser`                   | `hdfs`                              |

Afterwards press the **Save** button.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Execute the data pipeline)]
Press the **Run** button to execute the pipeline.

When the **Status** tab indicates that the pipeline is running, use the context menu **Open UI** of the **Terminal** operator to see the generated sensor data.

In contrast to the previous tutorial, this time the generated sensor data is not sent from the **Kafka Receiver** operator to the **Terminal** operator directly, but via HDFS. Hence the **Terminal** also shows you information about the created files.

Open `http://localhost:50070` and display the `/tmp/hdfsManager` directory. The longer the pipeline runs, the more files you will find there.

![picture_04](./datahub-pipelines-storeinhdfs_04.png)  

Stop the pipeline by pressing the **Stop** button.

[ACCORDION-END]

---
