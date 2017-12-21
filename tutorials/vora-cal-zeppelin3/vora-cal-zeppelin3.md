---
title: Working with Disk Engine using Apache Zeppelin
description: Disk-based storage allows you to use relational capabilities without loading data into memory.
primary_tag: products>sap-vora
tags: [  tutorial>beginner, topic>big-data, topic>sql, products>sap-vora ]
---

## Prerequisites  
 - [Working with Hierarchies using Apache Zeppelin ](https://www.sap.com/developer/tutorials/vora-cal-zeppelin2.html)


## Next Steps
 - [Working with Graph Engine using Apache Zeppelin](https://www.sap.com/developer/tutorials/vora-cal-zeppelin4.html)

## Details
### You will learn  
You will learn how to process data using SAP Vora disk engine.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Disk Engine)]
Besides the SAP Vora relational in-memory engine, SAP Vora also provides the following execution engines: document store, graph engine, time series engine, and disk engine.

These engines are integrated into Spark as either a Spark SQL data source (full integration) or raw data source (partial integration).

The data source `com.sap.spark.engines.disk` is used by the disk engine. SQL statements issued on the disk engine are fully integrated into Spark SQL. Disk engine tables therefore behave in exactly the same way as Spark SQL tables.



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Running 3_Data_on_Disk)]
The first engine to look at is the Disk Engine. Switch to Zeppelin notebook `3_Data_on_Disk`.
![Disk notebook](zep3_01.jpg)

First create a disk engine table. Disk engine tables need a partition function and a derived partition scheme. This is what you do in the first two paragraphs.
![Partitions](zep3_02.jpg)

Create a second disk engine table and verify tables created.
![Second table](zep3_03.jpg)

Run a simple cross-engine query, you can continue by writing your own SQL paragraph using the simple query.
```sql
%vora
SELECT COMPLAINTS_DISK.COMPLAINT_ID,PRODUCT  
FROM  COMPLAINTS_DISK
INNER JOIN PRODUCTS
ON COMPLAINTS_DISK.COMPLAINT_ID = PRODUCTS.COMPLAINT_ID
```

![cross-engine query](zep3_04.jpg)


[ACCORDION-END]


## Next Steps
- [Working with Graph Engine using Apache Zeppelin](https://www.sap.com/developer/tutorials/vora-cal-zeppelin4.html)
