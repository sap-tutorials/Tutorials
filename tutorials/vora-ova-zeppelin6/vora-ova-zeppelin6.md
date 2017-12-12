---
title: Working with ORC and Parquet Files using Apache Zeppelin
description: SAP Vora 1.4 supports popular file types in data management namely ORC and Parquet
primary_tag: products>sap-vora
tags: [  tutorial>beginner, topic>big-data, topic>sql, products>sap-vora ]
---

## Prerequisites  
 - [Working with Tables and Views using Apache Zeppelin](https://www.sap.com/developer/tutorials/vora-ova-zeppelin0.html)


## Next Steps
 - [Working with Disk Engine using Apache Zeppelin](https://www.sap.com/developer/tutorials/vora-ova-zeppelin3.html)

## Details
### You will learn  
You will learn how to load data from ORC and Parquet files into SAP Vora engine.

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](ORC and Parquet file types)]
Next up you will look into working with popular file types in data management namely ORC and Parquet files, the smallest, fastest columnar storage for Hadoop workloads. SAP Vora 1.4 supports both file types.

**Apache ORC** is a self-describing type-aware columnar file format designed for Hadoop workloads. It is optimized for large streaming reads, but with integrated support for finding required rows quickly.

**Apache Parquet** is a columnar storage format available to any project in the Hadoop ecosystem, regardless of the choice of data processing framework, data model or programming language.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Running notebook 6_ORC and Parquet files)]
Start this tutorial by launching the Zeppelin notebook `6_ORC and Parquet files`.

In order to create ORC and Parquet files you will switch to Spark context. More information about Spark SQL methods and function can be found here:
https://spark.apache.org/docs/1.6.1/api/java/org/apache/spark/sql/types/package-summary.html

Run the highlighted paragraphs in the notebook.
![Create files](zep6_02_14.jpg)

In the next step use `StructType` to create a structured data frame of the data set. `StructType` is a built-in data type in Spark SQL to represent a collection of fields.
![StructType](zep6_03_14.jpg)

Then create a fixed value called `options` that stores required option as strings. This will be passed in as Vora options.
![Vora options](zep6_04_14.jpg)

Create a data frame with the defined schema and options. This also includes the source file being loaded.
![Create a data frame](zep6_05_14.jpg)

Select the desired columns from the `PRODUCT_CSV` data frame and write them to HDFS in Parquet format.
>If you receive an error that the file already exists please change the output file to something like `/user/vora/products_p_2.parquet` to ensure its uniqueness.

![Write to Parquet](zep6_06_14.jpg)

Move to the next paragraph and create an ORC output file.
>If you receive an error that the file already exists, then please update the statement and change to something like `/user/vora/products_o_2.orc`.

![Write to ORC](zep6_07_14.jpg)

Now you can create an in-memory Vora resident table, using ORC and Parquet files for loading data.
>Ensure `paths` option is updated with previous step changes

![Create tables](zep6_08_14.jpg)

Run the next paragraph and select data from the newly created table containing data loaded from your Parquet file.
![Select from table - Parquet](zep6_09_14.jpg)

Final step of this tutorial is to create a table from your ORC based file and select data from the table.
>Ensure `paths` option is updated with previous step changes.

![Select from table - ORC](zep6_10_14.jpg)


[ACCORDION-END]


## Next Steps
 - [Working with Disk Engine using Apache Zeppelin](https://www.sap.com/developer/tutorials/vora-ova-zeppelin3.html)
