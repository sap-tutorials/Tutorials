---
title: Working with Tables and Views using Apache Zeppelin
description: SAP Vora 1.4: Use Apache Zeppelin to create tables and views, plus load sample data from files
primary_tag: products>sap-vora
tags: [  tutorial>beginner, topic>big-data, topic>sql, products>sap-vora ]
---

## Prerequisites  
 - You setup a virtual machine with [SAP Vora 1.4, developer edition](http://www.sap.com/developer/how-tos/2017/02/vora-ova-install.html)


## Next Steps
 - [Working with hierarchies using Apache Zeppelin](http://www.sap.com/developer/tutorials/vora-ova-zeppelin2.html)

## Details
### You will learn  
You will learn how to work with Apache Zeppelin, how to load sample data into SAP Vora tables and query data using views.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Apache Spark and SAP Vora)]
The SAP Vora developer edition has [Apache Spark 1.6.1](https://spark.apache.org/docs/1.6.1/) preinstalled. Apache Spark is a general-purpose open source cluster computing system. It provides high-level APIs in Java, Scala, Python and R programming languages. It also supports a higher-level Spark SQL for structured data processing.

Apache Spark allows native integration with a larger number of input sources. These integrations are made possible through the inclusion of the Spark SQL Data Sources API. The Data Sources API provides a pluggable mechanism for accessing structured data though Spark SQL.

SAP Vora extends Spark SQL. The Vora data source in Spark allows you to improve data processing performance by using SAP Vora as an in-memory engine. It supports an enhanced data source API implementation in Spark that allows you to create tables (Spark DataFrames) based on files contained in a local or distributed file system.

Using the SAP Vora data source, a Spark SQL query can be executed on multiple SAP Vora engines concurrently and the execution result returned back to the Spark runtime as an RDD (resilient distributed data set).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Using Apache Zeppelin)]
The SAP Vora 1.4, developer edition comes also with an installation of Apache Zeppelin 0.7.2 and some example notebooks.

Apache Zeppelin is an open source web-based notebook that enables interactive data analytics. Apache Zeppelin brings data ingestion, data exploration, visualization, sharing and collaboration features to Hadoop, Spark and SAP Vora.

Interactive browser-based notebooks enable data engineers, data analysts and data scientists to be more productive by developing, organizing, executing, and sharing data code and visualizing results without referring to the command line or needing the cluster details. Notebooks allow these users not only allow to execute, but to interactively work with long workflows.

You will use Apache Zeppelin to create, load and explore data in SAP Vora using the various engine capabilities such as time series, document engine, graph engine, disk engine, but also access external data sources such as SAP HANA.

Zeppelin makes use of the SAP Vora Spark Extension library by invoking it with the `%jdbc` interpreter key word in each paragraph.

To use Apache Zeppelin pre-installed on SAP Vora 1.4, developer edition, run a modern web browser of your choice and open `http://IP_ADDRESS:9099`

![Open Zeppelin](zep0_01_14.jpg)

You will see a few notebooks which focus on different Vora topics such as SAP Vora Tutorial, Tables and Views, Document Store, Disk Engine, Graph Engine, Time Series, etc.

You will get back to SAP Vora Tutorial notebook as a part of a different tutorial, but for now let's start by opening `0_DATA`.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Running notebook 0_DATA)]
Click on notebook `0_DATA`. This notebook contains SQL scripts to create in-memory tables and loading sample data.

The next page will present itself with the `0_DATA` notebook content, the content (scripts) are broken down to ___paragraphs___. These paragraphs contains the individual SQL scripts that you will execute.

A `CREATE TABLE` statement registers the table in the Spark `sqlContext` and creates a table in the SAP Vora engine.

You need to provide a table name and the fully qualified name of the SAP Vora data source package, `com.sap.spark.vora`, as well as a set of options required by the data source.

The initial SQL script execution does take some time to complete, this is due to Yarn resource initialization. Subsequent statements are executed much faster.

To run the SQL in a paragraph you can either click on **Run this Paragraph**. Or use the `SHIFT+ENTER` key combination.
![Run paragraph](zep0_02_14.jpg)

Continue by running all three "create table" paragraphs.

Get list of created tables by scrolling down the notebook and run the paragraph with the `%jdbc show tables using com.sap.spark.engines` SQL statement.

You can create your own paragraphs just remember that `%jdbc` keyword is required in each paragraph.

![Show tables](zep0_03_14.jpg)

Feel free to query the in-memory tables you have just created with queries like `%jdbc select * from FINANCIAL_INS limit 10`.

![Query data](zep0_04_14.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Running notebook 1_Tables and Views)]
Continue to the next notebook `1_Tables and Views`.
![Notebook 1](zep0_05_14.jpg)

Run each paragraph individually. First few paragraphs show how to create and then drop some test table `COMPLAINTS_TEST`.

Then create a view `COMPLAINTS_PRODUCTS` joining `COMPLAINTS` and `PRODUCTS` relations.
![Create the view](zep0_06_14.jpg)

Select data from the newly created view `COMPLAINTS_PRODUCTS`.
![Select from view](zep0_07_14.jpg)

Report the number of complaints by state and by product by means of simple aggregation. Run `Some joins and aggregations` paragraph and query data from it.

Zeppelin will plot the results once you choose one of chart representations. Settings of measures and dimensions are customizable.
![Select from new view](zep0_08_14.jpg)

Create dimension views. You can define annotations on relation columns. An ___annotation___ is a key/value pair that allows external tools, such as modelers and visual SQL editors, to store additional information about relation columns, like default aggregations or UI formatting tips. This makes integration with SAP Vora and Spark easier.

To view the annotations defined on a table, you can use the table-valued function `DESCRIBE_TABLE` as in the following Zeppelin paragraph.

![Dimension and annotations](zep0_09_14.jpg)

Create a CUBE called `PROD_FININS_CUBE` that joins fact table and dimension view. Feel free to play with the different chart types for this visualization.
![Cube](zep0_10_14.jpg)

[DONE]
[ACCORDION-END]


## Next Steps
- [Working with hierarchies using Apache Zeppelin](http://www.sap.com/developer/tutorials/vora-ova-zeppelin2.html)
