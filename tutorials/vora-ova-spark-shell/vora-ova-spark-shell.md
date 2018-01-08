---
title: Using Spark Shell
description: Examples in Scala that you can run and examine in Spark Shell (SAP Vora 1.4).
primary_tag: products>sap-vora
tags: [  tutorial>beginner, topic>big-data, products>sap-vora ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Working with SAP HANA data source](https://www.sap.com/developer/tutorials/vora-ova-hana-datasource.html)


## Next Steps
 - Select a tutorial group from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)

## Details
### You will learn  
You will learn how to open the Spark shell and try out some Scala code that uses SAP Vora engines

### Time to Complete
**15 Min**

---


[ACCORDION-BEGIN [Step 1: ](Open Spark shell)]

You can open the Spark shell and try out some Scala code that uses Vora.

For that run from OS level as user `vora`:

```sh
/opt/vora/lib/vora-spark/bin/start-spark-shell.sh
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Run the examples from Spark shell)]

SAP Vora comes with some examples that you can run and examine.

You can run the examples as `vora` user by executing `/etc/vora/run_examples.sh hdfs`.

Ignore any ___Address already in use___ error messages. Or to reduce the amount of information output modify the logger configuration in file `/opt/hadoop-2.7.3/etc/hadoop/log4j.properties` by changing line
```
hadoop.root.logger=INFO,console
```
to
```
hadoop.root.logger=ERROR,console
```

It is convenient to write the output of execution into a file to examine the results later:
```sh
/etc/vora/run_examples.sh hdfs > output_from_examples.log
```

You can also look at the source code which is at `/opt/vora/lib/vora-spark/examples`.

The examples source code can also be copied and pasted into `spark-shell`, so it can be executed step by step.

To check if everything works you can also run the examples one by one and check if the output matches the expectations.

- Find the jar file with the Vora examples:
```sh
export DATASOURCE_DIST=/opt/vora/lib/vora-spark/lib/spark-sap-datasources-*-assembly.jar
```
- Copy the test data to HDFS:
```sh
/opt/spark/bin/spark-submit --class com.sap.spark.vora.examples.tools.CopyExampleFilesToHdfs $DATASOURCE_DIST
```
- When `echo $?` returns `0` you were successful

Now you can run the single examples and check the output. Ignore all the Spark debug output about starting and finishing jobs.

If the expected snippet occurs in the output means, that the example ran successful.


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Load data into SAP Vora)]

`LoadDataIntoVora`:
```sh
/opt/spark/bin/spark-submit --class com.sap.spark.vora.examples.LoadDataIntoVora $DATASOURCE_DIST
```
This is expected to be outputted twice:
```
+-----------------+--------+------+-------+-------+
|         CARRNAME|AIRPFROM|AIRPTO|DEPTIME|ARRTIME|
+-----------------+--------+------+-------+-------+
|American Airlines|     JFK|   SFO| 133000| 163100|
|American Airlines|     JFK|   SFO| 110000| 140100|
|  United Airlines|     JFK|   SFO| 144500| 175500|
|  United Airlines|     JFK|   FRA| 162000| 054500|
|   Delta Airlines|     JFK|   SFO| 171500| 203700|
|   Delta Airlines|     JFK|   FRA| 193500| 093000|
|   Delta Airlines|     JFK|   SFO| 171500| 203700|
|        Lufthansa|     JFK|   FRA| 183000| 074500|
+-----------------+--------+------+-------+-------+
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Hash Partitioning)]

`HashPartitioning`:
```sh
/opt/spark/bin/spark-submit --class com.sap.spark.vora.examples.HashPartitioning $DATASOURCE_DIST
```
This is the expected output:
```
+-----------------+--------+------+--------+-------+-------+
|         CARRNAME|AIRPFROM|AIRPTO|DISTANCE|DEPTIME|ARRTIME|
+-----------------+--------+------+--------+-------+-------+
|American Airlines|     JFK|   SFO|  0.0000| 133000| 163100|
|American Airlines|     JFK|   SFO|  2.5720| 110000| 140100|
|   Delta Airlines|     JFK|   SFO|  0.0000| 171500| 203700|
|   Delta Airlines|     JFK|   FRA|  3.8510| 193500| 093000|
|   Delta Airlines|     JFK|   SFO|  2.5720| 171500| 203700|
|        Lufthansa|     JFK|   FRA|  6.1620| 183000| 074500|
|  United Airlines|     JFK|   SFO|  0.0000| 144500| 175500|
|  United Airlines|     JFK|   FRA|  6.1620| 162000| 054500|
+-----------------+--------+------+--------+-------+-------+
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Graph Engine)]

`GraphEngine`:
```sh
/opt/spark/bin/spark-submit --class com.sap.spark.vora.examples.GraphEngine $DATASOURCE_DIST
```
This is the expected output:
```
+--------------+
|      CITYFROM|
+--------------+
|      NEW YORK|
| SAN FRANCISCO|
|FRANKFURT/MAIN|
|FRANKFURT/MAIN|
|      NEW YORK|
| SAN FRANCISCO|
|     FRANKFURT|
|     FRANKFURT|
|     FRANKFURT|
| SAN FRANCISCO|
|     FRANKFURT|
|        BERLIN|
|        BERLIN|
|     FRANKFURT|
|     FRANKFURT|
|        BERLIN|
|           ROM|
|     FRANKFURT|
|     FRANKFURT|
|      NEW YORK|
+--------------+
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Document Store engine)]

`DocStoreEngine`:
```sh
/opt/spark/bin/spark-submit --class com.sap.spark.vora.examples.DocStoreEngine $DATASOURCE_DIST
```
This is the expected output:
```
+--------------------+
|             COLUMN1|
+--------------------+
|{"cn": "American ...|
|{"cn": "British A...|
|{"cn": "Air Berlin"}|
|{"cn": "Northwest...|
|     {"cn": "Swiss"}|
|{"cn": "Air Canada"}|
|{"cn": "Air Pacif...|
| {"cn": "Lufthansa"}|
|{"cn": "Qantas Ai...|
|{"cn": "United Ai...|
|{"cn": "Air France"}|
|{"cn": "Continent...|
| {"cn": "Lauda Air"}|
|{"cn": "South Afr...|
|  {"cn": "Alitalia"}|
|{"cn": "Delta Air...|
|{"cn": "Japan Air...|
|{"cn": "Singapore...|
+--------------------+
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Time Series engine)]

`TimeSeriesEngine`:
```sh
/opt/spark/bin/spark-submit --class com.sap.spark.vora.examples.TimeSeriesEngine $DATASOURCE_DIST
```
This is the expected output:
```
+--------------------+------+
|                  TS|CONNID|
+--------------------+------+
|2015-01-01 09:00:...|  null|
|2015-01-01 10:00:...|  null|
|2015-01-01 11:00:...|  null|
|2015-01-01 12:00:...|  null|
|2015-01-01 13:00:...|  null|
|2015-01-01 14:00:...|  null|
|2015-01-01 15:00:...|  null|
|2015-01-01 16:00:...|  null|
|2015-01-01 17:00:...|  null|
|2015-01-01 18:00:...|  null|
|2015-01-01 19:00:...|  null|
|2015-01-01 20:00:...|  null|
|2015-01-01 21:00:...|  null|
|2015-01-01 22:00:...|  null|
|2015-01-01 23:00:...|  null|
|2015-01-02 00:00:...|  null|
|2015-01-02 01:00:...|  null|
|2015-01-02 02:00:...|  null|
|2015-01-02 03:00:...|  null|
|2015-01-02 04:00:...|  null|
+--------------------+------+
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Disk engine)]

`DiskEngine`:
```sh
/opt/spark/bin/spark-submit --class com.sap.spark.vora.examples.DiskEngine $DATASOURCE_DIST
```
This is the expected output:
```
+------+
|carrid|
+------+
|    AC|
|    AF|
|    LH|
|    LH|
|    LH|
|    SQ|
|    LH|
|    AZ|
|    LH|
|    UA|
|    AZ|
|    LH|
|    QF|
|    SQ|
|    SQ|
|    LH|
|    JL|
|    JL|
|    LH|
|    UA|
+------+
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Run the examples from Zeppelin)]

By now you should know, that to use Apache Zeppelin pre-installed on SAP Vora 1.4, developer edition, open `http://IP_ADDRESS:9099` in some modern web browser.

The **SAP Vora Tutorial** notebook will give you a quick introduction into Vora processing in Zeppelin using different interpreters, including calling shell scripts.

- Paragraph ___Execute the Vora Examples___ will run a shell script that executes different examples from the OS level. Ignore all "`java.net.BindException: Address already in use`" warning messages as the executor is looking for an available port.
- Paragraph ___Show the tables___ will connect via JDBC to Vora and show all tables currently known to Vora.
- Paragraph ___Query tables in Vora___ will also use JDBC to do simple SQL querying on Vora tables
  - Bear in mind that you must first register all tables with the "`%jdbc` register all tables..." paragraph so that the table SCUSTOM is known to the Spark context.
- Paragraph ___Usage of the `SapSQLContext` in Spark___ shows how you can write Scala code and access Vora programmatically



[ACCORDION-END]



## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorials.html)
