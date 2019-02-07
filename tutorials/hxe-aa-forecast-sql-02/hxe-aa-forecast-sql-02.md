---
title: Import the Time series datasets (Forecast SQL)
description: Learn how to easily import flat dataset files in your SAP HANA, express edition instance
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 15
---

## Prerequisites
 - [Use Machine Learning to Build a Time Series model using SQL](https://developers.sap.com/group.hxe-aa-forecast-sql.html)

## Details
### You will learn
- How to load your dataset content in your SAP HANA, express edition instance

[ACCORDION-BEGIN [Info: ](Which Time series dataset?)]

In order to build your time series model, you will be using the sample datasets from SAP Predictive Analytics made available as part of the [online documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS).

The datasets archive structure for the [**Sample Time Series**](https://help.sap.com/http.svc/download?deliverable_id=20555051) is the following:

```
|--sample_time_series_3.3.1_en-us_production.zip
   |-- Time series.zip
   |   |-- Time series
   |       |-- CashFlows.csv
   |       |-- KxDesc_CashFlows.csv
   |       |-- Lag1AndCycles.csv
   |       |-- Lag1AndCyclesAndWn.csv
   |       |-- R_ozone-la.csv
   |       |-- TrendAndCyclic.csv
   |       |-- TrendAndCyclicAnd_4Wn.csv
   |       |-- TrendAndCyclicAndWn.csv
   |-- metadata.xml
```

> ### **Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.


#### **Cash Flows**

The Cash Flows file (`CashFlows.csv`) presents daily measures of cash flows from January 2, 1998 to September, 30 1998. Each observation is characterized by 25 variables.

In this scenario, you are an executive of a financial entity that manages cash-flows. Your role is to make sure that credits are available with the correct amount at the correct date to provide the best management possible of your financial flows.

#### **Los Angeles Ozone**

The Los Angeles Ozone file (`R_ozone-la.csv`) presents monthly averages of hourly ozone (O3) readings in downtown Los Angeles from 1955 to 1972.

Each observation is characterized by 2 variables, a time and an average of the hourly ozone readings for the month.

The purpose of this scenario is to confirm the decreasing trend of the ozone rate by predicting the next 18 months and describing the different signal elements based on the ozone rate.

#### **Lag 1 And Cycles** & **Trend And Cyclic** with and without White Noise (`Wn`)

These files can be used to observe and analyze the impact of a specific signal phenomenon like:

 - Lag 1
 - Cycles
 - Trends
 - White Noise

Each observation is characterized by 2 variables, a time and a signal value.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](SAP HANA data import options)]

There are multiple ways to import data set files inside of your SAP HANA, express edition instance.

- ***Eclipse IDE***

The ***SAP HANA Tools*** plugin for Eclipse provides an ***Import/Export*** feature which would allow you to create the appropriate physical tables first and then import the data.

However, this would require the ***Eclipse IDE*** to be locally installed and properly configured with the ***SAP HANA Tools*** plugin.

Then, you would need to know the complete data file format description in order to create the tables with the proper columns structure. And, last but not least, any changes would require to recreate the all structure and reload the data.

If you want to learn more about this import method, you can check the following tutorial: **Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse**

- ***SAP HANA HDB Client***

The **SAP HANA HDB Client** provides an ***IMPORT FROM*** statement allowing you to import CSV files physically located on your SAP HANA, express edition host using a SQL command.

However, this method requires that the table are created before the execution of the command.

If you want to learn more about this import method, you can check the following tutorial: **Import CSV into SAP HANA, express edition using IMPORT FROM SQL command**.

#### ***Solution***

Both options are valid but for the sake of simplicity only the Eclipse option will be described here.

To learn more about these options, you can refer to:

- [Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse](https://developers.sap.com/tutorials/mlb-hxe-import-data-eclipse.html)
- [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](https://developers.sap.com/tutorials/mlb-hxe-import-data-sql-import.html)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
create table forecast_cashflow (
    cashdate                    daydate,
    workingdaysindices          integer,
    reverseworkingdaysindices   integer,
    mondaymonthind              integer,
    tuesdaymonthind             integer,
    wednesdaymonthind           integer,
    thursdaymonthind            integer,
    fridaymonthind              integer,
    beforelastmonday            integer,
    lastmonday                  integer,
    beforelasttuesday           integer,
    lasttuesday                 integer,
    beforelastwednesday         integer,
    lastwednesday               integer,
    beforelastthursday          integer,
    lastthursday                integer,
    beforelastfriday            integer,
    lastfriday                  integer,
    last5wdaysind               integer,
    last5wdays                  integer,
    last4wdaysind               integer,
    last4wdays                  integer,
    lastwmonth                  integer,
    beforelastwmonth            integer,
    cash                        double,
    primary key (cashdate)
);
create table forecast_ozone (
    time                 daydate,
    reading              decimal(17,2),
    primary key (time)
);
create table forecast_lag_1_and_cycles (
    time                 daydate,
    signal               double,
    primary key (time)
);
create table forecast_lag_1_and_cycles_and_wn (
    time                 daydate,
    signal               double,
    primary key (time)
);
create table forecast_trend_and_cyclic (
    time                 daydate,
    signal               double,
    primary key (time)
);
create table forecast_trend_and_cyclic_and_wn (
    time                 daydate,
    signal               double,
    primary key (time)
);
create table forecast_trend_and_cyclic_and_4wn (
    time                 daydate,
    signal               double,
    primary key (time)
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import the Dataset)]

As the dataset files get updated periodically, and in order to ensure consistency of content and validation, a copy of the data is available under the <a href="https://github.com/SAPDocuments/Tutorials/tree/master/tutorials/hxe-aa-forecast-sql-02/data" target="new">data</a> directory within the SAP Tutorial GitHub repository.

Download the following files locally (right click on the link, then use the ***Save link as*** option):

- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/data/CashFlows.csv" target="new">`CashFlows`</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/data/Lag1AndCycles.csv" target="new">`Lag1AndCycles`</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/data/Lag1AndCyclesAndWn.csv" target="new">`Lag1AndCyclesAndWn`</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/data/R_ozone-la.csv" target="new">`R_ozone-la`</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/data/TrendAndCyclic.csv" target="new">`TrendAndCyclic`</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/data/TrendAndCyclicAndWn.csv" target="new">`TrendAndCyclicAndWn`</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/data/TrendAndCyclicAnd_4Wn.csv" target="new">`TrendAndCyclicAnd_4Wn`</a>

Using the  **File > Import...** menu, select **SAP HANA Content > Data From Local File**.

You can also use the search field to locate the entry.

![image Step 3](03-0.png)

Click on **Next**.

Select the Target System connection **HXE @ HXE (`ML_USER`)**.

![image Step 3](03-1.png)

Click on **Next**.

The following panel allows you to set a series of import options:

- **File Details:**

    - Set the **Field Delimiter** value to **Tab (\t)**.
    - Check the **Header row exists** box and set the **Header row** value to 1
    - Check the **Import all data** box.

- **Manage Table Definition and Data Mappings:**

    Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File                   | Target Table      
| ----------------------------- | -----------------
| `CashFlows.csv`               | `ML_USER.FORECAST_CASHFLOW`
| `R_ozone-la.csv`              | `ML_USER.FORECAST_OZONE`
| `Lag1AndCycles.csv`           | `ML_USER.FORECAST_LAG_1_AND_CYCLES`
| `Lag1AndCyclesAndWn.csv`      | `ML_USER.FORECAST_LAG_1_AND_CYCLES_AND_WN`
| `TrendAndCyclic.csv`          | `ML_USER.FORECAST_TREND_AND_CYCLIC`
| `TrendAndCyclicAndWn.csv`     | `ML_USER.FORECAST_TREND_AND_CYCLIC_AND_WN`
| `TrendAndCyclicAnd_4Wn.csv`   | `ML_USER.FORECAST_TREND_AND_CYCLIC_AND_4WN`

**Repeat this operation for each dataset files.**

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
select 'cashflow'                 as "TABLE", count(1) as "COUNT" FROM forecast_cashflow
union all
select 'ozone'                    as "TABLE", count(1) as "COUNT" FROM forecast_ozone
union all
select 'lag_1_and_cycles'         as "TABLE", count(1) as "COUNT" FROM forecast_lag_1_and_cycles
union all
select 'lag_1_and_cycles_and_wn'  as "TABLE", count(1) as "COUNT" FROM forecast_lag_1_and_cycles_and_wn
union all
select 'trend_and_cyclic'         as "TABLE", count(1) as "COUNT" FROM forecast_trend_and_cyclic
union all
select 'trend_and_cyclic_and_wn'  as "TABLE", count(1) as "COUNT" FROM forecast_trend_and_cyclic_and_wn
union all
select 'trend_and_cyclic_and_4wn' as "TABLE", count(1) as "COUNT" FROM forecast_trend_and_cyclic_and_4wn
;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

> ### **Note** If you are using Jupyter Notebook, you can download the following  [notebook](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-forecast-sql-02/hxe-aa-forecast-sql-02.ipynb) to run most of the SQL statement listed in the tutorial.
