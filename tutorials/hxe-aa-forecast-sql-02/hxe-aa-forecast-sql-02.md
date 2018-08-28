---
title: Import the Time series datasets
description: Learn how to easily import flat dataset files in your SAP HANA, express edition instance
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 15
---

## Prerequisites
 - [Use Machine Learning to Build a Time Series model using SQL](https://www.sap.com/developer/groups/hxe-aa-forecast-sql.html)

## Next Steps
 - [Use Machine Learning to Build a Time Series model using SQL](https://www.sap.com/developer/groups/hxe-aa-forecast-sql.html)

## Details
### You will learn

- Create a Multi-Target Application Project
- Save your Web IDE project in a GIT repository

[ACCORDION-BEGIN [Info: ](Which Time series dataset?)]

In order to build your time series model, you will be using the sample datasets from SAP Predictive Analytics made available as part of the [online documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS).

The datasets archive structure for the [**Sample Time Series**](https://help.sap.com/http.svc/download?deliverable_id=20555051) is the following:

```
|--sample_time_series_3.3.1_en-us_production.zip
   |-- Time series.zip
   |   |-- Time series
   |       |-- CashFlows.txt
   |       |-- KxDesc_CashFlows.txt
   |       |-- Lag1AndCycles.txt
   |       |-- Lag1AndCyclesAndWn.txt
   |       |-- R_ozone-la.txt
   |       |-- TrendAndCyclic.txt
   |       |-- TrendAndCyclicAnd_4Wn.txt
   |       |-- TrendAndCyclicAndWn.txt
   |-- metadata.xml
```

> ### **Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.


#### **Cash Flows**

The Cash Flows file (`CashFlows.txt`) presents daily measures of cash flows from January 2, 1998 to September, 30 1998. Each observation is characterized by 25 variables.

In this scenario, you are an executive of a financial entity that manages cash-flows. Your role is to make sure that credits are available with the correct amount at the correct date to provide the best management possible of your financial flows.

#### **Los Angeles Ozone**

The Los Angeles Ozone file (`R_ozone-la.txt`) presents monthly averages of hourly ozone (O3) readings in downtown Los Angeles from 1955 to 1972.

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

If you want to learn more about this import method, you can check the following tutorial: [Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html)

- ***SAP HANA HDB Client***

The **SAP HANA HDB Client** provides an ***IMPORT FROM*** statement allowing you to import CSV files physically located on your SAP HANA, express edition host using a SQL command.

However, this method requires that the table are created before the execution of the command.

If you want to learn more about this import method, you can check the following tutorial: [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-sql-import.html)

#### ***Solution***

Both options are valid and will be described here. To learn more about these options, you can refer to:

- [Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html)
- [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-sql-import.html)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Download the dataset CSV files)]

Open the [SAP Predictive Analytic online documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS) page.

Locate the **Samples** section and click on the **View All** button.

Select the [**Sample Time Series**](https://help.sap.com/http.svc/download?deliverable_id=20555051) entry.

The download should begin.

Once the download completes, extract the `Time series.zip` file included in the downloaded `sample_time_series_x.y.y_en-us_production.zip` archive file.

If you are planning on using the ***IMPORT FROM*** statement from the SAP HANA HDB Client, then you will need to transfer the extracted files using an FTP client to following location on the HXE server:

```
/usr/sap/HXE/HDB90/work/data/
```

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

Depending on your the import method option you have selected, complete one of the following steps.

### **Using the SAP HANA Tools for Eclipse**

You can import the data using the [SAP HANA Tools for Eclipse Import feature](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html) using the following details:

- **File Details:**

    - Set the **Field Delimiter** value to **Tab (\t)**.
    - Check the **Header row exists** box and set the **Header row** value to 1
    - Check the **Import all data** box.

- **Manage Table Definition and Data Mappings:**

    Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File                   | Target Table      
| ----------------------------- | -----------------
| `CashFlows.txt`               | `ML_USER.FORECAST_CASHFLOW`
| `R_ozone-la.txt`              | `ML_USER.FORECAST_OZONE`
| `Lag1AndCycles.txt`           | `ML_USER.FORECAST_LAG_1_AND_CYCLES`
| `Lag1AndCyclesAndWn.txt`      | `ML_USER.FORECAST_LAG_1_AND_CYCLES_AND_WN`
| `TrendAndCyclic.txt`          | `ML_USER.FORECAST_TREND_AND_CYCLIC`
| `TrendAndCyclicAndWn.txt`     | `ML_USER.FORECAST_TREND_AND_CYCLIC_AND_WN`
| `TrendAndCyclicAnd_4Wn.txt`   | `ML_USER.FORECAST_TREND_AND_CYCLIC_AND_4WN`

### **Using the IMPORT FROM SQL command**

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/data/`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
import from csv file '/usr/sap/HXE/HDB90/work/data/forecast/CashFlows.txt' into forecast_cashflow
with
   record delimited by '\n'
   field delimited by '\t'
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/CashFlows.txt.err'
;
import from csv file '/usr/sap/HXE/HDB90/work/data/forecast/R_ozone-la.txt' into forecast_ozone
with
   record delimited by '\n'
   field delimited by '\t'
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/R_ozone-la.txt.err'
;
import from csv file '/usr/sap/HXE/HDB90/work/data/forecast/Lag1AndCycles.txt' into forecast_lag_1_and_cycles
with
   record delimited by '\n'
   field delimited by '\t'
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/Lag1AndCycles.txt.err'
;
import from csv file '/usr/sap/HXE/HDB90/work/data/forecast/Lag1AndCyclesAndWn.txt' into FORECAST_LAG_1_AND_CYCLES_AND_WN
with
   record delimited by '\n'
   field delimited by '\t'
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/Lag1AndCyclesAndWn.txt.err'
;
import from csv file '/usr/sap/HXE/HDB90/work/data/forecast/TrendAndCyclic.txt' into FORECAST_TREND_AND_CYCLIC
with
   record delimited by '\n'
   field delimited by '\t'
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/TrendAndCyclic.txt.err'
;
import from csv file '/usr/sap/HXE/HDB90/work/data/forecast/TrendAndCyclicAndWn.txt' into FORECAST_TREND_AND_CYCLIC_AND_WN
with
   record delimited by '\n'
   field delimited by '\t'
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/TrendAndCyclicAndWn.txt.err'
;

import from csv file '/usr/sap/HXE/HDB90/work/data/forecast/TrendAndCyclicAnd_4Wn.txt' into FORECAST_TREND_AND_CYCLIC_AND_4WN
with
   record delimited by '\n'
   field delimited by '\t'
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/TrendAndCyclicAnd_4Wn.txt.err'
;
```

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
