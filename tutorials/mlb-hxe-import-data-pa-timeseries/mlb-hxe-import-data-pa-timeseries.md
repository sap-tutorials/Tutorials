---
title: Import a Time Series Sample Dataset
description: Import SAP Predictive Analytics Time Series Sample Datasets in your SAP HANA, express edition instance
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 10
---

## Prerequisites  
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://developers.sap.com/tutorials/mlb-hxe-setup-basic.html)

## Details
### You will learn
In this tutorial, you will learn how to download and import the SAP Predictive Analytics Time Series sample dataset into your SAP HANA, express edition instance.

[ACCORDION-BEGIN [Info: ](Sample file structure)]

The archive structure for the  [**Sample Time Series**](https://help.sap.com/http.svc/download?deliverable_id=20555051) is the following:

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

**Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.

#### **Cash Flows**

The Cash Flows file (`CashFlows.txt`) presents daily measures of cash flows from January 2, 1998 to September, 30 1998. Each observation is characterized by 25 variables described in the following table.

| Variable                                                                        | Description                                                        | Example of values
:---------------------------------------------------------------------------------|--------------------------------------------------------------------|--------------------------
| Date                                                                            | Day, month and year of the readings                                | A date
| <nobr>`Cash`</nobr>                                                             | Cash flow                                                          | A numerical value with n decimals
| <nobr>`BeforeLastMonday`</nobr> <br><nobr>`LastMonday`</nobr> <br><nobr>`BeforeLastTuesday`</nobr> <br><nobr>`LastTuesday`</nobr> <br><nobr>`BeforeLastWednesday`</nobr> <br><nobr>`LastWednesday`</nobr> <br><nobr>`BeforeLastThursday`</nobr> <br><nobr>`LastThursday`</nobr> <br><nobr>`BeforeLastFriday`</nobr> <br><nobr>`LastFriday`</nobr> | Boolean variables that indicate if the information is true or false   | 1 if the information is true.
| <nobr>`Last5WDays`</nobr><br><nobr>`Last4WDays`</nobr>                         | Boolean variables that indicate if the date is in the 5 or 4 last working days of the month | 1 if the information is true.
| <nobr>`LastWMonth`</nobr><br><nobr>`BeforeLastWMonth`</nobr>                   | Boolean variables that indicate if the information is true or false | 1 if the information is true.
| <nobr>`WorkingDaysIndices`</nobr><br><nobr>`ReverseWorkingDaysIndices`</nobr>  | Indices or reverse indices of the working days | An integer value
| <nobr>`MondayMonthInd`</nobr><br><nobr>`TuesdayMonthInd`</nobr><br><nobr>`WednesdayMonthInd`</nobr><br><nobr>`ThursdayMonthInd`</nobr><br><nobr>`FridayMonthInd`</nobr> | Indices of the week days in the month | An integer value
| <nobr>`Last5WDaysInd`</nobr><br><nobr>`Last4WDaysInd`</nobr>                   | Indices of the 5 or 4 last working days of the month | An integer value

#### **Los Angeles Ozone**

The Los Angeles Ozone file (`R_ozone-la.txt`) presents monthly averages of hourly ozone (O3) readings in downtown Los Angeles from 1955 to 1972.

Each observation is characterized by 2 variables described in the following table:

| Variable        | Description                                  | Example of values
:-----------------|----------------------------------------------|--------------------------
| `Time`          | Month and year of the readings               | A date
| `R_ozone-la`    | Average of the hourly readings for the month | A numerical value

#### **Lag 1 And Cycles** & **Trend And Cyclic**

These files can be used to observe and analyze the impact of specific signal phenomenon.  

Each observation is characterized by 2 variables described in the following table:

| Variable    | Description               | Example of values
:-------------|---------------------------|--------------------------
| `TIME`      | The date of the readings  | A date
| `Signal`    | the signal value          | A numerical value

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
CREATE TABLE PA_DATA.CASHFLOW (
    CASHDATE                    DAYDATE,
    WORKINGDAYSINDICES          SMALLINT,
    REVERSEWORKINGDAYSINDICES   SMALLINT,
    MONDAYMONTHIND              SMALLINT,
    TUESDAYMONTHIND             SMALLINT,
    WEDNESDAYMONTHIND           SMALLINT,
    THURSDAYMONTHIND            SMALLINT,
    FRIDAYMONTHIND              SMALLINT,
    BEFORELASTMONDAY            SMALLINT,
    LASTMONDAY                  SMALLINT,
    BEFORELASTTUESDAY           SMALLINT,
    LASTTUESDAY                 SMALLINT,
    BEFORELASTWEDNESDAY         SMALLINT,
    LASTWEDNESDAY               SMALLINT,
    BEFORELASTTHURSDAY          SMALLINT,
    LASTTHURSDAY                SMALLINT,
    BEFORELASTFRIDAY            SMALLINT,
    LASTFRIDAY                  SMALLINT,
    LAST5WDAYSIND               SMALLINT,
    LAST5WDAYS                  SMALLINT,
    LAST4WDAYSIND               SMALLINT,
    LAST4WDAYS                  SMALLINT,
    LASTWMONTH                  SMALLINT,
    BEFORELASTWMONTH            SMALLINT,
    CASH                        DECIMAL(17,6),
    PRIMARY KEY (CASHDATE)
);
CREATE TABLE PA_DATA.OZONE (
    OZONEDATE                   DAYDATE,
    OZONEREADING                DECIMAL(17,2),
    PRIMARY KEY (OZONEDATE)
);
CREATE TABLE PA_DATA.LAG_1_AND_CYCLES (
    READINGDATE                 DAYDATE,
    READINGVALUE                DECIMAL(17,9),
    PRIMARY KEY (READINGDATE)
);
CREATE TABLE PA_DATA.LAG_1_AND_CYCLES_AND_WN (
    READINGDATE                 DAYDATE,
    READINGVALUE                DECIMAL(17,9),
    PRIMARY KEY (READINGDATE)
);
CREATE TABLE PA_DATA.TREND_AND_CYCLIC (
    READINGDATE                 DAYDATE,
    READINGVALUE                DECIMAL(17,9),
    PRIMARY KEY (READINGDATE)
);
CREATE TABLE PA_DATA.TREND_AND_CYCLIC_AND_WN (
    READINGDATE                 DAYDATE,
    READINGVALUE                DECIMAL(17,9),
    PRIMARY KEY (READINGDATE)
);
CREATE TABLE PA_DATA.TREND_AND_CYCLIC_AND_4WN (
    READINGDATE                 DAYDATE,
    READINGVALUE                DECIMAL(17,9),
    PRIMARY KEY (READINGDATE)
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the Sample Data)]

Open the [SAP Predictive Analytics documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS) page in a browser and click on the **View All** for the **Sample** section.

This will display the list of sample dataset available.

Right-click on the **Download the Sample Time Series** link and use the **Save link address** to get the download URL.

Open the download URL and save the sample data archive either:

- the Eclipse host if you want to use the SAP HANA Tools for Eclipse
- the SAP HANA, express host if you want the IMPORT FROM SQL command

Extract the files (located in the `Time Series` subdirectory in the embedded `Time Series.zip` archive).

#### **Import Using the SAP HANA Tools for Eclipse**

You can extract the sample file anywhere you want on the Eclipse host.

You can now move to **Step 3: Import Using the SAP HANA Tools for Eclipse**.

#### **Import Using the IMPORT FROM SQL command**

You can extract the sample file anywhere you want on the Eclipse host.

Here is an example script that you reuses to download and extract the sample dataset from the SAP HANA, express edition host:

```shell
URL=https://help.sap.com/http.svc/download?deliverable_id=20555051
OUTPUT_FILE=sample_timeseries
OUTPUT_DIR=/usr/sap/HXE/HDB90/work/$OUTPUT_FILE

 # create a new subdirectory for the sample data
mkdir $OUTPUT_DIR

 # download the archive in the sample data directory
 # wget -O $OUTPUT_DIR/$OUTPUT_FILE.zip $URL
curl $URL -o $OUTPUT_DIR/$OUTPUT_FILE.zip

 # switch to the new directory
cd $OUTPUT_DIR

 # extract all archives and embedded archives
while [ "`find . -type f -name '*.zip' | wc -l`" -gt 0 ]; \
  do find -type f -name "*.zip" \
      -exec unzip -o --  '{}' \; \
      -exec rm -- '{}' \;; done
 # remove space from file and directory names
for f in *\ *; do mv "$f" "${f// /}"; done      
```

It requires CURL (or WGET) to be installed.

The dataset files should now be located in: **`/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries`**

You can now move to **Step 3: Import Using the IMPORT FROM SQL command**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the SAP HANA Tools for Eclipse)]

You can import the data using the [SAP HANA Tools for Eclipse Import feature](https://developers.sap.com/tutorials/mlb-hxe-import-data-eclipse.html) using the following details:

- **File Details:**

    - Set the **Field Delimiter** value to **Tab (\t)**.
    - Check the **Header row exists** box and set the **Header row** value to 1
    - Check the **Import all data** box.

- **Manage Table Definition and Data Mappings:**

    Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File                   | Target Table      
| ----------------------------- | -----------------
| `CashFlows.txt`               | `PA_DATA.CASHFLOW`
| `R_ozone-la.txt`              | `PA_DATA.OZONE`
| `Lag1AndCycles.txt`           | `PA_DATA.LAG_1_AND_CYCLES`
| `Lag1AndCyclesAndWn.txt`      | `PA_DATA.LAG_1_AND_CYCLES_AND_WN`
| `TrendAndCyclic.txt`          | `PA_DATA.TREND_AND_CYCLIC`
| `TrendAndCyclicAnd_4Wn.txt`   | `PA_DATA.TREND_AND_CYCLIC_AND_4WN`
| `TrendAndCyclicAndWn.txt`     | `PA_DATA.TREND_AND_CYCLIC_AND_WN`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the IMPORT FROM SQL command)]

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/CashFlows.txt' INTO PA_DATA.CASHFLOW
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/CashFlows.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/R_ozone-la.txt' INTO PA_DATA.OZONE
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/R_ozone-la.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/Lag1AndCycles.txt' INTO PA_DATA.LAG_1_AND_CYCLES
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/Lag1AndCycles.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/Lag1AndCyclesAndWn.txt' INTO PA_DATA.LAG_1_AND_CYCLES_AND_WN
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/Lag1AndCyclesAndWn.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/TrendAndCyclic.txt' INTO PA_DATA.TREND_AND_CYCLIC
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/TrendAndCyclic.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/TrendAndCyclicAnd_4Wn.txt' INTO PA_DATA.TREND_AND_CYCLIC_AND_4WN
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/TrendAndCyclicAnd_4Wn.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/TrendAndCyclicAndWn.txt' INTO PA_DATA.TREND_AND_CYCLIC_AND_WN
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_timeseries/Timeseries/TrendAndCyclicAndWn.txt.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
SELECT 'CASHFLOW'                 as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CASHFLOW
UNION
SELECT 'OZONE'                    as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.OZONE
UNION
SELECT 'LAG_1_AND_CYCLES'         as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.LAG_1_AND_CYCLES
UNION
SELECT 'LAG_1_AND_CYCLES_AND_WN'  as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.LAG_1_AND_CYCLES_AND_WN
UNION
SELECT 'TREND_AND_CYCLIC'         as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.TREND_AND_CYCLIC
UNION
SELECT 'TREND_AND_CYCLIC_AND_4WN' as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.TREND_AND_CYCLIC_AND_4WN
UNION
SELECT 'TREND_AND_CYCLIC_AND_WN'  as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.TREND_AND_CYCLIC_AND_WN
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
