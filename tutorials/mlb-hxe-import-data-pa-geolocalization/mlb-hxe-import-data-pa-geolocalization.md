---
title: Import a Geo localization Sample Dataset
description: Import SAP Predictive Analytics Geo localization Sample Dataset in your SAP HANA, express edition instance
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 10
---

## Prerequisites  
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://www.sap.com/developer/tutorials/mlb-hxe-setup-basic.html)

## Details
### You will learn
In this tutorial, you will learn how to download and import the SAP Predictive Analytics Geo localization sample dataset into your SAP HANA, express edition instance.

[ACCORDION-BEGIN [Info: ](Sample file structure)]

The sample structure for the [**Sample Geo localization**](https://help.sap.com/http.svc/download?deliverable_id=20555041) is the following:

```
|-- sample_geolocalization_3.3.1_en-us_production.zip
   |-- Geolocalization.zip
   |   |-- Geolocalization
   |       |-- gowalla_demo_2.txt
   |       |-- desc_gowalla_demo_2.txt
   |-- metadata.xml
```

**Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.

#### **`Gowalla`**

`Gowalla` was a location-based social network launched in 2007 and closed in 2012. Users were able to check in at "Spots" in their local vicinity, either through a dedicated mobile application or through the mobile website.

| Variable                   | Description                           | Example of Values
| :------------------------- | :-------------------------------------| :-----------------------------------
| <nobr>`user`</nobr>        | the user id                           | <nobr>Any numerical value</nobr>
| <nobr>`checkInTime`</nobr> | the check-in date/time                | <nobr>A timestamp</nobr>
| <nobr>`latitude`</nobr>    | the location latitude                 |
| <nobr>`longitude`</nobr>   | the location longitude                |
| <nobr>`locationId`</nobr>  | the location id                       | <nobr>Any numerical value</nobr>
| <nobr>`isTourist`</nobr>   | a flag indicating a tourist location  | <nobr>0 or 1</nobr>
| <nobr>`isMustSee`</nobr>   | a flag indicating a must see location | <nobr>0 or 1</nobr>

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
CREATE TABLE PA_DATA.GOWALLA (
    USERID      INT,
    CHECKINTIME TIMESTAMP,
    LATITUDE    DECIMAL(9,6),
    LONGITUDE   DECIMAL(9,6),
    LOCATIONID  INT,
    ISTOURIST   BOOLEAN,
    ISMUSTSEE   BOOLEAN
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the Sample Data)]

Open the [SAP Predictive Analytics documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS) page in a browser and click on the **View All** for the **Sample** section.

This will display the list of sample dataset available.

Right-click on the **Download the Sample Geo localization** link and use the **Save link address** to get the download URL.

Open the download URL and save the sample data archive either:

- the Eclipse host if you want to use the SAP HANA Tools for Eclipse
- the SAP HANA, express host if you want the IMPORT FROM SQL command

Extract the files (located in the `Geolocalization` subdirectory in the embedded `Geolocalization.zip` archive).

#### **Import Using the SAP HANA Tools for Eclipse**

You can extract the sample file anywhere you want on the Eclipse host.

You can now move to **Step 3: Import Using the SAP HANA Tools for Eclipse**.

#### **Import Using the IMPORT FROM SQL command**

Here is an example script that you can reuse to download and extract the dataset directly from the SAP HANA, express edition host:

```shell
URL=https://help.sap.com/http.svc/download?deliverable_id=20555041
OUTPUT_FILE=sample_geolocalization
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
```

It requires CURL (or WGET) to be installed.

The dataset files should now be located in: **`/usr/sap/HXE/HDB90/work/sample_geolocalization/Geolocalization`**

You can now move to **Step 3: Import Using the IMPORT FROM SQL command**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the SAP HANA Tools for Eclipse)]

You can import the data using the [SAP HANA Tools for Eclipse Import feature](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html) using the following details:

- **File Details:**

    - Set the **Field Delimiter** value to **Tab (\t)**.
    - Check the **Header row exists** box and set the **Header row** value to 1
    - Check the **Import all data** box.

- **Manage Table Definition and Data Mappings:**

    Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File                   | Target Table                        
| ----------------------------- | ----------------------------------  
| `gowalla_demo_2.txt`          | `PA_DATA.GOWALLA`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the IMPORT FROM SQL command)]

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/sample_geolocalization/Geolocalization`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_geolocalization/Geolocalization/gowalla_demo_2.txt' INTO PA_DATA.GOWALLA
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   TIMESTAMP FORMAT 'YYYY-MM-DD HH24:MI:SS'
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_geolocalization/Geolocalization/gowalla_demo_2.txt.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
SELECT 'GOWALLA'     as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.GOWALLA;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
