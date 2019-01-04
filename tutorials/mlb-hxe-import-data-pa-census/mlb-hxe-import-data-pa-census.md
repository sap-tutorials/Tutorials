---
title: Import a Census Sample Dataset
description: Import SAP Predictive Analytics Census Sample Dataset in your SAP HANA, express edition instance
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 10
---

## Prerequisites  
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://developers.sap.com/tutorials/mlb-hxe-setup-basic.html)

## Details
### You will learn
In this tutorial, you will learn how to download and import the SAP Predictive Analytics Census sample dataset into your SAP HANA, express edition instance.

[ACCORDION-BEGIN [Info: ](Sample file structure)]

The sample structure for the [**Sample Data Census**](https://help.sap.com/http.svc/download?deliverable_id=20555035) is the following:

```
|--sample_census_3.3.1_en-us_production.zip
    |-- Census.zp
    |    |-- Census
    |        |-- Census01.csv
    |        |-- Desc_Census01.csv
    |-- metadata.xml
```

**Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.

This Census data set (`Census01.csv`) presents the data on 48842 individual Americans, of at least 17 years of age. Each individual is characterized by 15 data items. These data, or variables, are described in the following table:

| Variable                      | Description                                       | Example of Values
| :---------------------------- | :------------------------------------------------ | :-----------------------------------
| <nobr>`age`</nobr>            | Age of individuals                                | <nobr>Any value greater than 17</nobr>
| <nobr>`workclass`</nobr>      | Employer category                                 | <nobr>Private, Self-employed, ...</nobr>
| <nobr>`fnlwgt`</nobr>         | Statistical Weight variable                       | <nobr>Any numerical value</nobr>
| <nobr>`education`</nobr>      | Level of study                                    | <nobr>11th, Bachelors</nobr>
| <nobr>`education_num`</nobr>  | Number of years of study                          | <nobr>A value between 1 and 16</nobr>
| <nobr>`marital_status`</nobr> | Marital status                                    | <nobr>Divorced, Married, ...</nobr>
| <nobr>`occupation`</nobr>     | Job classification                                | <nobr>Sales, Cleaners, ...</nobr>
| <nobr>`relationship`</nobr>   | Position in family                                | <nobr>Husband, Wife, ...</nobr>
| <nobr>`race`</nobr>           | Ethnicity                                         |
| <nobr>`sex`</nobr>            | Gender                                            | <nobr>Male, Female, ...</nobr>
| <nobr>`capital_gain`</nobr>   | Annual capital gains                              | <nobr>Any numerical value</nobr>
| <nobr>`capital_loss`</nobr>   | Annual capital losses                             | <nobr>Any numerical value</nobr>
| <nobr>`native country`</nobr> | Country of origin                                 | <nobr>United States, France, ...</nobr>
| <nobr>`class`</nobr>          | Variable indicating if the salary is greater $50 k | <nobr>if salary > $50 k then 1 else 0</nobr>

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a dedicated schema)]

It is a good practice to separate data into different schema based on their origin.

In this tutorial, you will be using the **`PA_DATA`** schema to load the SAP Predictive Analytics sample data.

If you have already created the schema, move to the next step.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
CREATE SCHEMA PA_DATA;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
CREATE TABLE PA_DATA.CENSUS (
    AGE             INT,
    WORKCLASS       VARCHAR(20),
    FNLWGT          INT,
    EDUCATION       VARCHAR(20),
    EDUCATION_NUM   INT,
    MARITAL_STATUS  VARCHAR(30),
    OCCUPATION      VARCHAR(20),
    RELATIONSHIP    VARCHAR(20),
    RACE            VARCHAR(20),
    SEX             VARCHAR(10),
    CAPITAL_GAIN    INT,
    CAPITAL_LOSS    INT,
    HOURS_PER_WEEK  INT,
    NATIVE_COUNTRY  VARCHAR(30),
    CLASS           INT
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Download the Sample Data)]

Open the [SAP Predictive Analytics documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS) page in a browser and click on the **View All** for the **Sample** section.

This will display the list of sample dataset available.

Right-click on the **Download the Sample Data Census** link and use the **Save link address** to get the download URL.

Open the download URL and save the sample data archive either:

- the Eclipse host if you want to use the SAP HANA Tools for Eclipse
- the SAP HANA, express host if you want the IMPORT FROM SQL command

Extract the files (located in the `Census` subdirectory in the embedded `Census.zip` archive).

#### **Import Using the SAP HANA Tools for Eclipse**

You can extract the sample file anywhere you want on the Eclipse host.

You can now move to **Step 1: Import Using the SAP HANA Tools for Eclipse**.

#### **Import Using the IMPORT FROM SQL command**

Here is an example script that you can reuse to download and extract the dataset directly from the SAP HANA, express edition host:

```shell
URL=https://help.sap.com/http.svc/download?deliverable_id=20555035
OUTPUT_FILE=sample_census
OUTPUT_DIR=/usr/sap/HXE/HDB90/work/$OUTPUT_FILE

 # create a new subdirectory for the sample data'
mkdir $OUTPUT_DIR

 # download the archive in the sample data directory'
 # wget -O $OUTPUT_DIR/$OUTPUT_FILE.zip $URL
curl $URL -o $OUTPUT_DIR/$OUTPUT_FILE.zip

 # switch to the new directory'
cd $OUTPUT_DIR

 # extract all archives and embedded archives'
while [ "`find . -type f -name '*.zip' | wc -l`" -gt 0 ]; \
  do find -type f -name "*.zip" \
     -exec unzip -o --  '{}' \; \
     -exec rm -- '{}' \;; done

```

It requires CURL (or WGET) to be installed.

The dataset files should now be located in: **`/usr/sap/HXE/HDB90/work/sample_census/Census`**

You can now move to **Step 1: Import Using the IMPORT FROM SQL command**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Import Using the SAP HANA Tools for Eclipse)]

You can import the data using the [SAP HANA Tools for Eclipse Import feature](https://developers.sap.com/tutorials/mlb-hxe-import-data-eclipse.html) using the following details:

- **File Details:**

    - Set the **Field Delimiter** value to **Comma (,)**.
    - Check the **Header row exists** box and set the **Header row** value to 1
    - Check the **Import all data** box.

- **Target Table:**

    - Select the **Existing** radio, and pick **`PA_DATA.CENSUS`**

- **Manage Table Definition and Data Mappings:**

    Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File                | Target Table                    
| -------------------------- | ----------------------------------  
| `Census01.csv`             | `PA_DATA.CENSUS`                

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Import Using the IMPORT FROM SQL command)]

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/sample_census/Census`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_census/Census/Census01.csv' INTO PA_DATA.CENSUS
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY ','
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_census/Census/Census01.csv.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
SELECT 'CENSUS' as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CENSUS;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
