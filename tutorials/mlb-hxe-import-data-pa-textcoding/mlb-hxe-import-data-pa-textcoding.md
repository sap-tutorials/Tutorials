---
title: Import a Text Coding Sample Dataset
description: Import SAP Predictive Analytics Text Coding Sample Datasets in your SAP HANA, express edition instance
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 10
---

## Prerequisites  
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://developers.sap.com/tutorials/mlb-hxe-setup-basic.html)

## Details
### You will learn
In this tutorial, you will learn how to download and import the SAP Predictive Analytics Text Coding sample dataset into your SAP HANA, express edition instance.

---

[ACCORDION-BEGIN [Info: ](Sample file structure)]

The sample structure for the  [**Sample Text Coding**](https://help.sap.com/http.svc/download?deliverable_id=20555049) is the following:

```
|-- sample_text_coding_3.3.1_en-us_production.zip
   |-- Text_Coding.zip
   |   |-- Text_Coding
   |       |-- dmcDefinition
   |       |   |-- ConceptList_dmc
   |       |   |-- StemmingRules_dmc
   |       |   |-- StopList_dmc
   |       |   |-- SynonymList_dmc
   |       |-- desc_dmc2006_enriched_no_textual.txt
   |       |-- desc_dmc2006_enriched_textual.txt
   |       |-- desc_dmc2006_with_textual.txt
   |       |-- desc_dmc2006_with_two_textual.txt
   |       |-- desc_dmc2006_without_textual.txt
   |       |-- dmc2006.txt
   |       |-- dmc2006_enriched.txt
   |-- metadata.xml
```

**Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
drop TABLE PA_DATA.DMC2006 ;
drop TABLE PA_DATA.DMC2006_ENRICHED ;

CREATE TABLE PA_DATA.DMC2006 (
  AUCT_ID                           INT,
  ITEM_LEAF_CATEGORY_NAME           VARCHAR(100),
  LISTING_TITLE                     VARCHAR(100),
  LISTING_SUBTITLE                  CLOB,
  LISTING_START_DATE                DAYDATE,
  LISTING_END_DATE                  DAYDATE,
  LISTING_DURTN_DAYS                INT,
  LISTING_TYPE_CODE                 INT,
  FEEDBACK_SCORE_AT_LISTING_TIME    INT,
  START_PRICE                       SMALLDECIMAL,
  BUY_IT_NOW_PRICE                  SMALLDECIMAL,
  BUY_IT_NOW_LISTED_FLAG            VARCHAR(1),
  BOLD_FEE_FLAG                     VARCHAR(1),
  FEATURED_FEE_FLAG                 VARCHAR(1),
  CATEGORY_FEATURED_FEE_FLAG        VARCHAR(1),
  GALLERY_FEE_FLAG                  VARCHAR(1),
  GALLERY_FEATURED_FEE_FLAG         VARCHAR(1),
  IPIX_FEATURED_FEE_FLAG            VARCHAR(1),
  RESERVE_FEE_FLAG                  VARCHAR(1),
  HIGHLIGHT_FEE_FLAG                VARCHAR(1),
  SCHEDULE_FEE_FLAG                 VARCHAR(1),
  BORDER_FEE_FLAG                   VARCHAR(1),
  QTY_AVAILABLE_PER_LISTING         INT,
  GMS                               SMALLDECIMAL,
  CATEGORY_AVG_GMS                  SMALLDECIMAL,
  GMS_GREATER_AVG                   SMALLDECIMAL,
  PRIMARY KEY (AUCT_ID)
);

CREATE TABLE PA_DATA.DMC2006_ENRICHED (
  AUCT_ID                           INT,
  ITEM_LEAF_CATEGORY_NAME           VARCHAR(100),
  LISTING_TITLE                     VARCHAR(100),
  LISTING_SUBTITLE                  CLOB,
  LISTING_START_DATE                TIMESTAMP,
  LISTING_END_DATE                  TIMESTAMP,
  LISTING_DURTN_DAYS                INT,
  LISTING_TYPE_CODE                 INT,
  FEEDBACK_SCORE_AT_LISTING_TIME    INT,
  START_PRICE                       SMALLDECIMAL,
  BUY_IT_NOW_PRICE                  SMALLDECIMAL,
  BUY_IT_NOW_LISTED_FLAG            VARCHAR(1),
  BOLD_FEE_FLAG                     VARCHAR(1),
  FEATURED_FEE_FLAG                 VARCHAR(1),
  CATEGORY_FEATURED_FEE_FLAG        VARCHAR(1),
  GALLERY_FEE_FLAG                  VARCHAR(1),
  GALLERY_FEATURED_FEE_FLAG         VARCHAR(1),
  IPIX_FEATURED_FEE_FLAG            VARCHAR(1),
  RESERVE_FEE_FLAG                  VARCHAR(1),
  HIGHLIGHT_FEE_FLAG                VARCHAR(1),
  SCHEDULE_FEE_FLAG                 VARCHAR(1),
  BORDER_FEE_FLAG                   VARCHAR(1),
  QTY_AVAILABLE_PER_LISTING         INT,
  GMS                               SMALLDECIMAL,
  CATEGORY_AVG_GMS                  SMALLDECIMAL,
  GMS_GREATER_AVG                   SMALLDECIMAL,
  LISTING_END_MONTHOFYEAR           INT,
  LISTING_START_MONTHOFYEAR         INT,
  LISTING_END_DAYOFMONTH            INT,
  LISTING_START_DAYOFMONTH          INT,
  LISTING_START_MONDAY              INT,
  LISTING_START_TUESDAY             INT,
  LISTING_START_WEDNESDAY           INT,
  LISTING_START_THURSDAY            INT,
  LISTING_START_FRIDAY              INT,
  LISTING_START_SATURDAY            INT,
  LISTING_START_SUNDAY              INT,
  LISTING_END_MONDAY                INT,
  LISTING_END_TUESDAY               INT,
  LISTING_END_WEDNESDAY             INT,
  LISTING_END_THURSDAY              INT,
  LISTING_END_FRIDAY                INT,
  LISTING_END_SATURDAY              INT,
  LISTING_END_SUNDAY                INT,
  START_PRICE_DIV_MEAN_CATEGORY      SMALLDECIMAL,
  BUY_IT_NOW_PRICE_DIV_MEAN_CATEGORY SMALLDECIMAL,
  PRIMARY KEY (AUCT_ID)
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the Sample Data)]

Open the [SAP Predictive Analytics documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS) page in a browser and click on the **View All** for the **Sample** section.

This will display the list of sample dataset available.

Right-click on the **Download the Sample Text Coding** link and use the **Save link address** to get the download URL.

Open the download URL and save the sample data archive either:

- the Eclipse host if you want to use the SAP HANA Tools for Eclipse
- the SAP HANA, express host if you want the IMPORT FROM SQL command

Extract the files (located in the `Text_Coding` subdirectory in the embedded `Text_Coding.zip` archive).

#### **Import Using the SAP HANA Tools for Eclipse**

You can extract the sample file anywhere you want on the Eclipse host.

You can now move to **Step 3: Import Using the SAP HANA Tools for Eclipse**.

#### **Import Using the IMPORT FROM SQL command**

You can extract the sample file anywhere you want on the Eclipse host.

Here is an example script that you reuses to download and extract the sample dataset from the SAP HANA, express edition host:

```shell
URL=https://help.sap.com/http.svc/download?deliverable_id=20555049
OUTPUT_FILE=sample_textcoding
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
      -exec unzip --  '{}' \; \
      -exec rm -- '{}' \;; done

 # convert extract data file from dos to unix
dos2unix ./Text_Coding/dmc2006.txt
dos2unix ./Text_Coding/dmc2006_enriched.txt
```

It requires CURL (or WGET) & DOS2UNIX to be installed.

It will also to explicitly convert the file from DOS to UNIX else the import will fail.

The dataset files should now be located in: **`/usr/sap/HXE/HDB90/work/sample_textcoding/Text_Coding`**

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

| Source File                   | Target Table                       | Field Delimiter
| ----------------------------- | ---------------------------------- | ----------------- |
| `dmc2006.txt`                 | `PA_DATA.DMC2006`                  | Tab (\t)
| `dmc2006_enriched.txt`        | `PA_DATA.DMC2006_ENRICHED`         | Tab (\t)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the IMPORT FROM SQL command)]

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/sample_textcoding/Text_Coding`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_textcoding/Text_Coding/dmc2006.txt' INTO PA_DATA.DMC2006
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '`'
   DATE FORMAT 'YYYY-MM-DD'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_textcoding/Text_Coding/dmc2006.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_textcoding/Text_Coding/dmc2006_enriched.txt' INTO PA_DATA.DMC2006_ENRICHED
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '`'
   TIMESTAMP FORMAT 'DD/MM/YYYY HH24:MI'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_textcoding/Text_Coding/dmc2006_enriched.txt.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
SELECT 'DMC2006'            as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.DMC2006
UNION
SELECT 'DMC2006_ENRICHED'   as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.DMC2006_ENRICHED;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
