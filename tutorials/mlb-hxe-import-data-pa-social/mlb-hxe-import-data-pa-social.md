---
title: Import a Social / Link Analysis Sample Dataset
description: Import SAP Predictive Analytics Social / Link Analysis  Sample Datasets in your SAP HANA, express edition instance
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 10
---

## Prerequisites  
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://developers.sap.com/tutorials/mlb-hxe-setup-basic.html)

## Details
### You will learn
In this tutorial, you will learn how to download and import the SAP Predictive Analytics Social / Link Analysis sample dataset into your SAP HANA, express edition instance.

[ACCORDION-BEGIN [Info: ](Sample file structure)]

The sample structure for the [**Sample Social**](https://help.sap.com/http.svc/download?deliverable_id=20555046) is the following:

```
|-- sample_social_3.3.1_en-us_production.zip
   |-- Social.zip
   |   |-- Social
   |       |-- Contact
   |       |   |-- demo_applyIn.txt
   |       |   |-- demo_applyIn_with_com.txt
   |       |   |-- demo_contact_events.txt
   |       |   |-- demo_contact_id_conversion.txt
   |       |   |-- demo_contact_nodes_description.txt
   |       |   |-- KxDesc_demo_contact_events.txt
   |       |   |-- KxDesc_demo_contact_id_conversion.txt
   |       |   |-- KxDesc_demo_contact_nodes_description.txt
   |       |-- Links
   |           |-- sn_links.txt
   |           |-- sn_nodes.txt
   |           |-- KxDesc_sn_links.txt
   |           |-- KxDesc_sn_nodes.txt
   |-- metadata.xml
```

**Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
CREATE TABLE PA_DATA.CONTACT_DEMO_APPLYIN (
  CUSTOMER_ID   INT,
  PRIMARY KEY (CUSTOMER_ID)
);
CREATE TABLE PA_DATA.CONTACT_DEMO_APPLYIN_WITH_COM (
  CUSTOMER_ID      INT,
  COMMUNITY_INDEX  INT,
  PRIMARY KEY (CUSTOMER_ID)
);
CREATE TABLE PA_DATA.CONTACT_DEMO_CONTACT_EVENTS (
  SRC         VARCHAR(10),
  DEST        VARCHAR(10),
  CALL_TYPE   VARCHAR(5),
  DATE        TIMESTAMP,
  DURATION    INT
);
CREATE TABLE PA_DATA.CONTACT_DEMO_CONTACT_ID_CONVERSION (
  LINE_NUMBER    VARCHAR(10),
  CUSTOMER_ID    INT,
  PRIMARY KEY (CUSTOMER_ID)
);
CREATE TABLE PA_DATA.CONTACT_DEMO_CONTACT_NODES_DESCRIPTION (
  CUSTOMER_ID         INT,
  GENDER              VARCHAR(1),
  AGE                 INT,
  ZIP_CODE            INT,
  DISTRIB_CHANNEL_ID  VARCHAR(10),
  DEVICE_BRAND_NAME   VARCHAR(10),
  DEVICE_MODEL_NAME   VARCHAR(10),
  TENURE_IN_MONTH     INT,
  NB_SMS_04           INT,
  NB_CALLS_04         INT,
  CHURNED_IN_04       INT,
  NB_SMS_05           INT,
  NB_CALLS_05         INT,
  CHURNED_IN_05       INT,
  NB_SMS_06           INT,
  NB_CALLS_06         INT,
  CHURNED_IN_06       INT,
  CHURN_DATE          DAYDATE,
  PRIMARY KEY (CUSTOMER_ID)
);
CREATE TABLE PA_DATA.LINKS_SN_LINKS (
  NODE_ID1  INT,
  NODE_ID2  INT,
  PRIMARY KEY (NODE_ID1, NODE_ID2)
);
CREATE TABLE PA_DATA.LINKS_SN_NODES (
  NODE_ID      INT,
  GENDER       VARCHAR(10),
  AGE          INT,
  NB_SENT_MSG  INT,
  PRIMARY KEY (NODE_ID)
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the Sample Data)]

Open the [SAP Predictive Analytics documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS) page in a browser and click on the **View All** for the **Sample** section.

This will display the list of sample dataset available.

Right-click on the **Download the Sample Social** link and use the **Save link address** to get the download URL.

Open the download URL and save the sample data archive either:

- the Eclipse host if you want to use the SAP HANA Tools for Eclipse
- the SAP HANA, express host if you want the IMPORT FROM SQL command

Extract the files (located in the `Social` subdirectory in the embedded `Social.zip` archive).

#### **Import Using the SAP HANA Tools for Eclipse**

You can extract the sample file anywhere you want on the Eclipse host.

You can now move to **Step 3: Import Using the SAP HANA Tools for Eclipse**.

#### **Import Using the IMPORT FROM SQL command**

Here is an example script that you can reuse to download and extract the dataset directly from the SAP HANA, express edition host:

```shell
URL=https://help.sap.com/http.svc/download?deliverable_id=20555046
OUTPUT_FILE=sample_social
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

 # remove trailing space
sed --in-place 's/[[:space:]]\+$//' $OUTPUT_DIR/Social/Contact/demo_contact_id_conversion.txt
```

It requires CURL (or WGET) & SED to be installed.

It will also trim the trailing spaces from `demo_contact_id_conversion.txt` which will cause import errors if you don't.

The dataset files should now be located in: **`/usr/sap/HXE/HDB90/work/sample_social/Social`**

You can now move to **Step 3: Import Using the IMPORT FROM SQL command**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the SAP HANA Tools for Eclipse)]

You can import the data using the [SAP HANA Tools for Eclipse Import feature](https://developers.sap.com/tutorials/mlb-hxe-import-data-eclipse.html) using the following details:

- **File Details:**

    - Set the **Field Delimiter** value as listed in the table bellow.
    - Check the **Header row exists** box and set the **Header row** value to 1
    - Check the **Import all data** box.

- **Manage Table Definition and Data Mappings:**

    Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File                           | Target Table                                      | Field Delimiter
| ------------------------------------- | ------------------------------------------------- | ----------------- |
| `demo_applyIn.txt`                    | `PA_DATA.CONTACT_DEMO_APPLYIN`                    | Tab (\t)
| `demo_applyIn_with_com.txt`           | `PA_DATA.CONTACT_DEMO_APPLYIN_WITH_COM`           | Tab (\t)
| `demo_contact_events.txt`             | `PA_DATA.CONTACT_DEMO_CONTACT_EVENTS`             | Tab (\t)
| `demo_contact_id_conversion.txt`      | `PA_DATA.CONTACT_DEMO_CONTACT_ID_CONVERSION`      | Tab (\t)
| `demo_contact_nodes_description.txt`  | `PA_DATA.CONTACT_DEMO_CONTACT_NODES_DESCRIPTION`  | Tab (\t)
| `sn_links.txt`                        | `PA_DATA.LINKS_SN_LINKS`                          | Semi Colon (;)
| `sn_nodes.txt`                        | `PA_DATA.LINKS_SN_NODES`                          | Semi Colon (;)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the IMPORT FROM SQL command)]

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/sample_social/Social`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_applyIn.txt' INTO PA_DATA.CONTACT_DEMO_APPLYIN
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_applyIn.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_applyIn_with_com.txt' INTO PA_DATA.CONTACT_DEMO_APPLYIN_WITH_COM
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_applyIn_with_com.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_contact_events.txt' INTO PA_DATA.CONTACT_DEMO_CONTACT_EVENTS
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   TIMESTAMP FORMAT 'YYYY-MM-DD HH24:MI:SS'
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_contact_events.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_contact_id_conversion.txt' INTO PA_DATA.CONTACT_DEMO_CONTACT_ID_CONVERSION
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_contact_id_conversion.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_contact_nodes_description.txt' INTO PA_DATA.CONTACT_DEMO_CONTACT_NODES_DESCRIPTION
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY '\t'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_social/Social/Contact/demo_contact_nodes_description.txt.err'
;

IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_social/Social/Links/sn_links.txt' INTO PA_DATA.LINKS_SN_LINKS
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY ';'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_social/Social/Links/sn_links.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_social/Social/Links/sn_nodes.txt' INTO PA_DATA.LINKS_SN_NODES
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY ';'
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/sample_social/Social/Links/sn_nodes.txt.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
SELECT 'CONTACT_DEMO_APPLYIN'                   as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CONTACT_DEMO_APPLYIN
UNION
SELECT 'CONTACT_DEMO_APPLYIN_WITH_COM'          as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CONTACT_DEMO_APPLYIN_WITH_COM
UNION
SELECT 'CONTACT_DEMO_CONTACT_EVENTS'            as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CONTACT_DEMO_CONTACT_EVENTS
UNION
SELECT 'CONTACT_DEMO_CONTACT_ID_CONVERSION'     as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CONTACT_DEMO_CONTACT_ID_CONVERSION
UNION
SELECT 'CONTACT_DEMO_CONTACT_NODES_DESCRIPTION' as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CONTACT_DEMO_CONTACT_NODES_DESCRIPTION
UNION
SELECT 'LINKS_SN_LINKS'                         as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.LINKS_SN_LINKS
UNION
SELECT 'LINKS_SN_NODES'                         as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.LINKS_SN_NODES
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
