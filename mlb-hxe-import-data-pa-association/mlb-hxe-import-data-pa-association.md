---
title: Import an Association Rules Sample Dataset
description: Import SAP Predictive Analytics Association Rules Sample Dataset in your SAP HANA, express edition instance
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 10
---

## Details
### You will learn
In this tutorial, you will learn how to download and import the SAP Predictive Analytics Association Rules sample dataset into your SAP HANA, express edition instance.

[ACCORDION-BEGIN [Info: ](Sample file structure)]

The sample structure for the [**Sample Data for Association Rules**](https://help.sap.com/http.svc/download?deliverable_id=20555031) is the following:

```
|-- sample_association_rules_3.3.1_en-us_production.zip
	|-- Association_Rules.zip
	| 	|-- Association_Rules
	| 	|-- customers_references.txt
	| 	|-- customers_references_desc.txt
	| 	|-- customers_transactions.txt
	| 	|-- customers_transactions_desc.txt
	| 	|-- website_references.csv
	| 	|-- website_references_desc.csv
	| 	|-- website_transactions.csv
	| 	|-- website_transactions_desc.csv
	| 	|-- readme.txt / lisezmoi.txt
	|-- metadata.xml
```

**Note:** `desc` and `KxDesc` files are SAP Predictive Analytics dataset description files and will not be loaded.

#### **Customers orders**

This data set presents website purchase, and includes a reference file (`customers_references.txt`) describing static information on customers and a transaction file (`customers_transactions.txt`) with the customer purchases.

- **Customers references**

| Variable 						| Description 					| Example of Values
| :---------------------------- | :---------------------------- | :-----------------------------------
| <nobr>`UserID`</nobr> 		| the customer key 				| <nobr>Any numerical value</nobr>
| <nobr>`Country`</nobr> 	 	| the customer country 			| <nobr>ITALY, FRANCE, ...</nobr>
| <nobr>`Site`</nobr> 		 	| the site domain 				| <nobr>net, com, .fr</nobr>
| <nobr>`IDProv`</nobr> 	 	| Level of study 				| <nobr>A value between 1 and 16</nobr>
| <nobr>`OrdersCount`</nobr> 	| the number of orders posted 	| <nobr>Any numerical value</nobr>

 - **Customers transactions**

| Variable 						 | Description 				 | Example of Values
| :----------------------------- | :------------------------ |  :-----------------------------------
| <nobr>`UserID`</nobr> 		 | the customer key 		 | <nobr>Any numerical value</nobr>
| <nobr>`ItemPurchased`</nobr> 	 | the item purchased 		 | <nobr>A product category</nobr>
| <nobr>`Date_PutInCaddy`</nobr> | the order date 			 | <nobr>A date</nobr>
| <nobr>`Quantity`</nobr> 		 | the ordered quantity 	 | <nobr>Any numerical value</nobr>
| <nobr>`TransactionID`</nobr> 	 | the order transaction id  | <nobr>Any numerical value</nobr>

#### **Website navigation**

This data set presents website navigation and includes a reference file (`website_references.txt`) including the session identifiers sessions and a transaction file (`website_transactions.txt`) with the list of page views associated with a session.

- **Session references**

| Variable 					| Description 		| Example of Values
| :------------------------ | :---------------- | :-----------------------------------
| <nobr>`SessionID`</nobr> 	| the session id  	| <nobr>Any numerical value</nobr>

 - **Session transactions**

| Variable 						| Description 					| Example of Values
| :---------------------------- | :-----------------------------| :-----------------------------------
| <nobr>`SessionID`</nobr> 		| the session id 				| <nobr>Any numerical value</nobr>
| <nobr>`Page`</nobr> 			| the visited page 				| <nobr>A page name</nobr>
| <nobr>`TransactionID`</nobr> 	| the associated transaction id | <nobr>A date</nobr>
| <nobr>`IPAddress`</nobr> 		| the visitor IP address 		| <nobr>An IP address</nobr>
| <nobr>`Time`</nobr> 			| the date/time of the visit 	| <nobr>A date/time</nobr>

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
CREATE TABLE PA_DATA.CUSTOMERS_REFERENCES (
 USERID INT,
 COUNTRY VARCHAR(20),
 SITE VARCHAR(3),
 IDPROV INT,
 ORDERS_COUNT INT,
 PRIMARY KEY (USERID)
);
CREATE TABLE PA_DATA.CUSTOMERS_TRANSACTIONS (
 USERID INT,
 ITEMPURCHASED VARCHAR(20),
 DATE_PUTINCADDY TIMESTAMP,
 QUANTITY VARCHAR(5),
 TRANSACTIONID INT,
 PRIMARY KEY (TRANSACTIONID)
);
CREATE TABLE PA_DATA.WEBSITE_REFERENCES (
 SESSIONID INT,
 PRIMARY KEY (SESSIONID)
);
CREATE TABLE PA_DATA.WEBSITE_TRANSACTIONS (
 SESSIONID INT,
 PAGE VARCHAR(50),
 TRANSACTIONID INT,
 IPADDRESS VARCHAR(15),
 VISITE_TIME TIMESTAMP,
 PRIMARY KEY (TRANSACTIONID)
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the Sample Data)]

Open the [SAP Predictive Analytics documentation](https://help.sap.com/viewer/p/SAP_PREDICTIVE_ANALYTICS) page in a browser and click on the **View All** for the **Sample** section.

This will display the list of sample dataset available.

Right-click on the **Download the Sample Data for Association Rules** link and use the **Save link address** to get the download URL.

Open the download URL and save the sample data archive either:

- the Eclipse host if you want to use the SAP HANA Tools for Eclipse
- the SAP HANA, express host if you want the IMPORT FROM SQL command

Extract the files (located in the `Association_Rules` subdirectory in the embedded `Association_Rules.zip` archive).

#### **Import Using the SAP HANA Tools for Eclipse**

You can extract the sample file anywhere you want on the Eclipse host.

You can now move to **Step 3: Import Using the SAP HANA Tools for Eclipse**.

#### **Import Using the IMPORT FROM SQL command**

Here is an example script that you can reuse to download and extract the dataset directly from the SAP HANA, express edition host:

```shell
URL=https://help.sap.com/http.svc/download?deliverable_id=20555031
OUTPUT_FILE=sample_association
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
 -exec unzip -o -- '{}' \; \
 -exec rm -- '{}' \;; done
```

It requires CURL (or WGET) to be installed.

The dataset files should now be located in: **`/usr/sap/HXE/HDB90/work/sample_association/Association_Rules`**

You can now move to **Step 3: Import Using the IMPORT FROM SQL command**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the SAP HANA Tools for Eclipse)]

You can import the data using the [SAP HANA Tools for Eclipse Import feature](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html) using the following details:

- **File Details:**

 - Set the **Field Delimiter** value as listed in the table bellow.
 - Check the **Header row exists** box and set the **Header row** value to 1
 - Check the **Import all data** box.

- **Manage Table Definition and Data Mappings:**

 Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File 					| Target Table 						| Field Delimiter
| ----------------------------- | --------------------------------- | ----------------- |
| `customers_references.txt` 	| `PA_DATA.CUSTOMERS_REFERENCES` 	| Tab (\t)
| `customers_transactions.txt` 	| `PA_DATA.CUSTOMERS_TRANSACTIONS` 	| Tab (\t)
| `website_references.txt` 		| `PA_DATA.WEBSITE_REFERENCES` 		| Semi Colon (;)
| `website_transactions.txt` 	| `PA_DATA.WEBSITE_TRANSACTIONS` 	| Semi Colon (;)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import Using the IMPORT FROM SQL command)]

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/sample_association/Association_Rules`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/customers_references.txt' INTO PA_DATA.CUSTOMERS_REFERENCES
WITH
 RECORD DELIMITED BY '\n'
 FIELD DELIMITED BY '\t'
 OPTIONALLY ENCLOSED BY '"'
 SKIP FIRST 1 ROW
 FAIL ON INVALID DATA
 ERROR LOG '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/customers_references.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/customers_transactions.txt' INTO PA_DATA.CUSTOMERS_TRANSACTIONS
WITH
 RECORD DELIMITED BY '\n'
 FIELD DELIMITED BY '\t'
 OPTIONALLY ENCLOSED BY '"'
 SKIP FIRST 1 ROW
 FAIL ON INVALID DATA
 ERROR LOG '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/customers_transactions.txt.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/website_references.csv' INTO PA_DATA.WEBSITE_REFERENCES
WITH
 RECORD DELIMITED BY '\n'
 FIELD DELIMITED BY ';'
 OPTIONALLY ENCLOSED BY '"'
 SKIP FIRST 1 ROW
 FAIL ON INVALID DATA
 ERROR LOG '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/website_references.csv.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/website_transactions.csv' INTO PA_DATA.WEBSITE_TRANSACTIONS
WITH
 RECORD DELIMITED BY '\n'
 FIELD DELIMITED BY ';'
 OPTIONALLY ENCLOSED BY '"'
 SKIP FIRST 1 ROW
 TIMESTAMP FORMAT 'YYYY-MM-DD:HH24:MI:SS'
 FAIL ON INVALID DATA
 ERROR LOG '/usr/sap/HXE/HDB90/work/sample_association/Association_Rules/website_transactions.csv.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
SELECT 'CUSTOMERS_REFERENCES' as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CUSTOMERS_REFERENCES
UNION
SELECT 'CUSTOMERS_TRANSACTIONS' as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.CUSTOMERS_TRANSACTIONS
UNION
SELECT 'WEBSITE_REFERENCES' as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.WEBSITE_REFERENCES
UNION
SELECT 'WEBSITE_TRANSACTIONS' as TABLE_NAME, count(1) as ROW_COUNT from PA_DATA.WEBSITE_TRANSACTIONS
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
