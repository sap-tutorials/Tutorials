---
title: Create a Flow Graph to Replicate Data
description: Create a flow graph using SAP HANA service smart data integration for SAP BTP to replicate data into SAP HANA service for SAP BTP.
auto_validation: true
time: 15
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, products>sap-hana, topic>big-data, tutorial>license]
primary_tag: products>sap-cloud-platform--sap-hana-service
---

## Prerequisites
 - This tutorial is designed for SAP HANA service for SAP Business Technology Platform. Consider newer tutorials designed for SAP HANA Cloud.
 - You have configured SAP HANA service smart data integration for SAP BTP [as explained in this tutorial](haas-dm-connect-sdi).
 - You have created a remote source and configured the access [as explained in this tutorial](haas-dm-access-cross-container-schema).

>**This tutorial cannot be completed with a trial account.**

## Details
### You will learn
  - How to Create a Virtual Table in SAP Web IDE
  - How to create a flowgraph to load data from the virtual table

>**This tutorial cannot currently be completed with a trial account.**

---

[ACCORDION-BEGIN [Step 1: ](Create a Virtual Table)]

Create a new virtual table in `db/data`.

![Create Flowgraph](7.png)

Call it `vt_salary_data` and flag **Generate configuration file**

![Create Flowgraph](9.png)

Fill the Properties as follows:

  - Remote Source Name: `LocalFile`
  - Database and Schema name: `<NULL>`
  - Object Name: `salarydata`

![Create Flowgraph](10.png)

**Save** and open the **File Format Editor**

![Create Flowgraph](11.png)

Here you can adapt the inbound file format. Change  **Skipped Header Lines** to 1.

![Create Flowgraph](12.png)

Copy the following lines to the simulation and run it. These have been taken from the `salarydata.csv` file

```text
ID,SALARY,START_YEAR,GENDER,REGION,T-LEVEL
100001,139171,1998,m,APJ,T5
```

![Create Flowgraph](13.png)

Use the **Copy** function and then use the pencil to edit the data types

![Create Flowgraph](14.png)

Change the types as follows:

![Create Flowgraph](15.png)

**Save** and **Build** the database module

![Create Flowgraph](16.png)


> As a reference, here is the code for the virtual table. You can access this mode by right-clicking on the virtual table artifact and choosing **Open Code Editor**.
>
> ```text
> VIRTUAL TABLE "vt_salary_data" AT "LocalFile"."<NULL>"."<NULL>"."salarydata"
REMOTE PROPERTY 'dataprovisioning_parameters'='<?xml version="1.0"  encoding="UTF-8" standalone="yes"?>
<Parameters>
<Parameter name="FORMAT">CSV</Parameter>
<Parameter name="FORCE_FILENAME_PATTERN">%.csv</Parameter>
<Parameter name="PARTITIONS">0</Parameter>
<Parameter name="CODEPAGE">utf-8</Parameter>
<Parameter name="LOCALE">en_US</Parameter>
<Parameter name="SKIP_HEADER_LINES">1</Parameter>
<Parameter name="ROW_DELIMITER">\n</Parameter>
<Parameter name="QUOTED_TEXT_CONTAIN_ROW_DELIMITER">false</Parameter>
<Parameter name="COLUMN_DELIMITER">,</Parameter>
<Parameter name="EXPONENTIAL">E</Parameter>
<Parameter name="LENIENT">true</Parameter>
<Parameter name="COLUMN">ID;INTEGER;</Parameter>
<Parameter name="COLUMN">SALARY;FLOAT;</Parameter>
<Parameter name="COLUMN">START_YEAR;INTEGER;</Parameter>
<Parameter name="COLUMN">GENDER;NVARCHAR(1);</Parameter>
<Parameter name="COLUMN">REGION;NVARCHAR(4);</Parameter>
<Parameter name="COLUMN">T-LEVEL;NVARCHAR(200);</Parameter>
</Parameters>';
>```

[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 2: ](Create a Flowgraph artifact)]

Create a new `Flowgraph` in `db/data`

![Create Flowgraph](1.png)

Call it `salary_flow`

![Create Flowgraph](2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add the data source)]

Click on the **+** button to add a node

![Create Flowgraph](3.png)

Choose **Data Source**  

![Create Flowgraph](4.png)

Click on the canvas to drop the node and then click **configure**

![Create Flowgraph](5.png)

Choose **HANA Object**

![Create Flowgraph](6.png)

Look for the virtual table `vt_salary_data`. Choose it and click **Create**.

![Create Flowgraph](8.png)

Mark all the control fields and delete them

![Create Flowgraph](17.png)

Click **Apply**

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Add a Data Target)]

Use the **+** sign to create a data target and drag the arrow from  the source to the target

![Create Flowgraph](20.png)

Click **Configure** on the target node

![Create Flowgraph](19.png)

The target mapping should be populated automatically using the source. Use `TGT_SALARIES` as the name of the target table.

![Create Flowgraph](21.png)

**Save and build** the Flowgraph.

![Create Flowgraph](22.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Execute the Flowgraph)]

Click on **Execute** in the Flowgraph.

![Create Flowgraph](23.png)

Confirm the warning

![Create Flowgraph](24.png)

You will see a success message in the console

![Create Flowgraph](25.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check data)]

Right-click on the database module and choose **Open HDI Container**

![Create Flowgraph](26.png)

In the HDI container, navigate to **Tables** and you will find the table created by the flowgraph. Right-click on it and choose **Open Data**

![Create Flowgraph](27.png)

Use ![Create Flowgraph](edit.png) **Edit SQL statement** to modify the select statement for a `SELECT COUNT(*)`. Count the total records to complete the validation below

[VALIDATE_1]
[ACCORDION-END]


---
