---
title: Create a Calculation View of type Cube in SAP HANA service for SAP BTP
description: Use SAP Web IDE Full Stack to create a calculation view to expose to SAP Analytics Cloud
auto_validation: true
time: 10
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, topic>cloud, products>sap-analytics-cloud, products>sap-hana, products>sap-cloud-platform\,-sap-hana-service, tutorial>license]
primary_tag: products>sap-hana
---

## Prerequisites
 - This tutorial is designed for SAP HANA service for SAP Business Technology Platform. Consider newer tutorials designed for SAP HANA Cloud.
 - You have created an anonymized view as explained [in this tutorial](haas-dm-calculation-view-differential-privacy)
 - You have created and loaded data in the `PERFORMANCE` table as explained [in this tutorial](haas-dm-create-db-mta)

>**This tutorial cannot be completed with a trial account.**

## Details
### You will learn
  - How to create a calculation view in SAP Web IDE Full Stack
  - How to join data from another calculation view

This calculation view will be consumed in a report in SAP Analytics Cloud

---

[ACCORDION-BEGIN [Step 1: ](Create a Calculation View)]

In the models folder, create a calculation view called `PERFORMANCE_SALARIES`

![New calculation view](2.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Join the calculation view and the table)]

Drop a `join` node into the modelling space

![New calculation view](3.png)

Use the ![plus sign](plus.png) sign to add the calculation view to the node.

![New calculation view](4.png)

Type in the name of the calculation view. Select if and click **Finish**

![New calculation view](5.png)

Click on the ![plus sign](plus.png) sign again and add the table `PERFORMANCE`.  Click **Finish** and you should see both artifacts in the join node:

![New calculation view](6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Configure the join)]

Double-click on the join node. A panel will open on the right.

Drag and drop the `ID` field to join the entries in `SALARIES_ANONYMIZED` with the records in `PERFORMANCE`.

![New calculation view](7.png)

Set the cardinality to `1..1`

![New calculation view](8.png)

In the mapping tab, add all the columns as output columns. Make sure `ID` is only added once

![New calculation view](9.png)

Connect the join node with the aggregation node using the ![arrow](arrow.png)

![New calculation view](1.gif)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure semantics)]

Click on the **aggregation** node and double-click on the join parent to add all the columns to the output

![New calculation view](10.png)

Go into the **Semantics** node and change the `START_YEAR` and `ID` to attributes. Set the aggregation of the remaining measures to `AVG`

![New calculation view](11.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Build and test)]

Right-click on the calculation view and select **Build selected files**

![New calculation view](12.png)

Once the build is successful, right-click on the view and select **Data preview**

> If you get an error, choose `Open HDI container` instead and look for the calculation view under `Column Views` in the database explorer.

Go into the **Analysis** tab. Drag the `T-LEVEL` attribute into the label axis and the `Evaluation Rating` into the values axis.


[VALIDATE_1]
[ACCORDION-END]

---
