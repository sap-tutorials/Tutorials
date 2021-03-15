---
title: Create a Calculation View with Differential Privacy in SAP Web IDE
description: Use differential privacy to anonymize confidential data
auto_validation: true
time: 20
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, products>sap-hana, tutorial>license]
primary_tag: products>sap-cloud-platform--sap-hana-service
---

## Prerequisites
 - This tutorial is designed for SAP HANA service for SAP Business Technology Platform. Consider newer tutorials designed for SAP HANA Cloud.
 - You have completed the previous tutorials to load data into the table `TGT_SALARIES`

>**This tutorial cannot be completed with a trial account.**

## Details
### You will learn
  - How to create Calculation View of type Cube using SAP Web IDE
  - How to configure differential privacy to add noise to a column


---

[ACCORDION-BEGIN [Step 1: ](Create a new Calculation view)]

Create a new folder called **models** under `db/src`. Create a new **Calculation View** and call it `SALARIES_ANONYMIZED`

![New calculation view](1.png)

Click on the `anonymization` node and then click on the white canvas to drop it.

![New calculation view](2.png)

Use the ![plus sign](3.png) on the node to add a table as a data source. Choose `TGT_SALARIES`

![New calculation view](4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure differential privacy)]

Double click on the `Anonymize_1` node. This will open the mapping. Double click on `TGT_SALARIES` to add all of the columns to the output

![New calculation view](5.png)

Click **Details** and switch `k-anonymity` to **Differential Privacy**

![Configure privacy](6.png)

Configure the following parameters for anonymity

  - Sequence Column: ID
  - Epsilon: 0.1
  - Sensitivity: 15000
  - Noised Column: SALARY

For example:

![Configure privacy](7.png)

> For more information about these parameters check the [SAP HANA Security Guide](https://help.sap.com/viewer/b3ee5778bc2e4a089d3299b82ec762a7/latest/en-US/ace3f36bad754cc9bbfe2bf473fccf2f.html)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Configure semantics)]

Connect the `Anonymize_1` node to the **Aggregation** node.

![Configure privacy](1.gif)

Double-click on the name of the node to move all the fields into the output columns.

![Configure privacy](8.png)

Go into the **Semantics** node and switch `START_YEAR` to attribute

![Configure privacy](12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Data preview)]

**Save** and **Build** the calculation view

![Configure privacy](9.png)

Right-click on it and choose **Data Preview**

![Configure privacy](10.png)

Click **Raw Data** and you will see the anonymized data:

![Configure privacy](11.png)

> See the negative salaries? The added noise guarantees privacy while keeping sum and average results similar to the original dataset.

Use the **Analysis** tab and drag the `GENDER` and `REGION` fields to the label axis and `ID` to the Value axis.
Change the aggregation for `SALARY` to `COUNT` to answer the question below.

![Configure privacy](14.png)


[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure the HANA Analytics Adapter)]

If you have a tenant in SAP Analytics Cloud or a reporting tool such as Analysis for Office, you can configure the SAP HANA Analytics Adapter. The adapter is a Java application that exposes Calculation Views for consumption through Information Access (or `InA`).

Follow the steps in the following blog post if you want to set up the HANA Analytics Adapter: <https://blogs.sap.com/2019/04/24/connecting-the-sap-hana-service-on-cloud-foundry-to-sap-analytics-cloud-the-lazy-approach-pt1/>

[DONE]
[ACCORDION-END]

---
