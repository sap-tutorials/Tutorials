---
title: Connect to the SAP HANA Cockpit
description: Open your application from the dashboard and connect to the SAP HANA cockpit.
auto_validation: true
time: 10
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [tutorial>beginner, products>sap-hana, products>sap-cloud-platform\,-sap-hana-service, tutorial>license]
primary_tag: products>sap-hana
---

## Prerequisites
 - You have created an instance of the SAP HANA service for SAP BTP in SAP BTP, Cloud Foundry environment


## Details
### You will learn
  - This tutorial is designed for SAP HANA service for SAP Business Technology Platform. Consider newer tutorials designed for SAP HANA Cloud.
  - How to connect to the SAP HANA cockpit for performing administrative tasks in the physical database

The SAP HANA cockpit is useful because it provides graphical tools to monitor resources and the overall health of your database.

>**This tutorial cannot be completed with a trial account.**

---

[ACCORDION-BEGIN [Step 1: ](Ensure that the instance is created)]

Navigate back to the **SAP BTP cockpit** to check on the progress status of your instance.

Click on the subaccount.

![New project from template](1X.png)

Click on **Spaces** and click on the **dev** space.

![New project from template](2X.png)

Click on **Service Instances**.

![New project from template](3.png)

Ensure that you see ***Created*** under **Last Operation** for the HDB instance.

![New project from template](4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access the SAP HANA service for SAP BTP dashboard)]

Click on the **Open Dashboard** icon ![New project from template](5.png) under **Actions**.

![New project from template](6.png)

When prompted, click **Authorize** to authorize access for the cockpit.

![New project from template](7.png)

The SAP HANA service for SAP BTP dashboard will open. Notice that you can see the connectivity endpoints and the Cockpit. You are also able to see your database ID.

>**Note**: Keep this tab open as you will use it later.

![New project from template](sap hana service dashboard.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access the SAP HANA cockpit)]

Access the **SAP HANA cockpit** using the button located at the top right corner.

![New project from template](8.png)

Enter the following credentials and then click **OK** to log in to the database:

**Username**: `SYSTEM`

**Password**: `HanaRocks1`

> Adapt the password to match the one you chose during setup if you changed it.

![New project from template](9.png)

You can now explore the SAP HANA cockpit for your database instance.

![New project from template](10.png)

>**Note**: Keep this tab open as you will be using it later.

Open the **SQL Console**.

![HaaS SQL Console](11.png)

Execute the following statement to complete the validation below.

![HaaS SQL Console](12.png)

[VALIDATE_1]
[ACCORDION-END]

---
