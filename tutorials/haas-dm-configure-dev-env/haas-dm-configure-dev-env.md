---
title: Configure the Development Environment
description: Configure SAP Web IDE for Full Stack, which you will use to create a multi-target application based on the SAP Cloud Application Programming Model.
auto_validation: true
time: 15
tags: [tutorial>beginner, products>sap-hana, products>sap-cloud-platform\,-sap-hana-service, tutorial>license]
primary_tag: products>sap-hana
---

## Prerequisites
 - You have a paid account in SAP Cloud Platform
 - You have created an instance of the SAP Cloud Platform, SAP HANA Service


## Details
### You will learn
  - How to access SAP Web IDE for Full Stack
  - How to create a project and configure credentials to perform a deployment in the Cloud Foundry stack

SAP Web IDE for Full Stack is currently available in Neo. You will be entering the service from the Neo subaccount, creating a project and configuring credentials to perform a deployment in the Cloud Foundry environment.

>**This tutorial cannot currently be completed with a trial account.**

---

[ACCORDION-BEGIN [Step 1: ](Navigate to the Neo subaccount)]

You can start configuring your development environment while the creation of your instance takes place.

Navigate back to the **Subaccounts** page by clicking on the ![New project from template](1.png) icon.

![New project from template](2.png)

Click on the **Neo** tile.

![New project from template](3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access the SAP Web IDE)]

Navigate to **Services**, search for ***Full*** and click on **SAP Web IDE Full-Stack**.

![New project from template](4.png)

Click on **Go to Service**. A new tab will open to load the SAP Web IDE Full-Stack. If prompted, log in to the service using your username and password.

![New project from template](5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure the Cloud Foundry space)]

You will now configure the Cloud Foundry space that will be used by default in your projects.

Click on the **Preferences** icon ![New project from template](6.png) and then click on **Cloud Foundry**.

![New project from template](7.png)

Select the same API Endpoint that you identified in the Deploying SAP HANA as a Service tutorial.

>**Note**: Please ensure that the API Endpoint matches your account.

![New project from template](8.png)

Login with your username and password.

![New project from template](9.png)

Choose your **Space** and click on **Install Builder**. Wait until the builder is installed.

![New project from template](10X.png)

Once the builder is installed...

![New project from template](11.png)

... click **Save**.

![New project from template](12X.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enable key SAP Web IDE extensions)]

Click on **Extensions**, search for the following extensions and enable them:

* SAP HANA Database Development Tools
* SAP HANA Database Explorer

![New project from template](13.png)

Search for the **SAP EIM Smart Data Integration Editors** extension and enable it.

![New project from template](14.png)

Click **Save**. Then click **Refresh** on the Confirmation Needed popup.

![New project from template](15.png)

[VALIDATE_1]
[ACCORDION-END]

---
