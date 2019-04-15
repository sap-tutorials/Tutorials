---
title: Explore data in SAP Vora in SAP Data Hub, trial edition 2.4
description: Explore data in SAP Vora (including profiling) by using SAP Data Hub, trial edition 2.4.
auto_validation: true
primary_tag: products>SAP-data-hub
tags: [  tutorial>beginner, topic>big-data, products>SAP-data-hub, products>SAP-VORA ]
---

## Details
### You will learn  
During this tutorial, you will learn that Metadata Explorer cannot only be used on files (for example stored in AWS S3 or Google Cloud Storage). Metadata Explorer also works on other data stores, in particular SAP Vora.
Please note here in this tutorial GCP refers to Google Cloud platform and AWS refers to Amazon Web Services.

### Time to Complete
**30 Min**

---

[ACCORDION-BEGIN [Step 1: ](Load data into SAP Vora)]
To be able to profile data in SAP Vora, you first need to load data into SAP Vora. Thereto open the SAP Data Hub App Launchpad via a web browser. To access the SAP Data Hub App Launchpad in AWS or GCP you need go to the chapters 3.3 and 3.4 as described in the [**Getting Started with SAP Data Hub, trial edition**] (https://caldocs.hana.ondemand.com/caldocs/help/Getting_Started_Data_Hub24.pdf) guide. Once you have opened the Launchpad click on the **Vora Tools**

Enter **DEFAULT** as the **Tenant**, `DATAHUB` as **Username** and the password which you have selected during system setup as **Password** to logon to the Launchpad. The system displays the **Application Launchpad** page.

![picture_01](./datahub-trial-v2-discovery-part02_01.png)  

Navigate to the **SAP Vora Tools** by clicking on the icon from the launchpad. The system then displays the **SAP Vora Tools**.

![picture_02](./datahub-trial-v2-discovery-part02_02.png)  

Create a new On Disk Relational Table by clicking **Create New** button. If you don't see the welcome screen like the one in the above screenshot, click on the **+** button (highlighted in the screenshot, in upper left corner) and navigate to **Create Relational Table on Disk**.

![picture_03](./datahub-trial-v2-discovery-part02_03.png)  

Enter the following information to create the relational table if you are using AWS S3 and then click **Next** :

| Field &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;                          | Value                                                                                       |
| :------------------------------ | :------------------------------------------------------------------------------------------- |
| `Name`                         | `CUSTOMERS`                                                                                       |
| `Schema`                       | `default`                                                                                         |
| `Engine`                       | `Relational Disk`                                                                                 |
| `Table Type`                   | `Data Source`                                                                                     |
| `File System`                  | `S3 or GCS`                                                                                       |
| `Connection Type`              | `Manual`                                                                                          |
| `S3 Host`                      | `Should be empty`                                                                                 |
| `S3 Access Key Id`             | `from your AWS management console go to My security credentials and copy it from there`           |
| `S3 Secret Access Key`         | `from your AWS management console go to My security credentials and copy it from there`           |
| `S3 Region`                    | `Open the Connection Management. Click on the "Action" button of the Connection Id CLOUD_STORAGE and the on "Edit". Copy the value of "Region".`          |
| `S3 Bucket`                    | `Open the Connection Management. Click on the "Action" button of the Connection Id CLOUD_STORAGE and the on "Edit". Copy the value of "ROOT PATH".`               |
| `File Path`                    | `file path via **Browse** button, in our case /Customers.csv`                                     |



Enter the following information to create the relational table if you are using GCS and then click **Next** :

| Field &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;                          | Value                                                                                       |
| :------------------------------ | :------------------------------------------------------------------------------------------- |
| `Name`                         | `CUSTOMERS`                                                                                       |
| `Schema`                       | `default`                                                                                         |
| `Engine`                       | `Relational Disk`                                                                                 |
| `Table Type`                   | `Data Source`                                                                                     |
| `File System`                  | `GCS`                                                                                             |
| `Connection Type`              | `Connection Manager`                                                                              |
| `Connection ID`                | `CLOUD_STORAGE`                                                                                   |
| `File Path`                    | `file path via **Browse** button, in our case /Customers.csv`                                     |

Finally click **Finish (2)** to create the table.

![picture_04](./datahub-trial-v2-discovery-part02_04.png)


After the table is created, click on the **Data Preview** button to display the preview of the loaded table data.

![picture_05](./datahub-trial-v2-discovery-part02_05.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Profile table)]
Go back to the SAP Data Hub Application Launchpad and navigate to **Metadata Explorer**.

![picture_06](./datahub-trial-v2-discovery-part02_06.png)

From the Metadata Explorer, click on **Browse Connections**

![picture_07](./datahub-trial-v2-discovery-part02_07.png)  

From the connections page, click on **VORA** and then click on **default** to display the tables in SAP Vora in schema `default`.
Based on our example, we select the table `CUSTOMERS`, which you have created during the previous step.

![picture_08](./datahub-trial-v2-discovery-part02_08.png)

Click on the **More Actions(1)** button for `CUSTOMERS` and from the context menu, click on **Start Profiling(2)**. The system indicates that profiling has started. It can take several minutes for profiling to complete.

As soon as the profiling completes, you will see a notification in the top right corner and you will see more details after you click on the icon.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Display metadata and fact sheet)]
You can take a look at metadata and fact sheet of the `CUSTOMERS` table just like you did during the previous tutorial for the files.

Open the fact sheet for `CUSTOMERS` that you have profiled just now and answer the following questions with the help of the below screenshot :

![picture_09](./datahub-trial-v2-discovery-part02_09.png)  

[VALIDATE_1]

[ACCORDION-END]

---
