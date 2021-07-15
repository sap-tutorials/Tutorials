---
title: SAP HANA Database Explorer Overview
description: Learn about the SAP HANA database explorer and how to start using it with the SAP HANA Cloud trial or SAP HANA, express edition.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
 - A machine that can run SAP HANA, express edition if the SAP HANA Cloud trial is not used

## Details
### You will learn
  - About the features provided by the SAP HANA database explorer
  - Details about the version differences between the SAP HANA database explorer in SAP HANA Cloud and in an on-premise installation (SAP HANA, express edition)
  - How to get started with the SAP HANA Cloud trial or SAP HANA, express edition

---

[ACCORDION-BEGIN [Step 1: ](Database explorer overview)]

The SAP HANA database explorer is a web-based tool for browsing and working with SAP HANA database objects such as tables, views, functions, stored procedures, importing and exporting data, debugging SQLScript, viewing trace files, and executing SQL statements. It was previously named SAP HANA Runtime Tools (HRTT) and was introduced in SAP HANA 1.0 SPS 12. As it is web-based, individual users do not need to worry about installing the software or applying patches.  As of March 25, 2021 it also offers support for Data Lake IQ connections.

![database explorer](dbx.png)

> The SAP HANA Client install provides a text based tool named `hdbsql` that can used to query an SAP HANA database.  For additional details see [Use Clients to Query an SAP HANA Database](mission.hana-cloud-clients).

The SAP HANA database explorer can be opened from multiple locations as shown below:

* From the SAP Business Technology Platform (SAP BTP) Cockpit from an SAP HANA or Data Lake instance.

    ![opening the SAP HANA database explorer from the SAP BTP Cockpit](open-dbx-from-hana-cloud.png)

* From SAP HANA Cloud Central on either a SAP HANA or Data Lake with IQ enabled.

    ![opening the SAP HANA database explorer from SAP HANA Cloud Central](from-directory.png)

* From the SAP HANA cockpit  

    ![opening the SAP HANA database explorer from the SAP HANA Cockpit](open-dbx-from-hana-cockpit.png)

* From the Business Application Studio

    ![BAS SAP HANA Dev Space](BusinessAppStudio.png)

    From the command palette or the SAP HANA Projects panel.

    ![BAS Open SAP HANA Database Explorer](BusinessAppStudioOpenDBX.png)

* From the SAP Web IDE for SAP HANA with the extension enabled

    ![opening the SAP HANA database explorer from the SAP Web IDE](open-dbx-from-web-ide.png)

    >Once the SAP HANA database explorer extension is enabled, the SAP HANA database explorer can be shown by selecting the icon highlighted above.

The following are some related documentation links for SAP HANA and the SAP HANA database explorer.

|  Version     | Notes |
|  :------------- | :------------- |
|  [SAP HANA Platform 2.0](https://help.sap.com/viewer/product/SAP_HANA_PLATFORM/latest/en-US) | Released in November 2016. Current version is 2.0 SPS 05, which was released on June 26, 2020 |
|  [SAP HANA Platform 2.0, SAP HANA Database Explorer](https://help.sap.com/viewer/e8d0ddfb84094942a9f90288cd6c05d3/latest/en-US) | Current version is 2.13, which was released in June 2021 |
|  [SAP HANA Platform 2.0, SAP HANA Database Explorer What's New](https://help.sap.com/viewer/e8d0ddfb84094942a9f90288cd6c05d3/latest/en-US/7534f22d5bbf44c3a0de259478ad043b.html) | What's new since the previous on-premise release.  New features are released approximately twice per year |
|  [SAP Web IDE for SAP HANA](https://help.sap.com/viewer/product/SAPWEBIDE/SAPWEBIDE4HANA/en-US) | SAP Help documentation set |
|  [SAP HANA, express edition](https://help.sap.com/viewer/product/SAP_HANA,_EXPRESS_EDITION/latest/en-US)   | Released in September 2016.  Current version is 2.0 SPS 05 Revision 54 |
|  [SAP HANA, express edition, release notes for SPS 05, revision 54](https://www.sap.com/documents/2021/06/3abec5ed-e97d-0010-bca6-c68f7e60039b.html)  | Note that the version of the Database Explorer (HRTT) for this release is  2.12 |
|  [SAP HANA, express edition, release history](https://search.sap.com/search.html?t=%22SAP%20HANA%2C%20express%20edition%202.0%20SPS%200%25%20Revision%22&n=1&s=boost&src=defaultSourceGroup)   | Query showing previous releases |
|  [SAP HANA Cloud](https://help.sap.com/viewer/product/HANA_CLOUD)   | Released in March 2020 |
|  [SAP HANA Cloud, SAP HANA Database Explorer](https://help.sap.com/viewer/a2cea64fa3ac4f90a52405d07600047b/cloud/en-US)   | Current version is 210401 |
|  [SAP HANA Cloud, SAP HANA Database Explorer What's New](https://help.sap.com/viewer/a2cea64fa3ac4f90a52405d07600047b/cloud/en-US/7a0c586f5a4e442d976459e0f0a70c7a.html)   | What's new.  New features are released as often as every two weeks.  This link is also available from the Help menu in the database explorer  |

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Version information)]
The SAP HANA database explorer releases independently of SAP HANA.  New features appear first in SAP HANA Cloud and are available to the on-premise version if applicable, in a later release.  Features are also enabled based on the database being connected to.  For example, the ability to import and export from cloud storage providers is a feature of an SAP HANA Cloud database.

The screenshots below show the version of the SAP HANA database explorer in SAP HANA Cloud and on-premise.  

The image below shows the SAP HANA database explorer running in SAP HANA Cloud.  

![latest available version in SAP HANA Cloud](dbx-hana-cloud-version.png)

The image below shows the SAP HANA database explorer running in an on-premise installation.  

![latest available version for on-premise](dbx-on-premise-version.png)

For the on-premise edition, only the currently released version receives patches.  For example, now that version 2.13.X has been released, there will no longer be fixes made to  the 2.12.X release.  For additional details see [SAP Note 2433181 - SAP HANA 2.0 Cockpit Revision and Maintenance Strategy](https://launchpad.support.sap.com/#/notes/2433181).

The SAP Software download links (requires an S-User ID to access) below are for the on-premise version of the SAP HANA Cockpit, the SAP Web IDE, and the SAP HANA database explorer.  These pages also contain links to release notes that describe fixes made to each release.

[SAP HANA Cockpit Download from SAP Software Downloads](https://launchpad.support.sap.com/#/softwarecenter/search/sap%2520hana%2520cockpit) (Includes the SAP HANA database explorer)

[SAP HANA Web IDE Download from SAP Software Downloads](https://launchpad.support.sap.com/#/softwarecenter/search/SAP%2520WEB%2520IDE%25202)

[SAP HANA Runtime Tools 2.0](https://launchpad.support.sap.com/#/softwarecenter/search/XSACHRTT) (Adds the SAP HANA database explorer to the SAP HANA Web IDE)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](SAP HANA Cloud trial)]

>To complete the tutorials in this group, an SAP HANA instance is needed. Step 3 and 4 in this tutorial provide two different, free options that can be used to set up an SAP HANA instance.  Only one of these steps needs to be completed if you currently do not have access to an SAP HANA instance.

Continue on to the next tutorial in this group once you have created an SAP HANA Cloud trial instance as shown below.

!![SAP HANA Cloud Trial instance](hana-cloud-instance.png)

>Note that the SAP HANA Cloud trial instances are shut down overnight and will need to be restarted before working with them the next day.

The instructions on how to setup a free SAP HANA Cloud trial within SAP BTP are well covered in a number of other sources listed below.  

  * [Help Thomas Get Started with SAP HANA](hana-trial-advanced-analytics) (Only the first 3 steps of this tutorial are needed for basic setup of SAP HANA Cloud trial.)

  * [Getting Started with your trial of SAP HANA Cloud](https://saphanajourney.com/hana-cloud/learning-track/getting-started-with-your-trial-of-sap-hana-cloud/)

  For more information on the SAP BTP see the following:

  * <https://www.sap.com/products/business-technology-platform/products.html>

  * <https://developers.sap.com/topics/business-technology-platform.html>

  * <https://help.sap.com/viewer/product/BTP/Cloud/en-US>


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](SAP HANA, express edition)]

>This step only needs to be completed if you currently do not have access to an SAP HANA Instance and did not setup an SAP HANA instance through the SAP HANA Cloud Trial as explained in the previous step.

An alternative option to using the SAP HANA Cloud trial is to use the SAP HANA, express edition.     

SAP provides a free, streamlined version of SAP HANA that runs on developer laptops called [SAP HANA, express edition](https://www.sap.com/cmp/td/sap-hana-express-edition.html).

SAP HANA runs on certain versions of Linux.  SAP HANA, express edition provides a binary install as well as virtual machine images that can be run on Microsoft Windows, macOS, and Linux machines.  This is described in the [SAP HANA, express edition](https://help.sap.com/viewer/product/SAP_HANA,_EXPRESS_EDITION/latest/en-US?task=implement_task) documentation under the implement section.  A database-only option and a database + XS Advanced Applications option are available. The database + XS Advanced Applications install includes the SAP HANA cockpit, the SAP HANA database explorer, and the SAP HANA Web IDE for SAP HANA.

>Database + XS Advanced Applications, requires sufficient disk space and memory (32 GB).

Once installed, a useful starting point is the page below.  

![XSA is up and running](xsa-is-up.png)

It contains links to the SAP Web IDE for SAP HANA, SAP HANA cockpit, and the SAP HANA cockpit manager.

[VALIDATE_1]
[ACCORDION-END]

---

Congratulations!  You have configured an instance of SAP HANA, either through the SAP HANA Cloud trial or SAP HANA, express edition.  




---
