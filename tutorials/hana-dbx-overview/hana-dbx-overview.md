---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud--sap-hana-database, software-product>sap-hana, software-product>sap-hana--express-edition]
primary_tag: software-product>sap-hana-cloud
---

# SAP HANA Database Explorer Overview
<!-- description --> Learn about the SAP HANA database explorer and how to start using it with SAP HANA Cloud free tier, trial, SAP HANA, express edition, or SAP HANA Cloud Guided Experience.

## Prerequisites
 - A machine that can run SAP HANA, express edition if the other options are not used

## You will learn
  - About the features provided by the SAP HANA database explorer
  - Details about the version differences between the SAP HANA database explorer in SAP HANA Cloud and in an on-premise installation such as SAP HANA, express edition
  - How to get started with SAP HANA Cloud free tier, trial, SAP HANA, express edition, or SAP HANA Cloud Guided Experience
---


## Intro
> Access help from the SAP community or provide feedback on this tutorial by navigating to the "Feedback" link located on the top right of this page.

### SAP HANA database explorer overview

The SAP HANA database explorer is a web-based tool for browsing and working with SAP HANA database objects such as tables, views, functions, stored procedures, importing and exporting data, executing SQL statements, creating remote sources, working with multi-model data such as graph, spatial and JSON collections, debugging SQLScript, viewing trace files, and troubleshooting.  

It was previously named SAP HANA Runtime Tools (HRTT) and was introduced in SAP HANA 1.0 SPS 12. As it is web-based, individual users do not need to worry about installing the software or applying patches.  The SAP HANA Cloud version offers support for data lake Relational Engine and data lake Files container connections.

![database explorer](dbx.png)

The following tools also provide the ability to execute SQL queries.

  * `hdbsql` is a text-based tool that can be used to query an SAP HANA database.  For additional details see [Use Clients to Query an SAP HANA Database](mission.hana-cloud-clients).

  * `dbisql` is a tool that can be used to query a data lake Relational Engine. For additional details see [Use Clients to Query Data Lake Relational Engine](group.hana-cloud-clients-data-lake).

  * [SAP HANA Database Explorer for Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=SAPSE.hana-database-explorer&ssr=false#overview) is an extension for Visual Studio Code that offers a subset of the functionality in the SAP HANA database explorer.

The SAP HANA database explorer can be opened from multiple locations as shown below:

* From SAP HANA Cloud Central on either a SAP HANA or SAP data lake instance found under the 'Actions' column. 

    ![opening the SAP HANA database explorer from SAP HANA Cloud Central](from-hcc.png)

* From the SAP HANA cockpit  

    ![opening the SAP HANA database explorer from the SAP HANA Cockpit](open-dbx-from-hana-cockpit.png)

* From the SAP Business Technology Platform (SAP BTP) Cockpit when the instances are deployed into a Cloud Foundry space.

    ![opening the SAP HANA database explorer from the SAP BTP Cockpit](open-dbx-from-hana-cloud.png)

* From the Business Application Studio in a SAP HANA Native Application development space

    The command palette or the SAP HANA Projects panel can be used to open the SAP HANA database explorer.

    ![BAS Open SAP HANA Database Explorer](BusinessAppStudioOpenDBX.png)

* From the SAP Web IDE for SAP HANA with the extension enabled

    ![opening the SAP HANA database explorer from the SAP Web IDE](open-dbx-from-web-ide.png)

    >Once the SAP HANA database explorer extension is enabled, the SAP HANA database explorer can be shown by selecting the icon highlighted above.

The following are some related documentation links for SAP HANA and the SAP HANA database explorer.

|  Version     | Notes |
|  :------------- | :------------- |
|  [SAP HANA Platform 2.0](https://help.sap.com/viewer/product/SAP_HANA_PLATFORM/latest/en-US) | Released in November 2016. Current version is 2.0 SPS 07, which was released on April 4, 2023 |
|  [SAP HANA Platform 2.0, SAP HANA Database Explorer](https://help.sap.com/docs/SAP_HANA_COCKPIT/f69e86dc57384ca7be4b8005a3f2d4ab/7fa981c8f1b44196b243faeb4afb5793.html) | Current version is 2.16, which was released in October 2023 |
|  [SAP HANA Platform 2.0, SAP HANA Database Explorer What's New](https://help.sap.com/docs/SAP_HANA_COCKPIT/a1199348948f4579b6bc3b7153999749/b30dd56165f3407e8fbce88aaf2c9b27.html) | What's new since the previous on-premise release.   |
|  [SAP Web IDE for SAP HANA](https://help.sap.com/viewer/product/SAPWEBIDE/SAPWEBIDE4HANA/en-US) | SAP Help documentation set |
|  [SAP HANA, express edition](https://help.sap.com/viewer/product/SAP_HANA,_EXPRESS_EDITION/latest/en-US)   | Released in September 2016.  Current version is 2.0 SPS 06 |
|  [SAP HANA, express edition, release notes for SPS 06, revision 61](https://www.sap.com/documents/2022/05/aca852e1-2a7e-0010-bca6-c68f7e60039b.html)  | The version of the Database Explorer (HRTT) for this release is  2.14 |
|  [SAP HANA Cloud](https://help.sap.com/viewer/product/HANA_CLOUD)   | Released in March 2020 with quarterly new releases |
|  [SAP HANA Cloud, SAP HANA Database Explorer](https://help.sap.com/docs/hana-cloud/sap-hana-database-explorer/getting-started-with-sap-hana-database-explorer)   | New features are released as often as every two weeks. |
|  [SAP HANA Cloud, SAP HANA Database Explorer What's New](https://help.sap.com/whats-new/2495b34492334456a49084831c2bea4e?Category=SAP%20HANA%20Database%20Explorer&locale=en-US)   | What's New.  This link is also available from the Help menu in the database explorer. A filter can be applied to limit results to SAP HANA database explorer updates only. |


### Version information

The SAP HANA database explorer releases independently of SAP HANA.  New features appear first in SAP HANA Cloud and are available to the on-premise version if applicable, in a later release.  Features are also enabled based on the database being connected to.  For example, the option to export data or catalog objects to the SAP HANA database's file system is not supported in SAP HANA Cloud where users do not have access to the file system unlike an on-premise install of SAP HANA.

The screenshots below show the version of the SAP HANA database explorer in SAP HANA Cloud and on-premise.  

The image below shows the SAP HANA database explorer running in the SAP BTP.  

![latest available version in SAP HANA Cloud](dbx-hana-cloud-version.png)

The image below shows the SAP HANA database explorer running in an on-premise installation.  

![latest available version for on-premise](dbx-on-premise-version.png)

For the on-premise edition, only the currently released version receives patches.  For example, now that version 2.15.X has been released, there will no longer be fixes made to  the 2.14.X release.  For additional details see [SAP Note 2433181 - SAP HANA 2.0 Cockpit Revision and Maintenance Strategy](https://launchpad.support.sap.com/#/notes/2433181).

The SAP Software download links (requires an S-User ID to access) below are for the on-premise version of the SAP HANA Cockpit, the SAP Web IDE, and the SAP HANA database explorer.  These pages also contain links to release notes that describe fixes made to each release.

[SAP HANA Cockpit Download from SAP Software Downloads](https://launchpad.support.sap.com/#/softwarecenter/search/sap%2520hana%2520cockpit) (Includes the SAP HANA database explorer)

[SAP HANA Web IDE Download from SAP Software Downloads](https://launchpad.support.sap.com/#/softwarecenter/search/SAP%2520WEB%2520IDE%25202)

[SAP HANA Runtime Tools 2.0](https://launchpad.support.sap.com/#/softwarecenter/search/XSACHRTT) (Adds the SAP HANA database explorer to the SAP HANA Web IDE)



### SAP HANA Cloud free tier or trial

To complete the tutorials in this group, an SAP HANA instance is needed. Steps 3 and 4 in this tutorial provide two  different, free options that can be used to set up an SAP HANA instance.  Only one of these steps needs to be completed if you currently do not have access to an SAP HANA instance.  Alternatively, step 7 provides a quick and easy way to try out SAP HANA Cloud although you will be given access to a user with fewer permissions.  Trial is only available on the US10 landscape and is in a separate SAP BTP trial account whereas free tier is available in multiple production SAP BTP accounts and provides a seamless transition from a free tier to a paid plan.

![SAP HANA Cloud Trial instance](hana-cloud-instance.png)

>SAP HANA Cloud free tier or trial instances are shut down overnight (i.e. 12:00 AM based on the location where your instance was provisioned) and will need to be restarted before working with them the next day. The tutorial group [Automating SAP HANA Cloud Tasks](https://developers.sap.com/group.sap-hana-cloud-automating.html) provides some examples of using tools such as the BTP CLI or the SAP Automation Pilot to help with repetitive tasks such as starting and stopping instances.

>---

>In QRC 3 of 2022, a new version of the SAP HANA Cloud tools (SAP HANA Cloud Central, SAP HANA cockpit, and SAP HANA database explorer) was released.  SAP BTP provides multiple runtime environments such as Kyma and Cloud Foundry. When a HANA Cloud or data lake instance is created, it can be provisioned at the BTP subaccount or in a Cloud Foundry space.  The runtime environment label in SAP HANA Cloud Central or in the SAP BTP Cockpit indicates where the instance has been provisioned with the value of Other Environments indicating that it was provisioned at the SAP BTP subaccount.  Further details can be found at [SAP HANA Cloud goes multi-environment](https://blogs.sap.com/2022/09/21/sap-hana-cloud-goes-multi-environment-part-1-feature-overview/).

The instructions on how to setup a free SAP HANA Cloud free tier or trial within SAP BTP are well covered in several other sources listed below.  

  * [Set Up Your SAP HANA Cloud, SAP HANA Database (free tier or trial) and Understand the Basics](group.hana-cloud-get-started-1-trial)

  * [SAP Learning Journey - Provisioning and Administering Databases in SAP HANA Cloud](https://learning.sap.com/learning-journey/provision-and-administer-databases-in-sap-hana-cloud)

  * [SAP Discovery Center - SAP HANA Cloud, SAP HANA Database Fundamentals](https://discovery-center.cloud.sap/protected/index.html#/missiondetail/3643/)

  * [Help Thomas Get Started with SAP HANA](hana-trial-advanced-analytics) (Only the first 3 steps of this tutorial are needed for basic setup of SAP HANA Cloud.)

  For more information on the SAP BTP see the following:

  * <https://www.sap.com/products/business-technology-platform/products.html>

  * <https://developers.sap.com/topics/business-technology-platform.html>

  * <https://help.sap.com/viewer/product/BTP/Cloud/en-US>

Continue on to the next tutorial in this group once you have access to an SAP HANA instance.

### SAP HANA, express edition

>This step only needs to be completed if you currently do not have access to an SAP HANA instance and did not setup an SAP HANA instance through the SAP HANA Cloud as explained in step 3.

An alternative option to using the SAP HANA Cloud free tier or trial is to use the SAP HANA, express edition.  SAP provides a free, streamlined version of SAP HANA that runs on developer laptops called [SAP HANA, express edition](https://www.sap.com/products/technology-platform/hana/express-trial.html).

SAP HANA runs on a few versions of Linux.  SAP HANA, express edition provides a binary install as well as virtual machine images that can be run on Microsoft Windows, macOS and Linux machines.  This is described in the [Getting Started with SAP HANA 2.0, express edition (Binary Installer Method)](https://help.sap.com/docs/SAP_HANA,_EXPRESS_EDITION/32c9e0c8afba4c87814e61d6a1141280) or [Getting Started with SAP HANA 2.0, express edition (Virtual Machine Method)](https://help.sap.com/docs/SAP_HANA,_EXPRESS_EDITION/8c3bbc4a904d42efac77c09da0bccf64).  The **Applications** option adds XS Advanced, the SAP HANA cockpit, the SAP HANA database explorer, and the SAP HANA Web IDE for SAP HANA.

![SAP HANA express download manager](express-download-manager.png)

>The full install requires sufficient disk space and memory (32 GB).

Once installed, a useful starting point is the page below.  

![XSA is up and running](xsa-is-up.png)

It contains links to the SAP Web IDE for SAP HANA, SAP HANA cockpit, and the SAP HANA cockpit manager.

### SAP HANA Cloud Guided Experience
The SAP HANA Cloud Guided Experience provides a database user and password that has access to a specific schema free for 30 days.  The database user can be used with the SAP HANA cockpit and SAP HANA database explorer.  The provided database user can be used to create database objects within the provided schema but cannot create new schemas or users.  To register, open the [SAP HANA Cloud product page](https://www.sap.com/products/technology-platform/hana.html) and select Experience SAP HANA Cloud.

![experience SAP HANA Cloud](experience.png)

A set of guided tours is also available by clicking on the SAP logo in the bottom right.

![Guided Experience in the SAP HANA database explorer](experience-dbx.png)

### Knowledge check

Congratulations!  You have configured an instance of SAP HANA, either through the SAP HANA Cloud free tier, trial, or SAP HANA, express edition. You've also learned how to start, stop, and manage an instance of SAP HANA Cloud via the Cloud Foundry Command Line Interface.

---
