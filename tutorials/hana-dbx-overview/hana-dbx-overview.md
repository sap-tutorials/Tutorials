---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud\,-sap-hana-database, software-product>sap-hana, software-product>sap-hana\,-express-edition]
primary_tag: software-product>sap-hana-cloud
---

# SAP HANA Database Explorer Overview
<!-- description --> Learn about the SAP HANA database explorer and how to start using it with the SAP HANA Cloud trial, free tier, or SAP HANA, express edition.

## Prerequisites
 - A machine that can run SAP HANA, express edition if the SAP HANA Cloud trial or free tier is not used

## You will learn
  - About the features provided by the SAP HANA database explorer
  - Details about the version differences between the SAP HANA database explorer in SAP HANA Cloud and in an on-premise installation (SAP HANA, express edition)
  - How to get started with the SAP HANA Cloud trial, free tier, or SAP HANA, express edition
---


## Intro
> Access help from the SAP community or provide feedback on this tutorial by navigating to the "Feedback" link located on the top right of this page.

### SAP HANA database explorer overview


The SAP HANA database explorer is a web-based tool for browsing and working with SAP HANA database objects such as tables, views, functions, stored procedures, importing and exporting data, debugging SQLScript, executing SQL statements, creating remote sources, working with multi-model data such as graph, spatial and JSON collections, viewing trace files, and troubleshooting.  

It was previously named SAP HANA Runtime Tools (HRTT) and was introduced in SAP HANA 1.0 SPS 12. As it is web-based, individual users do not need to worry about installing the software or applying patches.  The SAP HANA Cloud version offers support for data lake Relational Engine connections, and as of March 14 2022, support for the data lake Files container.

![database explorer](dbx.png)

> The following tools also provide the ability to execute SQL queries.

  * `hdbsql` is a text based tool that can be used to query an SAP HANA database.  For additional details see [Use Clients to Query an SAP HANA Database](mission.hana-cloud-clients).

  * `dbisql` is a text based tool that can be used to query a data lake. For additional details see [Use Clients to Query Data Lake Relational Engine](group.hana-cloud-clients-data-lake).

  * [SAP HANA Database Explorer for Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=SAPSE.hana-database-explorer&ssr=false#overview) is an extension for Visual Studio Code that offers a subset of the functionality in the SAP HANA database explorer.

The SAP HANA database explorer can be opened from multiple locations as shown below:

* From the SAP Business Technology Platform (SAP BTP) Cockpit from an SAP HANA or Data Lake instance.

    ![opening the SAP HANA database explorer from the SAP BTP Cockpit](open-dbx-from-hana-cloud.png)

* From SAP HANA Cloud Central on either a SAP HANA or Data Lake with Relational Engine enabled.

    ![opening the SAP HANA database explorer from SAP HANA Cloud Central](from-directory.png)

* From the SAP HANA cockpit  

    ![opening the SAP HANA database explorer from the SAP HANA Cockpit](open-dbx-from-hana-cockpit.png)

* From the Business Application Studio in a SAP HANA Native Application development space

    ![BAS SAP HANA Dev Space](BusinessAppStudio.png)

    The command palette or the SAP HANA Projects panel can be used to open the SAP HANA database explorer.

    ![BAS Open SAP HANA Database Explorer](BusinessAppStudioOpenDBX.png)

* From the SAP Web IDE for SAP HANA with the extension enabled

    ![opening the SAP HANA database explorer from the SAP Web IDE](open-dbx-from-web-ide.png)

    >Once the SAP HANA database explorer extension is enabled, the SAP HANA database explorer can be shown by selecting the icon highlighted above.

The following are some related documentation links for SAP HANA and the SAP HANA database explorer.

|  Version     | Notes |
|  :------------- | :------------- |
|  [SAP HANA Platform 2.0](https://help.sap.com/viewer/product/SAP_HANA_PLATFORM/latest/en-US) | Released in November 2016. Current version is 2.0 SPS 06, which was released in December, 2021 |
|  [SAP HANA Platform 2.0, SAP HANA Database Explorer](https://help.sap.com/viewer/e8d0ddfb84094942a9f90288cd6c05d3/latest/en-US) | Current version is 2.14, which was released in December 2021 |
|  [SAP HANA Platform 2.0, SAP HANA Database Explorer What's New](https://help.sap.com/docs/SAP_HANA_COCKPIT/a1199348948f4579b6bc3b7153999749/b30dd56165f3407e8fbce88aaf2c9b27.html) | What's new since the previous on-premise release.  New features are released approximately twice per year |
|  [SAP Web IDE for SAP HANA](https://help.sap.com/viewer/product/SAPWEBIDE/SAPWEBIDE4HANA/en-US) | SAP Help documentation set |
|  [SAP HANA, express edition](https://help.sap.com/viewer/product/SAP_HANA,_EXPRESS_EDITION/latest/en-US)   | Released in September 2016.  Current version is 2.0 SPS 06 |
|  [SAP HANA, express edition, release notes for SPS 06, revision 61](https://www.sap.com/documents/2022/05/aca852e1-2a7e-0010-bca6-c68f7e60039b.html)  | Note that the version of the Database Explorer (HRTT) for this release is  2.14 |
|  [SAP HANA Cloud](https://help.sap.com/viewer/product/HANA_CLOUD)   | Released in March 2020 with quarterly new releases |
|  [SAP HANA Cloud, SAP HANA Database Explorer](https://help.sap.com/viewer/a2cea64fa3ac4f90a52405d07600047b/cloud/en-US)   | New features are released as often as every two weeks. |
|  [SAP HANA Cloud, SAP HANA Database Explorer What's New](https://help.sap.com/whats-new/2495b34492334456a49084831c2bea4e?Category=SAP%20HANA%20Database%20Explorer&locale=en-US)   | What's New.  This link is also available from the Help menu in the database explorer. A filter can be applied to limit results to SAP HANA database explorer updates only. |


### Version information

The SAP HANA database explorer releases independently of SAP HANA.  New features appear first in SAP HANA Cloud and are available to the on-premise version if applicable, in a later release.  Features are also enabled based on the database being connected to.  For example, the ability to import and export from cloud storage providers is a feature of an SAP HANA Cloud database.

The screenshots below show the version of the SAP HANA database explorer in SAP HANA Cloud and on-premise.  

The image below shows the SAP HANA database explorer running in SAP HANA Cloud.  

![latest available version in SAP HANA Cloud](dbx-hana-cloud-version.png)

The image below shows the SAP HANA database explorer running in an on-premise installation.  

![latest available version for on-premise](dbx-on-premise-version.png)

For the on-premise edition, only the currently released version receives patches.  For example, now that version 2.14.X has been released, there will no longer be fixes made to  the 2.13.X release.  For additional details see [SAP Note 2433181 - SAP HANA 2.0 Cockpit Revision and Maintenance Strategy](https://launchpad.support.sap.com/#/notes/2433181).

The SAP Software download links (requires an S-User ID to access) below are for the on-premise version of the SAP HANA Cockpit, the SAP Web IDE, and the SAP HANA database explorer.  These pages also contain links to release notes that describe fixes made to each release.

[SAP HANA Cockpit Download from SAP Software Downloads](https://launchpad.support.sap.com/#/softwarecenter/search/sap%2520hana%2520cockpit) (Includes the SAP HANA database explorer)

[SAP HANA Web IDE Download from SAP Software Downloads](https://launchpad.support.sap.com/#/softwarecenter/search/SAP%2520WEB%2520IDE%25202)

[SAP HANA Runtime Tools 2.0](https://launchpad.support.sap.com/#/softwarecenter/search/XSACHRTT) (Adds the SAP HANA database explorer to the SAP HANA Web IDE)



### SAP HANA Cloud trial or free tier


>To complete the tutorials in this group, an SAP HANA instance is needed. Steps 3 and 5 in this tutorial provide two different, free options that can be used to set up an SAP HANA instance.  Only one of these steps needs to be completed if you currently do not have access to an SAP HANA instance. When creating an SAP HANA instance, a Data Lake is not needed.  Creating a Data Lake will be covered in a later tutorial.

Continue on to the next tutorial in this group once you have created an SAP HANA Cloud instance as shown below.

<!-- border -->![SAP HANA Cloud Trial instance](hana-cloud-instance.png)

>Note that the SAP HANA Cloud trial or free tier instances are shut down overnight and will need to be restarted before working with them the next day. Details on how to start or manage your trial or free tier instance can be viewed at [Provision an Instance of SAP HANA Cloud, SAP HANA Database](hana-cloud-mission-trial-2) or via the Cloud Foundry CLI in the next step.

The instructions on how to setup a free SAP HANA Cloud trial or free tier within SAP BTP are well covered in a number of other sources listed below.  

  * [Help Thomas Get Started with SAP HANA](hana-trial-advanced-analytics) (Only the first 3 steps of this tutorial are needed for basic setup of SAP HANA Cloud trial.)

  * [Set Up Your SAP HANA Cloud, SAP HANA Database (free tier or trial) and Understand the Basics](group.hana-cloud-get-started-1-trial)

  * [SAP Learning Journey - Provisioning and Administration with SAP HANA Cloud](https://learning.sap.com/learning-journey/provisioning-and-administration-with-sap-hana-cloud)

  * [SAP Discovery Center - SAP HANA Cloud, SAP HANA Database Fundamentals](https://discovery-center.cloud.sap/protected/index.html#/missiondetail/3643/)

  For more information on the SAP BTP see the following:

  * <https://www.sap.com/products/business-technology-platform/products.html>

  * <https://developers.sap.com/topics/business-technology-platform.html>

  * <https://help.sap.com/viewer/product/BTP/Cloud/en-US>


### Manage SAP HANA Cloud using the Cloud Foundry CLI (optional)

The Cloud Foundry Command Line Interface (cf CLI) provides users another option for managing their SAP HANA Cloud instances that are provisioned in Cloud Foundry spaces.

![environment is cloud foundry](ProvisionedInCloudFoundry.png)

The software can be downloaded from the [Cloud Foundry Foundation](https://github.com/cloudfoundry/cli/releases) and is available for Windows, Linux, and macOS operating systems. The following steps will demonstrate how to start and manage an SAP HANA Cloud instance using the CLI.

1. Complete the installation process by following the guide [Installing the cf CLI](https://docs.cloudfoundry.org/cf-cli/install-go-cli.html).

2. Copy the API endpoint from your SAP HANA Cloud account. This information can be found on the Overview page of your SAP BTP Cockpit. The API endpoint is listed under the Cloud Foundry subheading. Save the API endpoint for use in the next step.

    ![Find the API Endpoint from the BTP Cockpit](apiEndpoint.PNG)

3. In your terminal, type the following command. Replace the API endpoint with the one you copied from the previous step:

    ```Shell
    cf api https://api.cf.XXXX.hana.ondemand.com
    ```
    You will be asked to login with the email and password on your SAP HANA Cloud account. Finally you will be prompted to select your targeted space within your account. Additional details on logging in can be found at [Getting Started with the CF CLI](https://docs.cloudfoundry.org/cf-cli/getting-started.html#user-cups).

    <!-- border -->![Cloud CLI API](cf-api.png)

4. Next, view the list of services.

    ```Shell
    cf services
    ```

    <!-- border -->![cloud foundry services](cf-services.png)

5. The `hana` service can be started with a file named `start.json` with the below contents and the following command.

    ```JSON
    {
    	"data":
    	{
    		"serviceStopped":false
    	}
    }
    ```

    ```Shell
    cf update-service HC_HDB_Trial -c start.json
    ```
    <!-- border -->![starting a SAP HANA Cloud database](cf-start.png)

For more information on the operations available with the Cloud Foundry CLI, refer to the documentation [Using the Cloud Foundry CLI with SAP HANA Cloud](https://help.sap.com/viewer/9ae9104a46f74a6583ce5182e7fb20cb/hanacloud/en-US/921f3e46247947779d69b8c85c9b9985.html).  

>Note that SAP HANA Cloud Central provides an option in the actions menu to copy the JSON configuration of a running instance.  This can be used with the `cf create-service` command perhaps to create multiple instances of a database configured in the same way.

>![copy Configuration](hcc-copy-json.png)

>---

>Another tool that can be used to manage a SAP HANA Cloud instance is the SAP Automation Pilot.  It provides commands for working with an SAP HANA Cloud instances as well as Cloud Foundry applications.  The below is an example command used to stop an SAP HANA Cloud instance.  

>![stop SAP HANA Cloud command](automation-pilot.png)

>The Automation Pilot also provides a scheduler and integration with SAP Alert Notification Service.  For further details see [Take Action Following a SAP HANA Cloud Database Alert with SAP Automation Pilot](hana-cloud-alerts-autopilot).


### Manage SAP HANA Cloud using the BTP CLI (optional)

For SAP HANA Cloud instances provisioned created using the multi-environment tooling which provisions instances into the subaccount, the Business Technology Platform Command Line Interface (BTP CLI) can also be used for management.

![environment is cloud foundry](ProvisionedInSubAccount.png)

The BTP CLI provides the ability to manage an instance from the command line.  For information on installation, setup, and operations available with the BTP CLI, refer to the documentation [Account Administration Using the SAP BTP Command Line Interface (btp CLI)](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/7c6df2db6332419ea7a862191525377c.html).

Let's take a look at how we can start an instance using the BTP CLI.

1. Download the latest version of the BTP CLI from [SAP Development Tools](https://tools.hana.ondemand.com/#cloud-cpcli). Follow the steps to extract the client executable from the tar.gz archive depending on your operating system.:
    - Linux: Use the terminal to extract the tar.gz archive with `tar -vxzf <tar.gz name>`
    - macOS: Open the `tar.gz` file with a double click
    - Windows: Extract the tar.gz archive with `tar -vxzf <tar.gz name>`

    Then add it to your PATH, open a new terminal, and run `btp`.

2. Once installation is successful, login using the BTP CLI.  You can find your global account subdomain in the BTP Cockpit.

    ![Global Subdomain](global-subdomain.png)

    ```Shell
    btp login
    ```

    ![login](btp-login.png)

    >Make sure that the region (such as eu10) in the CLI server URL matches the region of your SAP BTP Cockpit URL.

    >---

    >If you have single sign-on configured, you can use the --sso option.


3. Set your target subaccount where the desired instance is located.

    ```Shell
    btp target --subaccount <subaccount id>
    ```

    ![Global Subdomain](sub-id.png)

4. You can start an instance by using the same `start.json` file used in the previous step on the Cloud Foundry CLI.  Be sure to provide the correct file path to the JSON file.

    ```Shell
    btp update services/instance --name <instance name> --parameters start.json
    ```

    ![Start Instance](update-service.png)


### SAP HANA, express edition


>This step only needs to be completed if you currently do not have access to an SAP HANA instance and did not setup an SAP HANA instance through the SAP HANA Cloud as explained in step 3.

An alternative option to using the SAP HANA Cloud trial or free tier is to use the SAP HANA, express edition.  SAP provides a free, streamlined version of SAP HANA that runs on developer laptops called [SAP HANA, express edition](https://www.sap.com/cmp/td/sap-hana-express-edition.html).

SAP HANA runs on certain versions of Linux.  SAP HANA, express edition provides a binary install as well as virtual machine images that can be run on Microsoft Windows, macOS, and Linux machines.  This is described in the [SAP HANA, express edition](https://help.sap.com/viewer/product/SAP_HANA,_EXPRESS_EDITION/latest/en-US?task=implement_task) documentation under the implement section.  A database-only option and a database + XS Advanced Applications option are available. The database + XS Advanced Applications install includes the SAP HANA cockpit, the SAP HANA database explorer, and the SAP HANA Web IDE for SAP HANA.

>Database + XS Advanced Applications, requires sufficient disk space and memory (32 GB).

Once installed, a useful starting point is the page below.  

![XSA is up and running](xsa-is-up.png)

It contains links to the SAP Web IDE for SAP HANA, SAP HANA cockpit, and the SAP HANA cockpit manager.

---

Congratulations!  You have configured an instance of SAP HANA, either through the SAP HANA Cloud trial, free tier, or SAP HANA, express edition. You've also learned how to start, stop, and manage an instance of SAP HANA Cloud via the Cloud Foundry Command Line Interface.

---
