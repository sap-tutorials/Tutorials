---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud--sap-hana-database, software-product>sap-hana, software-product>sap-hana--express-edition]
primary_tag: software-product>sap-hana-cloud
---

# Choose an SAP HANA Database
<!-- description --> Learn about SAP HANA Cloud and SAP HANA, express edition and choose one that will be used with the SAP HANA client interfaces in subsequent tutorials.

## Prerequisites
 - A Microsoft Windows, Linux, or Mac computer
 - A machine that can run SAP HANA, express edition if the SAP HANA Cloud trial or free tier is not used

## You will learn
  - How to create an instance of SAP HANA Cloud or SAP HANA, express edition
  - How to connect to a SAP HANA Cloud or an SAP HANA, express edition database

## Intro
This tutorial will provide tips and pointers on setting up an instance of [SAP HANA](https://www.sap.com/products/hana.html) running in the cloud or on-premise so that it can then be connected to using a few of the [SAP HANA Client](https://help.sap.com/docs/SAP_HANA_CLIENT) interfaces.  

For more information on SAP HANA, consult [What Is SAP HANA](https://help.sap.com/docs/SAP_HANA_PLATFORM/eb3777d5495d46c5b2fa773206bbfb46/d3b1adcdbb571014a93eff11ad9a1d89.html).

> Access help from the SAP community or provide feedback on this tutorial by navigating to the **Feedback** link located on the top right of this page.

>---

>For connections from additional sources such as SAP Analytics Cloud, `Jupyter` Notebooks, SAP Business Warehouse, SAP Data Intelligence Cloud, and SAP ERP Central Component see [Connection Guides for SAP HANA Cloud](group.hana-cloud-database-connection-guides).

>---

>For connections to the SAP HANA Cloud, Data Lake, see the tutorial [Use Clients to Query Data Lake Relational Engine](group.hana-cloud-clients-data-lake).

>---

>**IMPORTANT**: Complete the first 3 tutorials, and then you can select any of the following tutorials about connecting with different client interfaces.


---

### Overview of SAP HANA Cloud and SAP HANA, express edition

There are multiple versions of SAP HANA.  The information below is a list of links for the documentation of each version.

|  Version     | Notes
|  :------------- | :-------------
|  [SAP HANA Platform 2.0](https://help.sap.com/docs/SAP_HANA_PLATFORM)           | Released in November 2016. Current version is 2.0 SPS 07, which was released on April 4rd, 2023.
|  **[SAP HANA, express edition](https://help.sap.com/docs/SAP_HANA_EXPRESS_EDITION)**    | Released in September 2016.  Current version is 2.0 SPS 06.
|  **[SAP HANA Cloud](https://help.sap.com/docs/hana-cloud)**   | Released in March 2020.  New features are released quarterly.


### SAP HANA Cloud

Here are a few benefits of using SAP HANA Cloud:

  * Software updates are automatically applied by SAP.

  * Hardware is managed by a cloud provider (e.g. AWS, Azure, or GCP).

  * Many data center locations to choose from [Data Center Locations](https://www.sap.com/canada/about/trust-center/data-center.html?mode=solution&currentLevel=world&solutionId=NZA62)

  * [Backups](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-administration-guide/backup-and-recovery) are automatic and recovery can be initiated in SAP HANA Cloud Central.    

  * The memory, compute and storage settings can be changed as your needs change.  Note a few operations can be performed using [service requests](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-administration-guide/service-requests).

  * The ability is provided to expand data storage from in-memory, to native storage extensions, to a data lake, while providing a common access layer that enables you to have further control over performance and cost.  See also [Lower Your Data Management Costs With SAP HANA Cloud](https://blogs.sap.com/2019/10/29/lower-your-data-management-costs-with-sap-hana-cloud/).


  Here are a few differences between SAP HANA Cloud and an on-premise version:  

  * Every SAP HANA Cloud instance is one SAP HANA database.  An on-premise version can have multiple databases per install, which is known as multi-tenant.  

  * Connections to an SAP HANA Cloud instance must be secure and require a minimum SAP HANA client version of 2.4.167.

  * The administration user for SAP HANA Cloud is named DBADMIN while for an SAP HANA 2.0 database it is SYSTEM.  For additional details see [Predefined Users](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-security-guide/predefined-users), [SAP HANA Cloud Administrator DBADMIN](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-administration-guide/user-management-with-sap-hana-database-administrator-dbadmin), and [Predefined Users in HANA 2.0](https://help.sap.com/docs/SAP_HANA_PLATFORM/b3ee5778bc2e4a089d3299b82ec762a7/de4ee8bbbb5710148a04f023da147c8d.html).  

  Information on the instance size steps for SAP HANA Cloud, SAP HANA databases can be found at [Create an SAP HANA Database Instance Using SAP HANA Cloud Central](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-administration-guide/create-sap-hana-database-instance-using-sap-hana-cloud-central).  Service plan, pricing and data center availability can be found at [SAP HANA Cloud Service (SAP Discovery Center)](https://discovery-center.cloud.sap/serviceCatalog/sap-hana-cloud?region=all&tab=service_plan).  Details on limitations can be found at [System Limitations](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/system-limitations).  Compatibility information can be found at [Compatibility with Other SAP HANA Versions](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-migration-guide/compatibility-with-other-sap-hana-versions).


### Connect to SAP HANA Cloud


1. >To complete the tutorials in the mission, an SAP HANA instance is needed. Step 3 and 5 in this tutorial provide two different, free options that can be used to set up an SAP HANA instance.  Only one of these steps needs to be completed if you currently do not have access to an SAP HANA instance.

    Continue with this tutorial once you have created an SAP HANA Cloud trial or free tier instance as shown below.

    ![SAP HANA Cloud Trial instance](hana-cloud-instance.png)

    The instructions on how to setup a free SAP HANA Cloud trial or free tier within the SAP Business Technology Platform (SAP BTP), are well covered in a number of other sources listed below.  Trial is only available on the US10 landscape and is in a separate SAP BTP trial account whereas free tier is available in multiple production SAP BTP accounts and provides a seamless transition from a free tier to a paid plan.

    * [Set Up Your SAP HANA Cloud, SAP HANA Database (free tier or trial) and Understand the Basics](group.hana-cloud-get-started-1-trial)

    * [SAP Learning Journey - Provisioning and Administering Databases in SAP HANA Cloud](https://learning.sap.com/learning-journey/provision-and-administer-databases-in-sap-hana-cloud)
    
    * [SAP Discovery Center - SAP HANA Cloud, SAP HANA Database Fundamentals](https://discovery-center.cloud.sap/protected/index.html#/missiondetail/3643/)

    * [Help Thomas Get Started with SAP HANA](hana-trial-advanced-analytics)


    For more information on SAP BTP see the following product pages and help documentation:

    * [https://www.sap.com/products/business-technology-platform.html](https://www.sap.com/products/business-technology-platform.html)

    * [https://developers.sap.com/topics/business-technology-platform.html](https://developers.sap.com/topics/business-technology-platform.html)  

    * [https://help.sap.com/docs/btp](https://help.sap.com/docs/btp)

    Once the SAP HANA Cloud instance is created, take note of the SQL endpoint (host:port) needed to connect to the database.  The endpoint can be obtained via the copy menu item.  This will be needed in subsequent tutorials in the mission.  

    ![SQL Endpoint](SQLEndpoint.png)

    >The SAP HANA Cloud, HANA database free tier or trial instances are shut down on a nightly basis and will need to be restarted before working with them the next day.  

2. Open a SQL console for your database instance from SAP HANA Cloud Central.

    ![Open SQL console](sql-cons.png)

    If needed, you will be asked to enter the credentials for the database.  Enter **DBADMIN** and the password that was specified when the instance was created.

    The SAP HANA database explorer provides the ability to browse catalog objects and execute SQL statements from the SQL console.  For more information, see the tutorial group [Get Started with the SAP HANA Database Explorer](group.hana-cloud-get-started), [SQL Console](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-administration-guide/sql-console) and [SAP HANA Database Explorer](https://help.sap.com/docs/hana-cloud/sap-hana-database-explorer/getting-started-with-sap-hana-database-explorer).  


3. Run the following query to see the name of the database you are currently connected to.  

    ```SQL
    SELECT * FROM M_DATABASE;
    ```

    ![Database Explorer Result](m-database.png)  

4. A schema is a container for other database objects such as tables and views and the current schema can be seen via SQL as shown below.  The DUMMY table is available in every HANA database that has one column and one row.  It provides a convenient  way to call a function or perform a simple test.

    ```SQL
    SELECT CURRENT_USER, CURRENT_SCHEMA FROM DUMMY;
    ```

    ![SQL console results](select-user-schema.png)

5. To see the list of services, enter the following SQL statement:

    ```SQL
    SELECT * FROM M_SERVICES;
    ```

6. To see information about the database server, enter the following SQL statement:

    ```SQL
    SELECT * FROM M_HOST_INFORMATION;
    ```

>Views starting with `M_` are examples of [monitoring views](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/monitoring-views) and contain statistics and status details.

Congratulations! You have connected to SAP HANA Cloud and performed a few queries.


### SAP HANA, express edition
>This step only needs to be completed if you currently do not have access to an SAP HANA Instance and did not setup an SAP HANA instance through the SAP HANA Cloud Trial or free tier as explained in step 3.

SAP provides a free streamlined version of SAP HANA that runs on developer laptops called [SAP HANA, express edition](https://www.sap.com/products/technology-platform/hana/express-trial.html).  

SAP HANA runs on a few versions of Linux.  SAP HANA, express edition provides a binary install as well as virtual machine images that can be run on Microsoft Windows, macOS and Linux machines.  This is described in the [Getting Started with SAP HANA 2.0, express edition (Binary Installer Method)](https://help.sap.com/docs/SAP_HANA,_EXPRESS_EDITION/32c9e0c8afba4c87814e61d6a1141280) or [Getting Started with SAP HANA 2.0, express edition (Virtual Machine Method)](https://help.sap.com/docs/SAP_HANA,_EXPRESS_EDITION/8c3bbc4a904d42efac77c09da0bccf64).  A database-only option and a database + XS Advanced Applications option are available. The database + XS Advanced Applications install includes the SAP HANA cockpit, the SAP HANA database explorer, and the SAP HANA Web IDE for SAP HANA.

![SAP HANA express download manager](express-download-manager.png)

Further details can be found in the [release notes](https://www.sap.com/documents/2022/05/aca852e1-2a7e-0010-bca6-c68f7e60039b.html).   

Choose an option and proceed to install SAP HANA, express edition.  The server-only install, which requires less memory, is sufficient to complete the tutorials in the mission.  Be sure to note the SAP HANA database master password, as it will be needed later for connecting.

At this point, you should have a running instance of SAP HANA, express edition.


### Connect to SAP HANA, express edition
>This step only needs to be completed if you currently do not have access to an SAP HANA Instance and did not setup an SAP HANA instance through the SAP HANA Cloud Trial or free tier as explained in step 3.

A default installation will contain one [system](https://help.sap.com/docs/SAP_HANA_PLATFORM/6b94445c94ae495c83a19646e7c3fd56/39da3d057f56427ab1bb7f738ca9e7ce.html) database named **SYSTEMDB** and one [tenant](https://help.sap.com/docs/SAP_HANA_PLATFORM/6b94445c94ae495c83a19646e7c3fd56/623afd167e6b48bf956ebb7f2142f058.html) database named **HXE**.

The system database contains information about the tenant database(s) and is used for overall administration.  The default port for the system database is **39013**.  Later in this tutorial mission, a connection to the tenant database will be made and tables will be created and populated with data.  For more information, see [SAP HANA Tenant Databases Operations Guide](https://help.sap.com/docs/SAP_HANA_PLATFORM/78209c1d3a9b41cd8624338e42a12bf6/0ba9f43aed594a449d497fabf6bc381e.html).

The SAP HANA, express edition VM install includes a command line tool (part of the SAP HANA client install) called [HDBSQL](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/c22c67c3bb571014afebeb4a76c3d95d.html) that can be used to query the database.  


The following steps will demonstrate connecting to and examining a SAP HANA, express edition database.  

1.  Enter the following to connect to the system database:

    ```Shell
    hdbsql -n localhost:39013 -u SYSTEM -p Your_Password
    ```

    ![hdbsql connected](hdbsql-connect.png)

    Notice that the database being connected to is SYSTEMDB.

    The example above uses localhost since hdbsql is running on the same machine that the database is trying to connect to.  The host name, `hxehost`, is seen in the terminal above.  

    >If hdbsql is not found and you are logged in with another user, try connecting as the user `hxeadm` and run the command again.
    ```Shell
    su hxeadm
    ```
    If the hdbsql command cannot be found, simply read through the following examples.  The installation and further examples of running HDBSQL will be covered in subsequent tutorials.

2.  Determine the IP address of the machine that is running SAP HANA, express edition.  Record the value as it will be needed in later tutorials in the mission.  It is important to note the IP address or host name of the machine, as this will be needed in later tutorials.

    ```Shell
    ip addr
    ```

3.  The following are a some examples of [interactive options](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/c24d054bbb571014b253ac5d6943b5bd.html) followed by a SQL query, which when run against **SYSTEMDB** returns information about the databases running on the SAP HANA instance.

    ```SQL
    \al on
    \pa off
    \s
    SELECT * FROM SYS_DATABASES.M_SERVICES WHERE SQL_PORT != 0;
    \serverstats
    ```

    ![hdbsql commands](hdbsql-commands.png)

    The `\al` or align interactive option when enabled increases the readability of the output.  

    The `\s` command shows status information.  It shows that that the SAP System Identification (SID) is **HXE**, that the connected database is **SYSTEMDB**, and the currently logged in user is **SYSTEM**.

    The result of the SELECT against the `M_SERVICES` table shows that there are two databases, named **SYSTEMDB** and **HXE** and that they are accessible on ports **39013** and **39015**.  

    The '\serverstats' retrieves resource consumption information about the last executed SQL statement which can be helpful when diagnosing SQL statements.

    The instance number can be derived from a port number using the second and third numbers in the port number 39013.  In this case, the instance number is 90.  

    If the SQL statement returns more than one screen of text, entering a space will show the next screen of results.  See also the pager option `\pa`.  

4.  Enter one of the below commands to exit from viewing the results of the select statement.  Note that commands can be prefixed with a forward slash(\\) or a colon(:).

    ```HDBSQL
    \q
    :q
    ```

5.  Connect using the instance number and database name as shown below.

    ```Shell
    hdbsql -n localhost -i 90 -d HXE -u SYSTEM -p Your_Password
    ```

6.  Enter the following to display database connection information.

    ```HDBSQL
    \s
    ```

    Notice that this time the database connected to is HXE.  

    ![hdbsql commands](hdbsql-commands2.png)

    For further information, see [Port Assignment in Tenant Databases](https://help.sap.com/docs/SAP_HANA_PLATFORM/6b94445c94ae495c83a19646e7c3fd56/440f6efe693d4b82ade2d8b182eb1efb.html) and [Connecting to SAP HANA Databases and Servers](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/b250e7fef8614ea0a0973d58eb73bda8.html).  

### Knowledge check

Congratulations!  You now have access to an SAP HANA instance and understand some of the differences between SAP HANA Cloud and SAP HANA, express edition.




---
