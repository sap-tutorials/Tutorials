---
title: Prepare to Migrate an SAP ASE Database from On-Premise to SAP HANA Cloud
description: Prepare your SAP Adaptive Server Enterprise database for migration from on-premise to SAP HANA Cloud.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-hana-cloud, products>sap-adaptive-server-enterprise, software-product-function>sap-hana-cloud\,-sap-adaptive-server-enterprise]
primary_tag: products>sap-hana-cloud
---

## Prerequisites


## Details
### You will learn
- What the prerequisites for migrating to SAP HANA Cloud are
- How to collect information on server and database using a stored procedure
- How to prepare for migration

In this section, we will cover the steps to migrate an SAP ASE on-premise database to SAP HANA Cloud, SAP Adaptive Server Enterprise.

Migrating an SAP ASE database from on-premise to the cloud requires a bit of preparation and a few important steps. You can look at each of them in more detail, but these are the high-level steps required:

1. Prerequisites for migration
2. Encrypt the SAP ASE on-premise database
3. Migration -- Create the SAP HANA Cloud, SAP ASE database
4. Migration -- Copy the Encrypted Backup to MS Azure
5. Migration -- Load the Encrypted Backup

This tutorial will cover the first step – "Prerequisites for migration". The [second tutorial](hana-cloud-ase-migration-2) will explain how to encrypt the SAP ASE on-premise database. In the [last tutorial](hana-cloud-ase-migration-3), you will learn about the steps involving migration.

---

[ACCORDION-BEGIN [Step 1: ](Prerequisites for migrating to SAP HANA Cloud)]
Here are the main prerequisites for migrating to SAP HANA Cloud:

1.	Ensure that your on-premise database is on a supported OS and ASE version.

    Currently the only supported OS is Linux x86-64, but this will also be available for Windows OS in the future. The ASE version must be on ASE 16 SP03 PL10 or higher.

2.	Download supported SDK 20.0 from [SAP Download Center](https://launchpad.support.sap.com/#/softwarecenter).

3.	Install the **`installmigration`** script from the downloaded SDK.

    The script, which is used to install a few stored procedures needed for the migration, can be found in the $SYBASE/tools/scripts directory.

    Run the following:

    ```Shell/Bash
    isql -i installmigration
    ```

    The following shows the response.  

    ```
    [sybase@SAPcentos3 scripts]$ isql -i installmigration
    (1 row affected)
     Parameter Name                 Default     Memory Used   Config Value   Run Value    Unit   Type
     ------------------------------ ----------- ------------- -------------- ------------ ------ -------
     allow updates to system tables           0           0              1              1 switch dynamic

    (1 row affected)
    Configuration option changed. ASE need not be rebooted since the option is dynamic.
    Changing the value of 'allow updates to system tables' does not increase the amount of memory Adaptive Server uses.
    (return status = 0)
     Parameter Name                 Default     Memory Used   Config Value   Run Value    Unit   Type
     ------------------------------ ----------- ------------- -------------- ------------ ------ -------
     allow updates to system tables           0           0              0              0 switch dynamic
    Configuration option changed. ASE need not be rebooted since the option is dynamic.
    Changing the value of 'allow updates to system tables' does not increase the amount of memory Adaptive Server uses.
    Loading of SAP ASE migration procedures is complete.
    (return status = 0)
    [sybase@SAPcentos3 scripts]$ pwd
    /sap/python_client/OCS-16_0/scripts

    ```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Collect information on server and database)]

1.	Retrieve the server information. The first stored procedure to execute is **`sp_serverinfo_report`**.

2.	Retrieve the database information by running the **`sp_dbinfo_report`** stored procedure for each database that needs to be migrated to SAP HANA Cloud, SAP ASE.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Prepare for migration)]

1.	Use the information gathered from the previous steps to determine the size of your SAP ASE database in SAP HANA Cloud.

2.	DBCC and Backups
    -	Validate the database to be migrated with DBCC
    -	Back up the database before making changes to it
    -	Back up the SAP ASE server configuration

3.	Get Login Accounts and User Defined Roles from the SAP on-premise ASE database.

4.	Estimate the duration of the migration.

    To help prepare for the migration, it is important to know how long it may take to complete various steps within the migration process. The **`sp_migration_estimate`** stored procedure will help provide you with this estimate duration.

5.	Check the compatibility of your on-premise database with SAP HANA Cloud.

    It is important to know if there are any features being used by the SAP ASE on-premise database that will not be available in the SAP HANA Cloud, SAP ASE. You can do this by executing the stored procedure **`sp_featurecompat`**.

    Without any parameters, it will list out the syntax. When you pass in "`info`" as the parameter, it will show which options or features are enabled in the SAP ASE on-premise database but not supported in SAP HANA Cloud. Similarly, when you pass in "`info_db`" as the parameter, it will show which database options are enabled for each database but are not supported in SAP HANA Cloud, SAP ASE.

    You can find more information in our [technical documentation](https://help.sap.com/viewer/46353c3b724f4934bb0671dd82044acd/LATEST/en-US/574360dbd4cb41b4b8d4b6bc3aa5d2c8.html).

6.	Check for conflicts with existing login and user ID's.

    Another important pre-migration check is to validate that the login ID's and `sysuser` ID's are not in conflict with the login ID's used by SAP in SAP HANA Cloud, SAP ASE. This situation will be unusual, but we will want to check to see if any of the login ID's or user ID's in the SAP ASE on-premise and SAP HANA Cloud databases are between -4 and -7. Use the following commands:

    ```Shell/Bash
    use master
    go
    select * from syslogins where suid >= (-7) and suid <= (-4)
    go
    use testdb
    go
    select * from sysusers where suid >= (-7) and suid <= (-4)
    go
    ```

Now you know how to prepare for migration. As the next step towards migration, learn how to migrate your on-premise SAP ASE database in the next tutorial.



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test yourself)]



[VALIDATE_7]
[ACCORDION-END]

---
