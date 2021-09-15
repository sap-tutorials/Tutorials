---
title: Migrate SAP ASE Database from On-Premise to SAP HANA Cloud
description: Migrate your SAP ASE database from on-premise to SAP HANA Cloud.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-hana-cloud, products>sap-adaptive-server-enterprise, software-product-function>sap-hana-cloud\,-sap-adaptive-server-enterprise]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
- You have completed the [previous tutorial](hana-cloud-ase-migration-2) on how to encrypt your SAP ASE database to migrate from on-premise to SAP HANA Cloud.

## Details
### You will learn
- How to create an SAP HANA Cloud, SAP ASE database instance
- How to copy the encrypted backup to MS Azure using **`azcopy`**
- How to load the encrypted backup


Migrating an SAP ASE database from on-premise to the cloud requires a bit of preparation and a few important steps. You can look at each of them in more detail, but these are the high-level steps required:

1.	Prerequisites for migration
2.	Pre-Migration -- Encrypt the on-premise SAP ASE database
3.	Migration -- Create the SAP HANA Cloud, SAP ASE database
4.	Migration -- Copy the Encrypted Backup to MS Azure
5.	Migration -- Load the Encrypted Backup

This tutorial will cover the steps (3 to 5) involving the migration. You can learn about the [first](hana-cloud-ase-migration-1) and the [second](hana-cloud-ase-migration-2) step by referring to the previous tutorials.


---

[ACCORDION-BEGIN [Step 1: ](Create the SAP HANA Cloud, SAP ASE database instance)]

Now it's time to make sure your SAP ASE database in SAP HANA Cloud is ready to receive the data from your on-premise SAP ASE database. To ensure this, follow these steps:

1.	Provision an SAP HANA Cloud, SAP ASE database instance. The database in SAP HANA Cloud should be at least the same size as the on-premise one, but you might find that you need to increase the size as you go through the migration and performance test. Find more information on our technical documentation.

2.	Configure your SAP ASE database in SAP HANA Cloud. You can find details of the various operational steps to back up the database, SAP ASE configuration or list out the login accounts, roles, and database cache bindings on the SAP HANA Cloud, SAP ASE Migration Guide.

3.	Make sure the SAP HANA Cloud, SAP ASE master database is encrypted with a password and not using an external HSM.

    ```Shell/Bash
sp_encryption helpkey, sybencrmasterkey
```
    If it was not encrypted with a password, then add a password for the master key.

    ```Shell/Bash
alter encryption key master with external key
modify encryption with passwd "<PASSWORD>"
```
4.	Extract the database-encryption-key from the on-premise SAP ASE database.  There is a `dek_mig_gen.py` key migration script to automatically build the create encrypt key command.  Here is the syntax to use with this script in order to get the database-encryption-key:

    ```Shell/Bash
export SYBROOT=/sap/ASE16sp04GA/sap/python_client/testcode/dek_mig_gen.py -U sa -P <PASSWORD> -S <OnPremiseASE> -K <KEY_NAME> -M <PASSWORD>
```
    Optionally, you can manually build the database-encryption-key from the on-premise SAP ASE database using `ddlgen`. Instructions for building out the database-encryption-key using `ddlgen` can be found in the SAP HANA Cloud, SAP ASE Migration Guide.

    ```Shell/Bash
ddlgen -Usa -P<PASSWORD> -SASE16sp04GA -Dmaster -TEK -XOD -N %
```

5.	Create the **encrypted** database in the SAP HANA Cloud, SAP ASE. Execute the command previously created as part of the previous step (step 3) to create the database encryption key in the SAP HANA Cloud, SAP ASE.

6.	Create the database using the migrated DEK.

    > ### CAUTION
    >
    > It is very important to create the database **with the encrypted key**. If you don't, the database will still be created but you won't be able to load the database later.


    ```Shell/Bash
    create database <DATABASE_NAME> on datadev11="1G" log on logdev11="50M" for load encrypt with <KEY_NAME>
    ```



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Copy the encrypted backup to MS Azure using azcopy)]

You can see a migration demo video here, and then check out the detailed steps:

<iframe width="560" height="315" src="https://www.youtube.com/embed/zNAfk9Wt0Qo" frameborder="0" allowfullscreen></iframe>

&nbsp;

1.	If you don't already have an Azure account, you can go to [this website](https://azure.microsoft.com/en-us/free/) and create one.  Once you have logged into the Azure portal, proceed to create an Azure Storage Account of type `blogstorage`.

2.	Install the `azcopy` tool using the help of [this link](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli-linux?pivots=dnf).

    Then run this command:
    ```Shell/Bash
rpm --import https://packages.microsoft.com/keys/microsoft.asc
```

3.	Once you have `azcopy` loaded on your system, you will need to log in into your azure account with `azcopy`. Below is the command that you use to do this:

    ```Shell/Bash
/docker/toolkit/microsoft/azcopy_linux_amd64_10.9.0/azcopy login --tenant-id "XXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
```
    Replace the `tenant-id` above with the `tenant-id` of your MS Azure account.  After you have executed the `azcopy login` command above, you will be asked to open a browser and type in a code that was provided to allow the access.

4.	Next, create a container using the `azcopy` tool.

    ```Shell/Bash
/docker/toolkit/microsoft/azcopy_linux_amd64_10.9.0/azcopy make "https://salsestorage01.blob.core.windows.net/<DATABASE_NAME>container"
```
5.	Using the MS Azure portal, generate a shared access signature (SAS) and assign to signing key 1 (access key).

6.	Finally, upload the on-premise encrypted backup to the azure storage account using `azcopy`. Be sure to select the database directory name as the root of the directory tree that you want uploaded.

    ```Shell/Bash
/docker/toolkit/microsoft/azcopy_linux_amd64_10.9.0/azcopy copy '<DATABASE_NAME>_dumpdir/<DATABASE_NAME>' 'https://salsestorage01.blob.core.windows.net/<DATABASE_NAME>container?sp=racwdl&st=2021-02-28T14:19:34Z&se=2022-03-01T22:19:34Z&spr=https&sv=2020-02-10&sr=c&sig=yKCnItSYnfkIb%2BqFzTu7iiP3Saso2zPqY%2F1QtgiEpH8%3D' --recursive
```

Replace the full https parameter in the `azcopy` command above with your shared access signature (SAS) created in step 5.  



[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Load the encrypted backup)]

1.	Now, load the encrypted backup into SAP HANA Cloud, SAP ASE from the Azure storage account.

    > ### IMPORTANT
    >
    > In this step, the password is the **access key** and **not** the shared access signature (SAS) that we created for the container.

    ```Shell/Bash
load database <DATABASE_NAME> from 'syb_azure::-C<DATABASE_NAME>container::<DATABASE_NAME>.dmp'
 with extlib_login_name= 'salsestorage01',
      extlib_login_pwd = <Access Key 1>
```
2.	Bring your SAP HANA Cloud, SAP ASE database online.

    ```Shell/Bash
online database <DATABASE_NAME>
```

3.	Next, you can proceed to configure your new database as usual, by
    - Granting privileges to User Defined Roles and,
    - [Restoring Cache Bindings](https://help.sap.com/viewer/ecf4a49f0eee4a6a97019a1821dd090a/LATEST/en-US/de7cc3d785f94fef8559b69546dec89b.html)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check the post-migration steps)]

You can see all the detailed steps post-migration in our [technical documentation](https://help.sap.com/viewer/ecf4a49f0eee4a6a97019a1821dd090a/LATEST/en-US/b0cdf5e3d14846b8a454e2d88d41f9ca.html). These include a database validation, a functional test and performance tuning.

This completes the migration of an SAP ASE on-premise database to SAP HANA Cloud, SAP Adaptive Server Enterprise.

> **Well done!**
>
> You have completed the last tutorial of this group!
>
> Follow our tag in the [SAP Community](https://blogs.sap.com/tags/73554900100800002881/) to stay up-to-date on the latest updates and newest content! For more learning materials on SAP HANA Cloud, [click here](https://community.sap.com/topics/hana-cloud).



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

---
