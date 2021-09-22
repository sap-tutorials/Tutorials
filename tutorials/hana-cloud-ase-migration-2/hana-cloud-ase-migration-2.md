---
title: Encrypt SAP ASE Database to Migrate from On-Premise to SAP HANA Cloud
description: Encrypt your SAP ASE database to migrate from on-premise to SAP HANA Cloud.
auto_validation: true
time: 15
tags: [tutorial>beginner, products>sap-hana-cloud, products>sap-adaptive-server-enterprise, software-product-function>sap-hana-cloud\,-sap-adaptive-server-enterprise]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
- You have completed the [previous tutorial](hana-cloud-ase-migration-1) on how to prepare for migrating your SAP ASE database from on-premise to SAP HANA Cloud.

## Details
### You will learn
- How to enable column Encryption
- How to create the key-encryption-key and database-encryption-key
- How to configure addition worker processes
- How to confirm the completion of encryption
- How to back up the encrypted database to a file system


Migrating an SAP ASE database from on-premise to the cloud requires a bit of preparation and a few important steps. You can look at each of them in more detail, but these are the high-level steps required:

1.	Prerequisites for migration
2.	Pre-Migration -- Encrypt the on-premise SAP ASE database
3.	Migration -- Create the SAP HANA Cloud, SAP ASE database
4.	Migration -- Copy the Encrypted Backup to MS Azure
5.	Migration -- Load the Encrypted Backup

The first step was covered in the [previous tutorial](hana-cloud-ase-migration-1). This tutorial will cover the second step "Pre-Migration -- Encrypt the on-premise SAP ASE database". In the [last tutorial](hana-cloud-ase-migration-3), you will learn about the steps involving migration.

> ### About the code snippets in this tutorial
>
> In the code snippets of this tutorial, you find placeholders marked by ´<>´. If you enter your information, make sure to remove the ´<>´ characters before execution.

---

[ACCORDION-BEGIN [Step 1: ](Introduction)]

To prepare your SAP ASE on-premise database for migration, there are a few key steps you need to complete. This involves encrypting your on-premise database. The scripts in this tutorial are examples of the steps you need to follow, which include:

-	enabling column encryption
-	creating the key encryption key in the master database
-	creating the database encryption key
-	encrypting the database
-	optionally, setting the SAP ASE configuration to automatically use the encryption key
-	backing up the encrypted database to a file system




[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable column encryption)]

To encrypt a database, you need to enable encrypted columns. This is an on-premise SAP ASE option which requires a license. You will need to work with your SAP team obtaining the appropriate license to enable this option.

```Shell\Bash
sp_configure 'enable encrypted columns', 1
```


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create the key-encryption-key)]



Create the key-encryption-key (KEK) in the master database. This will allow you to encrypt the database encryption key.

```Shell/Bash
create encryption key master with passwd "<PASSWORD>"

set encryption passwd "<PASSWORD>" for key master
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the database-encryption-key)]

Create the database-encryption-key (DEK). This will represent the cipher that will be used to encrypt the database.

```Shell/Bash
create encryption key <KEY_NAME> for database encryption
```
Optionally, you can also set the configuration to automatically enable the key-encryption-key using the following command:

```Shell/Bash
alter encryption key master with passwd "<PASSWORD>" add encryption for automatic_startup
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure addition worker processes)]

Configure addition worker processes to support the encryption. The encryption process requires worker processes to perform the encryption changes in the background as quickly as possible. For our database, we need to configure at least two or more worker processes.

```Shell/Bash
sp_configure "number of worker processes", 2
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Confirm the completion of encryption)]

Now that we have created the key-encryption-key, the database-encryption-key, and we have added a few worker processes, we are ready to begin the process of encrypting the database.  You can use `sp_helpdb` to monitor the encryption progress and confirm its completion before continuing to the next step.

```Shell/Bash
alter database <DATABASE_NAME> encrypt with <KEY_NAME>

exec sp_helpdb <DATABASE_NAME>
```



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Backup the encrypted database to a file system)]

It is very important to back up the database with a directory structure starting with the database name, followed by a subdirectory called `DB`.

```Shell/Bash
sp_configure "enable dump_history", 1

dump database <DATABASE_NAME> to "/dumpdir/<dbname>/DB/<dbname>.dmp"

load database <DATABASE_NAME> with listonly=load_sql
```

In order to use `listonly=load_sql` (used later in this document), "enable dump history" needs to be set in ASE, otherwise you get the following message when trying to get the `load_sql` commands:

```Shell/Bash
1> load database <DATABASE_NAME> with listonly=load_sql
2> go
```

Below is an example of the commands used to back up the <DATABASE_NAME> database:

```Shell/Bash
1> sp_configure "enable dump_history", 1
2> go

1> dump database <DATABASE_NAME> to "/dumpdir/<DATABASE_NAME>/DB/<DATABASE_NAME>.dmp"
2> go

1> load database <DATABASE_NAME> with listonly=load_sql
2> go
LOAD DATABASE <DATABASE_NAME> FROM '/dumpdir/<DATABASE_NAME>/DB/<DATABASE_NAME>.dmp'
go
```

Now you know how to encrypt your SAP ASE on-premise database and you are ready for the actual migration. As the final step, learn how to create an SAP ASE on-premise database and load your encrypted data in the next tutorial.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]



[VALIDATE_1]
[ACCORDION-END]

---
