---
title: Import GDELT Data in SAP HANA, express edition
description: Learn how to import data from the Global Database of Events, Language and Tone (GDELT) Project into SAP HANA, express edition.
primary_tag: products>sap-hana\,-express-edition
auto_validation: true
tags: [  tutorial>beginner, topic>cloud, topic>sql, products>sap-hana\,-express-edition ]
time: 10
---

## Details
### You will learn  
In this tutorial, you will learn how to use the ***IMPORT FROM SQL*** command and load data set files to your SAP HANA, express edition.

The tutorial requires:

 - the files to be physically located on your SAP HANA, express edition host.
 - the table to be created before the execution of the command.

For more details on the available options, you can check the <a href="https://help.sap.com/viewer/4fe29514fd584807ac9f2a04f6754767/latest/en-US/20f712e175191014907393741fadcb97.html" target="&#95;blank">SAP HANA SQL and System Views Reference > IMPORT FROM Statement (Data Import Export)</a> documentation.

As an alternate solution, you can also use the ***SAP HANA Tools*** for Eclipse, which provides a subset of features available in the IMPORT FROM SQL command.

However, it allows you to import data directly from your client and not the SAP HANA, express edition host.

For more information, you can check the following tutorial: <a href="https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html" target="&#95;blank">Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse</a>

> ### **Warning:** If you are not planning to complete the all series and in order to avoid being billed for AWS usage, make sure to go thru the [Cleanup before you leave]https://developers.sap.com/tutorials/hxe-aws-athena-07.html) tutorial.

[ACCORDION-BEGIN [Step 1: ](Download the data files)]

The lookup tables that will be loaded in SAP HANA, express edition are available on the <a href="https://www.gdeltproject.org/data/lookups/" target="&#95;blank">GDELT CAMEO</a> site.

As stated above, you will need to download the data set files on your SAP HANA, express edition host.

Connect to your SAP HANA, express edition using an SSH client like ***`PuTTY`*** as **`ec2-user`**.

The prompt should be:

```
ec2-user@hxehost:~>
```

If the prompt is already **```hxeadm@hxehost:~>```**, you will not need to execute the next command to switch to the **`hxeadm`** user:

```shell
sudo su - hxeadm
```

Then, run the following commands to download the data file:

```shell
cd /usr/sap/HXE/HDB90/work/
wget https://www.gdeltproject.org/data/lookups/CAMEO.type.txt
wget https://www.gdeltproject.org/data/lookups/CAMEO.religion.txt
wget https://www.gdeltproject.org/data/lookups/CAMEO.knowngroup.txt
wget https://www.gdeltproject.org/data/lookups/CAMEO.goldsteinscale.txt
wget https://www.gdeltproject.org/data/lookups/CAMEO.eventcodes.txt
wget https://www.gdeltproject.org/data/lookups/CAMEO.ethnic.txt
wget https://www.gdeltproject.org/data/lookups/CAMEO.country.txt
```

> ### **Note**: by default, only file located in `/usr/sap/HXE/HDB90/work/` can be imported using the ***IMPORT FROM SQL*** command.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a dedicated database user)]

From the previous SSH session, start a HDBSQL session using:

```shell
hdbsql -i 90 -d HXE -u system
```

The prompt should become:

```
hdbsql HXE=>
```

You will be prompted for the database master password provided during the initialization process (the master password).

Then, execute the following SQL commands:

```sql
-- create the gedelt user in hana
create user gdelt_hana password Welcome18Welcome18 no force_first_password_change;
-- grant the required roles to complete the series
grant import to gdelt_hana;
grant create remote source to gdelt_hana;
grant create schema to gdelt_hana;
\q
```

> ### **Note:** **\q** is used to quit the current HDBSQL session.

You have just created a database user named ***`gdelt_hana`*** in SAP HANA.

The password for the ***`gdelt_hana`*** user is ***`Welcome18Welcome18`***.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create tables & import data)]

From the previous SSH session, start a new HDBSQL session for the ***`gdelt_hana`*** user using:

```shell
hdbsql -i 90 -d HXE -u gdelt_hana -p Welcome18Welcome18
```

The prompt should become:

```
hdbsql HXE=>
```

Then execute the following SQL statements:

```sql
-- create the tables
create table gdelt_hana.type           (code  varchar(3),  label           varchar(255));
create table gdelt_hana.religion       (code  varchar(3),  label           varchar(255));
create table gdelt_hana.knowngroup     (code  varchar(3),  label           varchar(255));
create table gdelt_hana.goldsteinscale (code  varchar(4),  goldsteinscale  decimal(5,3));
create table gdelt_hana.eventcodes     (code  varchar(4),  description     varchar(255));
create table gdelt_hana.ethnic         (code  varchar(3),  label           varchar(255));
create table gdelt_hana.country        (code  varchar(3),  label           varchar(255));

-- import the data
import from csv file '/usr/sap/HXE/HDB90/work/CAMEO.type.txt'           into gdelt_hana.type           with field delimited by '\t' skip first 1 row fail on invalid data;
import from csv file '/usr/sap/HXE/HDB90/work/CAMEO.religion.txt'       into gdelt_hana.religion       with field delimited by '\t' skip first 1 row fail on invalid data;
import from csv file '/usr/sap/HXE/HDB90/work/CAMEO.knowngroup.txt'     into gdelt_hana.knowngroup     with field delimited by '\t' skip first 1 row fail on invalid data;
import from csv file '/usr/sap/HXE/HDB90/work/CAMEO.goldsteinscale.txt' into gdelt_hana.goldsteinscale with field delimited by '\t' skip first 1 row fail on invalid data;
import from csv file '/usr/sap/HXE/HDB90/work/CAMEO.eventcodes.txt'     into gdelt_hana.eventcodes     with field delimited by '\t' skip first 1 row fail on invalid data;
import from csv file '/usr/sap/HXE/HDB90/work/CAMEO.ethnic.txt'         into gdelt_hana.ethnic         with field delimited by '\t' skip first 1 row fail on invalid data;
import from csv file '/usr/sap/HXE/HDB90/work/CAMEO.country.txt'        into gdelt_hana.country        with field delimited by '\t' skip first 1 row fail on invalid data;

-- switch to multiline commands
\mu

-- get the row count
          select 'type' as table , count(1) as count from gdelt_hana.type
union all select 'religion'      , count(1) from gdelt_hana.religion
union all select 'knowngroup'    , count(1) from gdelt_hana.knowngroup
union all select 'goldsteinscale', count(1) from gdelt_hana.goldsteinscale
union all select 'eventcodes'    , count(1) from gdelt_hana.eventcodes
union all select 'ethnic'        , count(1) from gdelt_hana.ethnic
union all select 'country'       , count(1) from gdelt_hana.country;
```

This will import the data files into the previously created tables and get the row counts:

> ### **Note**: **\mu** is used to enable the multi line command mode.
To exit the result mode, press the letter ***q*** and to quit the current HDBSQL session you can use **\q**.

Provide an answer to the question below, and then click **Validate**.

[VALIDATE_1]
[ACCORDION-END]
