---
title: Enable your SAP HANA, express edition instance for SAP HANA Smart Data Access
description: Enable your SAP HANA, express edition system for SAP HANA Smart Data Access.
primary_tag: products>sap-hana\,-express-edition
auto_validation: true
tags: [  tutorial>beginner, topic>cloud, topic>sql, products>sap-hana\,-express-edition ]
time: 10
---

## Details
### You will learn
- Create a SAP HANA Smart Data Access definition file for Amazon Athena
- Enable the Script server
- Enable the SAP HANA Smart Data Access safe Mode

**SAP HANA Smart Data Access** provide the ability to create remote sources and virtual tables.

For more information about SAP HANA Smart Data Access, you can check the online <a href="https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/latest/en-US/a07c7ff25997460bbcb73099fb59007d.html" target="&#95;blank">documentation</a>.

But first, you need to activate the **Script Server**, which is disabled by default on new installations.

Once enabled, you will also need to activate the **Safe Mode** for ODBC connections.

The **Safe Mode** provides the capability to load ODBC drivers and execute ODBC calls from within the **Script Server** process and reduces potential issues with the **Index Server** caused by third-party ODBC drivers.

For more details, you can check the <a href="https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/latest/en-US/6f01ebd7ed574fcfbf493ebd303eb6b1.html" target="&#95;blank">Safe Mode for ODBC Connections</a> documentation.

[ACCORDION-BEGIN [Step 1: ](Configure SAP HANA SDA for Amazon Athena)]

In order to best leverage the SAP HANA Smart Data Access, you will now add a dedicated configuration that will optimize the way queries will be executed between SAP HANA, express edition and Amazon Athena.

To learn more about it, you can check the [SAP HANA Smart Data Access](https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.03/en-US/a07c7ff25997460bbcb73099fb59007d.html) documentation from the SAP HANA Administration guide.

Connect to your SAP HANA, express edition using an SSH client as **`ec2-user`**.

So first, let's make sure that you are using the ***`ec2-user`*** user in your current SSH session.

The prompt should be:

```
ec2-user@hxehost:~>
```

Once your SSH session is with the ***`ec2-user`***, you can run the following command to create the configuration for Amazon Athena:

```shell
sudo vi /usr/sap/HXE/SYS/exe/hdb/config/property_athena.ini
```

Insert the following content then save and exit ***`vi`***:

```shell
CAP_SUBQUERY : true
CAP_ORDERBY : true
CAP_JOINS : true
CAP_GROUPBY : true
CAP_AND : true
CAP_OR : true
CAP_TOP : false
CAP_LIMIT : true
CAP_SUBQUERY :  true
CAP_SUBQUERY_GROUPBY : true

FUNC_ABS : true
FUNC_ADD : true
FUNC_ADD_DAYS : DATE_ADD(DAY,$2,$1)
FUNC_ADD_MONTHS : DATE_ADD(MONTH,$2,$1)
FUNC_ADD_SECONDS : DATE_ADD(SECOND,$2,$1)
FUNC_ADD_YEARS : DATE_ADD(YEAR,$2,$1)
FUNC_ASCII : true
FUNC_ACOS : true
FUNC_ASIN : true
FUNC_ATAN : true
FUNC_TO_VARBINARY : false
FUNC_TO_VARCHAR : false
FUNC_TRIM_BOTH : TRIM($1)
FUNC_TRIM_LEADING : LTRIM($1)
FUNC_TRIM_TRAILING : RTRIM($1)
FUNC_UMINUS : false
FUNC_UPPER : true
FUNC_WEEKDAY : false

TYPE_TINYINT : TINYINT
TYPE_LONGBINARY : VARBINARY
TYPE_LONGCHAR : VARBINARY
TYPE_DATE : DATE
TYPE_TIME : TIME
TYPE_DATETIME : TIMESTAMP
TYPE_REAL : REAL
TYPE_SMALLINT : SMALLINT
TYPE_INT : INTEGER
TYPE_INTEGER : INTEGER
TYPE_FLOAT : DOUBLE
TYPE_CHAR : CHAR($PRECISION)
TYPE_BIGINT : DECIMAL(19,0)
TYPE_DECIMAL : DECIMAL($PRECISION,$SCALE)
TYPE_VARCHAR : VARCHAR($PRECISION)
TYPE_BINARY : VARBINARY
TYPE_VARBINARY : VARBINARY

PROP_USE_UNIX_DRIVER_MANAGER : true
```

> ### **Note**: You can paste content in **`vi`** using the following keyboard combination:
> - Enable the insert mode : **ESC** then **I**
> - Paste the clipboard content : **CTRL+SHIFT+V**
> - Exit `vi`: **ESC** then **`:wq!`**

Then, switch the file ownership back to ***`hxeadm`***:

```shell
sudo chown hxeadm:sapsys /usr/sap/HXE/SYS/exe/hdb/config/property_athena.ini
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Connect to the System Database)]

The **Script Server** activation for the **HXE** tenant can only be done from a connection to the ***System Database***.

From the previous SSH session, switch to the **`hxeadm`** user using:

```shell
sudo su - hxeadm
```

Then, start a HDBSQL session to connect to the ***System Database*** using:

```shell
hdbsql -i 90 -d SYSTEMDB -u system
```

You will be prompted for the database master password provided during the initialization.

The terminal prompt should become:

```
hdbsql SYSTEMDB=>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Apply the configuration)]

You can now execute the following SQL commands to apply the required configuration:

```sql
alter database hxe add 'scriptserver';
alter system alter configuration ('indexserver.ini', 'SYSTEM') set ('smart_data_access', 'odbc_adapters_in_scriptserver') = 'all';
```

You can now quit the current HDBSQL session using **\q**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Restart your instance)]

Now, you will need to restart your SAP HANA, express edition for the change to be effective.

From the previous SSH session, run the following command:

```shell
HDB stop && HDB start
```

The restart process will take a few minutes to complete.

Although the database is running, the XS Advanced applications and services may still be loading. Check the progress by running the following command in the XS Command Line Interface.

```shell
xs-admin-login
xs apps | grep webide
```

When prompted, use the password you provided at the beginning of the installation script.

Repeat the second command until you see STARTED and 1/1 for applications `webide`.
Once started, you can run the following command to get the list of started processes:

```shell
HDB info
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
