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
CAP_AGGREGATES : true
CAP_AGGREGATES_COLNAME : true
CAP_AGGR_STDDEV : true
CAP_AND : true
CAP_AND_DIFFERENT_COLUMNS : true
CAP_ARRAY_FETCH : true
CAP_BETWEEN : true
CAP_CASE_EXPRESSION : true
CAP_COLUMN_CAP : true
CAP_CORRELATION_IN_UPDATE : true
CAP_CRT_TEMP_TABLES : false
CAP_DELETE : true
CAP_DIST_AGGREGATES : true
CAP_EXPR_IN_FULL_OUTER_JOIN : true
CAP_EXPR_IN_GROUPBY : true
CAP_EXPR_IN_INNER_JOIN : true
CAP_EXPR_IN_LEFT_OUTER_JOIN : true
CAP_EXPR_IN_ORDERBY : true
CAP_EXPR_IN_PROJ : true
CAP_EXPR_IN_WHERE : true
CAP_GROUPBY : true
CAP_GROUPING_SETS : false
CAP_INSERT : false
CAP_IN : true
CAP_JOINS : true
CAP_JOINS_FULL_OUTER : true
CAP_JOINS_OUTER : true
CAP_LIKE : true
CAP_LIMIT : true
CAP_NESTED_FUNC_IN_FULL_OUTER_JOIN : true
CAP_NESTED_FUNC_IN_GROUPBY : true
CAP_NESTED_FUNC_IN_INNER_JOIN : true
CAP_NESTED_FUNC_IN_LEFT_OUTER_JOIN : true
CAP_NESTED_FUNC_IN_ORDERBY : true
CAP_NESTED_FUNC_IN_PROJ : true
CAP_NESTED_FUNC_IN_WHERE : true
CAP_NONEQUAL_COMPARISON : true
CAP_OFFSET : true
CAP_OFFSET_ARG : false
CAP_OR : true
CAP_ORDERBY : true
CAP_ORDERBY_EXPRESSIONS : true
CAP_ORDERBY_NULLS_ORDERING : false
CAP_OR_DIFFERENT_COLUMNS : true
CAP_PARAM_FUNCTION_SUBSTITUTION : false
CAP_PROJECT : true
CAP_SELECT : true
CAP_SEQUENCE_EXPRESSION : true
CAP_SIMPLE_EXPR_IN_FULL_OUTER_JOIN : true
CAP_SIMPLE_EXPR_IN_GROUPBY : true
CAP_SIMPLE_EXPR_IN_INNER_JOIN : true
CAP_SIMPLE_EXPR_IN_LEFT_OUTER_JOIN : true
CAP_SIMPLE_EXPR_IN_ORDERBY : true
CAP_SIMPLE_EXPR_IN_PROJ : true
CAP_SIMPLE_EXPR_IN_WHERE : true
CAP_SUBQUERY : true
CAP_SUBQUERY_GROUPBY : true
CAP_SUBQUERY_UPDATE : true
CAP_TABLE_CAP : false
CAP_TOP : false
CAP_TOP_UNDER_UNION : false
CAP_TRUNCATE_TABLE : false
CAP_TSQL_DELUPD : true
CAP_UNIONALL : false
CAP_UPDATE : true
CAP_WHERE : true
CAP_OWNER_SUPPORTED : true
CAP_SUBQUERY_DELETE : true
CAP_DISTINCT : true
CAP_HAVING : true
CAP_BI_POWER : true
CAP_BI_ROUND : true
CAP_BI_SIGN : true
CAP_BI_SIN : true
CAP_BI_SQRT : true
CAP_BI_TAN : true
CAP_BI_LTRIM : true
CAP_BI_RTRIM : true
CAP_BI_ABS : true
CAP_BI_COS : true
CAP_BI_ROWID : true
CAP_BI_EXP : true
CAP_BI_FLOOR : true
CAP_BI_LENGTH : true

FUNC_ASCII : true
FUNC_ABS : true
FUNC_ACOS : true
FUNC_ADD : true
FUNC_ADD_DAYS : DATE_ADD(DAY,$2,$1)
FUNC_ADD_MONTHS : DATE_ADD(MONTH,$2,$1)
FUNC_ADD_SECONDS : DATE_ADD(SECOND,$2,$1)
FUNC_ADD_YEARS : DATE_ADD(YEAR,$2,$1)
FUNC_ASCII : false
FUNC_ASIN : true
FUNC_ATAN : true
FUNC_ATAN2 : true
FUNC_BITAND : (SELECT BIT_AND(x) FROM UNNEST([$1, $2]) as x)
FUNC_CAST : true
FUNC_CEIL : true
FUNC_CHR : false
FUNC_COALESCE : COALESCE($*)
FUNC_CONCAT : true
FUNC_COS : true
FUNC_COSH : true
FUNC_DAYS_BETWEEN : DATE_DIFF($1, $2, DAY)
FUNC_DIV : true
FUNC_EXP : true
FUNC_EXTRACT_DAY : true
FUNC_EXTRACT_HOUR : true
FUNC_EXTRACT_MINUTE : true
FUNC_EXTRACT_MONTH : true
FUNC_EXTRACT_SECOND : true
FUNC_EXTRACT_YEAR : true
FUNC_FLOOR : true
FUNC_GREATEST : true
FUNC_IFNULL : IFNULL($1, $2)
FUNC_LAST_DAY : false
FUNC_LEAST : true
FUNC_LENGTH : true
FUNC_LN : true
FUNC_LOCATE : false
FUNC_LOG : true
FUNC_LOWER : true
FUNC_LPAD : true
FUNC_MOD : true
FUNC_MUL : true
FUNC_NCHR : CODE_POINTS_TO_STRING([$1])
FUNC_NEXT_DAY : false
FUNC_NULLIF : false
FUNC_NVL2 : false
FUNC_POWER : true
FUNC_REPLACE{#2} : false
FUNC_REPLACE{#3} : true
FUNC_ROUND{#2} : true
FUNC_ROUND{#3} : false
FUNC_RPAD : true
FUNC_SECONDS_BETWEEN : TIME_DIFF($1, $2, SECOND)
FUNC_SIGN : true
FUNC_SIN : true
FUNC_SINH : true
FUNC_SQRT : true
FUNC_SUB : true
FUNC_SUBSTR : SUBSTR($*)
FUNC_SUBSTR_AFTER : false
FUNC_SUBSTR_BEFORE : false
FUNC_TAN : true
FUNC_TANH : true
FUNC_TO_ALPHANUM : false
FUNC_TO_BIGINT : CAST($1 AS INT64)
FUNC_TO_BINARY : CAST($1 AS BYTES)
FUNC_TO_BLOB : false
FUNC_TO_CHAR : CAST ($1 AS STRING)
FUNC_TO_CLOB : false
FUNC_TO_DATE{#1} : DATE($1)
FUNC_TO_DATE{#2} : false
FUNC_TO_DATS : false
FUNC_TO_DECIMAL : CAST($1 AS FLOAT64)
FUNC_TO_DOUBLE : CAST($1 AS FLOAT64)
FUNC_TO_FIXED16 : false
FUNC_TO_INT : false
FUNC_TO_INTEGER : false
FUNC_TO_NCHAR : CAST ($1 AS STRING)
FUNC_TO_NCLOB : false
FUNC_TO_NUMBER : false
FUNC_TO_NVARCHAR : CAST ($1 AS STRING)
FUNC_TO_REAL : false
FUNC_TO_SECONDDATE : CAST ($1 AS TIMESTAMP)
FUNC_TO_SMALLDECIMAL : false
FUNC_TO_SMALLINT : false
FUNC_TO_TIME : TIME($1)
FUNC_TO_TIMESTAMP : CAST ($1 AS TIMESTAMP)
FUNC_TO_TINYINT: false
FUNC_TO_VARBINARY : false
FUNC_TO_VARCHAR : false
FUNC_TRIM_BOTH : TRIM($1)
FUNC_TRIM_LEADING : LTRIM($1)
FUNC_TRIM_TRAILING : RTRIM($1)
FUNC_UMINUS : false
FUNC_UNICODE : false
FUNC_UPPER : true
FUNC_VAR : false
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
```

> ### **Note**: You can paste content in **`vi`** using the following keyboard combination:
> - Enable the insert mode : **ESC** then **I**
> - Paste the clipboard content : **CTRL+SHIFT+V**

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

Once completed, you can run the following command to get the list of started processes:

```shell
HDB info
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
