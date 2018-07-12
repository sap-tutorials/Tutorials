---
title: Import CSV into SAP HANA, express edition using IMPORT FROM SQL command
description: Provide details on the import data feature available via the IMPORT FROM SQL command
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition ]
---

## Prerequisites  
- Proficiency: beginner
- SSH and FTP client installed, running and properly connected to your SAP HANA, express edition.
- [Install a SQL query tool for SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-tools-sql.html).
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://www.sap.com/developer/tutorials/mlb-hxe-setup-basic.html)

### You will learn

In this tutorial, you will learn how to use the IMPORT FROM SQL command and load data set files to your SAP HANA, express edition.

The IMPORT FROM SQL command requires the files to be physically located on your SAP HANA, express edition host.

It requires the table to be created before the execution of the command.

For more details on the available options you can check the [SAP HANA SQL and System Views Reference > IMPORT FROM Statement (Data Import Export)](https://help.sap.com/viewer/4fe29514fd584807ac9f2a04f6754767/2.0.00/en-US/20f712e175191014907393741fadcb97.html) documentation.

As an alternate solution, you can also use the ***SAP HANA Tools*** for Eclipse which provides a subset of features available in the IMPORT FROM SQL command.
However, it allows you to import data directly from your client and not the AP HANA, express edition host.

For more information, you can check the following tutorial: [Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html)

## Details

### Time to Complete
**10 Min**.

[ACCORDION-BEGIN [Prerequisite: ](Prepare your environment)]

In order to run the IMPORT FROM SQL command, you will be running a series of SQL statement on your SAP HANA, express edition.

The steps detailed in this tutorial will assume that you have completed the following tutorial:

- [Install a SQL query tool for SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-tools-sql.html)
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://www.sap.com/developer/tutorials/mlb-hxe-setup-basic.html)

This will let you reuse existing database user and schema.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Configuration: ](IMPORT FROM & CSV File Path)]

For security reasons, only CSV files located at paths defined in the **`csv_import_path_filter`** configuration parameter can be loaded by using the IMPORT FROM SQL statement.

This feature is controlled by the **`enable_csv_import_path_filter`** configuration parameter.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
SELECT
  *
FROM
  M_INIFILE_CONTENTS
WHERE
    SECTION = 'import_export'
AND KEY     = 'enable_csv_import_path_filter'
```

By default, the value is **true**, which means that the IMPORT FROM SQL command will only work for CSV files located at the path configured by **`csv_import_path_filter`** which is not set by default.

You can disable this restriction using the following SQL statement when connected as the **SYSTEM** user:

```
ALTER SYSTEM
  ALTER CONFIGURATION ('indexserver.ini', 'database')
  SET ('import_export', 'enable_csv_import_path_filter') = 'false'
  WITH RECONFIGURE;
```

Execute the following SQL statement:

```SQL
SELECT
  *
FROM
  M_INIFILE_CONTENTS
WHERE
    SECTION = 'import_export'
AND KEY     = 'csv_import_path_filter';
```

By default, the configuration returns an empty value which implies that the following locations are used by default:

- `$DIR_INSTANCE/work`
- `$DIR_INSTANCE/backup`
- `$SAP_RETRIEVAL_PATH/trace`

The `$DIR_INSTANCE` value for most SAP HANA, express edition installation is `/usr/sap/HXE/HDB90`.

This is detailed in [SAP Note 2109565](https://launchpad.support.sap.com/#/notes/2109565).

You can enable additional path using the following SQL statement when connected as the **SYSTEM** user:

```
ALTER SYSTEM
  ALTER CONFIGURATION ('indexserver.ini', 'database')
  SET ('import_export', 'csv_import_path_filter') = '/mypath1;/mypath2'
  WITH RECONFIGURE;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Save the sample data)]

Connect to your SAP HANA, express edition system using a SSH client or a FTP client connected as the `hxeadm` user.

Save the following data set in *`/usr/sap/HXE/HDB90/work/data_1.csv`* on the remote host:

```csv
A,B,C,D,E
1,"DATA1","2012/05/20","14:30:25","123456789"
2,"DATA2","2012/05/21","15:30:25","234567890"
3,"DATA3","2012/05/22","16:30:25","345678901"
4,"DATA4","2012/05/23","17:30:25","456789012"
```

Save the following data set in *`/usr/sap/HXE/HDB90/work/data_2.csv`* on the remote host:

```csv
5;"DATA5";"01-20-2015";"223025";"123456.789"
6;"DATA6";"01-21-2015";"153025";"234567.890"
7;"DATA7";"01-22-2015";"163025";"345678.901"
8;"DATA8";"01-23-2015";"173025";"456789.012"
```

As you can notice the date format are different and one uses a European format when the other uses a US format.

For the time format, only the HH24 hour formation is supported.

For the number format, the supported decimal separator is a dot and there is no thousand separator supported nor scientific notation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a sample table)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
CREATE TABLE ML_DATA.MYTABLE_IMPORTSQL (
    A INT,
    B VARCHAR(10),
    C DATE,
    D TIME,
    E DECIMAL
);
```
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import the Data)]

The IMPORT FROM command support a series of options that can be useful when addressing specific file or data format.

It also supports the use of Control Files, where you can store the import configuration, instead of passing the details directly in the command.

Check the following documentation for more details: [SAP HANA SQL and System Views Reference > IMPORT FROM Statement (Data Import Export)](https://help.sap.com/viewer/4fe29514fd584807ac9f2a04f6754767/2.0.00/en-US/20f712e175191014907393741fadcb97.html).

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

#### **Import the data 1 CSV**

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/data_1.csv' INTO ML_DATA.MYTABLE_IMPORTSQL
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY ','
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/data_1.csv.err'
;
```

#### **Import the data 2 CSV**

Execute the following SQL statement:

```SQL
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/data_2.csv' INTO ML_DATA.MYTABLE_IMPORTSQL
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY ';'
   OPTIONALLY ENCLOSED BY '"'
   DATE FORMAT 'MM-DD-YYYY'
   TIME FORMAT 'HHMISS'
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/data_2.csv.err'
;
```

> ### **Note: insufficient privilege: Not authorized**
>If you receive this error, it probably mean that the user is not granted the IMPORT role.
>Connect to the **HXE** tenant using the **SYSTEM** user credentials and execute the following SQL statement:
>```SQL
GRANT IMPORT TO ML_USER;
```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

To verify that the data were properly imported, you can run the following query:

```SQL
SELECT * FROM ML_DATA.MYTABLE_IMPORTSQL;
```

Provide an answer to the question below then click on **Validate**.

[DONE]
[ACCORDION-END]
