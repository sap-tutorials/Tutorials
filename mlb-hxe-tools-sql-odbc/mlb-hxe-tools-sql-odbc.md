---
title: Use an ODBC based querying tool with SAP HANA, express edition
description: Provide details on the installation the SAP HANA client and instruction to connect a SAP HANA, express edition instance from almost any ODBC based querying tool.
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>how-to, tutorial>intermediate, products>sap-hana\, express-edition ]
time: 10
---

## Details
### You will learn
In this tutorial, you will install the SAP HANA client for SAP HANA, express edition.

Then, you will learn how to connect your SAP HANA, express edition instance using the Linux `isql` tool as a ODBC based querying tool.

The Linux `isql` tool is included in the unixODBC package. Please refer to your system documentation for installation instructions.

On Windows environments, there is no pre-installed tool available out-of-the-box.

For alternate options, you can also check the following link: [Select a SQL query tool for SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-tools-sql.html).

[ACCORDION-BEGIN [Prerequisites: ](Download & Install the SAP HANA HDB client)]

Before you can proceed with the next steps, you will need to complete the **Installing SAP HANA HDB Client** for your target platform from the [Install the SAP HANA, express edition clients](https://www.sap.com/developer/groups/hxe-install-clients.html) group.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Locate the ODBC Driver)]

The driver (`lbodbcHDB.dll/so`), which is installed as part of the SAP HANA client, is located at (unless specified otherwise during the installation):

 - on Linux and UNIX platforms `/usr/sap/hdbclient/`
 - on Microsoft Windows platforms: `C:\Program Files\SAP\hdbclient\`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure the driver)]

#### **Connecting from Linux environments**

For the Linux environment, you will first need the **unixODBC** package to be installed.

Please refer to your system documentation for installation instructions.

In addition, it is important to create the following addition symbolic links:

```shell
ln -s /etc/unixODBC/odbc.ini /etc/odbc.ini
ln -s /etc/unixODBC/odbcinst.ini /etc/odbcinst.ini
```

To validate that **unixODBC** is properly installed, you can run the following command to print the current version:

```shell
odbcinst --version
```

Now, you can append HDBODBC driver definition to the unixODBC drivers file (in **`/etc/odbcinst.ini`**):

```shel
[ODBC]
TraceFile   = /tmp/sql.log
Trace       = No

[HDBODBC]
Driver      = /usr/sap/hdbclient/libodbcHDB.so
Description = SAP HANA ODBC Driver
FileUsage   = 1

```

To validate that the SAP HANA ODBC Driver is properly registered, you can run the following command:

```shell
odbcinst -q -d
```

The following output should be returned:

```
[HDBODBC]
```

#### **Connecting from Windows environments**

To validate that the SAP HANA ODBC Driver is properly registered, run the **ODBC Data Source Administrator (64 bits)** using the following command and switch to the **Drivers** tab:

```shell
odbcad32
```

![image Step 4](04-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure your connections)]

Before connecting  with ODBC, you need to create an ODBC DSN (Data Source Name).

ODBC DSN can either be added as ***User*** or ***System*** DSN. A ***System*** DSN will be visible to any user compared to ***User*** DSN which are defined per users.

We will be configuring ***System*** DSN in this tutorial, but if you don't have the permission you can replicate with ***User*** DSN .

#### **Connecting from Linux environments**

To add your ODBC DSN on Linux environments, you need to add the a new section to your `odbc.ini` file.

This section will be based on the following template:

```
[<ODBC DSN Name>]
ServerNode  = <host>:<port>
Driver      = HDBODBC
Description = <description>
```

Edit the System DSN `odbc.ini` file is located in `/etc/odbc.ini` and append the following:

```shell
[DSN_HXE]
ServerNode  = <server ip>:39015
Driver      = HDBODBC
Description = HXE Tenant

[DSN_SYSTEMDB]
ServerNode  = <server ip>:39013
Driver      = HDBODBC
Description = SYSTEM Database
```

Make sure to update the `<server ip>` with the proper information.

> ### **Note** : for Docker users you will need to update the HXE port to ***39041*** and the SYSTEMDB one to ***39017***.
&nbsp;

To validate that your ODBC DSN are properly registered, you can run the following command:

```shell
odbcinst -q -s
```

The following output should be returned:

```
[HXE]
[SYSTEMDB]
```

#### **Connecting from Windows environments**

To add a new ODBC DSN, run the **ODBC Data Source Administrator (64 bits)** using the following command and switch to the **System DSN** tab:

```shell
odbcad32
```
Click on **Add...**.

![image Step 5](05-1.png)

Select **HDBODBC** and click on **Finish**.

![image Step 5](05-2.png)

Enter the following details (replace <server host> by your instance host name or IP address):

|-------------------|-------------|
| Data Source Name  | `DSN_HXE`
| Description       | HXE Tenant DSN
| Server:Port       | &lt;server host&gt;:39015

Click on **Connect**.

![image Step 5](05-3.png)

Enter the database user credentials and click on **OK**

![image Step 5](05-4.png)

The connection should be successful. Click on **OK**.

![image Step 5](05-5.png)

Repeat the steps for the SYSTEMDB:

|-------------------|-------------|
| Data Source Name  | `DSN_SYSTEMDB`
| Description       | SYSTEM Database DSN
| Server:Port       | &lt;server host&gt;:39013

![image Step 5](05-6.png)

At the end, you should have both the HXE and SYSTEMDB ODBC DSN listed.

![image Step 5](05-7.png)

> ### **Note** : for Docker users you will need to update the HXE port to ***39041*** and the SYSTEMDB one to ***39017***.
&nbsp;

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test the connections with SQL)]

#### **Connecting from Linux environments**

The unixODBC package comes with a command-line interactive SQL tool called `isql`.

You can run the `isql` command to get more details about the program options and switches:

In order to connect to the **`DSN_HXE`** DSN, you can run the following command (after adjusting the password):

```shell
echo "select * from M_DATABASES;" | isql DSN_HXE SYSTEM password -c -m10 -b
```

The result should look like this:

```
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
| DATABASE_N| DESCRIPTIO| ACTIVE_STA| ACTIVE_STA| OS_USER   | OS_GROUP  | RESTART_MO|
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
| HXE       | HXE-90    | YES       |           |           |           | DEFAULT   |
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
SQLRowCount returns 1
1 rows fetched
```

In order to connect to the **`DSN_SYSTEMDB`** DSN, you can run the following command (after adjusting the password):

```shell
echo "select * from M_DATABASES;" | isql DSN_SYSTEMDB SYSTEM password -c -m10 -b
```

The result should look like this:

```
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
| DATABASE_N| DESCRIPTIO| ACTIVE_STA| ACTIVE_STA| OS_USER   | OS_GROUP  | RESTART_MO|
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
| SYSTEMDB  | SystemDB-H| YES       |           |           |           | DEFAULT   |
| HXE       | HXE-90    | YES       |           |           |           | DEFAULT   |
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
SQLRowCount returns 2
2 rows fetched
```

#### **Connecting from Windows environments**

As stated before, there is no pre-installed querying tool installed by default on Windows environment.

There are several options available like:

 - `ODBC Test` by Microsoft:

    As part of the [Microsoft Data Access Components (MDAC) Software Development Kit](https://msdn.microsoft.com/en-us/library/ms810805.aspx) (which can be downloaded from the [Microsoft Download Center](https://www.microsoft.com/en-us/download/details.aspx?id=21995)), this tool shows all the ODBC API function call and allow you to run SQL

 - [`ODBC QueryTool`](https://sourceforge.net/projects/odbcquerytool/) on `SourceForge`

Make sure you always use a 64 bit version else your DSN won't be listed.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validation)]

Based on the available **ODBC Connection Properties** listed in the <a href="https://help.sap.com/viewer/0eec0d68141541d1b07893a39944924e/2.0.02/en-US/7cab593774474f2f8db335710b2f5c50.html" target="new">online documentation</a>, provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Connection String: ](The server host)]

You can use the IP address, the host name or the fully qualified name as long as the server host is reachable using a ping command from the machine that will run your program.

You can also specify one or more failover servers by adding additional hosts, as in the following example:

```bash
ServerNode=myServer:39015,failoverserver1:39015,failoverserver2:39015
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Connection String: ](The port number)]

The port number to be used in the connection string include the instance number which is assigned when a new instance is created.

And, the pattern used for port assignments is: ***3&lt;instance number&gt;&lt;service port&gt;***.

To execute SQL, you will need to connect to the ***SQL/MDX*** service port.

#### **SAP HANA, express edition 1.0 and Single Database Container**

In these earlier release, Single Database Container was the default installation mode and the default instance number was **00**.

The ***SQL/MDX*** service port to access the database of a single tenant system is **15**, so the port is **30015**.

To access a specific database, you will use the **`databasename`** in the option parameter.

#### **SAP HANA, express edition 2.0 and Multi Database Container**

With newer releases, Multi Database Container are installed by default and the instance number used by default value is **90**, unless specified otherwise during the setup.

With Multi Database Container you must consider the SQL/MDX service port to access the System database (also called SYSTEMDB) and the Tenant databases.

The ***SQL/MDX*** service port to access the **SYSTEMDB** ***System database*** of a multitenant system is **13**, so the port is **39013**

The ***SQL/MDX*** service port to access the **HXE** ***Tenant databases*** of a multitenant system is **15**, so the port is **39015**

Make sure that you can reach the port (using a telnet command) from the computer you will connect using Eclipse.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Connection String: ](The options)]

The ODBC driver supports a full set of options that can become handy when developing your application.

For example, you can use the ***options*** parameter in your DSN to specify the connection current schema, as in the following example:

```bash
CURRENTSCHEMA=ML_USER
```

For more information about the **ODBC Connection Properties**, you can check the <a href="https://help.sap.com/viewer/0eec0d68141541d1b07893a39944924e/2.0.02/en-US/7cab593774474f2f8db335710b2f5c50.html" target="new">online documentation</a>

[DONE]
[ACCORDION-END]
