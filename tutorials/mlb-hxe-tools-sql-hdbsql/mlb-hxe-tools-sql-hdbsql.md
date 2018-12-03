---
title: Use HDBSQL as a SQL query tool with SAP HANA, express edition
description: Provide details on the installation the SAP HANA client and instruction to connect a SAP HANA, express edition instance using the HDBSQL tool.
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>how-to, tutorial>intermediate, products>sap-hana\, express-edition ]
time: 10
---

## Details
### You will learn
  - How to install the SAP HANA client for SAP HANA, express edition
  - How to connect your SAP HANA, express edition instance using the SAP HANA HDBSQL tool

SAP HANA HDBSQL is a command line tool for executing commands on SAP HANA databases. The HDBSQL tools support an interactive mode, a non-interactive mode, and a file mode. HDBSQL provide a large number of features like variable substitution, use of input and output files, formatting options, and more.

For alternate options, you can also check the following link: [Select a SQL query tool for SAP HANA, express edition](https://developers.sap.com/tutorials/mlb-hxe-tools-sql.html).


[ACCORDION-BEGIN [Prerequisites: ](Download & Install the SAP HANA HDB client)]

Before you can proceed with the next steps, you will need to complete the **Installing SAP HANA HDB Client** for your target platform from the [Install the SAP HANA, express edition clients](https://developers.sap.com/group.hxe-install-clients.html) group.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Locate the HDBSQL tool)]

The HDBSQL tool, which is installed as part of the SAP HANA client, is located at (unless specified otherwise during the installation):

 - on Linux and UNIX platforms `/usr/sap/hdbclient/`
 - on Microsoft Windows platforms: `C:\Program Files\SAP\hdbclient\`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test your connections)]

As stated previously, you can use an interactive mode, where you will type/paste your command interactively.

You can also pass the SQL as part of the command:

```
hdbsql -n <host>:<port> -u <user> -p <password> "SQL statement"
```
or using an input SQL file:
```
hdbsql -n <host>:<port> -u <user> -p <password> -I "SQL file full path""
```

#### **Connection details**

HDBSQL support both the use of instance number or the port number to connect to your instances:

```
hdbsql -i <instance nr> -n <host> -u <user> -p <password>
```
or
```
hdbsql -n <host>:<port> -u <user> -p <password>
```

#### **Connect to the SYSTEM Database**

Open a terminal console and execute the following command:

```shell
cd <SAP HANA client installation directory>

hdbsql -n <host>:39013 -u SYSTEM -p <password> "SELECT DATABASE_NAME, ACTIVE_STATUS, RESTART_MODE FROM M_DATABASES ORDER BY 1;"
```

Make sure you adjust the SAP HANA Client installation directory, the host and password details.

The console output should look like this:

```
DATABASE_NAME,ACTIVE_STATUS,RESTART_MODE
"HXE","YES","DEFAULT"
"SYSTEMDB","YES","DEFAULT"

2 rows selected (overall time 116.252 msec; server time 376 usec)
```

#### **Connect to the HXE tenant**

Open a terminal console and execute the following command:

```shell
cd <SAP HANA Client installation directory>

hdbsql -n <host>:39015 -u SYSTEM -p <password> "SELECT DATABASE_NAME, ACTIVE_STATUS, RESTART_MODE FROM M_DATABASES ORDER BY 1;"
```

Make sure you adjust the SAP HANA Client installation directory, the host and password details.

The console output should look like this:

```
DATABASE_NAME,ACTIVE_STATUS,RESTART_MODE
"HXE","YES","DEFAULT"

1 row selected (overall time 51.027 msec; server time 312 usec)
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Validation)]

Based on the available HDBSQL options listed in the [SAP HANA HDBSQL Options](https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.02/en-US/c24d054bbb571014b253ac5d6943b5bd.html) documentation, provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Connection String: ](The server host)]

You can use the IP address, the host name or the fully qualified name as long as the server host is reachable using a ping command from the machine that will run your program.

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

[ACCORDION-BEGIN [Connection String: ](Credentials)]

If you prefer not to input your password as a command line parameter, you can omit the parameter and you will be prompted for it.

Alternatively, you can use a **Secure User Store** (`a.k.a.` ***`hdbuserstore`***).

When using the ***`hdbuserstore`***, your credentials (including your user name and server details) are saved and secured in a store that can used directly from the HDBSQL tool but not only.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Connection String: ](The options)]

For more information about the **HDBSQL options**, you can check the <a href="https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.02/en-US/c24d054bbb571014b253ac5d6943b5bd.html" target="new">online documentation</a>.

You also can check the [SAP HANA HDBSQL (Command-Line Reference)](https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.02/en-US/c22c67c3bb571014afebeb4a76c3d95d.html) for more details.

[DONE]
[ACCORDION-END]
