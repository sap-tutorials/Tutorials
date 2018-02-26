---
title: Connect to SAP HANA, express edition using Python
description: In this how-to, you will learn how to connect to SAP HANA, express edition using the Python PyDBAPI API
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>intermediate, products>sap-hana, products>sap-hana\,-express-edition , tutorial>how-to ]
---

## Prerequisites  
- **Proficiency:** Intermediate
- Python is installed and running properly on your system

## How-To Details
In many Python applications, you would need access to a database for storing, retrieving and manipulating data. The SAP HANA client includes a dedicated Python interface (`PyDBAPI`).

In this how-to, you will learn how to connect to HANA Express Edition using the Python interface.

For more details about the Python driver SAP HANA, please refer to the [online documentation](https://help.sap.com/viewer/0eec0d68141541d1b07893a39944924e/2.0.02/en-US/f3b8fabf34324302b123297cdbe710f0.html).

### Time to Complete
**10 Min**.

[ACCORDION-BEGIN [Step 1: ](Install the SAP HANA client)]

Follow the instructions from the following [How to](https://www.sap.com/developer/how-tos/2016/12/hxe-ua-howto-installing-clients.html) to complete your SAP HANA client installation.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install the Python driver)]

Once the **SAP HANA Client Software Packages** installation is completed, you can now proceed with the Python driver setup.

The driver is a Python package that needs to be installed using Python utility program (either `pip` or `easy_install`). The driver package will be available at:

| Platform                  | Location                                          |
|---------------------------|---------------------------------------------------|
| Microsoft Windows         | `C:\Program Files\SAP\hdbclient\hdbcli-X.Y.Z.zip` |
| Linux and UNIX            | `/usr/sap/hdbclient/hdbcli-X.Y.Z.zip`             |

Depending on your target platform and the Python utility program available on your system, choose one of the following command:

- `pip` install

```bash
pip install "<hdbcli-N.N.N.zip path>"
```

- `easy_install`

```bash
easy_install "<hdbcli-N.N.N.zip path>"
```

When the process completes, you should get the following messages (or similar) in your console:

```
Processing hdbcli-2.2.26.tar.gz
Writing /tmp/easy_install-5nJURq/hdbcli-2.2.26/setup.cfg
Running hdbcli-2.2.26/setup.py -q bdist_egg --dist-dir /tmp/easy_install-5nJURq/hdbcli-2.2.26/egg-dist-tmp-0VMt7k
zip_safe flag not set; analyzing archive contents...
Adding hdbcli 2.2.26 to easy-install.pth file

Installed /usr/lib/python2.7/site-packages/hdbcli-2.2.26-py2.7-linux-x86_64.egg
Processing dependencies for hdbcli==2.2.26
Finished processing dependencies for hdbcli==2.2.26
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test your Connectivity)]

Create a new Python file named `hxe-connect.py`, Then past the following content:

```python
import sys

from hdbcli import dbapi

connection = dbapi.connect('<server-host>', <port>, '<username>', '<password>')

#This statement prints true if the connection is successfully established
print connection.isconnected()
```

Before saving, adjust the values of :

 - the **Connection String** with the ***<server-host>*** & ***<port>***
 - the credentials with the ***<username>*** & ***<password>***

For more details about the **Connection String**, please check the last few steps.

Execute the code using the following command:

```python
python hxe-connect.py
```

You should see the following output:

```
True
```

Congratulations: You have just connected to HANA using Python!

[ACCORDION-END]

[ACCORDION-BEGIN [Connection String: ](The server host)]

You can use the IP address, the host name or the fully qualified name as long as the server host is reachable using a ping command from the machine that will run your program.

[ACCORDION-END]

[ACCORDION-BEGIN [Connection String: ](The port number)]

The port number to be used in the connection string include the instance number which is assigned when a new instance is created.

> ### **Note:** The default value changed between version 1.0 & 2.0 of **SAP HANA, express edition**, so here is a quick recap:
> - **SAP HANA 1.0, express edition**, the default the instance number is `00`
> - **SAP HANA 2.0, express edition**, the default the instance number is `90`


Prior to **SAP HANA 1.0, express edition** SPS12, there was no concept of ***Multi Database Container*** (MDC). It was still possible to create multiple database in a single instance, but not using container isolation.

With this mode, the port number will use the following pattern: `3<instance number>15`, and with the default instance number `00`, the port will be `30015`. To access a specific database, you will use the `databasename` in the option parameter.

Since **SAP HANA 1.0, express edition** SPS12 & **SAP HANA 2.0, express edition**, the ***Multi Database Container*** is the default mode.

This means that any created instance will have a **System Database** and potentially a series of **Tenant Databases**.

The **System Database** (also called **SYSTEMDB**) can be accessed via the following port: `3<instance number>15`.

However, with your **Tenant Databases**, the SQL port is assigned dynamically at the creation time and follows a different pattern: `3<instance number><SQL port>`.

You can determine the SQL port to use for a particular tenant database using the `M_SERVICES` system view, either from the tenant database itself or from the system database, using the following SQL.

- ***From the system database:***

```SQL
SELECT
	  DATABASE_NAME
	, SERVICE_NAME
	, PORT
	, SQL_PORT
	, (PORT + 2) HTTP_PORT
FROM
	SYS_DATABASES.M_SERVICES
WHERE
  (
        SERVICE_NAME      = 'indexserver'
    and COORDINATOR_TYPE  = 'MASTER'
  )
  or SERVICE_NAME = 'xsengine'
;
```

- ***From a particular tenant database:***

```SQL                
SELECT
	  SERVICE_NAME
	, PORT
	, SQL_PORT
	, (PORT + 2) HTTP_PORT
FROM
	SYS.M_SERVICES
WHERE
  (
        SERVICE_NAME      = 'indexserver'
    and COORDINATOR_TYPE  = 'MASTER'
  )
  or SERVICE_NAME = 'xsengine'
;
```

[ACCORDION-END]
