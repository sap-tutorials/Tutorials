---
title: Use Open Database Connectivity to Connect to SAP ASE in SAP HANA Cloud
description: Connect to SAP ASE in SAP HANA Cloud with the help of an odbc.ini file using Open Database Connectivity (ODBC).
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-adaptive-server-enterprise]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
- You need to provision an SAP HANA Cloud, SAP ASE instance for use.
- You require a Linux OS to follow the instructions in this tutorial.

## Details
### You will learn
- What an odbcinst.ini file contains
- About the two ways of connecting to an instance using ODBC
- How to verify the connection to an SAP ASE instance in SAP HANA Cloud


In this tutorial group, you will learn how to connect to your SAP HANA Cloud, SAP Adaptive Server Enterprise database.

There are a few tools you can use to access and connect to the database. With these programming interfaces and drivers, you can then query and manipulate the data available there.
In each tutorial of this tutorial group, you can learn how to connect using a different interface or driver:

-	[Open Client Server](hana-cloud-ase-cloud-connection-1) (OCS), which is the native driver for SAP ASE
-	This tutorial will cover <sub-style="font-size:30px">&#9755;</sub> **ODBC**
-	[iSQL](hana-cloud-ase-cloud-connection-3)
-	[Python Sybase Driver](hana-cloud-ase-cloud-connection-4)
-	[`jConnect`(Link)](hana-cloud-ase-cloud-connection-5)

---

[ACCORDION-BEGIN [Step 1: ](Prepare to connect)]


Before you can connect to any SAP HANA Cloud, SAP ASE instance, you need to make sure the instance is running in SAP HANA Cloud Central.

Then, you need to make sure you have the SAP HANA Cloud, SAP ASE Client SDK drivers are installed in a Linux system. To learn more about this, check out our [technical documentation](https://help.sap.com/viewer/46353c3b724f4934bb0671dd82044acd/LATEST/en-US/c3cf39f75b744b8799a01a9a154f2e9c.htmlhttps://help.sap.com/viewer/46353c3b724f4934bb0671dd82044acd/LATEST/en-US/c3cf39f75b744b8799a01a9a154f2e9c.html). There you will find a description and the link to download the SDK.

Then, to set up a connection, you will need the SQL Endpoint of the instance. To get it, follow these steps:

1.	In SAP HANA Cloud Central, click on the three dots button on the right side of the row for your SAP ASE instance.

2.	In the drop-down menu, select SQL Endpoint. This is the host name that you need to enter in the interfaces file to connect with your client.

Now that your preparations are done, you can get started. In the coming steps, learn how to connect using the Open Database Connectivity (ODBC).

You can either follow the steps below or watch this video:

<iframe width="560" height="315" src="https://www.youtube.com/embed/7IXfYhmjIak" frameborder="0" allowfullscreen></iframe>


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Update odbc.ini file)]


To connect to an SAP HANA Cloud, adaptive server enterprise instance using ODBC, you can choose one of two ways. The first way is to use a DSN, which is within your `odbc.ini` file. The second way is to use DSN lists, which is when you provide the parameters in the connection string within your code. For this second type of connection, you don't need to refer to your `odbc.ini` file.

> Complete the Steps 2 and 3 to connect using the odbc.ini file. If you want to connect to your instance without using the file, you must also complete the Step 4.

1.	To start, go to your PuTTy session and connect to your Sybase directory.

2.	If you need to check the location of your ODBC driver, you can do that by checking the contents of the `odbcinst.ini` file. Your driver should be on the directory: `sap/python_client/DataAccess64/ODBC/lib/libsybdrvodb.so`

    Use the command given below to verify the location:

    ```Shell/Bash
cat odbcinst.ini
```

3.	Now access the odbc.ini file by using:

    ```Shell/Bash
vi odbc.ini
```

4.	You need to fill out some of the fields in this file. In the first field, which should be empty, insert the name of the instance you want to connect to. Here is an example:

    ```
<DEMO_INSTANCE_NAME>
```

5.	In the field `Server=`, insert the SQL Endpoint of your instance. Remember to remove the colon and the port number at the end.

6.	Enter the port number in the field named `Port=`.

7.	Now you need to add two new lines in this `odbc.ini` file:

    ```Shell/Bash
Encryption=ssl
TrustedFile=<TRUSTED FILE POINTING TO YOUR CERTIFICATE>
```

    You can find your certificate within your SDK folder.

8.	Save the changes to your file.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Connect to your instance using DSN)]

1.	Use the following command to go to your `testcode` directory:

    ```Shell/Bash
cd testcode
ls -arlt
```

2.	Next, use this command to connect using the first method:

    ```Shell/Bash
cat python_test1.py
```

    For example, after you run your connection script, the output would look like this:

    ```
import pyodbc
conn = pyodbc.connect('DSN=<DEMO_INSTANCE_NAME>')
conn = conn.cursor()
cursor.execute("select @@servername")
row = cursor.fetchone()
while row:
	print(row[0])
row = cursor.fetchone()
    ```

    After running the script, you will be connected to your ASE instance and will be able to access and use all data there.

3.	To verify the connection to your instance, run the following command:

    ```Shell/Bash
python3 python_test1.py
```

This will show your instance name as the output.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Connect to your instance using DSN list (optional))]


1.	The second way of connecting, by using DSN list, uses a fully qualified connection string. Run the following command:

    ```Shell/Bash
cat python_test2.py
```

    You can see that, in the connection string, you should directly refer to the:

    -	Server
    -	Port
    -	Username
    -	Password
    -	`Encrypt=ssl`
    -	`TrustedFile=<TRUSTED FILE POINTING TO YOUR CERTIFICATE>`
    -	Location of the driver, which can be found on your `odbcinst.ini` file

    Here is an example:

    ```
import pyodbc
conn = pyodbc.connect(server='<>', port=<>, user='<>', password='<>', driver='', Encryption='<>', TrustedFile='<>')
conn = conn.cursor()
cursor.execute("select @@servername")
row = cursor.fetchone()
while row:
	print(row[0])
row = cursor.fetchone()
```

2.	To verify the connection to your instance, run the following command:

    ```Shell/Bash
python3 python_test2.py
```

    This will show your instance name as the output.

> Now, you know how to connect to your instance using ODBC. In the next tutorial, you can learn how to connect to an SAP ASE database in SAP HANA Cloud using isql.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]



[VALIDATE_7]
[ACCORDION-END]

---
