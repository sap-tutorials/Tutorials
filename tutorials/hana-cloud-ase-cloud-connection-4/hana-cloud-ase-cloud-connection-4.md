---
title: Use Python Sybase Driver to Connect to SAP ASE in SAP HANA Cloud
description: Connect to SAP ASE in SAP HANA Cloud with the help of an interfaces file using Python Sybase Driver.
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-adaptive-server-enterprise]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
- You need to have a running SAP HANA Cloud, SAP ASE instance.
- You require a Linux OS to follow the instructions in this tutorial.

## Details
### You will learn
- How to create an interfaces file
- How to connect to an instance using Python Sybase Driver
- How to verify the connection to an SAP ASE instance in SAP HANA Cloud


In this tutorial group, you will learn how to connect to your SAP HANA Cloud, SAP Adaptive Server Enterprise database.

There are a few tools you can use to access and connect to the database. With these programming interfaces and drivers, you can then query and manipulate the data available there.
In each tutorial of this tutorial group, you can learn how to connect using a different interface or driver:

-	[Open Client Server](hana-cloud-ase-cloud-connection-1) (OCS), which is the native driver for SAP ASE
- [ODBC](hana-cloud-ase-cloud-connection-2)
-	[iSQL](hana-cloud-ase-cloud-connection-3)
-	This tutorial will cover <sub-style="font-size:30px">&#9755;</sub> **Python Sybase Driver**
-	[`jConnect`(Link)](hana-cloud-ase-cloud-connection-5)


---

[ACCORDION-BEGIN [Step 1: ](Prepare to connect)]

Before you can connect to any SAP HANA Cloud, SAP ASE instance, you need to make sure the instance is running in SAP HANA Cloud Central.

Then, you need to make sure you have the SAP HANA Cloud, SAP ASE Client SDK drivers are installed in a Linux system. To learn more about this, check out our [technical documentation](https://help.sap.com/viewer/46353c3b724f4934bb0671dd82044acd/LATEST/en-US/c3cf39f75b744b8799a01a9a154f2e9c.html). There you will find a description and the link to download the SDK.

Then, to set up a connection, you will need the SQL Endpoint of the instance. To get it, follow these steps:

1.	In SAP HANA Cloud Central, click on the three dots button on the right side of the row for your SAP ASE instance.

2.	In the drop-down menu, select SQL Endpoint. This is the host name that you need to enter in the interfaces file to connect with your client.

Now that your preparations are done, you can get started.

In the coming steps, learn how to connect using the Python Sybase Driver.

You can either follow the steps below or watch this video:

<iframe width="560" height="315" src="https://www.youtube.com/embed/gz-LRO2ulkQ" frameborder="0" allowfullscreen></iframe>

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Edit the interfaces file)]

To connect to a running SAP ASE database in SAP HANA Cloud using the Python Sybase Driver, follow these steps:

1.	Download and install the Python Sybase Driver from [here](https://sourceforge.net/projects/python-sybase/).

2.	First, open a PuTTy session and go to the Sybase directory.

3.	The Python Sybase Driver uses an interface file, so you need to make sure there is an entry in the interfaces file for this ASE instance that you want to connect to.

4.	Open your text editor, in this example, you will see the Vi text editor:

    ```Shell/Bash
vi interfaces
```

5.	Enter the name of the instance you want to connect to.

6.	Under it, add a query line following this structure:

    ```Shell/Bash
query tcp ether <SQL Endpoint> <Port number> ssl="CN=hanacloud.ondemand.com"
```

    Delete the colon between the server and the port number of your SQL Endpoint and insert a space in between. Here is an example:

    ```
query tcp ether 1a1a1a1a-2b2b-3c3c-4d4d-5e5e5e5e5e5e.ase.hxtp.xxxxxxxxxxxxxx.com 443 ssl="CN=hanacloud.ondemand.com"
```

7.	Save the interfaces file and exit.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Connect to the database instance)]

1.	Now check that your Python path is pointing to the directory where your Python Sybase Driver is located. Run the following command:

    ```Shell/Bash
env | grep PYTHONPATH
```

    An example of what this path looks like is:

    ```
PYTHONPATH=/sap/python_client/OCS-16_0/python/python34_64r/lib
```

2.	From the `testcode` directory, run the following command given below:

    ```Shell/Bash
cat python_test3.py
```
    After you run your python connection script, the output looks like this:

    ```
import sypydb
conn = sypydb.connect("aseadmin", "YOUR_PASSWORD", "YOUR_INSTANCE_NAME", "chainxacts=off")
cursor = conn.cursor ()
cursor.execute("select @@servername")
row = cursor.fetchone()
while row:
	print(row[0])
	row = cursor.fetchone()
```
    The `python_test3.py` program imports `sypydb` to establish connection using username, password, and instance name information.

3.	To verify the connection to your instance, run the following command:

    ```Shell/Bash
python3 python_test3.py
```
    This will show your instance name as the output.

And that is it. You are connected to your SAP ASE database in SAP HANA Cloud using the Python Sybase Driver.

> Now, you know how to connect to your instance using Python Sybase Driver. In the next tutorial, you can learn how to connect to an SAP ASE database in SAP HANA Cloud using `jConnect`.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Test yourself)]



[VALIDATE_1]
[ACCORDION-END]

---
