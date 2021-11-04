---
title: Use iSQL to Connect to SAP ASE in SAP HANA Cloud
description: Connect to SAP ASE in SAP HANA Cloud with the help of an interfaces file using isql.
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
- How to create an interfaces file
- How to establish connection to an instance using isql


In this tutorial group, you will learn how to connect to your SAP HANA Cloud, SAP Adaptive Server Enterprise database.

There are a few tools you can use to access and connect to the database. With these programming interfaces and drivers, you can then query and manipulate the data available there.
In each tutorial of this tutorial group, you can learn how to connect using a different interface or driver:

-	[Open Client Server](hana-cloud-ase-cloud-connection-1) (OCS), which is the native driver for SAP ASE
- [ODBC](hana-cloud-ase-cloud-connection-2)
-	This tutorial will cover <sub-style="font-size:30px">&#9755;</sub> **iSQL**
-	[Python Sybase Driver](hana-cloud-ase-cloud-connection-4)
-	[`jConnect`(Link)](hana-cloud-ase-cloud-connection-5)

---

[ACCORDION-BEGIN [Step 1: ](Prepare to connect)]


Before you can connect to any SAP HANA Cloud, SAP ASE instance, you need to make sure the instance is running in SAP HANA Cloud Central.

Then, you need to make sure you have the SAP HANA Cloud, SAP ASE Client SDK drivers are installed in a Linux system. To learn more about this, check out our [technical documentation](https://help.sap.com/viewer/46353c3b724f4934bb0671dd82044acd/LATEST/en-US/c3cf39f75b744b8799a01a9a154f2e9c.html). There you will find a description and the link to download the SDK.

Then, to set up a connection, you will need the SQL Endpoint of the instance. To get it, follow these steps:

1.	In SAP HANA Cloud Central, click on the three dots button on the right side of the row for your SAP ASE instance.
2.	In the drop-down menu, select SQL Endpoint. This is the host name that you need to enter in the interfaces file to connect with your client.

Now that your preparations are done, you can get started. In the coming steps, learn how to connect using isql.

You can either follow the steps below or watch this video:
<iframe width="560" height="315" src="https://www.youtube.com/embed/bapd_ncOPzI" frameborder="0" allowfullscreen></iframe>

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an interfaces file)]

To connect to your SAP ASE database in SAP HANA Cloud using isql, you must first create an interfaces file. To do that, you will need the SQL Endpoint of your instance.

1.	Switch to your PuTTy session. Go to your Sybase directory to create the interface file. You can use any editor you wish, but in this example, you will see the use of Vi editor. Run the following command:

    ```Shell/Bash
vi interfaces
```

2.	Note that the file is initially empty. The first entry you need to create is the name of your instance. Here is an example:

    ```
<DEMO_ASE_INSTANCE_NAME>
```
3.	Since the instance is running remotely and not locally, you need to add a query line. Here is an example:

    ```
query tcp ether 1a1a1a1a-2b2b-3c3c-4d4d-5e5e5e5e5e5e.ase.hxtp.xxxxxxxxxxxxxx.com 443 ssl="CN=hanacloud.ondemand.com"
```

    Delete the colon between the server and the port number of your SQL Endpoint and insert a space in between.

4.	Save and exit.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Connect to your database)]

1.	To connect to the server, enter the following syntax:

    ```Shell/Bash
isql -Uaseadmin -S<DEMO_ASE_INSTANCE_NAME>
```

2.	Provide your instance password once prompted.

You are now connected to your SAP ASE database instance in SAP HANA Cloud.

> Now, you know how to connect to your instance using isql. In the next tutorial, you can learn how to connect to an SAP ASE database in SAP HANA Cloud using the Python Sybase Driver.



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test yourself)]



[VALIDATE_1]
[ACCORDION-END]

---
