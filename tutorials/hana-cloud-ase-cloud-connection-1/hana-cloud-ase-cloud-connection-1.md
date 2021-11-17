---
title: Use Open Client Server to Connect to SAP ASE in SAP HANA Cloud
description: Connect to SAP ASE in SAP HANA Cloud with the help of an interfaces file using Open Client Server (OCS).
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-adaptive-server-enterprise]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
- You need to provision an SAP HANA Cloud, SAP ASE instance for use.
- You require a Linux OS to follow the instructions in this tutorial.
- To make a SQL connection with your SAP ASE instance, you need an updated version of the ASE client software. It needs to support the SNI protocol to connect through an HTTPS server on Port 443.

## Details
### You will learn
- How to connect to your superuser account
- How to create an interfaces file
- How to verify the connection to the database


In this tutorial group, you will learn how to connect to your SAP HANA Cloud, SAP Adaptive Server Enterprise database.

There are a few tools you can use to access and connect to the database. With these programming interfaces and drivers, you can then query and manipulate the data available there.
In each tutorial of this tutorial group, you can learn how to connect using a different interface or driver:

-	This tutorial will cover <sub-style="font-size:30px">&#9755;</sub> **Open Client Server (OCS), which is the native driver for SAP ASE**
-	[ODBC](hana-cloud-ase-cloud-connection-2)
-	[iSQL](hana-cloud-ase-cloud-connection-3)
-	[Python Sybase Driver](hana-cloud-ase-cloud-connection-4)
-	[`jConnect`(Link)](hana-cloud-ase-cloud-connection-5)


---

[ACCORDION-BEGIN [Step 1: ](Prepare to connect)]

Before you can connect to any SAP HANA Cloud, SAP ASE instance, you need to make sure the instance is running in SAP HANA Cloud Central.

Then, you need to make sure you have the SAP HANA Cloud, SAP ASE Client SDK drivers installed in a Linux system. To learn more about this, check out our [technical documentation](https://help.sap.com/viewer/46353c3b724f4934bb0671dd82044acd/LATEST/en-US/c3cf39f75b744b8799a01a9a154f2e9c.html). There, you will find a description and the link to download the SDK.

Then, to set up a connection, you will need the SQL Endpoint of the instance. To get it, follow these steps:

1.	In SAP HANA Cloud Central, click on the three dots button on the right side of the row for your SAP ASE instance.

2.	In the drop-down menu, select SQL Endpoint. This is the host name that you need to enter in the interfaces file to connect with your client.

Now that your preparations are done, you can get started. In the coming steps, learn how to connect using the Open Client Server (OCS).

You can either follow the steps below or watch this video:

<iframe width="560" height="315" src="https://www.youtube.com/embed/cAI7zfNsbj8" frameborder="0" allowfullscreen></iframe>

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Connect to your superuser account)]

This is very similar to setting up an SAP ASE interfaces definition on-premise, but you will see a few differences in the process.

Follow these steps for Linux operating systems:

1.	To set up an open client connection to your database, go to your Linux instance and use your superuser account to run the following in the command prompt:

    ```Shell/Bash
sudo -i
```

2.	Next, enter your password to login.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create interfaces file)]

Continue with these steps in the command prompt:

1.	Now you can create and access an interfaces file. Make sure to create it in the folder `/ase/release#`. You can do this with the data source edit utility or the Vim text editor using the following command:

    ```Shell/Bash
vim interfaces
```

2.	In the Vim editor, enter the instance name as the first line. Here is an example:

    ```
<DEMO_INSTANCE_NAME>
```

3.	In the next line, press Tab and then type the following code, pasting the SQL Endpoint of your instance that you had copied between the <> characters:

    ```Shell/Bash
query tcp ether <SQL_Endpoint>
```

4.	Delete the colon between the server and the port number of your SQL Endpoint and insert a space in between. Here is an example:

    ```
query tcp ether 1a1a1a1a-2b2b-3c3c-4d4d-5e5e5e5e5e5e.ase.hxtp.xxxxxxxxxxxxxx.com 443
```

5.	Now, you need to add a space after the port number and specify a new parameter, which is a new requirement for cloud databases, since all connections must be encrypted. Here is an example:

    ```
query tcp ether 1a1a1a1a-2b2b-3c3c-4d4d-5e5e5e5e5e5e.ase.hxtp.xxxxxxxxxxxxxx.com 443 ssl="CN=hanacloud.ondemand.com"
```

6.	After this, you can save the interfaces file and exit the editor.

7.	Before connecting to your database, run the following command on your ASE release folder:

    ```Shell/Bash
source SYBASE.sh
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify connection to the database)]


1.	Run the following command to confirm that the connection was properly configured, and you can connect to the database:

    ```Shell/Bash
which isql
```

2.	Now, to connect to your instance, run the following command:

    ```Shell/Bash
isql -Uaseadmin -S <INSTANCE_NAME>
```

    > Please note that `aseadmin` is the initial database user created when you provisioned your instance.

3.	Next, enter your instance password. Now you will be able to connect to your instance using isql.


4.	Use the command below to see all databases in your system:

    ```Shell/Bash
select name from sysdatabases
go
```

5.	At this point, you will not see any user databases in the list as they have not been created yet. But you will be able to see all system databases.

    Now you have an open client connection to the database. And the good news is that you can share the same client software for SAP ASE instances that are both on-premise and in SAP HANA Cloud. Note that the SAP ASE instance looks very much like a regular ASE16 system.

> Now, you know how to connect to your instance using open client connection. In the next tutorial, you can learn how to connect to an SAP ASE database in SAP HANA Cloud using Open Database Connectivity (ODBC).




[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]



[VALIDATE_7]
[ACCORDION-END]

---
