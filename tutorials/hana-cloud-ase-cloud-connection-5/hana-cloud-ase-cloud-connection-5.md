---
title: Use jConnect to Connect to SAP ASE in SAP HANA Cloud
description: Connect to SAP ASE in SAP HANA Cloud with the help of an interfaces file using jConnect.
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
- How to connect to an instance using **`jConnect`**
- How to verify the connection to an SAP ASE instance in SAP HANA Cloud


In this tutorial group, you will learn how to connect to your SAP HANA Cloud, SAP Adaptive Server Enterprise database.

There are a few tools you can use to access and connect to the database. With these programming interfaces and drivers, you can then query and manipulate the data available there.
In each tutorial of this tutorial group, you can learn how to connect using a different interface or driver:

-	[Open Client Server](hana-cloud-ase-cloud-connection-1) (OCS), which is the native driver for SAP ASE
- [ODBC](hana-cloud-ase-cloud-connection-2)
-	[iSQL](hana-cloud-ase-cloud-connection-3)
-	[Python Sybase Driver](hana-cloud-ase-cloud-connection-4)
-	This tutorial will cover <sub-style="font-size:30px">&#9755;</sub> **`jConnect`**


---

[ACCORDION-BEGIN [Step 1: ](Prepare to connect)]

Before you can connect to any SAP HANA Cloud, SAP ASE instance, you need to make sure the instance is running in SAP HANA Cloud Central.

Then, you need to make sure you have the SAP HANA Cloud, SAP ASE Client SDK drivers are installed in a Linux system. To learn more about this, check out our technical documentation. There you will find a description and the link to download the SDK.

Then, to set up a connection, you will need the SQL Endpoint of the instance. To get it, follow these steps:

1.	In SAP HANA Cloud Central, click on the three dots button on the right side of the row for your SAP ASE instance.

2.	In the drop-down menu, select SQL Endpoint. This is the host name that you need to enter in the interfaces file to connect with your client.

Now that your preparations are done, you can get started. In the coming steps, learn how to connect using `jConnect`.

You can either follow the steps below or watch this video:

<iframe width="560" height="315" src="https://www.youtube.com/embed/JlSfBOG0ASY" frameborder="0" allowfullscreen></iframe>


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Edit the interfaces file)]


There are two ways to connect to a running SAP ASE database in SAP HANA Cloud using `jConnect`. One is using the interfaces file and the second is using the code within the Java code in your program.

> Complete the Steps 2 and 3 to connect using the interfaces file. If you want to connect to your instance without using the interfaces file, you must also complete the Step 4.

1.	First, open a PuTTy session and go to the Sybase directory.

2.	The Python Sybase Driver uses an interface file, so you need to make sure there is an entry in the interfaces file for this ASE instance that you want to connect to.

3.	Open your editor. In this example, you will see a Vi editor:

    ```Shell/Bash
vi interfaces
```
4.	Add the name of the instance you want to connect to, but also add a "j" after the name without adding any spaces. Here is an example:

    ```
<DEMO_ASE_INSTANCE_NAME>j
```

5.	Under the instance name, add a query line following this structure:

    ```Shell/Bash
query tcp ether <SQL Endpoint> <Port number> ssl
```

    Delete the colon between the server and the port number of your SQL Endpoint and insert a space in between. Here is an example:

    ```
query tcp ether 1a1a1a1a-2b2b-3c3c-4d4d-5e5e5e5e.ase.hxtp.xxxxxxxxxxxxxx.com 443 ssl
```

6.	Save the interfaces file and exit.



[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Compile the Java connection script)]


1.	Use the following command to check your Java connection
script using Vi editor:

    ```Shell/Bash
cat  java_test1.java
```
    Here is an example of how your Java connection script should look like:

    ```
import java.util.Properties;
import java.sql.*;
import com.sybase.jdbcx.*;
public class java_test1
{
        public static void main(String[] args) throws ClassNotFoundException, SQLException, InstantiationException, IllegalAccessException
        {
		String url="jdbc:sybase:Tds:";
		SybConnection dbconn=null;
                 ResultSet dbList_rs = null;
                 Statement myStmt = null;
		SybDriver sybDriver =
 (SybDriver) Class.forName("com.sybase.jdbc42.jdbc.SybDriver").newInstance();
		sybDriver.setVersion(com.sybase.jdbcx.SybDriver.VERSION_LATEST);
		DriverManager.registerDriver(sybDriver);
		url = "jdbc:sybase:jndi:file:///sap/python_client/interfaces?ASEInstance1j";
		// set connection/login properties
		Properties connectProps = new Properties();
		// Get the username that will be connecting
		connectProps.put("USER","aseadmin");
		connectProps.put("PASSWORD","YOUR-PASSWORD");
		connectProps.put("CHARSET","iso_1");
connectProps.put("ENABLE_SSL", "true");
connectProps.put("CN", "hanacloud.ondemand.com");
connectProps.put("SYBSOCKET_ FACTORY", "hanacloud.ondemand.com");
		dbconn = (SybConnection) DriverManager.getConnection(url, connectProps);
myStmt = dbconn.createStatement();
dbList_rs = myStmt.executeQuery("select @@servername");
while (dbList_rs.next())
{
			System.out.println(dbList_rs.getString(1));
}
dbList_rs.close();
myStmt.close();
		dbconn.close();
	}
}
```

    There, you can see the entry you made previously in the interfaces file (<DEMO_ASE_INSTANCE_NAME>j).

2.	Now, you can compile your Java connection script using the following command:

    ```Shell/Bash
compile java_test1
```
3.	Then you can run the script using this command:

    ```Shell/Bash
run java_test1
```
4.	Your connection is established.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Connect without an interfaces file (optional))]

Alternatively, you may also connect without using an interfaces file by modifying the java script directly.

1.	Use the following command to modify your Java connection script using Vi editor:

    ```Shell/Bash
vi java_test1.java
```
2.	If you wish to connect using `jConnect` without the interfaces file, you need to modify your code, as shown below. Instead of pointing to the interfaces file on the URL, your URL line should point directly to the SQL Endpoint of the ASE instance, for example:

    ```
url = "jdbc:sybase:Tds:<YOUR_SQL_ENDPOINT>/CN=hanacloud.ondemand.com";
```

    > Note that this time, no changes to the SQL endpoint, like removing the colon as in the previous step, are necessary.

3.	Here is an example of how your Java connection script should look like:

    ```
import java.util.Properties;
import java.sql.*;
import com.sybase.jdbcx.*;
public class java_test1
{
        public static void main(String[] args) throws ClassNotFoundException, SQLException, InstantiationException, IllegalAccessException
        {
String url="jdbc:sybase:Tds:";
SybConnection dbconn=null;
                 ResultSet dbList_rs = null;
                 Statement myStmt = null;
SybDriver sybDriver =
 (SybDriver) Class.forName("com.sybase.jdbc42.jdbc.SybDriver").newInstance();
sybDriver.setVersion(com.sybase.jdbcx.SybDriver.VERSION_LATEST);
DriverManager.registerDriver(sybDriver);
url = "jdbc:sybase:Tds:YOUR-SQL-ENDPOINT/CN=hanacloud.ondemand.com";
// set connection/login properties
Properties connectProps = new Properties();
// Get the username that will be connecting
connectProps.put("USER","aseadmin");
connectProps.put("PASSWORD","YOUR-PASSWORD");
connectProps.put("CHARSET","iso_1");
connectProps.put("ENABLE_SSL", "true");
connectProps.put("CN", "hanacloud.ondemand.com");
connectProps.put("SYBSOCKET_ FACTORY", "hanacloud.ondemand.com");
dbconn = (SybConnection) DriverManager.getConnection(url, connectProps);
myStmt = dbconn.createStatement();
dbList_rs = myStmt.executeQuery("select @@servername");
while (dbList_rs.next())
{
System.out.println(dbList_rs.getString(1));
}
dbList_rs.close();
myStmt.close();
dbconn.close();
}
}
```

4.	Save the changes and exit the editor.
5.	Now, compile your Java connection script using the following command:

    ```Shell/Bash
compile java_test1
```

6.	Then run the connection script to establish the connection to your instance.

    ```Shell/Bash
run java_test1
```
7.	This will show your instance name as the output and you are successfully connected.


> Congratulations! You have completed this tutorial group and you know the different tools and programming interfaces to connect to an SAP HANA Cloud, SAP ASE database.
>
> For more learning materials on **SAP HANA Cloud**, [click here](https://community.sap.com/topics/hana-cloud). Follow our tag in the [**SAP Community**](https://blogs.sap.com/tags/73554900100800002881/) to stay up-to-date on the latest updates and newest content!



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]



[VALIDATE_7]
[ACCORDION-END]

---
