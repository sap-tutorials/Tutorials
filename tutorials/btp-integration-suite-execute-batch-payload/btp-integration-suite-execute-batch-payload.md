---
title: Execute Batch Payload operations using Java Database Connectivity (JDBC) Receiver Adapter
description: Learn how to connect, interact, and perform batch operations on your database via JDBC receiver adapter.

auto_validation: true
time: 40
tags: [ tutorial>intermediate, software-product>sap-integration-suite]
primary_tag: software-product>sap-business-technology-platform
---
            
## Prerequisites
 - You have created a trial account on SAP Business Technology Platform: Get a Free Trial Account on SAP BTP

 - You have enabled Cloud Integration, capability of SAP Integration Suite, as described in Set Up Integration Suite Trial.

 - <https://blogs.sap.com/2018/10/31/configuring-jdbc-adapter-a-step-by-step-guide/>


## Details
### You will learn:

-	To connect Java Database Connectivity (JDBC) receiver adapter with your database (on the receiver system) and send, receive, or update data on the database.

-	To interact with your database using you SQL queries, XML.

-	To perform operations on database using Batch Payload functionality.


The data that you are transferring (sending or receiving) is called payload. The payload sent to the JDBC adapter can be modified. You can modify multiple records in a single payload using either INSERT, UPDATE, or DELETE modes. This can be achieved using Batch Payload functionality.

There are two types of Batch operations:

-	Atomic: Considers each batch operation as a single unit. It updates the whole batch operation successfully or reverts the entire operation to its initial state if anything in the batch operation fails.

-	Non-Atomic- This is based on the behaviour of the database and its driver. It updates all the successfully executed records and throws an exception if anything fails. It does not revert the failed records to its initial state.

Following the steps below, you will be able to setup a sample database, create an integration flow, upload drivers, and data sources required for JDBC receiver adapter. Then, you can create a payload, execute and benefit with Batch Payload functionality.


---

[ACCORDION-BEGIN [Step 1:](Choose your Database)]
Based on your requirements, choose a database you wish to work on. JDBC receiver adapters supports more than ten databases. To know more, see [JDBC: Supported Databases](https://help.sap.com/docs/CLOUD_INTEGRATION/368c481cd6954bdfa5d0435479fd4eaf/88be64412f1b46d684dfba11f2767c5b.html?locale=en-US&version=Cloud)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create data source and upload driver)]
JDBC adapter supports third party and in-house databases on Neo and Cloud foundry BTP environments. Before you establish the connection to these databases, or setup an integration flow, you need to upload drivers (Applicable for HANA, ASE, PostgreSQL only) and add Data Sources.

Refer [Configure JDBC Drivers](https://help.sap.com/docs/CLOUD_INTEGRATION/368c481cd6954bdfa5d0435479fd4eaf/77c7d9550e12494eb600ec82496ef215.html?locale=en-US&version=Cloud)to understand process to upload the drivers and the databases for which you need to do so.
!![Uploading Drivers](Deploy-Driver.png)

Refer [Managing JDBC Data Sources](https://help.sap.com/docs/CLOUD_INTEGRATION/368c481cd6954bdfa5d0435479fd4eaf/4c873fac537248e58767f74e4a74d867.html?locale=en-US&version=Cloud) to understand the process to create a data source.
!![Deploying Data Sources](DS-Deploy.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3:](Connect with Receiver/Database via JDBC adapter)]
Let's create a basic integration flow to understand the basic function of JDBC adapter.
In this basic integration scenario, a sender is connected to the Start event via HTTPS Sender adapter and the End event is connected with receiver via JDBC adapter. This JDBC adapter enables the connection of the integration flow with the receiver's database. It allows you to send, receive, update the data of receiver's database.
!![Initial Integration Flow](Basic-Integration-Flow.png)

You have to set the following properties

-	For the HTTPS adapter:
-	Under **Connection tab**:
-	**Address**: Define your end point.

!![The HTTPS settings](HTTPS-Setting.png)

To understand about other settings, refer to [HTTPS Sender Adapter](https://help.sap.com/docs/CLOUD_INTEGRATION/368c481cd6954bdfa5d0435479fd4eaf/0ae4a78909c4479cbc3cc414250919de.html?locale=en-US&version=Cloud)

-	For the JDBC adapter:
-	Under **Connection tab**:
-	**JDBC Data Source Alias**: Enter the name of your database source to which the adapter should connect to.

!![The JDBC settings](JDBC-Settings.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4:](Create Sample Database)]

To execute this scenario, let us create a table with few columns.

1.  Add a content modifier and then add following query to its **Message Body**. To ensure duplicate row entry is restricted in your database, let us make one of our columns as primary key.

`create table samplejdbc (emp_id integer primary key, emp_name varchar(255));`
!![Add Content Modifier and input Query](Input-Query.png)

2.  Deploy this i-flow. We will now try to execute this via [Postman]( https://www.getpostman.com/). You can use any other HTTPS client application too. In the **Request** section, enter the **End-point** and **Send** the blank query. This will setup connection of Postman with your integration flow.

!![Find Endpoint](Find-Endpoint.png)
!![Send query from Postman](Blank-Query-Send.png)


3.  Before proceeding to the next step, remove Content Modifier from your i-flow. We had added it to create a database successfully.



> For simplicity, we will use Postman to push the payload, later we will also see how to use Groovy Script to push the payload.



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Execute Batch Payload)]

1.  In your i-flow, edit the settings for JDBC Receiver adapter. Under **Connections** tab, check the **Batch Payload** checkbox and select Atomic as **Batch Operation**. We are doing this because, Batch supports XML payload in INSERT, UPDATE and DELETE modes only. Native SQL queries are supported with prepared statements only.
!![Batch Payload Settings](Batch-Payload-Setting.png)


2. Open your HTTP client (in our case Postman) to create a payload. Enter the **Username** and **Password** under **Authorisation** tab.

!![Postman- Authorization](Postman-Authorization.png)

3. To create Payload, we will use INSERT mode and create a payload (with table name sample JDBC) to test Batch Payload scenario. To use these modes, you must follow the syntax. **Send** the following payload from **Request > Body** tab of postman:

```
<root>
	<insert_statement1>
		<dbTableName action="INSERT">
			<table> samplejdbctest</table>
			<access>
				<emp_id>1</emp_id>
				<emp_name hasQuot="Yes">v XXXXX</emp_name>

			</access>
		</dbTableName>
	</insert_statement1>
	<insert_statement2>
		<dbTableName action="INSERT">
			<table>samplejdbctest</table>
			<access>
				<emp_id>2</emp_id>
				<emp_name hasQuot="Yes"> g28</emp_name>

			</access>
		</dbTableName>
	</insert_statement2>
</root>
```

!![Sample Playload](Payload.png)



To check the Message Processing Log (MPL) of this integration flow, from your tenant, go to **Monitoring** > **Overview** > **All Integration flows**. Choose the latest record with your artefact name.

!![Payload Processed](Payload-Processed.png)

> Ensure to check the time stamp to determine which record to check.



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Atomic Batch Operation)]

Atomic Batch Operation doesn't allow any action to be performed on the payload if you have any error. To verify that:


1. Copy the same payload and modify just one record and make it unique. For example, here we are making the **`emp_id`** (of second record) as 3. Thus, making that record unique, but the first record is duplicate. Send this query.

!![Atomic Batch Operation](Atomic-Operation.png)

 Postman displays error. To get more clarity, let's check the MPL.  

 !![MPL for Atomic Batch Operation](MPL-Atomic.png)


2.  Let's confirm, whether unique new data (with `emp_id` 3) is inserted or not in the database. To do this, uncheck the **Batch Mode** checkbox and **Deploy** your i-flow.

!![Batch Mode Off](Batch-Off.png)

Then, **Send** the following query from Postman:

`select * from samplejdbctest`

!![Atomic Batch Operation Verification](Verify-Atomic.png)
This shows that no new data has been inserted. Hence, verifying the Atomic operation

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Non-Atomic Batch Operation)]


Depending upon your driver (which depends upon choice of database), the behaviour of Non- Atomic mode differs.
Let's say for SQL server database, you would be able to insert the new unique record and it would fail all the duplicate records. SO, the complete payload doesn't fail. But for some other database and driver, it would act similar to Atomic mode and fail the payload even if single error is encountered.



1.  In your i-flow, edit the settings for JDBC Receiver adapter. Under **Connections** tab, check the **Batch Payload** checkbox and select Non-Atomic as **Batch Operation**.
!![Batch Payload Setting Non-Atomic](Batch-Payload-Setting-Non-Atomic.png)

2.  **Send** this payload from Postman:

```
<root>
	<insert_statement1>
		<dbTableName action="INSERT">
			<table> samplejdbctest</table>
			<access>
				<emp_id>1</emp_id>
				<emp_name hasQuot="Yes">v XXXXX</emp_name>

			</access>
		</dbTableName>
	</insert_statement1>
	<insert_statement2>
		<dbTableName action="INSERT">
			<table>samplejdbctest</table>
			<access>
				<emp_id>4</emp_id>
				<emp_name hasQuot="Yes"> g28</emp_name>

			</access>
		</dbTableName>
	</insert_statement2>
</root>
```

!![Non-Atomic Payload](Payload-Non-Atomic.png)

Check the MPL too.
![Payload MPL](Payload-Processed-Non-Atomic.png)

3. Let's confirm, whether the unique new data (with `emp_id` 4) is inserted or not in the database. To do this, uncheck the **Batch Mode** checkbox and **Deploy** your i-flow. Then, **Send** the following query from Postman:

`select * from samplejdbctest`  

!![Non-Atomic Batch Mode Verification](Verify-Non-Atomic.png)

So, it is evident that using Non-Atomic mode, the new record is getting inserted and the duplicate record fails.

>The function/behaviour of Non-Atomic Batch Operation may depend upon the choice of database.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Send Payload using Groovy Script)]

You can also upload the payload using the **Groovy Script**. In this case you must use the prepared statements.


1.  Let's create a table with **Batch Mode** off in integration flow.

!![Batch Mode Off](Batch-Off.png)

Let's insert a new database table. We have already seen creating sample database via **Content Modifier**. Now we will do it directly from Postman. Send this query from Postman.

`create table testjdbcsample (emp_id integer primary key, emp_name varchar (255), join_date varchar(255), a1 integer, a2 integer, email varchar(255));`

!![Create database from Postman](Database-Postman.png)

2. In the integration flow, insert the **Groovy script** to pass the payload.

!![Insert Groovy Script](Groovy-Script.png)

3.  Click **Select** and add the following payload:


```
import com.sap.gateway.ip.core.customdev.util.Message;
import java.util.HashMap;
import java.util.Arrays;

//Insert script
def Message processData(Message message) {

    //Headers
    List paramList = new ArrayList();
    paramList.add(Arrays.asList(108, 'test', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(107, 'test', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(111, 'test', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(105, 'test', 2021-01-01,1,1,'test@gmail.com'));
    message.setHeader("CamelSqlParameters",paramList);

    //Body
    message.setBody("INSERT INTO testjdbcsample (emp_id,emp_name,join_date,a1,a2,email) VALUES(?,?,?,?,?,?)");
    return message;
}
```
4. Turn the check the **Batch Mode** checkbox and click **Deploy**.

5. Run a blank query in Postman to send the payload which we have entered via Groovy Script.
!![Send Payload from Postman for Groovy Script](Postman-Groovy.png)

6. Let's verify if the payload has been successfully sent to the database. Remove **Groovy Script**, switch off the **Batch Mode** and **Deploy** the i-flow. Then, run the following query from Postman:

`select * from testjdbcsample`

!![Link text e.g., Destination screen](Postman-Groovy-Success.png)

This displays the table data. Hence, we have successfully sent a payload via Groovy Script. You can now alter the table data and verify the Atomic and Non Atomic behaviours as we did earlier.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Atomic Batch Operation with Groovy Script)]
We will replicate the above Atomic scenario with a Groovy Script.


1.  Choose **Atomic Batch Mode** and add the following script in Groovy Script as explained in Step 8.

Here we have modified the third and fourth record from the original Groovy Script.
```
import com.sap.gateway.ip.core.customdev.util.Message;
import java.util.HashMap;
import java.util.Arrays;

//Insert script
def Message processData(Message message) {

    //Headers
    List paramList = new ArrayList();
    paramList.add(Arrays.asList(108, 'test', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(107, 'test', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(112, 'test112', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(106, 'test106', 2021-01-01,1,1,'test@gmail.com'));
    message.setHeader("CamelSqlParameters",paramList);

    //Body
    message.setBody("INSERT INTO testjdbcsample (emp_id,emp_name,join_date,a1,a2,email) VALUES(?,?,?,?,?,?)");
    return message;
}
```

2. **Send** a blank query in Postman to send the payload which we have entered via Groovy Script.

3. Check the error from MPL.
!![Link text e.g., Destination screen](Groovy-Atomic-MPL.png)

4. Remove **Groovy Script**, switch off the **Batch Mode** and **Deploy** the i-flow. Then, run the following query from Postman to check the database updates, if any:

`select * from testjdbcsample`
!![Link text e.g., Destination screen](Groovy-Atomic-Error.png)

This proves the Atomic mode functionality as the database table is not updated. It is the same as it was after Step 8.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10:](Non-Atomic Batch Operation with Groovy Script)]

1. Choose **Non-Atomic Batch Mode** and add the following script in Groovy Script as explained in Step 8.

```
import com.sap.gateway.ip.core.customdev.util.Message;
import java.util.HashMap;
import java.util.Arrays;

//Insert script
def Message processData(Message message) {

    //Headers
    List paramList = new ArrayList();
    paramList.add(Arrays.asList(108, 'test', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(107, 'test', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(112, 'test112', 2021-01-01,1,1,'test@gmail.com'));
    paramList.add(Arrays.asList(106, 'test106', 2021-01-01,1,1,'test@gmail.com'));
    message.setHeader("CamelSqlParameters",paramList);

    //Body
    message.setBody("INSERT INTO testjdbcsample (emp_id,emp_name,join_date,a1,a2,email) VALUES(?,?,?,?,?,?)");
    return message;
}
```

2. **Send** a blank query in Postman to send the payload which we have entered via Groovy Script.

3. Check the error from MPL.
!![MPL for Atomic Groovy Script](Groovy-Non-Atomic-MPL.png)

4. Remove **Groovy Script**, switch off the **Batch Mode** and **Deploy** the i-flow. Then, run the following query from Postman to check the database updates, if any:

`select * from testjdbcsample`

!![MPL for Non-Atomic Groovy Script](Groovy-Non-Atomic-Success.png)

This shows that the database table has been appended with the new records, proving the Non-Atomic mode.

[DONE]
[ACCORDION-END]


---
