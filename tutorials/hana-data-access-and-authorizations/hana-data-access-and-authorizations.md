---
title: Hello Data! Access your first data in a native HANA Application
description: In this tutorial you will make your very first steps to access data on HANA. This tutorial will write a native HANA application, using the Web-based Development Workbench.
tags: [tutorial:technology/amazon_aws, tutorial:technology/SQL, tutorial:product/hcp_cloud_connector, tutorial:product/hcp, tutorial:interest/gettingstarted, tutorial:product/hcp_web_workbench]
---

## Prerequisites  
- [Set up a HANA instance on a cloud provider](http://go.sap.com/developer/tutorials/setup-hana-for-cloud.html)
- [Hello World!  Develop your first HANA application](http://go.sap.com/developer/tutorials/hana-web-development-workbench.html)

## Next Steps
Follow our other tutorials on the HANA Cloud Platform website
- [Hello User!  Authorize your first user](http://hcp.sap.com/developers/TutorialCatalog/nat200_03_native_hana_hello_user_with_webide)
- [Check out the other tutorials on our HANA Cloud Platform website](http://hcp.sap.com/developers/TutorialCatalog.html)

## Details

### You will learn  
1. How to browse data using the SAP HANA Web-based Development Workbench.
2. Opening a connection to the SAP HANA database.
3. Preparing and executing a very simple SQL query on the system table ```DUMMY```.

> ### Information
>The full application build in this tutorial can be found [in this Github repository](https://github.com/SAP/cloud-hana-helloworld/).

### Time to Complete
Beginners might take **10-15 min** to execute this tutorial.


### ![](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_01.svg) Open the Web-based Development Workbench

#### Using HANA Cloud Platform
Each Trial HANA instance comes with the HANA Web-based Development Workbench. The workbench allows you to develop on HANA without the need to set up a local development environment.

Login to the [HANA Cloud Cockpit](https://account.hanatrial.ondemand.com/cockpit) with your free developer edition account.

Choose HANA Instances and choose then SAP HANA Web-based Development Workbench link.

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/1.png)

You are now in the Editor and can immediately start developing in HANA.

#### Using HANA on Amazon AWS or Microsoft Azure

Access the web page of your HANA server using the IP address of your server.  Enter the address ```http://XXX.XXX.XXX.XXX``` to the address bar of your browser. (Replace XXX.XXX.XXX.XXX with the IP address of your server.)

On the web page, there is a link in the center column for **Web-Based Development Workbench**.  Click this link to start the workbench.


### ![](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_02.svg) Browse Data with the Catalog

Before accessing the data from a HANA application we will first have a look at the respective tables using the Catalog.

Open the Catalog of the Web-based Development Workbench by expanding the + button,  and then choosing Catalog:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/2.png)

The Catalog will open in a new browser window:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/3.png)

This tutorial will access data from the system table ```DUMMY```.

Open this table by choosing ```SYS > Tables > DUMMY``` in the catalog:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/4.png)

The resulting view gives you more information about the DUMMY table: You can see that it consists of only one column:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/5.png)

To see what is the content of the table right-click on the DUMMY table and choose ```Open Content```:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/6.png)

The resulting view shows the SQL query executed to fetch the content and the result of this query: You can see that DUMMY contains exactly one entry X:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/7.png)

Now let's access this data from a SAP HANA application.


### ![](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_03.svg) Access Data from a HANA Application

You must have finished the previous tutorial ["Hello World! Develop your first HANA Application using Web-based Development Workbench"](http://go.sap.com/developer/tutorials/hana-web-development-workbench.html) so that you have a working hello world application ready.

Go back to the Editor by expanding the + button and choosing Editor:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/8.png)

Open the already existing ```helloworld.xsjs```.

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/9.png)

Replace the current code in the ```helloworld.xsjs``` file with the following code that opens a database connection, prepares a simple SQL statement, executes it and returns the result of the query:

```js
$.response.contentType = "text/html";
var output = "Hello World!<br><br>";

//Open a database connection
var conn = $.db.getConnection();

//Prepare a simple SQL statement on the system table "DUMMY"
var pstmt = conn.prepareStatement("select * from DUMMY");

//Execute the query
var rs = pstmt.executeQuery();

//Check the query result
if (!rs.next()) {
    //Something went wrong: Return an error
    $.response.setBody("Failed to retrieve data");
    $.response.status = $.net.http.INTERNAL_SERVER_ERROR;
} else {
    //All went fine: Return the Query result
    output = output + "This is the response from my SQL: " + rs.getString(1);
}

//Close the database connection
rs.close();
pstmt.close();
conn.close();

//Return the HTML response.
$.response.setBody(output);
```
Save the file using the Save button or by pressing ```ctrl+s```. Again, the successful save is confirmed in the console.

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/10.png)

Now you are ready to run the application.


### ![](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_04.svg) Deploy, Run and Test the Application

Now the application is ready to be tested. As you are developing with the Web-based Development Workbench the application is already deployed and activated to your HANA Trial Instance. So you can immediately continue to test it.

Select the helloworld.xsjs file to enable the Run on Server in the toolbar. Then click the ```Run on Server``` button:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/11.png)

The application will open in your browser and greet you with **Hello World** and the just accessed data from system table ```DUMMY```:

![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-and-authorizations/12.png)

Congratulations: You have just accessed your first data on SAP HANA!


### Optional: Related Information
[SAP HANA Development Information - Official Documentation](http://help.sap.com/hana_platform#section6)

## Next Steps
[Hello User! Authorize your first HANA User using the Web-based Development Workbench](http://hcp.sap.com/developers/TutorialCatalog/nat200_03_native_hana_hello_user_with_webide.html)
Take your first steps to authorize users to access native HANA applications with roles and privileges using the Web-based Development Workbench.

*This tutorial is part of the HANA Cloud Platform tutorials set.*

[Check out the entire list of tutorials for HANA and the HANA Cloud Platform](http://hcp.sap.com/developers/TutorialCatalog.html)
Even more tutorials are available in our HANA Cloud Platform tutorial catalog.  Try a few!
