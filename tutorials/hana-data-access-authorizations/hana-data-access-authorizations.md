---
title: Access your first data in a native HANA Application
description: In this tutorial you will make your very first steps to access data on HANA. This tutorial will write a native HANA application, using the Web-based Development Workbench.
tags: [ products>sap-hana, products>sap-hana-studio, products>sap-hana-cloud-platform, products>cloud-connector-for-sap-hana-cloud-platform, topic>sql, topic>big-data, tutorial>beginner]
---

## Prerequisites  
- [Set up a HANA instance on a cloud provider](http://go.sap.com/developer/tutorials/hana-setup-cloud.html)
- [Hello World!  Develop your first HANA application](http://go.sap.com/developer/tutorials/hana-web-development-workbench.html)

## Next Steps
- Your first XS OData service with SAP HANA (coming soon)


## Details

### You will learn  
1. How to create a simple schema and table.
2. Importing data automatically via a CSV file.
3. Preparing and executing a very simple SQL query on the table.

> ### Information
>The full application build in this tutorial can be found [in this GitHub repository](https://github.com/SAPDocuments/Tutorials).

### Time to Complete
Beginners might take **10-15 minutes** to execute this tutorial.


### ![icon_gold_circle_01.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_01.svg) Open the Web-based Development Workbench

#### Using the SAP HANA Developer Edition or SAP HANA Cloud Platform
The workbench allows you to develop on HANA without the need to set up a local development environment.

Login to the [HANA Cloud Cockpit](https://account.hanatrial.ondemand.com/cockpit) with your free developer edition account.

![1.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/1.png)

Choose Databases and Schemas, and choose then the instance that you created in the previous tutorials. From here you can access the Workbench.

![2.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/2.png)

You are now in the Editor and can immediately start developing in HANA.

#### Using HANA on Amazon AWS or Microsoft Azure

Access the web page of your HANA server using the IP address of your server.  Enter the address ```http://XXX.XXX.XXX.XXX``` to the address bar of your browser. (Replace ```XXX.XXX.XXX.XXX``` with the IP address of your server.)

On the web page, there is a link in the middle column for **Web-Based Development Workbench**.  Click this link to start the workbench.


### ![icon_gold_circle_02.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_02.svg) Create your catalog object

Our first step will be to create a package to organize our files. To do this we will create a new package under our existing ```codejam``` package and call it ```data```.

![3.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/3.png)

![4.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/4.png)

Now that we have a location for our objects we will create our schema file, this will be a new file called ```MYCJ``` which is short for "My CodeJam" but you can actually call it anything you like.

![5.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/5.png)

![6.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/6.png)

![7.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/7.png)

Now that we have a home for our table, we will go ahead and define a simple table. So it's time for a new file called ```mydata.hdbdd```, again you can call it whatever you like.

![8.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/8.png)

In this file we will define our table entities.

![9.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/9.png)

```
namespace codejam.data;

@Schema: 'MYCJ'

context mydata {

 	type SDate : UTCTimestamp;
 	type SString : String(40);
 	type LString : String(255);

 	@Catalog.tableType : #COLUMN
 	Entity Book {
 		key ID: Integer;
        BOOKNAME: LString;
        CATEGORY: LString;
        INVDATE: SDate;
    };
};
```

Once you save this, you should see (provided no mistakes in typing) the following output in the console.

![10.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/10.png)

Now for a bit of sample data into our CSV file and then we can automatically import that into the new table.

![11.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/11.png)

![12.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/12.png)

Now we will create the automatic import file.

![13.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/13.png)

![14.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/14.png)

Once we save the file, our data should automatically import into our table.

In order to now view our data we will need to ensure our user has access to it, for this we will need to create a new role and assign it to our user.

![15.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/15.png)

![16.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/16.png)

![17.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/17.png)

![18.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/18.png)

![19.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/19.png)

Now we will need to open the "Security" tab and add this new role to our user.

![20.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/20.png)

![21.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/21.png)

![22.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/22.png)

There now we are set to view the data from the "Catalog" tab.

![23.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/23.png)

![24.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/24.png)

![25.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/25.png)

The resulting view shows the SQL query executed when we clicked the "Open Content" button.

![26.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/26.png)

Now let's access this data from a SAP HANA application.


### ![icon_gold_circle_03.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_03.svg) Access Data from a HANA Application

You must have finished the previous tutorial ["Hello World! Develop your first HANA Application using Web-based Development Workbench"](http://go.sap.com/developer/tutorials/hana-web-development-workbench.html) so that you have a working hello world application ready.

Go back to the Editor and open the already existing ```helloworld.xsjs```.

![27.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/27.png)

Replace the current code in the ```helloworld.xsjs``` file with the following code that opens a database connection, prepares a simple SQL statement, executes it and returns the result of the query:

```js
$.response.contentType = "text/html";
var output = "Hello World!<br><br>";

//Open a database connection
var conn = $.db.getConnection();

//Prepare a simple SQL statement on the system table "DUMMY"
var pstmt = conn.prepareStatement('SELECT * FROM "MYCJ"."codejam.data::mydata.Book"');

//Execute the query
var rs = pstmt.executeQuery();

//Check the query result
if (!rs.next()) {
    //Something went wrong: Return an error
    $.response.setBody("Failed to retrieve data");
    $.response.status = $.net.http.INTERNAL_SERVER_ERROR;
} else {
    //All went fine: Return the Query result
    output = output + "This is the response from my SQL:<br><br>";
    output = output + rs.getString(1) + ' ' +  rs.getString(2) + ' ' +  rs.getString(3) + ' ' +  rs.getString(4) + '<br>';
}

//Close the database connection
rs.close();
pstmt.close();
conn.close();

//Return the HTML response.
$.response.setBody(output);
```

Save the file using the Save button or by pressing ```ctrl+s```. Again, the successful save is confirmed in the console.

![28.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/28.png)

Now you are ready to run the application.

You can even loop through all records like this,

```js
    while (rs.next()) {
        output = output + rs.getString(1) + ' ' +  rs.getString(2) + ' ' +  rs.getString(3) + ' ' +  rs.getString(4) + '<br>';
    }
```


### ![icon_gold_circle_04.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_04.svg) Deploy, Run and Test the Application

Now the application is ready to be tested. As you are developing with the Web-based Development Workbench the application is already deployed and activated so you can immediately continue to test it.

Select the ```helloworld.xsjs``` file to enable the Run on Server in the toolbar. Then click the ```Run on Server``` button:

![29.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/29.png)

The application will open in your browser and greet you with **Hello World** and the just accessed data from your table:

![30.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-data-access-authorizations/30.png)

Congratulations: You have just accessed your first data on SAP HANA!


### Optional: Related Information
[SAP HANA Development Information - Official Documentation](http://help.sap.com/hana_platform#section6)

## Next Steps
- Your first XS OData service with SAP HANA (coming soon)

Take your first steps to authorize users to access native HANA applications with roles and privileges using the Web-based Development Workbench.

*This tutorial is part of the SAP HANA and SAP HANA Cloud Platform tutorials set.*
