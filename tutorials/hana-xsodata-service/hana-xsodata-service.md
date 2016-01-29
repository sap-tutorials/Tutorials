---
title: Hello World! Develop your first HANA XS oData service using the Web-based Development Workbench
description: In this tutorial you will make your very first steps on SAP HANA and develop a very simple "Hello World" application using the SAP HANA Web-based Development Workbench within SAP HANA or SAP HANA Cloud Platform.
tags: [tutorial:technology/amazon_aws, tutorial:product/sapHana, tutorial:product/hcp, tutorial:interest/gettingstarted, tutorial:product/hcp_web_workbench, tutorial:technology/odata]
---

## Prerequisites  
You need a HANA account. Pick one of the following:
- [Get a free account in HANA Cloud Platform](https://account.hanatrial.ondemand.com/register)
- [Set up an account on Amazon AWS or Microsoft Azure](http://go.sap.com/developer/tutorials/hana-setup-cloud.html)
You should have made your first steps into the Web Based Development Workbench
- [Hello Data! Access your first Data from a native SAP HANA Application](http://go.sap.com/developer/tutorials/hana-data-access-authorizations.html)

## Next Steps
Perhaps it is time to enhance our XS application to display our oData service together with some UI5 controls?

Coming soon: `Hello Data! Access your service inside of a UI5 control`.

## Details
### You will learn  
How to expose data within your SAP HANA server as an oData service.

### Time to Compete
Beginners might take **10-15 minutes** to execute this tutorial.

> ### Information
>The full application build in this tutorial can be found [in this GitHub repository](https://github.com/SAP/cloud-hana-helloworld/).

### ![icon_gold_circle_01.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_01.svg) Exposing your Table via oData
Provided you have followed the prerequisites and have created your first table in SAP HANA then the next steps here will be very quick and easy.

You should have under your ```codejam```package a ```data```package as well as several other files. The first step will be to add a second new package called ```services```which will give us a location to place our xsodata service file.

![1.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/1.png)

![2.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/2.png)

Now we will create a service definition file called ```books.xsodata```

![3.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/3.png)

![4.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/4.png)

Within this file we will define our simple xsodata service.

```js
service namespace "codejam.services" {
    "codejam.data::mydata.Book" as "books";
}
```
![5.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/5.png)

### ![icon_gold_circle_02.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_02.svg) Deploy, Run and Test the Application
Now to test our new service.

Select the ```books.xsodata``` file to enable the Run on Server in the toolbar. Then click the Run on Server button:

![6.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/6.png)

The application will open in your browser and initially you will see the meta data of the service definition:

![7.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/7.png)

If we modify the url and add our ```books``` identifier then we will see all the actual data of our table.

![8.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-xsodata-service/8.png)

Congratulations: You just have your first oData service together with your SAP HANA application!

### Related information
[SAP HANA Development Information - official documentation](http://help.sap.com/hana_platform#section6)
[oData Protocol and Definition](http://odata.org)

## Next Steps
Perhaps it is time to enhance our XS application to display our oData service together with some UI5 controls?

Coming soon: `Hello Data! Access your service inside of a UI5 control`.
