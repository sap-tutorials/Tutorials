---
title: Hello World! Develop your first HANA Application using the Web-based Development Workbench
description: In this tutorial you will make your very first steps on SAP HANA and develop a very simple "Hello World" application using the SAP HANA Web-based Development Workbench on the SAP HANA Cloud Platform.
tags: [tutorial:technology/amazon_aws, tutorial:product/sapHana, tutorial:product/hcp, tutorial:interest/gettingstarted, tutorial:product/hcp_web_workbench]
---

## Prerequisites  
You need a HANA account. Pick one of the following:
- [Get a free account in HANA Cloud Platform](https://account.hanatrial.ondemand.com/register)
- [Set up an account on Amazon AWS or Microsoft Azure](http://go.sap.com/developer/tutorials/hana-setup-cloud.html)

## Next Steps
[Hello Data! Access your first Data from a native SAP HANA Application using the SAP HANA Web-based Development Workbench](http://go.sap.com/developer/tutorials/hana-data-access-authorizations.html)

## Details
### You will learn  
1. How to use the SAP HANA Web-based Development Workbench.
2. How to develop a simple server-side application.
3. How to publish and run an application.

### Time to Compete
Beginners might take 10-15 minutes to execute this tutorial.

> ### Information
>The full application build in this tutorial can be found [in this GitHub repository](https://github.com/SAP/cloud-hana-helloworld/).

### ![icon_gold_circle_01.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_01.svg) Using HANA Cloud Platform
Each Trial HANA instance comes with the HANA Web-based Development Workbench. The workbench allows you to develop on HANA without the need to set up a local development environment.

Login to the [HANA Cloud Cockpit](https://account.hanatrial.ondemand.com/cockpit) with your free developer edition account.
Choose HANA Instances and choose then SAP HANA Web-based Development Workbench link.

    ![1.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/1.png)

You are now in the Editor and can immediately start developing in HANA.

#### Using HANA on Amazon AWS or Microsoft Azure
Access the web page of your HANA server using the IP address of your server. Enter the address ```http://XXX.XXX.XXX.XXX``` to the address bar of your browser. (Replace ```XXX.XXX.XXX.XXX``` with the IP address of your server.)

On the web page, there is a link in the middle column for **Web-Based Development Workbench**. Click this link to start the workbench.

### ![icon_gold_circle_02.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_02.svg) Create Package for the Hello World Application
The first step to start developing a SAP HANA application with SAP HANA Web-based Development Workbench is to create a new package for the application.

Open the first package in the Content folder that is named like your account, e.g. ```p1234567890trial```. Then select the package that is named like your HANA trial instance, e.g. ```hana```:

    ![2.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/2.png)

Right-Click on the selected package of your SAP HANA trial instance and choose Create Package. Enter ```helloworld``` as new package name and save with Enter:

    ![3.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/3.png)

Now you have a package that is waiting for your code:

    ![4.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/4.png)

### ![icon_gold_circle_03.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_03.svg) Create .xsapp File
In SAP HANA, an empty .xsapp file indicates that a package is containing a HANA XS application. As we want to create such an application the next step is to create such a file.

Open the context menu of the just created ```helloworld``` package by right-clicking on the name and choose Create File. Enter .xsapp as file name and save with Enter:

    ![5.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/5.png)

The new file is now open in the Editor:

    ![6.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/6.png)

Save the empty file using the Save button or by pressing ```ctrl+s```. The successfully save is confirmed in the console:

    ![7.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/7.png)

### ![icon_gold_circle_04.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_04.svg) Create the "Hello World" Application
Now is the time to actually create some application code. In SAP HANA XS application code essentially is JavaScript code provided in .xsjs files. Now such a file will be created.

Open the context menu of the ```helloworld``` package by right-clicking on the name and choose Create File. Enter ```helloworld.xsjs``` as file name and save with Enter:

    ![8.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/8.png)

The new empty file is now open in the Editor:

    ![9.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/9.png)

Copy and paste this code to the just created ```helloworld.xsjs``` file:

```js
$.response.contentType = "text/html";
$.response.setBody("Hello World!");
```

Save the file using the Save button or by pressing ```ctrl+s```. Again, the successful save is confirmed in the console.

    ![10.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/10.png)

### ![icon_gold_circle_05.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_05.svg) Configure Access to the Application
The just developed application can't be accessed yet. For this it must be declared with a .xsaccess file that the application shall be accessible.

Open the context menu of the ```helloworld``` package by right-clicking on the name and choose Create File. Enter ```.xsaccess``` as file name and save with Enter:

    ![11.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/11.png)

The new empty file is now open in the Editor:

    ![12.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/12.png)

Copy and paste this code to the just created ```.xsaccess``` file. This code declares that the package shall be accessible as application:

```json
{
    "exposed" : true
}
```
Save the file using the Save button or by pressing ```ctrl+s```. Again, the successful save is confirmed in the console.

    ![13.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/13.png)

Now you are ready to run the application.

### ![icon_gold_circle_06.svg](http://go.sap.com/dam/application/shared/icons/icon_gold_circle_06.svg) Deploy, Run and Test the Application
Now the application is ready to be tested. As you are developing with the SAP HANA Web-based Development Workbench the application is already deployed and activated to your SAP HANA Trial Instance. So you can immediately continue to test it:

Select the ```helloworld.xsjs``` file to enable the Run on Server in the toolbar. Then click the Run on Server button:

    ![14.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/14.png)

The application will open in your browser and greet you with Hello World:

    ![15.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/15.png)

Congratulations: You just have your first own native SAP HANA application running on SAP HANA Cloud Platform!

### Related information
[SAP HANA Development Information - official documentation](http://help.sap.com/hana_platform#section6)


## Next Steps
Make your very first steps to access data on HANA. This tutorial will write a native HANA application, using the Web-based Development Workbench.

[Hello Data! Access your first Data from a native SAP HANA Application using the SAP HANA Web-based Development Workbench](http://go.sap.com/developer/tutorials/hana-data-access-authorizations.html)
