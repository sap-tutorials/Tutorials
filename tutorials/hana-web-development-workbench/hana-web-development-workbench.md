---
title: SAP HANA XS Classic, Develop your first SAP HANA XSC Application
description: In this tutorial you will make your very first SAP HANA XSC application using the SAP HANA Web-based Development Workbench.
primary_tag: products>sap-hana
tags: [ products>sap-hana, products>sap-cloud-platform, tutorial>beginner]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** You need a HANA account or server. Pick one of the following:
   - [Get a free account in SAP Cloud Platform](https://account.hanatrial.ondemand.com/register)
   - [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.html)

## Next Steps
[Access your first Data from a native SAP HANA Application](http://www.sap.com/developer/tutorials/hana-data-access-authorizations.html)

## Details

### You will learn  
1. How to use the SAP HANA Web-based Development Workbench.
2. How to develop a simple server-side application.
3. How to publish and run an application.

### Time to Complete
Beginners might take **10-15 minutes** to execute this tutorial.

[ACCORDION-BEGIN [OPTION A: ](Using SAP Cloud Platform)]

Each Trial HANA MDC instance comes with the HANA Web-based Development Workbench. The workbench allows you to develop on HANA without the need to set up a local development environment.

Login to the [HANA Cloud Cockpit](https://account.hanatrial.ondemand.com/cockpit) with your free developer edition account.
Choose Persistence in the left column menu and then Database & Schemas. You will need to create your new instance. To do this simply give it a name, enable web access and of course give a password. This password you will need to remember as it is the password for your SYSTEM user and how you will be able to access the server.

![New Database and Schema](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/1.png)

Once you begin the creation process, you will be redirected to an events tab.

![Database Events](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/2.png)

Remember the instance runs for a limited time, so if you have to come back later you may need to restart it.

![Properties](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/3.png)

Just click start to restart it. Also note it's only valid for 30 days.

**Couple of more steps before we hit the editor**

You'll need to click on the "Admin Cockpit" first to trigger the authorizations you will need going forward.

![Manage Roles](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/4.png)

Once your authorizations have applied, you should then select the "Manage Roles and Users".

![System User](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/5.png)

Here you can create a new user. This is recommended so you do not risk the "SYSTEM" user.

![Copy User](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/6.png)

![Find roles](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/7.png)

Now simply apply new roles to the user to give this user the appropriate authorizations to begin developing.

![Find roles](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/9.png)

![Admin roles](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/10.png)

With that completed we can now log into the "SAP HANA Web Based Development Workbench"

![Access workbench](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/11.png)

You are now in the Editor and can immediately start developing in HANA.

[ACCORDION-END]

[ACCORDION-BEGIN [OPTION B: ](Using SAP HANA, express edition)]

For HXE enter the address `http://hxehost:8090/sap/hana/ide/` in to the address bar of your browser. This works provided you have the `Server + Applications` version or if you followed these [steps](https://blogs.sap.com/2016/10/28/enhancing-hxe-server-image/) to add the XSC tooling to your `Server Only` instance.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create Package for your Application)]

The first step to start developing a SAP HANA application with SAP HANA Web-based Development Workbench is to create a new package for the application.

Here we will create a package called `codejam`, Right-Click on the content folder on the left, and enter the package details.

![Create Package](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/12.png)

Now you have a package that is waiting for your code, now we will Right-Click on the `codejam` package and choose `Create Application`

![Create Application](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/13.png)

This step will generate 3 files, the `.xsapp`, `.xsaccess` and an `index.html`.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Now Create the Application)]

Now is the time to actually create some application code. In SAP HANA XSC application code essentially is JavaScript code provided in `.xsjs` files. Now such a file will be created.

Open the context menu of the `codejam` package by right-clicking on the name and choose Create File. Enter `mylibrary.xsjs` as file name and save with Enter:

![New File](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/14.png)

The new empty file is now open in the Editor:

![XSJS file in editor](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/15.png)

Copy and paste this code to the just created `mylibrary.xsjs` file:

```js
$.response.contentType = "text/html";
$.response.setBody("My Personal Library");
```

Save the file using the Save button or by pressing `ctrl+s`. Again, the successful save is confirmed in the console.

![Save file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/16.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Deploy, Run and Test the Application)]

Now the application is ready to be tested. As you are developing with the SAP HANA Web-based Development Workbench the application is already deployed and activated to your SAP HANA Trial Instance. So you can immediately continue to test it:

Select the `mylibrary.xsjs` file to enable the Run on Server in the toolbar. Then click the Run on Server button:

The application will open in your browser and greet you with the beginning of your personal library:

![Web Preview](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-web-development-workbench/17.png)

Congratulations: You just have your first own native SAP HANA application running on SAP Cloud Platform!

[ACCORDION-END]

### Related information
[SAP HANA Development Information - official documentation](https://help.sap.com/hana_platform#section6)
