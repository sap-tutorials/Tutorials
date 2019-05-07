---
title: Add a Model and Test the SAPUI5 App
description: Define a new data source which references the created OData service and use it for the default model.
auto_validation: true
time: 15
author_name: Marius Obert
author_profile: https://github.com/iobert
primary_tag: topic>sapui5
tags: [  tutorial>beginner, topic>html5, topic>sapui5, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment, products>sap-web-ide ]
---


## Details
### You will learn  
Now that you have set up a destination in the SAP Cloud Platform cockpit, you will connect that destination to your application.  


There are three steps to connect the OData service to your application.  

1.  Define the Data Source in your `mainfest.json` file.  
2.  Create a Model.   (The Model is the data source, based on [the Model-View-Controller architecture](https://blog.codinghorror.com/understanding-model-view-controller/)).  
3.  Test the application


---

[ACCORDION-BEGIN [Step : ](Open the descriptor editor)]

In a [previous tutorial](hcp-create-destination) the OData test service called "Northwind" was set up for all SAP Cloud Platform applications.  Next, this specific application must connect to that "Destination".  


1.  Open the `mta_app/app/webapp/mainfest.json` file with the **Descriptor Editor**, by double clicking on the file.

    > If you open the file, and you get a code editor (and not the form editor shown below), click the **Descriptor Editor** link at the bottom of the page.  This will change to the Code Editor.


2.  Click the **Data Sources** tab at the top of the screen.

3.  Click the **+** icon next to the *Define OData services for the application and...* box.

    ![Open the `mainfest.json` file](1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Define the Data source)]

1.  From the **Sources** list, choose **Service URL**.  

2.  Then choose the following:

    | -------------:| ---------------------------------- |
    | Drop-down Box | **`Northwind Sample OData`**          |
    | URI           | **`/V2/Northwind/Northwind.svc`**      |

3.  Click the **Test** button to test the service.  If the data is correct, you should see the service entities appear.  

4.   Click **Next** to continue.

    ![Test the new OData service](2.png)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Use the data source as default model)]

1. Keep the default value **Use default model** to use this data source with the default model.

    ![Finish creating the OData service](3.png)

2. On the confirmation screen, click **Finish**.

    ![Finish creating the OData service](4.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Test the application in the Cloud Foundry environment)]

1.  To test your application, click on `webapp` in the project tree, followed by a click on **Run** ![Run Icon](run-icon.png)

2.  Choose the `index.html` file to run and confirm with **OK**.

    ![Run](5.png)

3.  In the console, you should be able to see that the test deployment started.

    ![Deploymentprocess](6.png)

> This initial deployment might take a couple of minutes. But don't worry, the next deployment will be super fast :)!


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Inspect the application network trace)]

1. You might see this logon screen. If so, please log on with your SAP Cloud Cockpit credentials.

    ![logon](7.png)

2. Now you should see your (blank) SAPUI application. Right click on the web page, and choose **Inspect**

    ![Google Chrome inspector tool](8.png)

3.  The inspector will appear.  Click on the **Network** tab to view the network traffic for the application.

    ![network tab in the inspector tool](test-3.png)

4.  To view all the network traffic, reload the page.  

    The network traffic will appear as the page is loading...
    >*What about the red text?* Several files will appear in red.  This is normal, as we have not set up that part of the application.

5.  Scroll down until you find the *$metadata* item.  

    > **NOTE** Typing in `metadata` to the filter box in upper right corner will make it easy to find the correct row.

    ![metadata in the network view](test-5.png)

6.  Click on *$metadata*, then click the **Preview** tab.

    This will show you the data coming from the server to the browser.  If you see this, the OData request is working.  You are ready to move on to the next step of the tutorial.

    ![preview of the data coming from the server](test-6.png)

Expand the nodes **`<edmx:Edmc>`**, **`<edmx: DataServices>`** and the **`first <Schema> node`** in the Chrome Network trace.

[VALIDATE_1]
[ACCORDION-END]


----

## Troubleshooting

 - **Missing a file?**  If the list of files doesn't match the picture, you may have used the wrong template when you created the project.  Delete the project, and start the [Create a new project](sapui5-webide-create-project) tutorial again.

 - **$metadata file not listed?**  This means one of the files in your project is incorrect.  Check the files, and make sure no red X marks appear in the left hand column.  These indicate a problem with the file syntax.  Check the pictures carefully.

 - **The `Northwind` system does not appear in the drop down box.**  This can happen when the Web IDE is "out of sync" with the server.  Reload the Web IDE (by clicking the reload button in your browser).  You will come back to the same place, and you can start the steps to create a new OData service again.

 - **Don't forget to save your files!**  If a file name has a * next to it, the file isn't saved.  

 ---
