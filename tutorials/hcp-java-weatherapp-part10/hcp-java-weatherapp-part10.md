---
title: End-to-End Weather App Scenario Part 10
description: Deploying your Java app to SAP Cloud Platform
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
 - [End-to-End Weather App Scenario Part 9](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part9.html)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
### You will learn  
In the final part of this tutorial series you will learn how to deploy your Java application to SAP Cloud Platform.

### Time to Complete
**10 min**

---

1. Create a new server by opening up the corresponding context menu on the Servers view and selecting **New > Server**. Enter the following and click **Next**.

    - **Server type:** `SAP Cloud Platform`
    - **Landscape host:** `hanatrial.ondemand.com`
    - **Server name:** `SAP Cloud Platform on hanatrial`

    ![Creating the SAP Cloud Platform server in your project](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-1.png)


2. Configure the new server with the following and click **Next**.

    - **Application name:** `weatherapp`
    - **Runtime:** `Java Web`
    - **Account name:** `<userid>trial`
    - **User name:** `<userid>`
    - **Password:** `<SCN password>`

    ![Configuring the SAP Cloud Platform server](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-2.png)

3. Select the `weatherapp` on the list on the left and add it to the (remote) server by clicking on the **Add** button. Then, click on **Finish**.  

4. Now create an SAP Cloud Platform Destination for the remote server via the SAP Cloud Platform cockpit (similar to what was done in section 8 for our local server). Enter the following:

    - **Name:** `openweathermap`
    - **Type:** `HTTP`
    - **Description:** `openweathermap service`
    - **URL:** `http://api.openweathermap.org/data/2.5/weather?APPID=YOUR_APPID`
    - **Proxy Type:** `Internet`
    - **Authentication:** `NoAuthentication`

    ![Creating a new destination in SAP Cloud Platform for the external weather service](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-4.png)

5. Finally publish the application to the (remote) server and run it. You app should look like this:

    ![Screenshot of the Java app running on SAP Cloud Platform](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-5.png)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
