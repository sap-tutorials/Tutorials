---
title: Deploying your Java app to SAP Cloud Platform
description: Deploying your Java app to SAP Cloud Platform
primary_tag: topic>java
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [End-to-End Weather App Scenario Part 9](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part9.html)

## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorials.html)

## Details
### You will learn  
In the final part of this tutorial series you will learn how to deploy your Java application to SAP Cloud Platform.

### Time to Complete
**10 min**

---


[ACCORDION-BEGIN [Step 1: ](Create a new server)]

Create a new server by opening up the corresponding context menu on the Servers view and selecting **New > Server**. Enter the following and click **Next**.

- **Server type:** `SAP Cloud Platform`
- **Landscape host:** `hanatrial.ondemand.com`
- **Server name:** `SAP Cloud Platformon hanatrial`

![Creating the SAP Cloud Platform server in your project](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-1.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure the server)]

Configure the new server with the following and click **Next**.

- **Application name:** `weatherapp`
- **Runtime:** `Java Web`
- **Account name:** `<userid>trial`
- **User name:** `<userid>`
- **Password:** `<SCN password>`

![Configuring the SAP Cloud Platformserver](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add weather app)]

Select the `weatherapp` on the list on the left and add it to the (remote) server by clicking on the **Add** button. Then, click on **Finish**.  


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create an SAP Cloud Platform Destination)]

Now create an SAP Cloud Platform Destination for the remote server via the SAP Cloud Platform cockpit (similar to what was done in section 8 for our local server). Enter the following:

- **Name:** `openweathermap`
- **Type:** `HTTP`
- **Description:** `openweathermap service`
- **URL:** `http://api.openweathermap.org/data/2.5/weather?APPID=YOUR_APPID`
- **Proxy Type:** `Internet`
- **Authentication:** `NoAuthentication`

![Creating a new destination in SAP Cloud Platform for the external weather service](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Publish the app)]

Finally publish the application to the (remote) server and run it. You app should look like this:

![Screenshot of the Java app running on HCP](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-5.png)


[ACCORDION-END]



## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorials.html)
