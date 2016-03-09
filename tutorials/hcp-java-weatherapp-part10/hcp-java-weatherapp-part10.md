---
title: End-to-End Weather App Scenario Part 10
description: Deploying your Java app to SAP HANA Cloud Platform
tags: [tutorial:interest/gettingstarted, tutorial:interest/cloud, tutorial:product/hcp, tutorial:technology/java]
---

## Prerequisites  
 - [End-to-End Weather App Scenario Part 9](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part9.html)

## Next Steps
 - [Return to the HCP tutorial catalog](https://hcp.sap.com/developers/TutorialCatalog.html#/) to select other tutorials
 
## Details
### You will learn  
In the final part of this tutorial series you will learn how to deploy your Java application to SAP HANA Cloud Platform.

### Time to Complete
**10 min**


1. Create a new server by opening up the corresponding context menu on the Servers view and selecting **New > Server**. Enter the following and click **Next**.

 - **Server type:** `SAP HANA Cloud Platform`
 - **Landscape host:** `hanatrial.ondemand.com`
 - **Server name:** `HCP on hanatrial` 

 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-1.png)
 

2. Configure the new server with the following and click **Next**.

 - **Application name:** `weatherapp`
 - **Runtime:** `Java Web`
 - **Account name:** `<userid>trial`
 - **User name:** `<userid>`
 - **Password:** `<SCN password>`

  ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-2.png)

3. Select the weatherapp on the list on the left and add it to the (remote) server by clicking on the **Add** button. Then, click on **Finish**.  

4. Now create an HCP Destination for the remote server via the HCP cockpit (similar to what was done in section 8 for our local server). Enter the following:

 - **Name:** `openweathermap`
 - **Type:** `HTTP`
 - **Description:** `openweathermap service` 
 - **URL:** `http://api.openweathermap.org/data/2.5/weather?APPID=YOUR_APPID`
 - **Proxy Type:** `Internet`
 - **Authentication:** `NoAuthentication` 

  ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-4.png)

5. Finally publish the application to the (remote) server and run it. You app should look like this:

  ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part10/e2e_10-5.png)
 
##Next Steps
  - [Return to the HCP tutorial catalog](https://hcp.sap.com/developers/TutorialCatalog.html#/) to select other tutorials

