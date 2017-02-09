---
title: Deploying a Feature to SAP Cloud Platform
description: In this tutorial, you will deploy your feature (including all the plugins inside) to SAP Cloud Platform. This enables other developers to load and use your plugin.
tags: [  tutorial>beginner, topic>cloud, products>sap-hana-cloud-platform, products>sap-web-ide ]
---

## Prerequisites  
 - [Create a New Plugin Project](http://www.sap.com/developer/tutorials/webide-sdk-helloworld1.html)
 or
 - [Change Code in a Plugin](http://www.sap.com/developer/tutorials/webide-sdk-helloworld2.html)
 or
 - Create a working plugin.


## Next Steps
 - [Creating a Destination for Consuming a Plugin]

## Details
### You will learn  
  - How to deploy a feature (and its plugins) to SAP Cloud Platform

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Deploy the application)]
In the workspace, right-click the plugin folder and choose **Deploy** | **Deploy to SAP Cloud Platform**.
> If prompted, log in to your account.

![Start deploy process](Step1-Deploy.png)

In the **Deploy Application to SAP Cloud Platform** dialog, choose **Deploy**.
![Choose Deploy](Step1-DeployButton.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Check the deployment)]
In the **Successfully Deployed** dialog box, choose **Open the application's page in the SAP Cloud Platform cockpit**. The SAP Cloud Platform cockpit opens in a new tab.
![Open application in cockpit](Step2-OpenApplication.png)
Check the status of the application to make sure it is started.
![Open application in cockpit](Step2-Started.png)
The application URL is shown under **Active Version**. Save this link for later use.
![Open application in cockpit](Step2-URL.png)

[DONE]
[ACCORDION-END]


## Next Steps
- [Creating a Destination for Consuming a Plugin](http://www.sap.com/developer/tutorials/webide-sdk-helloworld4.html)
