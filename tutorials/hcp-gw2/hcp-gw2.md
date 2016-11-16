---
title: Deploy your mobile web app to SAP HANA Cloud Platform
description: Deploy your app to SAP HANA Cloud Platform and access it from your desktop and mobile devices
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>mobile, topic>odata, topic>html5, tutorial>beginner ]
---
## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/hcp-gw1.html)

## Next Steps
 - [Add labels and a field to your app](http://go.sap.com/developer/tutorials/hcp-gw3.html)

## Details

### You will learn
After creating your app in SAP WEB IDE, you will now deploy it to your HANA Cloud Platform trial account and be able to see it on a mobile device.

Since any project that is created initially in the SAP Web IDE contains a `neo-app.json` file, it is ready to be deployed to HANA Cloud Platform. 

### Time to Complete
**5 min**


1. Open the SAP Web IDE.

2. In SAP Web IDE, select the **orders** project folder and open the context menu by right-clicking on it. Choose **Deploy > Deploy to SAP HANA Cloud Platform**.

3. You will be prompted for your SAP HANA Cloud Platform Password and click on **Login**.

4. Because all HTML application names on HCP are lower case, the application name will be converted (if required) to lower-case letters. Verify the version number is **1.0.0** and the **Activate** check box is marked. Click on **Deploy**.

6. The deployment process will take a few seconds, and after it has finished, you will see a success message.


7. Click on **Open the active version of the application** link to see your app running on HCP. 


8. You can now enter the application URL in your mobile device browser to see it running there.


## Next Steps
 - [Add labels and a field to your app](http://go.sap.com/developer/tutorials/hcp-gw3.html)
