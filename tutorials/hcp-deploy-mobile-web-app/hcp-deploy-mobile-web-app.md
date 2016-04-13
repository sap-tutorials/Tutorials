---
title: Deploy your mobile web app to SAP HANA Cloud Platform
description: Deploy your app to SAP HANA Cloud Platform and access it from your desktop and mobile devices
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>mobile, topic>odata, topic>html5, tutorial>beginner ]
---
## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/hcp-template-mobile-web-app.html)

## Next Steps
 - [Add labels and a field to your app](http://go.sap.com/developer/tutorials/hcp-webide-add-labels-field.html)

## Details

### You will learn
After creating your app in SAP WEB IDE, you will now deploy it to your HANA Cloud Platform trial account and be able to see it on a mobile device.

Since any project that is created initially in the SAP Web IDE contains a ```neo-app.json``` file, it is ready to be deployed to HANA Cloud Platform. During the deployment process, Web IDE creates the HTML5 application in HANA Cloud Platform and also the related Git repository (which will track code changes) for your app automatically.

### Time to Complete
**5 min**


1. Open the SAP Web IDE.

2. In SAP Web IDE, select the **northwind** project folder and open the context menu by right-clicking on it. Choose **Deploy > Deploy to SAP HANA Cloud Platform**.

    ![HCP Deployment contextual menu](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_2.png)

3. If this is your first time deploying an app to the SAP HANA Cloud Platform, you may see a dialog box requesting information to configure your Git repository. Enter your **email** and **username** and click **OK**.

    ![HCP Git user login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_3.png)

4. You will be prompted for your SAP HANA Cloud Platform Password and click on **Login**.

    ![HCP user validation](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_4.png)

5. Because all HTML application names on HCP are lower case, the application name will be converted (if required) to lower-case letters. Check the **Connect to the SAP HANA Cloud Platform Git repository...** checkbox, verify the version number is **1.0.0** and the **Activate** check box is marked. Click on **Deploy**.

    ![HCP HTML5 Deployment dialog box](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_5.png)

6. The deployment process will take a few seconds, and after it has finished, you will see a success message.

    ![HCP deployment confirmation dialog box](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_6.png)

7. Click on **Open the active version of the application** link to see your app running on HCP. Your app should look like this:

    ![Basic master-detail app using Northwind OData Service](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_7.png)

8. You can now enter the application URL in your mobile device browser to see it running there.

    ![iPhone view of master-detail app showing responsive design adaptation](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_8.png)

## Next Steps
 - [Add labels and a field to your app](http://go.sap.com/developer/tutorials/hcp-webide-add-labels-field.html)
