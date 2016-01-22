---
title: Deploy your mobile web app to SAP HANA Cloud Platform
description: Deploy your app to SAP HANA Cloud Platform and access it from your desktop and mobile devices
tags: [tutorial:product/hcp, tutorial:product/mobile, tutorial:interest/gettingstarted]
---
## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/hcp-template-mobile-web-app.html)

## Next Steps
You can add labels and additional data field to your app.

## Details

### You will learn
After creating your app in SAP WEB IDE, you will now deploy it to your HANA Cloud Platform trial account and be able to see it on a mobile device.

Since any project that is created initially in the SAP Web IDE contains a ```neo-app.json``` file, it is ready to be deployed to HANA Cloud Platform. During the deployment process, Web IDE creates the HTML5 application in HANA Cloud Platform and also the related Git repository (which will track code changes) for your app automatically.

### Time to Complete
**5 min**


1. Open the SAP Web IDE.
*If you are not logged in yet, refer to the previous tutorial, steps 1-3.*

2. In SAP Web IDE, select the ```index.html``` file of the Northwind project and open the context menu by right-clicking on it. Choose **Deploy > Deploy to SAP HANA Cloud Platform** to open the deploy dialog box.

    ![mob1-3_2.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_2.png)

3. If this is your first time deploying an app to the SAP HANA Cloud Platform, you may see a dialog box requesting information to configure your Git repository. Enter your **email** and **username** and click **OK**.

    ![mob1-3_3.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_2.png)

4. You will be prompted for your SAP HANA Cloud Platform Password and click on **Login**.

    ![mob1-3_4.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_4.png)

5. Because there is a restriction for the names of HTML applications which can be deployed on SAP HANA Cloud Platform (only lower-case alphanumeric characters are allowed) the application name is converted to lower-case letters. Enter the version number **1.0.0** and check on **Activate** box. Click on **Deploy**.

    ![mob1-3_5.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_5.png)

6. The deployment process will take a few seconds, and after it has finished, you will see a success message.

    ![mob1-3_6.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_6.png)

7. Click on **Open the active version of the application** link to see your app running on HCP. Your app should look like this:

    ![mob1-3_7.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_7.png)

8. You can now enter the application URL in your mobile device browser to see it running there.

    ![mob1-3_8.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-deploy-mobile-web-app/mob1-3_8.png)
