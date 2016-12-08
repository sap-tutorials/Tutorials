---
title: Deploy gamified HelpDesk application to SAP HANA Cloud Platform
description: Build HelpDesk application in Eclipse then perform a standalone deployment to your SAP HANA Cloud Platform account.
tags: [  tutorial>beginner, topic>cloud, products>sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:**
     - [Create the gamified HelpDesk application](http://www.sap.com/developer/tutorials/hcp-gamification-sample-application.html)
     - Configure Eclipse environment for Maven projects (Coming soon)

## Next Steps
 - [Customize game mechanics of HelpDesk application in SAP HANA Cloud Platform](http://www.sap.com/developer/tutorials/hcp-customize-gamified-application.html)

## Details
### You will learn  
In this tutorial, you will retrieve the gamified HelpDesk application from GitHub, import it to Eclipse and deploy it to the SAP HANA Cloud Platform. From within the cloud platform, you will then configure the authorization settings for your gamified application by assigning appropriate roles.

### Time to Complete
**15 Min**.

---

1. Download the [ZIP file of the sources](https://github.com/SAP/gamification-demo-app/archive/master.zip) of the gamified HelpDesk application from [GitHub](https://github.com/SAP/gamification-demo-app).

    ![Download Zip](1.png)

2. Extract the archive to your local drive.

3. In Eclipse, switch to the **Java EE** perspective.

4. Choose **File** > **Import** > **Maven** > **Existing Maven Projects**

    ![Create Maven Project](3.png)

5. Navigate to the folder in which you extracted the GitHub archive, and choose **Finish**.

    ![Build Maven Project](4.png)

6. Setup the Java Web runtime environment in Eclipse:
    - Choose **Window** > **Preferences**
    - Scroll to **Server**, select **Runtime Environments** and click **Add**
    - In the **New Server Runtime Environment** dialogue box, select **SAP** > **Java Web** and choose **Next**

    ![Set Up Java Web Runtime Environment](5.png)

    - Select **Use Java Web SDK from the following location**, confirm or browse to the location of the HANA Cloud Platform SDK, and choose **Finish**
    - In the **Preferences** window, choose **OK**

    ![Set Up Java Web Runtime Environment](6.png)

7. Back in the Project Explorer, right-click on the **`helpdesk`** project and from the context menu choose **Run As** > **Run on Server**.

    ![Run Project on Server](7.png)

8. Select **SAP** > **SAP HANA Cloud Platform** as the server, verify `hanatrial.ondemand.com` as the **Landscape host**, and choose **Next**.

    ![Define Server](9.png)

9. Name your application `helpdesk`, select the **Java Web** from the **Runtime** menu and verify that the values in the **Account name**, **User name**, and **Password** fields are accurate. Choose **Finish**.

    ![Name Application on Server](10.png)

10. In your Web browser, open the cockpit of the [SAP HANA Cloud Platform](https://account.hanatrial.ondemand.com/cockpit). Select **Java Applications** from the left-hand navigation and you should see the **`helpdesk`** application listed. Once the application is fully started, click on the name of the application.

    ![Select HelpDesk Application](11.png)

11. Select **Roles** from the left-hand navigation, select the **`helpdesk`** role, and assign your user to it.

    ![Assign User to Role](12.png)

12. Select **Overview** from the left-hand navigation and click on the URL provided under **Application URLs** to launch the application in a new browser window.

    ![Application URL](13.png)

    > If configured correctly, your browser should open the following page:

    ![Successfully Launched HelpDesk Application](14.png)

## Next Steps
 - [Customize game mechanics of HelpDesk application in SAP HANA Cloud Platform](http://www.sap.com/developer/tutorials/hcp-customize-gamified-application.html)
