---
title: Enable SAP HANA Cloud Platform mobile services
description: Enable and configure the mobile services in SAP HANA Cloud Platform
tags: [products>sap-hana-cloud-platform, topic>cloud, topic>mobile, tutorial>intermediate ]
---
## Prerequisites  
 - **Proficiency:** Intermediate

## Next Steps
 - [Create an app in SAP HANA Cloud Platform mobile services](http://go.sap.com/developer/tutorials/hcpms-create-app.html)

## Details
### You will learn  
In this tutorial, you will enable and configure SAP HANA Cloud Platform mobile services in your trial account.  Once configured, you can use mobile services to run hybrid and native apps on SAP HANA Cloud Platform

### Time to Complete
**5 Min**.

---

1. Go to your [HCP Cockpit](https://account.hanatrial.ondemand.com) and log on.


2. From the SAP HANA Cloud Platform Cockpit page, click **Subscriptions** in the navigation bar, scroll down until you see Mobile Services and click on its tile.

    ![Subscriptions tab](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/2.png)

3. If Mobile Services is not enabled, click on the blue **Enable** button and wait a few seconds until the green **Enabled** badge appears. Return to the cockpit by clicking on your User ID "breadcrumb" link near the top left of the browser window.

    ![Enable HCPms](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/3.png)

4. You will need to add an `Admin` role to HCPms. To do so, click on the **Configure Mobile services** link.

    ![Configure Mobile Services](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/4.png)

5. Click on the **Roles** tab, select **Administrator** under **New Role** then click on **Assign** next to **Individual Users**.

    ![Role configuration](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/5.png)

6. Enter your User ID (e.g. `p12345678`) then click **Assign**. Notice that the dialog box shows the role name you are assigning.

    ![Role assignment](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/6.png)

7. Your Roles page should look similar to this now:

    ![Role assigned](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/7.png)


9. The next step is to create two HCP Destinations. To display the navigation bar, click on your **User ID** link again (near the top left of the window), click on the **Destinations** tab, then click on **New Destination**.

10. For the first Destination, complete the form with the info below, and click Save.

    Field Name                | Value
    :------------------------ | :-------------
    Name                      | `HMAdminJaxrs`
    Type                      | `HTTP`
    Description               |`<you can leave this blank>`
    URL                       | `https://hcpms-trial.hanatrial.ondemand.com/Admin/`
    Proxy Type                | `Internet`
    Authentication            | `AppToAppSSO`

    Make sure the “Use default JDK truststore is checked.

    ![Destination 1](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/10.png)

11. For the second Destination, complete the form with the info below, and click Save.

    Field Name                | Value
    :------------------------ | :-------------
    Name                      | `HMAdminHandlers`
    Type                      | `HTTP`
    Description               |`<you can leave this blank>`
    URL                       | `https://hcpms-trial.hanatrial.ondemand.com/Admin/AdminData`
    Proxy Type                | `Internet`
    Authentication            | `AppToAppSSO`

    Make sure the “Use default JDK truststore is checked.

    ![Destination 2](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-enable-mobile-services/11.png)


## Next Steps
 - [Create an app in SAP HANA Cloud Platform mobile services](http://go.sap.com/developer/tutorials/hcpms-create-app.html)
