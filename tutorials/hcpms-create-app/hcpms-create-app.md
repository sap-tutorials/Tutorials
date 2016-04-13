---
title: Create an app in SAP HANA Cloud Platform mobile services
description: Learn how to create an app definition in HCPms
tags: [ products>sap-hana-cloud-platform, topic>cloud, topic>mobile, tutorial>intermediate]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:**
   - [Enable SAP HANA Cloud Platform mobile services](http://go.sap.com/developer/tutorials/hcpms-enable-mobile-services.html)
   - You must have completed at least the three tutorials below in the Web IDE series:  
     - [Create a Destination on HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-create-destination.html)
     - [Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/hcp-template-mobile-web-app.html)
     - [Deploy your mobile web app to SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html)

## Next Steps
 - [Configure an SAP Web IDE project for hybrid builds](http://go.sap.com/developer/tutorials/hcpms-webide-hybrid-config.html)

## Details
### You will learn  
In this tutorial, you will learn how to create (or define) an app in SAP HANA Cloud Platform mobile services. This process is very similar to to the creation of a Destination in HCP, and allows your HCPms instance know how to handle incoming application requests (registration, authentication and data).

### Time to Complete
**5 Min**.

---

1. Go to your [HCP Cockpit](https://account.hanatrial.ondemand.com) and log on.


2. From the SAP HANA Cloud Platform Cockpit page, click **Subscriptions** in the navigation bar, scroll down until you see Mobile Services and click on its tile.

    ![Subscriptions tab](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/2.png)

3. Click on the **Go to Service** link to launch the HCPms Cockpit application.

    ![Go to Service](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/3.png)


4. The Mobile Services Cockpit will be displayed, click on the **Applications** tile.

    ![Applications tile](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/4.png)

5. Click on the **Create Application** icon to add a new application definition.

    ![Create Application](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/5.png)

6. Edit the fields as shown below (make sure you scroll down in the **Create Application** dialog box to fill in all fields) and click **Save**.

    Field Name                | Value
    :------------------------ | :-------------
    Application ID            | `com.northwind.hybrid`
    Version                   | `1.0` (default)
    Name                      | `Northwind`
    Type                      | `Hybrid` (from drop-down)
    Description               | `Hybrid Northwind app`
    Vendor                    | `SAP`
    Enable CSRF protection    | `<checked>`
    Ignore Case for User Name | `<checked>`
    Security Configuration    | `None` (from drop-down)

    ![Applications tile](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/6.png)

7. When the page updates, click on the **BACK END** tab.

    ![Applications tile](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/7.png)

8. Fill in the connection information as shown below, click **Save** and after your edits are saved, click the **back** arrow to return to the list of applications.

    The Backend URL will point to an external OData service, and will be proxied through HCPms. You can copy and paste the information below.

    Field Name                | Value
    :------------------------ | :-------------
    Backend URL         | `http://services.odata.org/V2/Northwind/Northwind.svc`
    Proxy Type          | `Internet` (from drop-down) 
    Authentication Type | `No Authentication` (from drop-down)
    Maximum Connections | `100`
    Rewrite mode        | `Rewrite URL on HCPms` (from drop-down)
    Relative Paths      | (leave blank)

    ![Applications tile](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/8.png)


9. You have successfully created the application in HCPms. Click on the Home icon (in the upper left corner of window) to return to the Mobile Services Cockpit. Note that the application and connection numbers have incremented.

    ![Applications tile](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-create-app/9.png)


## Next Steps
 - [Configure an SAP Web IDE project for hybrid builds](http://go.sap.com/developer/tutorials/hcpms-webide-hybrid-config.html)
