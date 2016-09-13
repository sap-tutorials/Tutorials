---
title: Create a Destination on HANA Cloud Platform
description: Create a Destination to allow HANA Cloud Platform to read/write data
tags: [ products>sap-hana-cloud-platform, topic>cloud, topic>mobile, topic>odata, tutorial>beginner ]
---

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  This tutorial is used in several different series.  Choose the series to move backward:
     - **Mobile Application Series**  [Sign up for an account on HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-create-trial-account.html)
     - **SAPUI5 with Web IDE Series** [Create an empty UI5 Web IDE project](https://go.sap.com/developer/tutorials/sapui5-webide-create-project.html)

## Next Steps
- This tutorial is used in several different series.  Choose the correct series below to move forward.
    - **Mobile Application series** [Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/hcp-template-mobile-web-app.html)
    - **SAPUI5 with Web IDE Series** [Set up the `datasource` in the application](https://go.sap.com/developer/tutorials/sapui5-webide-setup-datasource.html)

## Details

### You will learn
Destinations are used for the outbound communication of your application to a remote system (which can be a cloud or on-premise system). You will create a destination by giving it a name, URL of the remote system or service, the authentication type, and some other configuration data.

The data source you will use in this tutorial series is called "Northwind", which is a publicly accessible OData source hosted by <http://www.odata.org>:

The data source URL is <http://services.odata.org/V2/Northwind/Northwind.svc/>

### Time to Complete
**< 5 min**

---

1. Go to <https://account.hanatrial.ondemand.com> and log in to your HCP cockpit.

    ![HCP log in page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-create-destination/mob1-1_1.png)

2. Select the **Destinations** tab on the left side, and then click on **New Destination** to open a new destination configuration form.

    ![Web IDE Destination tab](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-create-destination/mob1-1_2.png)

3. Enter/confirm all fields in the Destination configuration section with the information below.

    Field Name     | Value
    :------------- | :-------------
    Name           | `Northwind`
    Type           | `HTTP`
    Description    | `Northwind OData Service`
    URL            | `http://services.odata.org`
    Proxy Type     | `Internet`
    Authentication | `NoAuthentication`

    Add three **Additional Properties** fields by clicking on the **New Property** button once for each property.

    Field Name       | Value
    :--------------- | :-------------
    `WebIDEEnabled`  | `true`
    `WebIDESystem`   | `Northwind_Data`
    `WebIDEUsage`    | `odata_gen`

    ![Completed HCP destination](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-create-destination/mob1-1_3.png)

4. Click **Save**, and that's it.  You are ready for the next step in your tutorial.

## Next Steps
Several of the tutorial series create a Northwind destination.  To return to your tutorial series, pick the series from the list below:

- [**Mobile Application series** - Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/hcp-template-mobile-web-app.html)
- [**SAPUI5 with Web IDE series** - Create an empty UI5 Web IDE project](https://go.sap.com/developer/tutorials/sapui5-webide-create-project.html)
