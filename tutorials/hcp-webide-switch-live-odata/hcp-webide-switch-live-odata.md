---
title: Switch your app from mock data to a live OData service
description: After running your app with mock data, learn how to switch it to a live OData service
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, tutorial>intermediate ]
---

## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:**
   - [Build an SAPUI5 app based on your data model and run it with mock data](http://go.sap.com/developer/tutorials/hcp-webide-build-app-mock-data.html)
   - [Create an account on the Gateway Demo system](http://go.sap.com/developer/tutorials/gateway-demo-signup.html)

## Next Steps
 - [Select a new tutorial series in the tutorial catalog](http://go.sap.com/developer/tutorials.html)

## Details

### You will learn  

The previous two tutorials introduced a scenario where app development was going to precede the availability of an OData service. To work in parallel with the service development, you created an OData model (which would have been based on Design Thinking sessions with the users) and then built your SAPUI5 app based on that model. You also were able to run it against the SAP Web IDE mock data server. This tutorial will show you how to configure your finished application to run against a live service.

Converting an app built on mock data to a live service requires editing only two files if there is perfect agreement between the OData models (the one you created by hand, and the model in the final service). For completeness, the metadata model in the app project should also be updated (but it is not downloaded to the client or used when the app runs). In this tutorial, you will make a third required change to show how to accommodate a less than perfect alignment between the models.

### Time to Complete

**15 Min**.

---

1. The live service you will use is the [ES4 Gateway service](https://sapes4.sapdevcenter.com/). To begin, please follow the steps in the Gateway Service sign up tutorial, and then continue with step 2 below.

    > For reference, the service document URL you will use is: <https://sapes4.sapdevcenter.com/sap/opu/odata/IWBEP/GWDEMO>


2. To provide access to the service, you will need create a destination in HCP. Open a browser window to your HCP Cockpit, click on **Destinations**, then **New Destination**. Enter the field values and **Additional Properties** below and click **Save**.

    Field Name     | Value
    :------------- | :-------------
    Name           | `Gateway_SAPES4`
    Type           | `HTTP`
    Description    | `DevCenter Gateway`
    URL            | `https://sapes4.sapdevcenter.com`
    Proxy Type     | `Internet`
    Authentication | `BasicAuthentication`
    User           | `<Your ES4 Gateway user> (e.g. p12345678)`
    Password       | `<Your ES4 Gateway password>`

    Add three Additional Properties fields by clicking on the **New Property** button once for each property.


    Field Name       | Value
    :--------------- | :-------------
    `WebIDEEnabled`  | `true`
    `WebIDESystem`   | `gateway`
    `WebIDEUsage`    | `odata_gen`


3. To establish a baseline for the source code you have written so far, you will deploy it to SAP HANA Cloud Platform and commit the project to Git. If you aren't sure how to do this, refer to the two tutorials below:

    - [HCP Deployment tutorial](http://go.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html) (but select your current project not `northwind`)
    - Commit the code to Git following the [Git Tutorial](http://go.sap.com/developer/tutorials/hcp-webide-commit-git.html)


4. As mentioned above, the four files file to be changed are:

    - `Component.js`
    - `neo-app.json`
    - `metadata.xml`
    - `Master.view.xml` (required because there is a slight difference between the models)

5. Open `Component.js` in Web IDE, and search for `serviceConfig` in the metadata definition.

    ![Component.js](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_5.png)

6. Update the two fields under `serviceConfig` (`name` and `serviceUrl`) to point to the ES4 Gateway service document and save your edits.

    Field Name       | Value
    :--------------- | :-------------
    `name`           | `GWDEMO`
    `serviceUrl`     | `/destinations/Gateway_SAPES4/sap/opu/odata/IWBEP/GWDEMO`

    ![serviceConfig edit](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_6.png)

7. Open the `neo-app.json` file, paste in the additional route definition below to point to the ES4 Gateway and save your edits.

    ```json
   {
      "path": "/destinations/Gateway_SAPES4",
      "target": {
        "type": "destination",
        "name": "Gateway_SAPES4"
      },
      "description": "DevCenter Gateway"
    },
    ```
    ![neo-app.json edit](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_7.png)


8. To display the metadata document used by the service, open a browser tab to the service document URL and add `$metadata` to the end (if prompted to log in, use your ES4 Gateway user ID and password).

    <https://sapes4.sapdevcenter.com/sap/opu/odata/IWBEP/GWDEMO/$metadata>

    At the top of the page, note that the `Namespace` is `GWDEMO`.

    ![namespace field in Gateway data model](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_8.png)

9. Scroll down a bit and you will see the `SalesOrder EntityType` with a structure that is consistent with the `SalesOrder EntityType` in the OData model you created.

    ![EntityType in Gateway data model](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_9.png)


10. In the same page, search for `EntitySet Name=` to find the name assigned to the `GWDEMO.SalesOrder EntityType`. The name of the collection is `SalesOrderCollection`.

    ![Collection name in Gateway data model](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_10.png)


11. Since you have the metadata displayed, copy the contents of the entire page from the opening `<edmx.Edmx>` element to the closing `</edmx.Edmx>` tag and replace the contents of the `model/metadata.xml` file in your project and save your edits.

    >Note: do not include the first line of the web page that starts with “This XML file…”

    While updating this file is not necessary for the app to run, it should be done for future reference.

    Your `metadata.xml` file should now look like this:

    ![updated metadata.xml](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_11.png)


12. Open the `view/Master.view.xml` file, and locate the `List` element.

    ![Master.view.xml List element](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_12.png)

13. In the `List` element, the items attribute points to the primary collection (displayed in the “master” list of the app). It currently points to `SalesOrders` which is the name you assigned when you created your OData model. Change the items attribute to the collection name used in the final data model (from step 10 above: `SalesOrderCollection`).

    ![Updating Collection name](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_13.png)

14. Save all your changes, select the `index.html` file and run the app. The data displayed is now sourced from the ES4 Gateway rather than your mock data.

    ![App running against Gateway OData service](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-switch-live-odata/m104_3_14.png)

15. You should now commit and push your edits to Git and redeploy the app to HCP.


## Next Steps
 - [Select a new tutorial series in the tutorial catalog](http://go.sap.com/developer/tutorials.html)
