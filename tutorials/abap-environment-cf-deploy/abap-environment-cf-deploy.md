---
title: Deploy UI to Cloud Foundry
description: Create UI in Neo and deploy it to Cloud Foundry.
auto_validation: true
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
author_name: Niloofar Naseri
author_profile: https://github.com/niloofar-naseri
---
   
## Prerequisites  
 - Existing OData service
 - Existing service catalog

## Details
### You will learn  
  - How to create UI on Neo
  - How to deploy the UI form Neo to the Cloud Foundry

---

[ACCORDION-BEGIN [Step 1: ](Create multi-target application)]

  1. Log in to your SAP Cloud Platform Cockpit with your global account and choose your Neo subaccount.

  2. Click on **Services** and choose **SAP Web IDE Full-Stack**.

      ![open web ide](WebIDE.png)

  3. Click on **Go to Service**.

      ![go to service](gotoservice.png)

  4. Choose **New Project from Template**.

      ![template](fromTemplate.png)

  5. Click on **Multi-Target Application** and **Next**.

      ![multi target](MultiTarget.png)

  6. Enter a **Project Name** and click on **Next**.

      ![project name](projectName.png)

  7. Enter **Application ID** and **Application Version**. Check **Use HTML5 Application Repository** and click on **Finish**.

      ![HTML5 app](AppID.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create HTML5 module)]

  1. Open HTML5 Module with a right click on project name > **New** > **HTML5 Module**.

      ![HTML5 Module](HTML5.png)

  2. Choose **List Report Application** and click on **Next**.

      ![Module](module1.png)

  3. Enter a **Module Name**, **Title** and click on **Next**.

      ![Module](module2.png)

  4. Choose an existing **Service Catalog**, select a **Service** and click on **Next**.

      ![Choose service catalog](ServiceCatalog.png)

  5. Select an **Annotation**, click on **Next**, select an **OData Collection** and click on **Finish**.

      ![Annotation](annotation.png)

      ![Collection](odataCollection.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create index.html file in webapp folder)]

  1. Do a right click on `webapp` folder under your application and choose **New** > **File**, enter a **File Name** like `index.html` and click on **OK**.

      ![new File](newFile.png)

      ![index](index.png)

  2. Edit `index.html` file. Replace `Fiori_App` with your HTML5 module name.

```swift
  <!DOCTYPE html>​
<html>​
<head>​
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />​
    <meta http-equiv="Content-Type" content="text/html;charset=UTF-8"/>​
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />​
    <title>MyFirstFiori</title>​
    <!-- Bootstrapping UI5 -->​
    <script id="sap-ui-bootstrap"​
            src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js"​
            data-sap-ui-libs="sap.m"​
            data-sap-ui-theme="sap_bluecrystal"​
            data-sap-ui-compatVersion="edge"​
            data-sap-ui-preload="async"​
            data-sap-ui-resourceroots='{"Fiori_App": "."}'​
            data-sap-ui-frameOptions="trusted">​
    </script>​
    <script>​
        sap.ui.getCore().attachInit(function () {​
            sap.ui.require([​
                "sap/m/Shell",​
                "sap/ui/core/ComponentContainer"​
            ], function (Shell, ComponentContainer) {​
                // initialize the UI component​
                new Shell({​
                    app: new ComponentContainer({​
                        height : "100%",​
                        name : "Fiori_App"​
                    }),​
                    appWidthLimited : false​
                }).placeAt("content");​
            });​
        });​
    </script>​
</head>​
<!-- UI Content -->​
<body class="sapUiBody" id="content">​
</body>​
</html>​

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Adapt neo-app.json and xs-app.json)]

  1. Open `neo-app.json` file and change the `welcmeFile` form `/webapp/test/flpSandbox.html` to `/webapp/index.html`.

      ![neo-app](neo-app.png)

  2. Open `xs-app.json` file and change the `welcmeFile` form `/test/flpSandbox.html` to `index.html`. In routes change `authenticationType` to  `xsuaa` and `service` to `abapcp` and remove `destination`. If there is no `service`, you can add it yourself.

      ![xs-app](xs-app.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Delete local annotations (optional) )]

  1. Open `annotations.xml` and delete `UI.Facets` under **Local Annotation**.

      ![delete annotation](localanno.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test UI on Neo)]

  1. Do a right click on your application and choose **Run** > **Run as** > **Web Application**, and choose `index.html` as the file to run and click on **OK**.

      ![run Application](run.png)

      ![run Application](runAs.png)

  2. Click on setting button and choose the fields which should be shown on UI and click on **OK** and then click on **Go** to see the fields.

      ![UI Setting](UISetting.png)

      ![UI Fields](UIFields.png)

      ![UI Elements](UIElements.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Adapt app router configuration)]

  1. Remove destination from MTA modules and resources. For that open the `mta.yaml` file and remove `dest_MYFIORI_TEST` from `MTA_TEST_appRouter` under **Modules** and under **Resources** as well.

      ![app Router config](appRouter1.png)

      ![app Router config](appRouter2.png)

  2. Add ABAP application runtime instance as resource MTA. For that click on **+** button under **Resources**, enter `org.cloudfoundry.existing-service` as **Type**, `service-name` as **Key** and the name of ABAP service instance in space in Cloud Foundry subaccount as **Value**.

      ![app Router config](appRouter3.png)

  3. Save `mta.yaml` file and close it and reopen the file.

  4. Add ABAP resource to **Requires** of `AppRouter`. For that click on **Modules**, click on `MTA_TEST_appRouter`, click on **+** button and find the created resource in the last step.

      ![app Router config](appRouter4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Deploy UI to Cloud Foundry)]

  1. Right click on your application and choose **Project** > **Project Settings**.

      ![Open project setting](projectEdit.png)

  2. Set details of **Use the following Cloud Foundry configuration** in **Cloud Foundry**. You will need to enter the **API Endpoint**, **Organization** and **Space**. Click **Install/Reinstall Builder** and **Save**.

      ![project setting](save.png)

  3. Do again a right click on your application and choose **Build**. It will take a while and you can see the build result in `mta_archives`.

      ![Build](Build.png)

      ![Build](Build1.png)

 4. Right click on `.mtar` file and choose **Deploy** > **Deploy to SAP Cloud Platform**. Choose again the **API Endpoint**, **Organization** and **Space**. Now click on **Deploy**.

      ![Deploy](Deploy.png)

      ![Deploy1](Deploy1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Check app existence on Cloud Foundry)]

  1. Log in to SAP Cloud Platform Cockpit, go to your space and choose **Application**. Check if your `appRouter` is started.

      ![cloud](cloud1.png)

  2. Click on your `appRouter` and check the **Service Binding**.

      ![cloud](cloud2.png)

  3. Click on **Overview** and copy the link under **Application Routes**.

      ![cloud](cloud3.png)

  4. Use following URL-Pattern to run the application: `<route-on-cf>/<id>.<version>`

      ID: id used in `webapp/manifest.json`.

      Version: `applicationVersion` provided in `webapp/manifest.json`.

      Example URL: `https://<link-from-application-routes>/Fiori_App-1.0.0/`.

  5. Now you can test your application UI. Click on setting button and choose the fields which should be shown on UI and click on **OK** and then click on **GO** to see the fields.

      ![UI Setting](UISetting.png)

      ![UI Fields](UIFields.png)

      ![UI Elements](UIElements.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Test yourself)]


[VALIDATE_1]
[ACCORDION-END]


---
