---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, topic>cloud, tutorial>license, products>sap-business-technology-platform, products>sap-business-application-studio  ]
primary_tag: topic>abap-extensibility
---

# Develop a Custom UI for an SAP S/4HANA Cloud Tenant
<!-- description --> Develop a custom UI based on a custom business object OData service in SAP Business Application Studio for an SAP S/4HANA Cloud tenant.

## Prerequisites
- You have an **SAP S/4HANA Cloud tenant** for development and a business user with UI Development authorization (this requires a business role with unrestricted write access containing business catalog *Extensibility - Fiori App Development* *`SAP_CORE_BC_EXT_UI`*).
- You have a custom business object with OData service running in the SAP S/4HANA Cloud tenant, for example `YY1_BONUSPLAN`, see [Create a Custom Business Object](abap-extensibility-cbo-create) and first two steps of [Expose Custom Business Object as External Web Service](abap-custom-ui-business-object)
- You have an SAP Business Technology Platform (SAP BTP) trial account with an SAP Business Application Studio subscription and a dev space within that, see preceding tutorials of this tutorial group

## You will learn
  - How to generate a Fiori elements list report with object page UI
  - How to preview the UI
  - How to deploy the UI as custom application to your SAP S/4HANA Cloud tenant
  - This process of custom UI development is the way to go if the UI generated within the SAP S/4HANA Cloud tenant does not match your needs

## Intro
You can get a generated UI for your custom business object and adapt it to your needs within the SAP S/4HANA Cloud tenant (in-app-extensibility). As building UIs in SAP BTP (side-by-side extensibility) offers more possibilities than in-app-extensibility, this tutorial gives you an idea how to build a UI based on a custom business object's OData service in SAP Business Application Studio, test it, and get it into the SAP S/4HANA Cloud tenant.

>Tutorial last updated in May 2021

---

### Launch SAP Business Application Studio

SAP Business Application Studio is a subscription-based service in SAP BTP. The SAP trial account offers an easy access directly from <https://account.hanatrial.ondemand.com>, however the following sequence describes the procedure via the global account in SAP BTP cockpit as needed for customer accounts.

1.  In your web browser, open the SAP BTP Trial cockpit <https://account.hanatrial.ondemand.com> and **Enter Your Trial Account**, which is a so called global account.

    <!-- border -->![Enter Global Trial Account ](btp-enter-global-trial-account.png)

2.  On your global account page, select default subaccount `trial`.

    <!-- border -->![Enter trial Subaccount](btp-enter-trial-subaccount.png)

3.  In the navigation pane expand the **Services** section.

4.  Select **Instances and Subscriptions**.

5.  Click the link or the icon at the SAP Business Application Studio Subscription.

<!-- border -->![Open SAP Business Application Studio from Subaccount](btp-open-bas.png)



### Open UI project creation wizard

SAP Business Application Studio offers UI generators with a wizard approach to create UI projects.

1. Start your Dev Space in case it is not running

    <!-- border -->![Start Dev Space](bas-dev-space-start.png)

2. Open your Dev Space

    <!-- border -->![Open Dev Space](bas-dev-space-open.png)

3. Go to **View > Find Command**.

    <!-- border -->![Open "Find Command"](bas-find-command.png)

4. Search for command **Fiori: Open Application Generator** and select it.

    <!-- border -->![search and choose "Fiori: Open Application Generator" command](bas-find-command-fiori-app-gen.png)



### Select floorplan

For **Application Type**, select the default  **SAP Fiori Elements**, choose **List Report Object Page** as **Floorplan** and click **Next**.

<!-- border -->![UI Wizard: Floorplan Selection](bas-appgen-floorplan-sel.png)


### Select Data Source

During the **Data Source and Service Selection** step, you define which system and OData Service the UI is based on, so that data structure and action information is used to generate a UI. It's also used for the preview to show and change data.

<!-- border -->![UI Wizard: Select Data Source](bas-appgen-source-sel.png)

1. As your **Data Source**, select **Connect to an SAP System**.

2. As your **System**, choose the destination you have created in the tutorial earlier (see [Connect SAP Business Application Studio and SAP S/4HANA Cloud tenant](abap-custom-ui-bas-connect-s4hc)).

3. Select your the OData service of your custom business object ends with `_CDS`, for example `YY1_BONUSPLAN_CDS`.

4. Click **Next**.


### Select entity

In this step you define the root node for your UI in the OData service.

<!-- border -->![UI Wizard: Select Entity](bas-appgen-entity-sel.png)

1. As **Main Entity**, select the custom business object root node `YY1_BONUSPLAN`.

2. Choose **Next**.


### Set Project Attributes

In this step, you set project attributes and choose to add further optional configurations.

<!-- border -->![UI Wizard: Set Project Attributes](bas-appgen-proj-attrs.png)

1. Define a **Module name**, which will later be the folder name of the UI Project and -- in combination with optional namespace - the application ID in SAP S/4HANA Cloud tenant. Example: `bonusplans`

2. Set the **Application title**, which will be visible as the browser tab title and title within the app. Example: `Bonus Plans`

3. Choose to set the **Deployment configuration** within the wizard by selecting `Yes`.

4. Choose to **Add FLP configuration** within the wizard by selecting `Yes`.

5. Select **Next**.


### Configure deployment settings

In this step, you define where you want the UI project to be deployed to as a runnable application.

<!-- border -->![UI Wizard: Configure Deployment Settings](bas-appgen-deploy-config.png)

1. Leave the default `ABAP` as **target** platform and as **Destination** the one you have created earlier (see [Connect SAP Business Application Studio and SAP S/4HANA Cloud tenant](abap-custom-ui-bas-connect-s4hc)).

2. Enter a **Name** that represents the UI5 ABAP repository the application will be deployed to. Example: `YY1_BONUSPLAN`. This repository name will be visible as your **Custom UI App ID** in your SAP S/4HANA Cloud tenant.

3. Enter a **Deployment Description** for the UI5 ABAP repository. This repository description will be visible as **Custom UI App Description** in your SAP S/4HANA Cloud tenant.

4. Select **Next**.

>Note that you can also configure the deployment later via the command line interface by using the following command:
```Shell/Bash
npx fiori add deploy-config
```


### Configure SAP Fiori launchpad settings and generate

The SAP Fiori launchpad (FLP) configuration is required to embed your application as a tile into the FLP.

<!-- border -->![UI Wizard: Configure FLP settings](bas-appgen-flp-config-finish.png)

1. Set a **Semantic Object**, for example `bonusplan`

2. Define the general **Action** that you want to be executed on the semantic object with the app, for example `manage`

3. Enter a **Title**, which will be displayed as the tile title in the FLP, for example `Manage Bonus Plans`

4. Select **Finish**.


The UI project is now being generated and dependencies are being installed. This may take a while and is displayed at the bottom right.

<!-- border -->![Installation toast on bottom Right](bas-appgen-installing-after-finish.png)

>Note that you can also configure the launchpad later via the command line interface by using the following command:
```Shell/Bash
npx fiori add flp-config
```


### Open UI project folder

1. To view the newly created project in the Explorer, select **Open Folder**.

    <!-- border -->![Choose Open Folder Option](bas-open-folder.png)

2. Choose the `user/projects` folder and select **Open**.

    <!-- border -->![Choose projects folder to open](bas-open-projects-folder.png)



### Preview UI

In this step, you can test the UI with the preview functionality.

1. Right-click the project folder and choose **Preview Application** from the context menu.

    <!-- border -->![Start Preview](bas-preview-start.png)

2. Select the `start` option, which will perform a preview based on the configured data source system, retrieve real data and enable you to create, edit and delete data in that system.

    <!-- border -->![Choose start preview option](bas-preview-system-data.png)

    A terminal is opened that automatically executes the underlying command.

    <!-- border -->![At preview opening terminal](bas-preview-terminal.png)

    Once the command has reached the required state, a new browser tab with the preview is opened.

3. To view existing entries, select **GO** or **Create** to add a new entry.

    <!-- border -->![Resulting preview browser tab](bas-preview-result.png)



### Deploy UI to S/4HANA ABAP tenant

Once the UI is set up to your needs, you can deploy it to the development SAP S/4HANA Cloud tenant, where it can be tested and transported to test or productive tenants.

1. Open a terminal for your project by right-clicking it and choosing **Open in Terminal** from the context menu.

    <!-- border -->![Open Project in Terminal](bas-open-project-in-terminal.png)

2. In the `cli` (command line interface) terminal, enter the following command:

    ```Shell/Bash
    npm run deploy
    ```
    <!-- border -->![Enter deploy command in cli](bas-cli-enter-deploy-command.png)

    Press return.

3. Check the deployment configuration.

    <!-- border -->![Check deployment configuration in command output](bas-cli-check-deploy-config.png)

4. Confirm the deployment by entering `y`

    <!-- border -->![Confirm deployment configuration](bas-cli-confirm-deployment.png)

5. Deployment will start, which might take a while.

    <!-- border -->![Deployment log](bas-cli-deployment-log.png)

    Once the deployment is completed, a **Deployment Successful.** message is displayed in the log.


### Test yourself








---
