---
title: Create an SAP Fiori App Using SAP Business Application Studio
description: Create an SAP Fiori application in SAP Business Application Studio and deploy it to your SAP Cloud Platform, Cloud Foundry environment.
auto_validation: true
time: 25
tags: [ tutorial>beginner, topic>sapui5, products>sap-fiori, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

## Prerequisites
  - You have created an SAP Fiori dev space in SAP Business Application Studio. See [Create a preconfigured dev space for developing SAP Fiori apps](appstudio-devspace-fiori-create).
  - The SAP Fiori dev space is in status `RUNNING` and you opened it.
  - You configured a destination to SAP Gateway Demo System (ES5) (see [Connect SAP Cloud Platform to Your SAP Gateway Demo System Account (ES5)](cp-portal-cloud-foundry-gateway-connection)).
  - For the deployment step, additional prerequisites apply. You need to have the following available in the Cloud Foundry space to which you will log on (see [Add a New Entitlement to Your Subaccount](cp-cf-entitlements-add)):
      - Application Runtime: 1GB free
      - Destination: 1 free
      - HTML5 Applications: 1 free


## Details
### You will learn
  - How to create an SAPUI5 application for SAP Cloud Platform, Cloud Foundry environment
  - How to configure Cloud Foundry settings in SAP Business Application Studio
  - How to build and deploy an application to Cloud Foundry

---

[ACCORDION-BEGIN [Step 1: ](Create new SAP Fiori project)]

1. Create a new SAP Fiori project from a template.

    !![Open Dev Space](03-01-02 AppStudio Welcome Tab_.jpg)

    >Using the UI wizard you can at any point click the `reset` button to reset the wizard at the top-right of the wizard screen, click the `Back` button to go back to the previous step, or click the specific wizard step to go back to that step.

2. Make sure that the target folder is `/home/user/projects`, select the **Fiori Project** template, and click **Next**.

    !![Fiori project template](03-03 AppStudio Fiori Project Template_.jpg)

3. For **Target Running Environment**, select the following, and click **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Select the target running environment | **Cloud Foundry** |
    | B | Select the template you want to use | **SAPUI5 Application** |

    !![fiori project template](03-04 AppStudio Fiori Project Template Target Running Environment_.png)

4. For **Project Name**, enter `FioriDemo`, and click **Next**.

    !![Fiori project template - project name](03-05 AppStudio Fiori Project Template Project Name.jpg)

5. For **HTML5 Applications**, select **Standalone Approuter**, and click **Next**.

    !![Standalone Approuter](03-06-01 AppStudio HTML5 Applications Runtime.png)

6. For **Basic Attributes**, select the following, and click **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Enter an HTML5 module name | **`BusinessPartners`** |
    | B | Do you want to add authentication | **No** |
    | C | Enter a namespace | **`ns`** |

    !![Fiori project template - basic attributes](03-06 AppStudio Fiori Project Basic Attributes.jpg)

7. For **View Name**, select the following, and click **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Enter a view name | **`Suppliers`** |
    | B | Do you want to add a data service | **Yes** |

    !![Fiori project template - view name](03-07 AppStudio Fiori Project View Name.png)

8. For **Consume Services**, select the following, and click **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Select a system | **My SAP systems** |
    | B | Select a source | **`ES5 [Catalog]`** |
    | C | Select a service | **`GWSAMPLE_BASIC`** |

    !![Fiori project template - consume services](03-08 AppStudio Fiori Project Providers.jpg)

    >A notification that the project has been generated appears at the bottom right of the screen.

    !![Fiori project template - project generated](03-08-02 AppStudio Fiori Project Project Generated.jpg)

9. Click **Open in New Workspace** in the notification or **File > Open Workspace**, and choose `FioriDemo`.

    !![AppStudio open workspace](03-09 AppStudio Open Workspace_.png)

    >The **Explorer** opens and you can see the `FioriDemo` project, its folder structure, and files. If not, you can click the **Explorer** view button at the top left of the screen.

    >The status bar color changes to blue, indicating that a workspace is open.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the layout editor)]

Open the layout editor in SAP Business Application Studio to easily make a few changes. In this case, you will make changes so that data from the backend service is displayed when the app is running.

1. Choose **`FioriDemo` > `BusinessPartners` > `webapp` > `view`** and right-click the `Suppliers.view.xml` file that you created with the template in a previous step.

2. Choose **Open With > Layout Editor**.

    !![Open with Layout Editor](04-01 AppStudio Open Layout Editor_.jpg)

    >The **Suppliers** view is opened in the **Layout Editor**.

    >!![Layout Editor Opened](04-01-02 AppStudio Open Layout Editor.jpg)

3. You can optionally choose to open it with the code editor and see how modifications in the Layout Editor are manifested in the code editor.

    !![Open code editor](04-03 AppStudio Open Code Editor-XML_.jpg)

    >The **Suppliers** view is opened in the code editor in a tab next to the **Layout Editor**.

    >!![Code editor opened](04-03-02 AppStudio Open Code Editor-XML.jpg)

4. For convenience, place the code editor below the Layout Editor. Use the drag & drop functionality.

    !![Drag-Drop editor](04-04 AppStudio Drag-Drop Code Editor.jpg)

    >The **Layout Editor** and code editor are stacked so you can see how making change to one will be reflected on the other.

    >!![Editor dropped](04-04-02 AppStudio Drag-Drop Code Editor.jpg)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Make changes to the UI)]

Make some changes using the layout editor, with no need to do any coding.

1. In the **Controls** pane, enter `List` to filter the controls list in the search box.

    !![Filter List control](05-01 AppStudio Layout Editor Filter Controls-List.jpg)

2. Drag the **List** control and drop it on the **View** control in the canvas.

    !![Drag and drop](05-02 AppStudio Layout Editor List Dropped_.jpg)

3. Select the **Standard List Item** control and, in the **Entity Set** property in the **Properties** pane, click the Bind icon.

    !![Open entity set bind window](05-03 AppStudio Layout Editor Bind to Entity Set_.jpg)

    >The **Select Entity Set** view is displayed.

4. Select the **Define entity set and set the selected control as template** option, and in the **Entity Set** dropdown list, choose the `BusinessPartnerSet` entity set. Click **Bind** to complete the operation.

    !![entity set bind window](05-04 AppStudio Layout Editor Bind to Entity Set_.jpg)

    >The bind operation is reflected in both the **Layout Editor** and the code editor.

    >!![entity set bind window](05-04-02 AppStudio Layout Editor Bind to Entity Set_.jpg)

5. In the **Properties** pane, in the **Title** property, click the **Bind** icon.

    !![open Title bind window](05-05 AppStudio Layout Editor Bind Title_.jpg)

    >The **Data Binding** view is displayed.

6. Click the **Clear expression** (eraser) icon to clear the default text, and in the data fields double click  `CompanyName`. Click **Bind** to complete the operation.

    !![Title bound](05-06 AppStudio Layout Editor Bind Title_.jpg)

7. Repeat the last steps for the **Description** property in the **Properties** pane. Choose  `BusinessPartnerID`.

    !![Bind Description](05-07 AppStudio Layout Editor Bind Description_.jpg)

[VALIDATE_5]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test run the application)]

Run your new application to test it.

1. Open the **Run Configurations** view.

    !![Open Run Configurations](06-01 AppStudio Run Configurations_.jpg)

2. Click **+** and select **`BusinessPartners`**.

    !![Create new run configuration](06-02 AppStudio Run Configurations_.jpg)

3. For the next steps of the wizard, select the following:

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | What would you like to run | **`BusinessPartners`** |
    | B | Select a runnable file | **`index.html`** |
    | C | Select a UI5 version | **latest** |
    | D | Enter a name | `Run BusinessPartners (ES5)` |

    >A new run configuration is generated for the `FioriDemo` project.

4. Expand the run configuration to display the services that can be bound.

    !![Bindable objects](06-04 AppStudio Run Configurations_.jpg)

    >SAP Business Application Studio allows you to test your app with resources.

5. To bind to the destination, click the **Bind** icon to the right of the destination resource
to get a list of available destinations.
    !![Bind to Destination](06-05 AppStudio Run Configurations Bind Destination_.jpg)

6. Select the `ES5` destination from the list.
    !![Select Destination](06-05-02 AppStudio Run Configurations Bind Destination_.jpg)

    >Once the destination has been bound, the **Bind** icon turns green.

    >To unbind the destination, click the **Unbind** icon.

    >!![Destination is bound](06-05-03 AppStudio Run Configurations Bind Destination_.jpg)

7. Hover over the run configuration and click the Run Module icon.  

    !![Running the app locally](06-06 AppStudio Run Configurations Run_.jpg)

8. Wait for the **A service is listening to port 6004** notification and then click the button to open the app.

    >The left side pane changes to the debug pane and the status bar color changes to orange to indicate that the app is running in debug mode.

    >If you are running the app for the first time, the button in the notification will say **Expose and Open**. Otherwise it will say **Open in New Tab**.

    !![App is running locally](06-07 AppStudio Run Configurations Run.jpg)

    >You may optionally add a port description.

    >You may need to authenticate yourself to access the backend.

    The app is opened in a new tab and a list of suppliers is displayed.

    !![SAP Fiori app is running](AppStudio Run Configurations-16.jpg)

[VALIDATE_7]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Build the application)]

1. In the menu bar, select **Terminal | New Terminal**.

    >A terminal is opened in the project's folder.

    !![terminal in project folder](07-01 AppStudio Terminal set to Project Folder_.png)

2. Execute the following command:

    ```Shell/Console
    mbt build -p=cf
    ```

    !![terminal mbt build](07-02 AppStudio Terminal MBT Build.jpg)

    >The build process creates a multitarget archive (`MTAR`) file in your project that packages all the project modules for deployment. You can find the `MTAR` file in the `DemoFiori/mta_archives` folder.

    !![terminal mbt build results](07-02-02 AppStudio Terminal MBT Build_.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Set Cloud Foundry preferences)]

Before you can deploy your new application, set your Cloud Foundry preferences.

1. In the menu bar, select **View** | **Find Command** to open the **command palette**.

    !![Command Palette-Login to CF](08-01 AppStudio CF Login_.jpg)

2. Select the command **CF: Login to cloud foundry**.

    >Type `cf` to filter commands.

    !![Command Palette-Login to CF](08-01-02 AppStudio CF Login_.jpg)

3. When prompted, provide your credentials, select the API endpoint, organization, and space for your project.

    >The Cloud Foundry organization and space appear in the status line at the bottom left part of the screen.

    !![Logged in to CF](08-02 AppStudio CF Login_.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Deploy the application)]

Deploy your application to SAP Cloud Platform, Cloud Foundry environment.

Right-click the `mtar` file and select **Deploy MTA Archive**.

!![deploy mtar](09-01 AppStudio Fiori Project Deploy_.jpg)

>The application deployment to the space you are connected to starts and a notification appears. The deployment process takes a few minutes. You can see that the deployment is still in progress in the **Task: Deploy** console at the bottom right of your screen.

>When the deployment process is complete, you should see a notification.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Get URL to access the application)]

Access your deployed application in the SAP Cloud Platform cockpit. The steps below show you how to create a URL that you can use to access your new application.

1. Access the space to where the app is deployed and go to the **Applications** tab.

    !![Application's space](10-01 SCP Space Applications_.jpg)

2. Make sure your application is in **Started** state, and  click its name (`fioridemo_approuter`). The **`Application: fioridemo_approuter - Overview`** page opens.

3. Right-click the URL under **Application Routes** and save the URL in a text file.

    !![Get application base URL](10-03 SCP Space Application URL_.jpg)

4. Locate the `sap.app id` from the `manifest.json` file located in your HTML5 module, and add it to the copied link after removing the periods.

    !![app id from manifest](10-04 AppStudio SAP Fiori Project Manifest_.jpg)

    > For future reference, this is the construct of the final URL: `<URL_from_application_overview_page>/<mynamespace><project_name>/index.html`.

    >Example: `https://SUBACCOUNT-SPACE-fioridemo-approuter.cfapps.eu10.hana.ondemand.com/nsBusinessPartners/index.html`

    You can use this URL in any browser to access your new application.

[VALIDATE_10]
[ACCORDION-END]



---
