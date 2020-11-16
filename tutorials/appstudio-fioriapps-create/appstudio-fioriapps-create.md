---
title: Create an SAP Fiori App Using SAP Business Application Studio
description: Develop a simple SAP Fiori application in SAP Business Application Studio. The application will present a list of suppliers from an on-premise backend. The suppliers' data will be retrieved from the BusinessPartnerSet collection in the GWSAMPLE_BASIC OData service that is available from SAP's ES5 demo gateway.
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


## Details
### You will learn
  - How to create an SAPUI5 application for SAP Cloud Platform, Cloud Foundry environment
  - How to test-run the app locally in the dev space

After a period of idle time the dev space is automatically stopped. In order to re-start the dev space open the [dev space manager](https://triallink.eu10.trial.applicationstudio.cloud.sap/), click the **Play** icon, and click the dev space.

The period for idle time for Factory accounts is different than for trial accounts.

---

[ACCORDION-BEGIN [Step 1: ](Create new SAP Fiori project)]

1. In the **Welcome Page** click **Create project from a template**.

    !![Open Dev Space](03-01-02 AppStudio Welcome Tab withOUT Extensions Loaded Notification_.jpg)

    >If the Welcome Page does not appear, in the menu bar, select **View | Find Command** to open the **command palette** and select the command **SAP Business Application Studio: Welcome Page**. The command palette is opened at the top-center of the SAP Business Application Studio window.

    >!![Welcome Page from command palette](03-01-03 AppStudio Welcome Page from Command Palette_.jpg)

    >The easiest way to develop an SAP Fiori freestyle app from scratch is to create it from a template. To continue developing an existing application, the best practice is to use git source code management and clone the repository.

    >Using the UI wizard you can at any point click the `reset` button to reset the wizard at the top-right of the wizard screen, click the `Back` button to go back to the previous step, or click the specific wizard step to go back to that step.
    >For convenience, click the **Explorer** view button to close the `Explorer` view.

    > You can also create a project from the terminal using Yeoman.

2. Make sure that the target folder is `/home/user/projects`, select the **SAP Fiori Freestyle Project** template, and click **Next**.

    !![Fiori project template](03-03 AppStudio Fiori Project Template_.jpg)

3. For **Target Running Environment**, select the following, and click **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Select the target running environment | **Cloud Foundry** |
    | B | Select the template you want to use | **SAPUI5 Application** |

    !![fiori project template](03-04 AppStudio Fiori Project Template Target Running Environment_.png)

4. For **Project Name**, enter `FioriDemo`, and click **Next**.

    !![Fiori project template - project name](03-05 AppStudio Fiori Project Template Project Name.jpg)

5. For **HTML5 Applications**, if you plan to integrate the app to a launchpad site, select **Managed by SAP Cloud Platform** and provide a unique service name. Otherwise, select **Standalone Approuter**, and click **Next**.

    !![Standalone Approuter](03-06-01 AppStudio HTML5 Applications Runtime.png)

    >The application router is the single point-of-entry for an application running in the Cloud Foundry environment on SAP Cloud Platform. The application router is used to serve static content, authenticate users, rewrite URLs, and forward or proxy requests to other micro services while propagating user information.

    >To simplify the tutorial the Standalone Approuter option is used.

6. For **Basic Attributes**, select the following, and click **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Enter an HTML5 module name | **`BusinessPartners`** |
    | B | Do you want to add authentication | **No** |
    | C | Enter a namespace | **`ns`** |
    | D | Do you want to enable Karma tests? | **No** |

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

    !![AppStudio open workspace](03-09 AppStudio Open Workspace_.jpg)

    >The **Explorer** view opens and you can see the `FioriDemo` project, its folder structure, and files. If not, you can click the **Explorer** view button at the top left of the screen.

    >The status bar color changes to blue, indicating that a workspace is open.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the layout editor)]

Open the layout editor in SAP Business Application Studio to easily make a few changes. In this case, you will make changes so that data from the backend service is displayed when the app is running.

1. Choose **`FioriDemo` > `BusinessPartners` > `webapp` > `view`** and right-click the `Suppliers.view.xml` file that you created with the template in a previous step.

2. Choose **Open With > Layout Editor**.

    !![Open with Layout Editor](04-01 AppStudio Open Layout Editor_.jpg)

    >To have the Layout Editor option available after opening the workspace, you may need to wait a bit for the Layout Editor extension to be loaded.

    >The **Suppliers** view is opened in the **Layout Editor**.

    >!![Layout Editor Opened](04-01-02 AppStudio Open Layout Editor.jpg)

3. You can optionally choose to open it with the code editor and see how modifications in the Layout Editor are manifested in the code editor.

    !![Open code editor](04-03 AppStudio Open Code Editor-XML_.jpg)

    >The **Suppliers** view is opened in the code editor in a tab next to the **Layout Editor**.

    >!![Code editor opened](04-03-02 AppStudio Open Code Editor-XML.jpg)

4. For convenience, place the code editor below the Layout Editor. Use the drag & drop functionality.

    !![Drag-Drop editor](04-04 AppStudio Drag-Drop Code Editor.jpg)

    >The **Layout Editor** and code editor are stacked so you can see how making changes to one will be reflected on the other.

    >!![Editor dropped](04-04-02 AppStudio Drag-Drop Code Editor.jpg)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Make changes to the UI)]

Make some changes using the layout editor, with no need to do any coding.

1. In the **Controls** pane, enter `List` to filter the controls list in the search box.

    !![Filter List control](05-01 AppStudio Layout Editor Filter Controls-List.jpg)

2. Drag the **List** control and drop it on the **View** control in the canvas.

    !![Drag and drop](05-02 AppStudio Layout Editor List Dropped_.jpg)

3. Select the **Standard List Item** control by clicking it (the breadcrumb indicates which control is selected) and, in the **Entity Set** property in the **Properties** pane, click the Bind icon.

    !![Open entity set bind window](05-03 AppStudio Layout Editor Bind to Entity Set_.jpg)

    >The **Select Entity Set** view is displayed.

4. Select the **Define entity set and set the selected control as template** option, and in the **Entity Set** dropdown list, choose the `BusinessPartnerSet` entity set. Click **Bind** to complete the operation.

    !![entity set bind window](05-04 AppStudio Layout Editor Bind to Entity Set_.jpg)

    >The space of the **Select Entity Set** view may be to narrow to show all options. In case you do not see the **Define entity set and set the selected control as template** option, scroll down in the **Select Entity Set** view to make it available.

    >The bind operation is reflected in both the **Layout Editor** and the code editor.

    >!![entity set bind window](05-04-02 AppStudio Layout Editor Bind to Entity Set_.jpg)

5. In the **Properties** pane, in the **Title** property, click the **Bind** icon.

    !![open Title bind window](05-05 AppStudio Layout Editor Bind Title_.jpg)

    >The **Data Binding** view is displayed.

6. Click the **Clear expression** (eraser) icon to clear the default text, and in the data fields double click  `CompanyName`. Click **Bind** to complete the operation.

    !![Title bound](05-06 AppStudio Layout Editor Bind Title_.jpg)

7. Repeat the last two steps for the **Description** property in the **Properties** pane. Choose  `BusinessPartnerID`.

    !![Bind Description](05-07 AppStudio Layout Editor Bind Description_.jpg)

[VALIDATE_5]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test run the application)]

Run your new application to test it.

1. Open the **Run Configurations** view.

    !![Open Run Configurations](06-01 AppStudio Run Configurations_.jpg)

2. Click **+** to create a new **Run Configurations**.

    !![Create new run configuration](06-02 AppStudio Run Configurations_.jpg)

    >Creating a new **Run Configuration** launches the command palette, a text-based mini wizard. The command palette is opened at the top-center of the SAP Business Application Studio window.

3. When "What would you like to run?" question appears, select **`BusinessPartners`**.

    >!![run configuration select BusinessPartners](06-02-01 AppStudio Run Configurations_.jpg)

4. For the next steps of the wizard, select the following:

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Select a runnable file | **`index.html`** |
    | B | Select a UI5 version | **latest** |
    | C | Enter a name | `Run BusinessPartners (ES5)` |

    >A new run configuration is generated for the `FioriDemo` project.

5. Expand the run configuration to display the services that can be bound.

    !![Bindable objects](06-04 AppStudio Run Configurations_.jpg)

    >SAP Business Application Studio allows you to test your app with resources.

6. To bind to the destination, click the **Bind** icon to the right of the destination resource to get a list of available destinations.

    !![Bind to Destination](06-05 AppStudio Run Configurations Bind Destination_.jpg)

7. Select the `ES5` destination from the list.

    !![Select Destination](06-05-02 AppStudio Run Configurations Bind Destination_.jpg)

    >Once the destination has been bound, the **Bind** icon turns green.

    >To unbind the destination, click the **Unbind** icon.

    >!![Destination is bound](06-05-03 AppStudio Run Configurations Bind Destination_.jpg)

8. Hover over the run configuration and click the Run Module icon.  

    !![Running the app locally](06-06 AppStudio Run Configurations Run_.jpg)

9. Wait for the **A service is listening to port 6004** notification and then click the button to open the app.

    >The left side view changes to the debug view and the status bar color changes to orange to indicate that the app is running in debug mode.

    >If you are running the app for the first time, the button in the notification will say **Expose and Open**. Otherwise it will say **Open in New Tab**.

    !![App is running locally](06-07 AppStudio Run Configurations Run_.jpg)

    >Some of the notifications appear on the screen for a short period of time.

    >You may optionally add a port description.

    >You may need to authenticate yourself to access the backend. Use your ES5 username and password.

    The app is opened in a new tab and a list of suppliers is displayed.

    !![SAP Fiori app is running](AppStudio Run Configurations-16.jpg)

[VALIDATE_7]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Stop the running application)]

1. Return to the SAP Business Application Studio tab.

2. In the **Debug** view click the Stop icon.

    !![Stop running app](05-02 AppStudio Stop Running App_.jpg)

    >The status bar background color changes from orange to blue.

    >You can re-run the app clicking the Start Debugging icon in the **Debug** view or clicking the Run Module icon in the **Run Configurations** view.

[DONE]
[ACCORDION-END]

---

Congratulations!

With this, you have successfully completed the development of an SAP Fiori app using SAP Business Application Studio, including test-running the app locally in the dev space. In this tutorial, you learned about high productivity tools that are available out-of-the-box in SAP Business Applications Studio, such as: templates and wizards, command palette, Layout Editor, local run, and more.
