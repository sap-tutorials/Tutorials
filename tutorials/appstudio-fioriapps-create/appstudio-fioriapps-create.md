---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>sapui5, software-product>sap-fiori, software-product>sap-business-technology-platform, software-product>sap-btp--cloud-foundry-environment]
primary_tag: software-product>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

# Create an SAP Fiori App Using SAP Business Application Studio
<!-- description --> Develop a simple SAPUI5 freestyle application, adhering to SAP Fiori design guidelines, that will present a list of suppliers from an on-premise backend.

## Prerequisites
  - You have created an SAP Fiori dev space in SAP Business Application Studio. See [Create a preconfigured dev space for developing SAP Fiori apps](appstudio-devspace-fiori-create).
  - The SAP Fiori dev space is in status `RUNNING` and you have opened it.
  - You configured a destination to SAP Gateway Demo System (ES5) (see [Connect SAP BTP to Your SAP Gateway Demo System Account (ES5)](cp-portal-cloud-foundry-gateway-connection)).


## You will learn
  - How to create an SAPUI5 application for SAP BTP, Cloud Foundry environment
  - How to test-run the app locally in the dev space

## Intro
You'll build an application that presents a list of suppliers from an on-premise backend. The suppliers' data will be retrieved from the `BusinessPartnerSet` collection in the `ZGWSAMPLE_BASIC` OData service that is available from SAP's ES5 demo Gateway system.

The flow consists of the following parts:

1. Running a wizard that creates a multi-target application (MTA) project that is configured to use Managed Application Router. An MTA is required in order to create the deployment artifact for SAP BTP, Cloud Foundry environment. If you are not familiar with the MTA concepts, read this [guide](https://www.sap.com/documents/2016/06/e2f618e4-757c-0010-82c7-eda71af511fa.html). A Managed Application Router is SAP's best practice for applications that run on SAP BTP and is required in order to include the app in SAP launchpad. To learn more about Managed Application Router, read this [guide](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/11d77aa154f64c2e83cc9652a78bb985.html).

2. Running a wizard that generates the app based on the SAPUI5 Application template. The app is generated as an HTML5 module within the MTA.

3. Enhancing the application by creating the UI and presenting data fetched from an SAP backend.

>**Important**

>After a period of idle time the dev space is automatically stopped. In order to restart the dev space, open the [dev space manager](https://triallink.eu10.trial.applicationstudio.cloud.sap/), click the **Play** icon, and click the dev space.

>The period of idle time for enterprise accounts is different than for trial accounts.

---

### Create a New Multitarget Application Project


1. In the left-hand sidebar, select **the hamburger icon | View | Command Palette...** to open the **command palette**.

    <!-- border -->![open command palette](BAS-Create-MTA-1-.png)

2. The command palette opens at the top-center of the SAP Business Application Studio window.

    <!-- border -->![command palette opened](AppStudio-Create-MTA-2-.png)

3. Enter the **Fiori: Open CF Application Router Generator** command in the command palette.

    >Type `fiori: open` in the command palette text field to filter the commands.

    <!-- border -->![cf mta and approuter wizard](BAS-Create-MTA-3-.png)

4. The **Application Router Generator Wizard** tab opens. For **Application Router Configuration**, configure the following, and click **Finish**.

    | Parameter | Value |
    |:----------|:------|
    | Application router project path | **/home/user/projects** (default) |
    | MTA ID | **`FioriDemo`** |
    | MTA Description | Can be left empty (default) |
    | Add route module | **Managed Approuter** |

    <!-- border -->![Fill-in cf mta and approuter wizard](BAS-Create-MTA-4-.png)

    >When end users access an app in the Cloud Foundry environment, they actually access the Application Router first. The application router is used to serve static content, authenticate users, rewrite URLs, and forward or proxy requests to other micro services while propagating user information.

    >We recommend using the **Managed Application Router**. It provides many benefits, compared to the Standalone Application Router, such as saving resources, lowering maintenance efforts, etc. The Standalone Application Router should only be used in advanced cases, for example when application router extensibility is required. More information is available in [Developing HTML5 Applications in the Cloud Foundry Environment](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/11d77aa154f64c2e83cc9652a78bb985.html).

5. Wait until the creation of project is completed. A notification that "The files has been generated" appears at the bottom right of the screen.

    <!-- border -->![project creation completed](BAS-Create-MTA-5-.png)


### Open the Project's Folder


1. In the left-hand sidebar, select **the hamburger icon | File | Open Folder...** to open the **Open Folder** dialog.

    <!-- border -->![open workspace dialog](BAS-Open-Workspace-1-.png)

2. The **Open Folder** dialog opens at the center of the SAP Business Application Studio window. First, select the **projects** entry.

    <!-- border -->![open folder dialog](BAS-Open-Workspace-1_1-.png)

3. Then select the **`FioriDemo`** project within the **projects** folder and click **OK**.

    <!-- border -->![open workspace dialog](BAS-Open-Workspace-2-1-2.png)

4. SAP Business Application Studio reloads with the `FioriDemo` project open in its workspace. In the Explorer view you can see the `FioriDemo` project, its folder structure, and files.

    <!-- border -->![open workspace dialog](BAS-Open-Workspace-3-.png)


### Generate an App Based on the SAP Fiori Basic Template


1. In the **Get Started** tab, click **New Project from Template**.

    <!-- border -->![launch generate app](BAS-Generate-App-1-1-.png)

    >If the **Get Started** Page does not appear, in the left side bar, select **the hamburger icon | Help | Get Started**.

    ><!-- border -->![Welcome Page from help menu](BAS-Welcome-Page-from-Help-Menu-2.png)

    >The easiest way to develop an SAPUI5 freestyle app from scratch is to create it from a template. To continue developing an existing application, the best practice is to use git source code management and clone the repository.

    >Using the UI wizard you can at any point click the `Back` button to go back to the previous step, or click the specific wizard step to go back to that step.

    > You can also create a project from the terminal using Yeoman (`@sap/fiori` generator).

2. Select the **SAP Fiori Application** tile, and click **Start**.

    <!-- border -->![fiori template group](BAS-Generate-App-2-1-.png)

3. For **Template Selection**, select the following, and click **Next**.

    | Parameter | Value |
    |:----------|:------|
    | Application type | **SAP Fiori** |
    | Which `template` do you want to use? | **Basic** tile |

    <!-- border -->![Floorplan Selection](BAS-Generate-App-3-1-.png)

4. For **Data Source and Service Selection**, select the following, and click **Next**.

    | Parameter | Value |
    |:----------|:------|
    | Data source | **Connect to a system** |
    | System | **`ES5`** |
    | Service | **`ZGWSAMPLE_BASIC (1) - OData V2`** |

    <!-- border -->![Data Source and Service Selection](BAS-Generate-App-4-1-.png)

5. For **Entity Selection > View name**, enter `Suppliers`, and click **Next**.

    <!-- border -->![Entity Selection > View name](BAS-Generate-App-5-1-.png)

6. For **Project Attributes**, select the following, and click **Next**.

    | Parameter | Value |
    |:----------|:------|
    | Module name | **`businesspartners`** |
    | Application title | **Suppliers** |
    | Application namespace | **ns** |
    | Description | **An SAP Fiori app to view a list of suppliers (demo)** |
    | Project folder path | **`/home/user/projects/FioriDemo`** |
    | Minimum SAPUI5 version | (Use the default) |
    | Add deployment configuration | **Yes** (default after setting the project folder path)|
    | Add FLP configuration | **Yes** |
    | Configure advanced options | **No** (default) |

    <!-- border -->![Project Attributes](BAS-Generate-App-6-1-.png)

7. For **Deployment Configuration**, select the following, and click **Next**.

    | Parameter | Value |
    |:----------|:------|
    | Please choose the target | **Cloud Foundry** (default) |
    | Destination name | **ES5 - https: //sapes5.sapdevcenter.com** |

    <!-- border -->![deployment configuration](BAS-Generate-App-7-1-.png)

8. For **Fiori Launchpad Configuration**, enter the following, and click **Finish**.

    | Parameter | Value |
    |:----------|:------|
    | Semantic Object | **Object** |
    | Action | **display** |
    | Title | **Suppliers** |
    | Subtitle (optional) | **Our Suppliers** |

    <!-- border -->![launchpad configuration](BAS-Generate-App-8-1-.png)

9. Wait until the installation of project dependencies is completed. A notification that "The project has been generated" appears at the bottom right of the screen. The **Application Information** tab opens, and the files and project structure in the **Explorer** view are updated.

    <!-- border -->![application generated](BAS-Generate-App-10-1-.png)




### Run the App Locally in the Dev Space


1.	Click the **Run Configurations** button to open the `Run Configurations` view. You'll see a set of run configurations that were created when you generated the app.

    <!-- border -->![local run](BAS-Local-Run-1-1-.png)

2.	Hover over the **`Start businesspartners`** run configuration and click the **Play** icon next to it to run the app locally in the dev space.

    <!-- border -->![local run](BAS-Local-Run-2-1-.png)

    >You may be prompted to allow pop-ups.

    >The **Debug** view opens, and the status bar color changes to orange, indicating that a debug session is in progress.

    >A new tab opens in SAP Business Application Studio where you can see the log of the running app.

    <!-- border -->![local run](BAS-Local-Run-3-1-3-.png)

3. A new browser tab opens showing the app. At this stage of development, the app only shows a title.

    <!-- border -->![app running locally](AppStudio-Local-Run-3-.png)



### Open the Layout Editor and the Text Editor


The layout editor allows users to easily make changes in the app using a visual editor. In this tutorial, you will make changes so that data from the backend service is displayed when the app is running.

1. Click **Toggle Bottom Panel** to free screen space for the editors pane.

    <!-- border -->![toggle bottom pane](BAS-Close-Bottom-Pane-1-.png)

2. Click the **Explorer** button to open the `Explorer` view.

    <!-- border -->![open explorer view](BAS-Open-Explorer-View-.png)

3. Choose **`FioriDemo` > `businesspartners` > `webapp` > `view`**, right-click the `Suppliers.view.xml` file, and click **Open with...**

     <!-- border -->![Open with Layout Editor](BAS-Open-Layout-Editor-1-.png)

4. The **Select editor** dialog opens at the top center of the SAP Business Application Studio window. Select the **Layout Editor** entry.

    <!-- border -->![Open with Layout Editor](BAS-Open-Layout-Editor-1-2-.png)

    >You may need to wait a bit for the layout editor extension to load.

    The **Suppliers** view opens in the layout editor.

    <!-- border -->![open layout editor](BAS-Open-Layout-Editor-2-.png)

5. Open the `Suppliers.view.xml` file with the **Text Editor** and see how modifications in the layout editor show up in the text editor. You don't have to open both editors for development. You can do the development work using either of the editors, or both. In this tutorial, you'll have both editors open for demo purposes.

    <!-- border -->![Open code editor](BAS-Open-Code-Editor-1-.png)

6. The **Suppliers** view opens in the text editor in a tab next to the layout editor.

    <!-- border -->![open code editor](BAS-Open-Code-Editor-2-.png)

7. For convenience, drag the text editor below the layout editor.

    <!-- border -->![drag-drop editor](BAS-Drag-Drop-Code-Editor-1-.png)

    >The layout editor and text editor are stacked so you can see how making changes to one shows up in the other.

    ><!-- border -->![drag-drop editor](BAS-Drag-Drop-Code-Editor-2-.png)



### Make Changes to the UI


Edit your app using the layout editor, with no need to do any coding.

>Tip 1: Since autosave is enabled by default, every change to a file triggers the live reload of the app. If you place the browser with the running app and the browser window with SAP Business Application Studio side by side, you'll be able to see how changes trigger the app's live reload.

>Tip 2: To trigger live reload only when you save a file, in the menu bar, select **File | Auto Save** to toggle auto save from enable (default) to disable. A checkmark next to auto save indicates that auto save is enabled.

1. In the **Controls** pane, enter `List` in the search box to filter the controls.

    <!-- border -->![Filter List control](BAS-Enhance-App-1-.png)

    >To get more screen real-estate, click the **Explorer** button to close the `Explorer` view, and adjust the ratio between the layout editor and the code editor.
    
    ><!-- border -->![Explorer Button](BAS-explorer-button.png)

2. Drag the **List** control and drop it on the **View** control in the canvas.

    <!-- border -->![Drag and drop](BAS-Enhance-App-2-.png)

    >Adding the list control to the view is reflected in both the layout editor and the text editor.

    ><!-- border -->![Link text e.g., Destination screen](BAS-Enhance-App-2-2-.png)

3. Select the **Standard List Item** control by clicking the **List** control you just added (the breadcrumb indicates which control is selected) and, in the **Entity Set** property in the **Properties** pane, click the Bind icon.

    <!-- border -->![Open entity set bind window](BAS-Enhance-App-3-.png)

    >The **Select Entity Set** view is displayed.

4. Select the **Define entity set and set the selected control as template** option, and in the **Entity Set** dropdown list, choose the `BusinessPartnerSet` entity set. Click **Bind** to complete the operation.

    <!-- border -->![entity set bind window](BAS-Enhance-App-4-.png)

    >The space of the **Select Entity Set** view may be too narrow to show all options. If you do not see the **Define entity set and set the selected control as template** option, scroll down in the **Select Entity Set** view to make it available.

    >The bind operation is reflected in both the **Layout Editor** and the code editor.

    ><!-- border -->![entity set bind window](BAS-Enhance-App-5-.png)

5. In the **Properties** pane, in the **Title** property, click the **Bind** icon.

    <!-- border -->![open Title bind window](BAS-Enhance-App-5-2-.png)

    >The **Data Binding** view is displayed.

6. Click the **Clear expression** (eraser) icon to clear the default text, and in the data field double-click  `CompanyName`. Click **Bind** to complete the operation.

    <!-- border -->![Title bound](BAS-Enhance-App-6-.png)

7. Repeat the last two steps for the **Description** property in the **Properties** pane. Choose  `WebAddress`.

    <!-- border -->![Bind Description](BAS-Enhance-App-8-.png)

8. Wait for the live reload of the app to complete. A list of suppliers is displayed.

    <!-- border -->![Bind Description](AppStudio-Change-UI-9-.png)



---

Congratulations!

You have now successfully developed an SAP Fiori app using SAP Business Application Studio, including test-running the app locally in the dev space. In this tutorial, you learned about the high productivity tools that are available out-of-the-box in SAP Business Application Studio, such as: templates and wizards, command palette, layout editor, local run (Preview Application), and more.
