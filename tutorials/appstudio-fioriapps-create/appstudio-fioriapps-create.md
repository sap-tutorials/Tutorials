---
title: Develop an SAP Fiori App using SAP Business Application Studio
description: Create an SAP Fiori application in SAP Business Application Studio and deploy it to your SAP Cloud Platform, Cloud Foundry environment.
auto_validation: true
time: 30
tags: [ tutorial>beginner, topic>sapui5, products>sap-fiori, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment, tutorial>license]
primary_tag: products>sap-business-application-studio
---

## Prerequisites
 - You have access to SAP Business Application Studio.
 - A destination to ES5 is configured in the subaccount from which you accessed the SAP Business Application Studio.
 - A UAA service instance is available in the space to which you will logon.
 - A Destination service instance is available in the space to which you will logon.
 - For the deployment step, additional pre-requisites apply. You need to have the following available in the space to which you will logon:
    - Application Runtime: 1GB free
    - Destination: 1 free
    - HTML5 Applications: 1 free

## Details
### You will learn
  - How to create an SAPUI5 application for SAP Cloud Platform, Cloud Foundry environment
  - How to configure Cloud Foundry settings in SAP Business Application Studio
  - How to build and deploy an application to Cloud Foundry

---

[ACCORDION-BEGIN [Step 1: ](Open SAP Business Application Studio)]
1. Go to your Cloud Foundry environment subaccount and click the **Subscriptions** tab.

    !![AppStudio Tile in SAP Cloud Platform Cockpit](2020-02 AppStudio Tile_.png)

2. Locate the **SAP Business Application Studio** tile.

3. Click **Go to Application**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a preconfigured SAP Fiori dev space)]

1. Choose **Create Dev Space**.

    !![Open AppStudio](AppStudio Dev Space Manager_.jpg)

2. Choose `Demo_Fiori` for your dev space **name**.

3. Choose **SAP Fiori** as the application type.

4. Click **Create Dev Space**.

    !![Create Dev Space](AppStudio Create Dev Space Fiori_.jpg)

    >The dev space is in status **STARTING**. Wait until it is in status **RUNNING**.

    !![Dev Space Starting](AppStudio Dev Space Starting_.jpg)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create new project)]

1. In SAP Business Application Studio, open the SAP Fiori dev space you created by clicking the name of the dev space.

    !![Open Dev Space](AppStudio Open Dev Space_.jpg)
    &nbsp;
    !![Open Dev Space](AppStudio Dev Space Open.jpg)

2. Click **Open Workspace** or **File > Open Workspace**, and choose **`projects`**.

    ![Open Workspace](AppStudio Open Workspace-1.jpg)
    &nbsp;
    ![Select projects](AppStudio Open Workspace-2.jpg)

3. Go to **Terminal > New Terminal** to open a command-line window within SAP Business Application Studio and change the directory to the `projects` directory:

    ```Shell/console
    cd ~/projects
    ```

    >You can check that you're in the `projects` folder using the `pwd` command.

    ```Shell/console
    pwd
    ```
    ![Open new terminal](AppStudio Open Terminal-1_.jpg)

4. Create a new SAP Fiori project using the Yeoman generator.

    ```Shell/console
    yo
    ```
5. In the generator, select the following options:

    >Use the up/down arrow keys to select an option and press ENTER.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Run a generator | **Fiori Project** |
    | B | Target running environment | **Cloud Foundry** |
    | C | Template you want to use | **SAPUI5 Application** |
    | D | Enter the project name | **`FioriDemo`** |
    | E | Enter an HTML5 module name | **`BusinessPartners`** |
    | F | Enter a namespace | **ns** |
    | G | Do you want to create a UAA Service and bind it to the approuter module | **Yes** |
    | H | Enter a View name | **Suppliers** |
    | I | Do you want to add a data service | **No** |

    ![Yo parameters](AppStudio Yo Parameters 200203_.jpg)

    >The **Explorer** opens and you can see the `FioriDemo` project, its folder structure, and files. If not, you can click the **Explorer** view button at the top left of the screen.

    ![Project created](AppStudio Yo Completed 200203__.jpg)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add an OData service to the project)]

Now you need to consume a service to bind the UI controls to the service's collection properties. Later, when you test run the app, it will present data coming from the backend service.

1. Open the command palette ( **View > Find Command** or **F1**) and select **Consume SAP Services**.

    ![AppStudio Open Command Palette](AppStudio Command Palette.jpg)
    &nbsp;
    ![Consume SAP Service](AppStudio Consume SAP Service-1.jpg)

2. Select the **ns.BusinessPartners** folder.

    ![Choose the UI module](AppStudio Consume SAP Service-2.jpg)

    >If the project contains more than one HTML5 module, you'll have a list to select from.

3. Choose **My SAP Systems** as the data source.

    ![Choose the data source](AppStudio Consume SAP Service-3.jpg)

    >A list of data sources is displayed.

4. Choose **ES5** as the data source.

    ![Choose data source](AppStudio Consume SAP Service-4.jpg)

5. Choose **`GW_SAMPLE_BASIC`** as the service.

    ![Choose service](AppStudio Consume SAP Service-5.jpg)

    >The service is successfully bound to the application. You can see that the service appears in the module's `manifest.json` `dataSources` section (**`FioriDemo` > `BusinessPartners` > `webapp`**). You can see the service metadata in **`FioriDemo` > `BusinessPartners` > `webapp` > `localService`**.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open the layout editor)]

Open the layout editor in SAP Business Application Studio to easily make a few changes. In this case, we will make changes so that data from the backend service is displayed when the app is running.

1. Choose **`FioriDemo` > `webapp` > `view`** and right-click the `Suppliers.view.xml` file that you created with yo generator in a previous step.

2. Choose **Open With > Layout Editor**.

    ![Open with Layout Editor](AppStudio Open Layout Editor.jpg)

    ![Layout Editor Opened](AppStudio Layout Editor Open.jpg)

3. You can optionally choose to open it with the code editor and see how modifications in the Layout Editor are manifested in the code editor.

    ![Open code editor](AppStudio Open Code Editor-XML.jpg)
    &nbsp;
    ![Code editor opened](AppStudio Code Editor-XML-Open.jpg)

4. For convenience, place the code editor below the Layout Editor. Use the drag & drop functionality.

    ![Drag-Drop editor](AppStudio Drag-Drop Code Editor.jpg)
    &nbsp;
    ![Editor dropped](AppStudio Code Editor Dropped.jpg)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Make changes to the UI)]

Make some changes using the layout editor, with no need to do any coding.

1. In the **Controls** pane, enter `List` to filter the controls list in the search box.

    ![Filter List control](AppStudio Layout Editor Filter Controls-List.jpg)

2. Drag the **List** control and drop it on the **View** control in the canvas.

    ![Drag and drop](AppStudio Layout Editor List Dropped_.jpg)

3. Select the **List** control and, in the **Entity Set** property in the **Properties** pane, click the Bind icon.

    ![Open entity set bind window](AppStudio Layout Editor Bind to Entity Set-1_.jpg)

    >The `Select Entity Set` view is presented.

4. Select the **Define entity set and set the selected control as template** option, and in the **Entity Set** dropdown list, choose the `BusinessPartnerSet` entity set. Click **Bind** to complete the operation.

    ![entity set bind window](AppStudio Layout Editor Bind to Entity Set-2.jpg)

5. In the **Properties** pane, in the **Title** property, click the Bind icon.

    ![open Title bind window](AppStudio Layout Editor Bind to Entity Set-3.jpg)

    >The `Data Binding` view is displayed.

6. Click the **Clear expression** (eraser) icon to clear the default text, and in the data fields double click  `CompanyName`. Click **Bind** to complete the operation.

    ![Title bound](AppStudio Layout Editor Bind Title-3.jpg)

7. Repeat the last steps for the **Description** property in the **Properties** pane. Choose  `BusinessPartnerID`.

    ![Bind Description](AppStudio Layout Editor Bind Description_.jpg)


[VALIDATE_5]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Set Cloud Foundry preferences)]

Before you can test run your new application, set your Cloud Foundry preferences.

1. Open the **command palette** and select **CF: Login to cloud foundry**.

    ![Command Palette-Login to CF](AppStudio Login to CF-1.jpg)

2. When prompted, select the API endpoint, organization and space for your project.

    >The Cloud Foundry organization and space appear in the status line at the bottom left part of the screen.

    ![Logged in to CF](AppStudio Login to CF-8_.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test run the application)]

Run your new application to test it.

1. Open the **Run Configurations** view.

    ![Open Run Configurations](AppStudio Run Configurations-1_.jpg)

2. Click `+` and select **`BusinessPartners`**.

    ![Create new run configuration](AppStudio Run Configurations-3_.jpg)

    >A new run configuration is generated for the `FioriDemo` project.

3. Expand the run configuration to display the services that can be bound.

    ![Bindable objects](AppStudio Run Configurations-4_.jpg)

    >SAP Business Application Studio allows you to test your app with resources.

4. To bind to the test UAA service instance in your space, click the `bind` icon to the right of the UAA service resource and select an instance from the list.

    ![Bind to UAA service](AppStudio Run Configurations-5_.jpg)

    ![Select UAA service instance](AppStudio Run Configurations-7_.jpg)

    >Once the service has been bound, the Bind icon turns green.

    >To unbind the service instance, click the Unbind icon.

    ![UAA service instance bound](AppStudio Run Configurations-9_.jpg)

5. Repeat the above step to bind the app to the test destination service instance in your space.

    ![Destination service is bound](AppStudio Run Configurations-13_.jpg)

6. Hover over the run configuration and click the Run Module icon. Wait for the notification `A service is listening to port 6004`. and click the button to launch the app in a new tab.

    ![Running the app locally](AppStudio Run Configurations-14_.jpg)

7. If you are running the app for the first time, the button in the notification will say `Expose and Open`. Otherwise it will say `Open in New Tab`. Click the button.

    ![App is running locally](AppStudio Run Configurations-15_.jpg)

    >You may optionally add a port description.

    >You may need to authenticate yourself to run the app (UAA is configured for this app).

    >You may need to authenticate yourself to access the backend.

    The app is opened in a new tab and a list of suppliers is displayed.

    ![SAP Fiori app is running](AppStudio Run Configurations-16.jpg)

[VALIDATE_7]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Build the application)]

1. In a terminal, go to the project's folder.

    ```Shell/console
    cd ~/projects/FioriDemo
    ```

2. Execute the following command:

    ```Shell/console
    mbt build -p=cf
    ```

    >The build process creates a multi target archive (`MTAR`) file in your project that packages all the project modules for deployment. You can find the MTAR file in the `DemoFiori/mta_archives` folder.

    ![Application build result](AppStudio Application Build_.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Deploy the application)]

Deploy your application to SAP Cloud Platform, Cloud Foundry environment.

1. In a terminal, go to the project's folder.

    ```Shell/console
    cd ~/projects/FioriDemo
    ```

2. Execute the following command:

    ```Shell/console
    cf deploy mta_archives/FioriDemo_0.0.1.mtar
    ```

    ![Deploy to CF](AppStudio Application Deployment-1_.jpg)

    >The application deployment to the space you are connected to starts and a notification appears. The deployment process takes a few minutes. You can see that the deployment is still in progress in the `Task: Deploy` console at the bottom right of your screen.

    >When the deployment process is complete, you should see a notification.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Get URL to access the application)]

Access your deployed application in the SAP Cloud Platform cockpit. The steps below show you how to create a URL that you can use to access your new application.

1. Access the space to where the app is deployed and go to the **Applications** tab.

    ![Application's space](AppStudio Application Deployment-5_.jpg)

2. Make sure your application is in **Started** state, and  click its name (`FioriDemo_appRouter`). The **Application: `FioriDemo_appRouter` - Overview** page opens.

3. Right-click the URL under **Application Routes** and save the URL in a text file.

    ![Get application base URL](AppStudio Application Deployment-6_.jpg)

4. Locate the `sap.app id` from the `manifest.json` file, located in your HTML5 module, and add it to the copied link after removing the periods.

    ![Link text e.g., Destination screen](AppStudio Application Deployment-3_.jpg)

    > For future reference, this is the construct of the final URL: `<URL_from_application_overview_page>/<mynamespace><project_name>/index.html`

    You can use this URL in any browser to access your new application.

[VALIDATE_10]
[ACCORDION-END]



---
