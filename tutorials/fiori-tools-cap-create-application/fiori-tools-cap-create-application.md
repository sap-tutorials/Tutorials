---
title: Generate an SAP Fiori Elements Application based on a CAP-based Service
description: Generate an SAP Fiori elements application of type List Report Object Page and add additional features.
auto_validation: true
time: 15
tags: [ products>sap-fiori-elements, products>sap-fiori-tools, tutorial>beginner, products>sap-fiori, products>sap-business-application-studio, software-product-function>sap-cloud-application-programming-model, products>sap-business-technology-platform]
primary_tag: products>sap-fiori
---

## Details
### You will learn
- How to create an SAP Fiori application of type List Report Object Page
- How to use an SAP Fiori elements template with a service in the SAP Business Application Studio

---

[ACCORDION-BEGIN [Step 1: ](Create new SAP Fiori elements application)]
1. Start the Application Generator

    If not already done, open your trial account on SAP Cloud Platform and start the SAP Business Application Studio you set up in the previous tutorial [Prepare Your Development Environment](fiori-tools-cap-prepare-dev-env). Then start your development space and select the sample project you created there.

    Once the **Welcome** page is shown, click **Start from template**.

    !![Select link "Create project from template"](select-create-project.png)

    Select the tile **SAP Fiori elements application** and click **Next**.

    !![Choose tile "SAP Fiori elements application"](choose-tile-sap-fiori-elements.png)

2. On the page **New Project From Template**, select the tile **List Report Object Page** and click **Next**.

    !![Choose tile "List Report Object Page"](choose-tile-list-report.png)

3. Now you connect the application template with your OData service. The OData service you use for this example was already prepared during the previous tutorial:  [Prepare Your Development Environment](fiori-tools-cap-prepare-dev-env)

    Select the data source for the project. Since you use the locally installed service, choose **Use a local CAP Node.js project** from the dropdown field **Data source**.

    Navigate to and select the project folder location in the input field for your project folder path (as shown in the screenshot).

    Choose the service name **`IncidentService`** from the dropdown field **OData service**.

    When finished, click **Next**.

    !![Select service related parameters](enter-service-parameters.png)

4. For your application you need to choose the main entity set from the OData service. Objects of this type will be displayed in the list report.

    In your application, start with `Incidents`. As your application will not have a sub-object page, you do not need a navigation entity.

    When finished, click **Next**.

    !![Choose main entity set](choose-main-entity-set.png)

5. Maintain specific attributes of the application project as follows:

    !![Provide project attributes](provide-project-attributes.png)

    >Be sure to choose exactly the **Module name** and the **Application namespace** as shown above, because these are referenced in the sample code.

    After completion, click **Finish**. The new SAP Fiori elements application is now created from the template using the service and the configurations you provided in this step.

    You will now see a popup asking whether you want to open the project in a new workspace. Simply close it by clicking the  **X**.

    !![Close popup new workspace](close-popup-open-new-workspace.png)

    You will now see a new folder `incidents` inside the `app` folder.

    !![Review the generated artifacts](review-generated-artifacts.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Start the application)]
Your SAP Fiori elements application needs a server to run. This server is provided by the command line client and development toolkit for the SAP Cloud Application Programming Model. The setup for using the server was done in the previous tutorial [Prepare Your Development Environment](fiori-tools-cap-prepare-dev-env).

1. Open a new terminal in your SAP Business Application Studio.

    !![Start a new Termimnal](open-new-terminal.png)

2. Ensure that your terminal prompt shows **fiori-elements-incident-management**.

    Then type `cds watch` and press **Enter** to start the server.

    !![Change to folder fiori-elements-incidents-management](change-to-project-root-run-cds-watch.png)

    Two dialog windows will pop up. Click **Expose and Open**

    !![Click button "Expose and Open" on popup](click-expose-and-open.png)

    and then **Open in New Tab**.

    !![Click button Open in New Tab on popup](click-open-in-new-tab.png)

    >Please check for a browser popup blocker in case the popup windows are not visible.

3. After confirmation of the two popup windows, the application server launches a new browser window with a set of links.

    !![Press "/incidents/webapp/index.html" link](press-incidents-webapp-index-html.png)

    Start the SAP Fiori launchpad with your sample application by selecting the corresponding link.

    !![Select tile "My Incidents" on launchpad](select-tile-my-incidents.png)

    You can now start the new SAP Fiori elements application by selecting the tile on the SAP Fiori launchpad.

4. The application starts with an empty list.

    !![List Report without items](list-report-empty.png)

    Press **Go**. The list report table will then show the data from the sample service.

    !![List Report with items](list-report-go.png)

    Filter fields, actions, and table columns are defined by the annotations in the Core Data Service (CDS) files. These files are part of the OData service definition.

Your have now finished the initial setup of your list report object page sample application.

In the next tutorial, you will modify and enhance the list report page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test yourself)]


[VALIDATE_1]
[ACCORDION-END]



---
