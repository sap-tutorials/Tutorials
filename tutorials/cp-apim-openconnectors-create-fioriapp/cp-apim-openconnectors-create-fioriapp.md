---
title: Create an SAP Fiori App Consuming HubSpot Data
description: Build an SAP Fiori application showing data from a third-party CRM system (HubSpot) using Open Connectors to retrieve the data.
auto_validation: true
time: 30
tags: [ tutorial>advanced, products>sap-cloud-platform]
primary_tag: products>sap-api-management
---

## Prerequisites
- **Tutorials:** [Enable SAP Cloud Platform API Management Service](https://developers.sap.com/tutorials/hcp-apim-enable-service.html)

## Details
### You will learn
  - How to build a SAP Fiori application showing data from a third-party CRM system.

---

[ACCORDION-BEGIN [Step 1: ](Create a Destination to API Portal)]

1. Log onto your SAP Cloud Platform trial and navigate to the **Neo Environment**.

    ![Navigate Destination](01-destination.png)

2. Select the **Destinations** tab and then select **New Destination** to create a new destination.

    ![Create Destination](02-create-destination.png)

    >Use the existing destination pointing to API Portal if it already exists.

3. Enter **`API_Portal`** as the destination name and then enter the URL to SAP Cloud Platform API Management trial tenant.

    ```
    https://<your_number>trial-trial.apim1.hanatrial.ondemand.com
    ```
    ![Destination Configuration](03-destination-configuration.png)

4. Under the additional properties tab, add the following properties:

    **Field** | **Value**
    ---- | ----
    `WebIDEEnabled` |`true`
    `TrustAll` |`true`
    `WebIDESystem` |`apiportal`

    >The name of this destination would have to be noted and it would be later used in the SAP Fiori application (in the `neo-app.json` file).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create SAP Fiori app)]

1. Go to **Services**, click on SAP Web IDE Full-Stack, and then click on **Go to Service**.

      ![Navigate Service](04-navigate-service.png)

2. Click on **New Project** from template to create a project.

    ![Project Template](05-new-project-template.png)

3. Click on **SAPUI5 Application** and then click **Next**.

    ![UI5 Application](06-UI5-application.png)

4. Fill the following details:

    **Field** | **Value**
    ---- | ----
    `Project Name` |`thirdpartydemo`
    `Namespace` |`ocn`

    Click **Next**.

    ![Project Namespace](07-project-namespace.png)

5. Click **Finish** to create the project.

    ![Project Creation](08-project-creation-finish.png)

6. Open `neo-app.json` and copy the following code:

    ```Destination

    {
      "path": "/<replace_by_account_id>trial/HubSpotCRM/Basic_Companies",
      "target": {
        "type": "destination",
        "name": "API_Portal",
        "entryPath": "/<replace_by_account_id>trial/HubSpotCRM/Basic_Companies"
      },
      "description": "API_Portal OpenConnectors"
    },

    ```
    ![Neojson File](09-neojson.png)

    >Make sure you right-click on the editor and select `Beautify` to format the code.

7. Add the `headerWhiteList`.

    ```JSON
    "headerWhiteList": [
    "authorization"
    ],
    ```
    ![Whitelisting](10-whitelisting.png)

8. Go to the folder `model` and open `model.js` and replace the code with the following code:

    ```JavaScript
    createDataModel: function () {
    var oModel = new JSONModel();
    var sHeaders = {};
    oModel.loadData("/<you_number>trial/HubSpotCRM/<your common Resource template name>", null, true, "GET", null, false, sHeaders);
    return oModel;
    }

    ```
    >UI binding has been done based on the response from the basic-companies common resource templates
    and therefore the same binding can be re-used irrespective of the actual non-SAP CRM
    application that you would be connecting to.
    
    ![Model File1](11-model.png)

9. In the `Component.js` file, paste the following code:

    ```JavaScript
    this.setModel(models.createDataModel(),"data");
    ```
    ![Model File](11-model2.png)

10. Paste the following table control XML in to `View1.view.xml`:

    ```XML
    <<Table items="{data>/}">
							<columns>
								<Column>
									<Text text="Name"/>
								</Column>
								<Column>
									<Text text="Phone"/>
								</Column>
								<Column>
									<Text text="Website"/>
								</Column>
								<Column>
									<Text text="Industry"/>
								</Column>
								<Column>
									<Text text="Shipping Address"/>
								</Column>
								<Column>
									<Text text="Billing Address"/>
								</Column>
							</columns>
							<items>
								<ColumnListItem>
									<cells>
										<ObjectIdentifier title="{data>name}"/>
										<Text text="{data>phone}"/>
										<Text text="{data>website}"/>
										<Text text="{data>industry}"/>
										<Text text="{data>shippingAddress/composite}"/>
										<Text text="{data>billingAddress/composite}"/>
									</cells>
								</ColumnListItem>
							</items>
						</Table>
    ```
    ![View File](12-view.png)

11. Go to the `i18n` folder and open `i18n.properties`, change the title of the page as follows:

    ```i18n
    title = Accounts Data from HubSpot CRM
    ```
    ![Properties File](13-properties.png)

12. Save all the changes and run your SAP Fiori app as **Web Application**

    ![Save All](14-saveall-run.png)

13. Now the contents of the Open Connectors are visible in the app.

    ![Fiori App](15-fiori-app.png)

[VALIDATE_2]
[ACCORDION-END]

---
