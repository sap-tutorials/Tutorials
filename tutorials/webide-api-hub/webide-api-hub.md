---
title: Using API Hub with SAP Web IDE
description: Learn how to use the SAP API Business Hub integration with SAP Web IDE
primary_tag: topic>sapui5
tags: [  tutorial>beginner, products>sap-cloud-platform, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **How-Tos:** Enable the SAP Web IDE Full-Stack Service
 - **Tutorials:** [Add API Business Hub API to a UI5 Application](https://www.sap.com/developer/tutorials/hcp-abh-api-ui5-app.html)

## Next Steps
 - Select a tutorial group from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)

## Details
### You will learn  
In this tutorial, you will explore some of the new features that are part of SAP Web IDE Full-Stack. Learn how to use the SAP API Business Hub services catalog that is available out of the box in this version of SAP Web IDE. Create and run an SAPUI5/Fiori Application. 

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Open SAP Web IDE Full-Stack)]
Make sure you have SAP Web IDE Full-Stack open.

![sap web ide multi-cloud service](1.png)

Steps on how to find and enable SAP Web IDE Full-Stack are available under the how-to guides.

![start up page for web ide](2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new project)]
In the SAP Web IDE, select **New Project from Template** by either selecting the icon on the home screen or by going to `File > New > Project from Template`.

![new project from template icon](3.png)

On the Template Selection page, choose the **SAP Fiori Worklist Application** Template.

![template selection location](4.png)

Click **Next**.


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Name the project)]
On the **Basic Information** page of the template wizard, provide a name for the project in the **Project Name** field.

![basic information page of template wizard](5.png)

Click **Next**.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Select the data connection)]
On the **Data Connection** page of the template wizard, select **SAP API Business Hub** from under the _Sources_ options.

![SAP API Business Hub in the service Catalog](6.png)

A list of APIs will populate in the table. You can filter down the APIs by either selecting an API Package from the drop-down or by entering a search term in the search box. **In the search box, type _employee_** to find the services available for employee information. Select the **Employee Entity Type** service from the list.

![Services search on Data Connection page of wizard](6b.png)

You may be prompted for a username and password. This is in order for SAP API Business Hub to generate an API key for you. Enter your credentials for the SAP Cloud Platform, which should be the same credentials you use to log in to the SAP API Business Hub. **Enter your credentials and click Login**.

![login for SAP API Business Hub](7.png)

Once you are successfully logged in, you should see a blue message on the top of the screen saying that _Service: Employee Entity Type selected_. Once the proper data service is selected, click **Next**.

![selection of Employee Entity Type](8.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Customize the template)]
On the **Template Customization** page of the template wizard, you will define the properties of the application as well as bind some of the data.

In the **Application Settings** section, provide the following information for the application.

Field Name | Value
--- | ---
Type | Standalone App
Title | Employee Information
Namespace | `api.employee.app`
Description | _(optional)_

![application settings section on Template Customization page](9.png)

In the **Data Binding** section, you will define what collection and attributes to bind to the view.

Provide the following values for the specified fields in this section.

Field Name | Value
--- | ---
Object Collection | `EmployeeCollection`
Object Collection ID | `ObjectID`
Object Title | `Name`
Object Numeric Attribute | _(blank)_
Object Unit of Measure | `Email`

![data binding section on Template Customization page](10.png)

Once the data binding and application settings are complete, click **Next**.

![Template Customization page with next selected](11.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Finish the app)]
On the **Confirmation** page of the template wizard, click **Finish**  to create your application.

![Confirmation page with finish indicated](12.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run the application)]
Once the application is finished being created, you will see a new project in your workspace.

![Project file structure in Web IDE](13.png)

**Right Click** on the application. Select `Run > Run as > Web Application` to bring up the run configuration menu.

![Right click path for running an app](14.png)

When prompted to **Choose the File to Run**, select the `index.html` file from the File Name list.

![file selection for running the application](15.png)

Click **OK**.

When your application loads, you will see a list of employee names and emails pulled from the SAP API Business Hub.

![expected output of the project as a web app](16.png)


[ACCORDION-END]


## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)
