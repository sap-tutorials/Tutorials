---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>mobile, software-product>sap-btp--cloud-foundry-environment, software-product>sap-fiori, software-product>sap-hana-cloud]
primary_tag: software-product>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

# Create a Responsive User Interface Application
<!-- description --> Create a responsive application (user interface) that connects to your data through a service you previously created, using the high productivity development capabilities of SAP Business Application Studio.

## Prerequisites
- You have created a data model and exposed it as a service. (See [Create a Data Model and Expose It as a Service](appstudio-lcap-create-db-service))

## You will learn
- How to create a responsive application for a service
- How to preview your application
- How to customize your application (for example, auto reload initial data and change titles)

---

### Create an application


1. From the storyboard, click the **+** of the **User Interface** tile.

    <!-- border -->![bas lcap launch create ui wizard](BAS-LCAP-Add-UI-Responsive-1-.png)

2. For **UI Application Details**, choose the following, and choose **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Display name | **`Capex Report`** |
    | B | Application name | **`CapexReport`** |
    | C | Description | **List of Expenses** |

    <!-- border -->![bas lcap launch create ui wizard - ui application details](BAS-LCAP-Add-UI-Responsive-2-.png)

3. For **UI Application Type**, select **Template-Based, Responsive Application**, and choose **Next**.

    <!-- border -->![bas lcap launch create ui wizard - ui application type](BAS-LCAP-Add-UI-Responsive-3-.png)

4. For **UI Application Template**, select **List Report Page**, and choose **Next**.

    <!-- border -->![bas lcap launch create ui wizard - ui application template](BAS-LCAP-Add-UI-Responsive-4-.png)

5. For **Data Objects**, select the following, and choose **Finish**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Main entity | **`Capex`** |
    | B | Automatically add table ... | **Yes** (default) |

    <!-- border -->![bas lcap launch create ui wizard - data objects](BAS-LCAP-Add-UI-Responsive-5-.png)

6. Wait for the success notification ( **The files have been generated** ) to appear.

    <!-- border -->![bas lcap launch create ui wizard - app generation completed](BAS-LCAP-Add-UI-Responsive-6-.png)

    The app is added to the **User Interface** tab in the storyboard.

    The **Page Map** (`CapexReport`) editor is opened side-by-side to the storyboard.


### Preview the app


1. If the **Project Preview** tab is open, close it.

2. Choose **Run and Debug** green button from the top right of the screen.

    <!-- border -->![bas lcap preview with sample data](BAS-LCAP-Preview.png)

3. In the **Project Preview** tab, choose the **`Capex Report`** application's tile to launch the app.

    <!-- border -->![bas lcap preview app - launch app](BAS-LCAP-Preview-Generated-UI-Responsive-4-.png)

4. Choose **Go** to view the data in a list page.

    <!-- border -->![bas lcap preview app - present data](BAS-LCAP-Preview-Generated-UI-Responsive-5-.png)

    You can view the data in a list page.

    <!-- border -->![bas lcap preview app - data presented](BAS-LCAP-Preview-Generated-UI-Responsive-6-.png)

6. You can optionally click one of the list items to view its object page.

    <!-- border -->![bas lcap preview app - present object page](BAS-LCAP-Preview-Generated-UI-Responsive-7-.png)

7. Choose the **Back** option to return to the list page or twice to return to the launch page.

    <!-- border -->![bas lcap preview app - return to list page](BAS-LCAP-Preview-Generated-UI-Responsive-8-.png)



### Customize the app - Enable initial load of data in the list page


To avoid clicking on the **Go** option every time you want to present the information in the list page, in this section you'll configure the page to load the initial data as soon as it is loaded.

1. Close the browser tab where the **`Capex Report`** application is running.

2. In the **Project Preview** tab, choose the **`Capex Report`** application's tile to launch the app.

    <!-- border -->![bas lcap preview app - launch app](BAS-LCAP-Preview-Generated-UI-Responsive-4-.png)

3. The app is loaded with an empty list page.

    >From this step onwards you'll leverage the auto-refresh capability of the development environment. Every time you make any new change to the app, it gets refreshed automatically and you can view the updated changes.

    <!-- border -->![bas lcap enable initial load - launch app](BAS-LCAP-Load-Initial-Data-3-.png)

4. Go back to the storyboard of SAP Business Application Studio, and choose the **Configure Page** button of the **List Report** page.

    <!-- border -->![bas lcap enable initial load - launch list report edit](BAS-LCAP-Load-Initial-Data-4-.png)

5. Choose the **Table** element.

    <!-- border -->![bas lcap enable initial load - launch table element properties](BAS-LCAP-Load-Initial-Data-5-.png)

6. For the **Initial Load** property, choose **Enabled** in the drop-down.

    <!-- border -->![bas lcap enable initial load - set table property to enabled](BAS-LCAP-Load-Initial-Data-6-.png)

7. Access the application tab. Now, the data appears in the table without choosing the **Go** option.

    <!-- border -->![bas lcap enable initial load - table auto-reloaded](BAS-LCAP-Load-Initial-Data-7-.png)




### Customize the app - Update table column titles


1. In the **Page Map** editor of SAP Business Application Studio, expand **Columns**.

    <!-- border -->![bas lcap list page update columns title - expand columns](BAS-LCAP-Update-Column-Titles-1-.png)

2. Choose **description**.

    <!-- border -->![bas lcap list page update columns title - open description properties](BAS-LCAP-Update-Column-Titles-2-.png)

3. Change the **Label** to **Description**.

    <!-- border -->![bas lcap list page update columns title - change description label](BAS-LCAP-Update-Column-Titles-3-.png)

4. Repeat the previous steps to change the **`total_cost`** **Label** to **Total Cost**.

    <!-- border -->![bas lcap list page update columns title - change total_cost label](BAS-LCAP-Update-Column-Titles-4-.png)

5. Repeat the previous steps to change the **`contractor_contractor`** **Label** to **Contractor**.

    <!-- border -->![bas lcap list page update columns title - change contractor_contractor label](BAS-LCAP-Update-Column-Titles-5-.png)

6. Go back to the application's tab. You can see that the column titles changed as per your updates.

    <!-- border -->![bas lcap list page update columns title - preview app](BAS-LCAP-Update-Column-Titles-6-.png)


### Customize the app - Present contractor name


1. In the **Page Map** editor of SAP Business Application Studio, choose **contractor/name** for the **Text** property.

    <!-- border -->![bas lcap list page present contractor name - set text property](BAS-LCAP-Present-Contractor-Name-1-.png)

2. Choose **Text Only** for the **Text Arrangement** property.

    <!-- border -->![bas lcap list page present contractor name - set text arrangement property](BAS-LCAP-Present-Contractor-Name-2-.png)

3. Go back to the application's tab. You can see that the ID is replaced by the name of the contractor.

    <!-- border -->![bas lcap list page present contractor name - preview app](BAS-LCAP-Present-Contractor-Name-3-.png)


### Customize the app - Change titles in the object page


1. In the application's tab select one object, for example, **Bicycles**. The column titles are not updated yet. In this step we will change the column titles for the single objects.
    <!-- border -->![bas lcap object page ](BAS-LCAP-Update-Object-Page-Titles-1-1.png)

2. In the SAP Business Application Studio, choose the **Page Map** option.

    <!-- border -->![bas lcap object page change titles - show page map](BAS-LCAP-Update-Object-Page-Titles-1-.png)

3. Choose the **edit** option of the **Object Page**.

    <!-- border -->![bas lcap object page change titles - launch object page edit](BAS-LCAP-Update-Object-Page-Titles-2-.png)

4. Expand **General Information** -> **Form** -> **Fields**.

    <!-- border -->![bas lcap object page change titles - expand](BAS-LCAP-Update-Object-Page-Titles-3-.png)

5. Make the following changes to the **Fields** **Label** property. (Repeat the steps listed in the two previous sections.)

    >Hint: Trigger the **Label** update, by pressing [ENTER] or clicking somewhere on the screen.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | description | **Description** |
    | B | `total_cost` | **Total Cost** |
    | C | `contractor_contractor` | **Contractor** |

    <!-- border -->![bas lcap object page change titles - change titles](BAS-LCAP-Update-Object-Page-Titles-4-.png)

6.  Go back to the application's tab. You can see that the titles are changed as per your updates.

    <!-- border -->![bas lcap object page change titles - preview app](BAS-LCAP-Update-Object-Page-Titles-5-.png)


### Customize the app - Add contractor section to the object page


1. In the **Page Map** editor of SAP Business Application Studio, hover over **Sections**, and choose the **Add Sections** option.

    <!-- border -->![bas lcap object page add section - add section](BAS-LCAP-Add-Contractor-Section-1-.png)

2. Choose **Add Form Section**.

    <!-- border -->![bas lcap object page add section - add form section](BAS-LCAP-Add-Contractor-Section-2-.png)

3. Set the **Label** to **Contractors** and choose **Add**.

    <!-- border -->![bas lcap object page add section - set form section label](BAS-LCAP-Add-Contractor-Section-3-.png)

4. Expand **Contractors** -> **Form**.

    <!-- border -->![bas lcap object page add section - expand contractor form section](BAS-LCAP-Add-Contractor-Section-4-.png)

5. Drag and drop **Contractor** from **General Information -> Form -> Fields** to **Contractors -> Form**.

    <!-- border -->![bas lcap object page add section - move contractor between form sections](BAS-LCAP-Add-Contractor-Section-5-.png)

6.  Go back to the application's tab. You can see that a new **Contractors** section appears with the **Contractor** field.

    <!-- border -->![bas lcap object page add section - app preview](BAS-LCAP-Add-Contractor-Section-6-.png)


### Customize the app - Change object page title


1. In the **Page Map** editor of SAP Business Application Studio, choose **Header**.

    <!-- border -->![bas lcap object page change title - select header](BAS-LCAP-Change-Object-Page-Title-1-.png)

2. For the **Title** property choose **description**.

    <!-- border -->![bas lcap object page change title - change title property](BAS-LCAP-Change-Object-Page-Title-2-.png)

3.  Go back to the application's tab. You can see that the object page title changed as per your updates.

    <!-- border -->![bas lcap object page change title - app preview](BAS-LCAP-Change-Object-Page-Title-3-.png)



**Congratulations!**

With this, you have successfully completed developing a business application using the high productivity development capabilities of SAP Business Application Studio.

During the development you have modelled your business data. You have created a service through which data in the data model is accessed. You have created a user interface, which is actually an app, from which an application user can interact with the data. Throughout the development process, you continually test run the app, which is a development best practice to make sure the development is on-track.

And in this tutorial, you learned about high-productivity tools that are available out-of-the-box in SAP Business Applications Studio, such as: storyboard, project explorer, data model editor, service editor, templates and wizards, application editors, application preview, and much more.


---
