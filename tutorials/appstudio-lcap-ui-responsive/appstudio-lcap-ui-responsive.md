---
title: Create a Responsive User Interface Application
description: Create a responsive application (user interface) that connects to your data through a service you previously created, using low-code capabilities of SAP Business Application Studio.
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>mobile, software-product>sap-btp--cloud-foundry-environment, software-product>sap-fiori, software-product>sap-hana-cloud, tutorial>license, tutorial>free-tier]
primary_tag: software-product>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

## Prerequisites
- You have access to your Application Development Lobby.

## Details
### You will learn
- How to create a responsive application for a service
- How to preview your application
- How to customize your application (for example, auto reload initial data and change titles)

---

[ACCORDION-BEGIN [Step 1: ](Create an application)]

1. From the home page, click the **+** of the **User Interface** tile.

    !![bas lcap launch create ui wizard](BAS-LCAP-Add-UI-Responsive-1-.png)

2. For **UI Application Details**, choose the following, and choose **Next**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Display name | **`Capex Report`** |
    | B | Application name | **`CapexReport`** |
    | C | Description | **List of Expenses** |

    !![bas lcap launch create ui wizard - ui application details](BAS-LCAP-Add-UI-Responsive-2-.png)

3. For **UI Application Type**, select **Template-Based, Responsive Application**, and choose **Next**.

    !![bas lcap launch create ui wizard - ui application type](BAS-LCAP-Add-UI-Responsive-3-.png)

4. For **UI Application Template**, select **List Report Object Page**, and choose **Next**.

    !![bas lcap launch create ui wizard - ui application template](BAS-LCAP-Add-UI-Responsive-4-.png)

5. For **Data Objects**, select the following, and choose **Finish**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Main entity | **`Capex`** |
    | B | Automatically add table ... | **Yes** (default) |

    !![bas lcap launch create ui wizard - data objects](BAS-LCAP-Add-UI-Responsive-5-.png)

6. Wait for the success notification ( **The files have been generated** ) to appear.

    !![bas lcap launch create ui wizard - app generation completed](BAS-LCAP-Add-UI-Responsive-6-.png)

    The app is added to the **User Interface** tab in the home page.

    The **Page Map** (`CapexReport`) editor is opened side-by-side to the home page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Preview the app)]

1. If the preview is running, choose **Stop Preview**. If the **Project Preview** tab is open, close it.

2. Choose **Preview** > **With Sample Data** to preview (test run) the app.

3. In the **Project Preview** tab, choose the **`Capex Report`** application's tile to launch the app.

    !![bas lcap preview app - launch app](BAS-LCAP-Preview-Generated-UI-Responsive-4-.png)

4. Choose **Go** to view the data in a list page.

    !![bas lcap preview app - present data](BAS-LCAP-Preview-Generated-UI-Responsive-5-.png)

    You can view the data in a list page.

    !![bas lcap preview app - data presented](BAS-LCAP-Preview-Generated-UI-Responsive-6-.png)

6. You can optionally click one of the list items to view its object page.

    !![bas lcap preview app - present object page](BAS-LCAP-Preview-Generated-UI-Responsive-7-.png)

7. Choose the **Back** option to return to the list page or twice to return to the launch page.

    !![bas lcap preview app - return to list page](BAS-LCAP-Preview-Generated-UI-Responsive-8-.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Customize the app: Enable initial load of data in the list page)]

To avoid clicking on **Go** option every time you want to present the information in the list page, in this section you'll configure the page to load the initial data as soon as it is loaded.

1. Close the browser tab where the **`Capex Report`** application is running.

2. In the **Project Preview** tab, choose the **`Capex Report`** application's tile to launch the app.

    !![bas lcap preview app - launch app](BAS-LCAP-Preview-Generated-UI-Responsive-4-.png)

3. The app is loaded with an empty list page.

    >From this step onwards you'll leverage the auto-refresh capability of the development environment. Every time you make any new change to the app, it gets refreshed automatically and you can view the updated changes.

    !![bas lcap enable initial load - launch app](BAS-LCAP-Load-Initial-Data-3-.png)

4. Go back to the homepage of SAP Business Application Studio for low-code development, and choose the **edit** button of the **List Report** page.

    !![bas lcap enable initial load - launch list report edit](BAS-LCAP-Load-Initial-Data-4-.png)

5. Choose the **Table** element.

    !![bas lcap enable initial load - launch table element properties](BAS-LCAP-Load-Initial-Data-5-.png)

6. For the **Initial Load** property, choose **Enabled** in the drop-down.

    !![bas lcap enable initial load - set table property to enabled](BAS-LCAP-Load-Initial-Data-6-.png)

7. Access application tab. Now, the data appears in the table without choosing the **Go** option.

    !![bas lcap enable initial load - table auto-reloaded](BAS-LCAP-Load-Initial-Data-7-.png)


[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Customize the app: Update table column titles)]

1. In the **Page Map** editor of SAP Business Application Studio for low-code development, expand **Columns**.

    !![bas lcap list page update columns title - expand columns](BAS-LCAP-Update-Column-Titles-1-.png)

2. Choose **description**.

    !![bas lcap list page update columns title - open description properties](BAS-LCAP-Update-Column-Titles-2-.png)

3. Change the **Label** to **Description**.

    !![bas lcap list page update columns title - change description label](BAS-LCAP-Update-Column-Titles-3-.png)

4. Repeat the previous steps to change **`total_cost`** **Label** to **Total Cost**.

    !![bas lcap list page update columns title - change total_cost label](BAS-LCAP-Update-Column-Titles-4-.png)

5. Repeat the previous steps to change **`contractor_contractor`** **Label** to **Contractor**.

    !![bas lcap list page update columns title - change contractor_contractor label](BAS-LCAP-Update-Column-Titles-5-.png)

6. you can see that in the application's tab the column titles changed as per your updates. 

    !![bas lcap list page update columns title - preview app](BAS-LCAP-Update-Column-Titles-6-.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Customize the app: Present contractor name)]

1. In the **Page Map** editor of SAP Business Application Studio for low-code development, choose **contractor/name** for the **Text** property.

    !![bas lcap list page present contractor name - set text property](BAS-LCAP-Present-Contractor-Name-1-.png)

2. Choose **Text Only** for the **Text Arrangement** property.

    !![bas lcap list page present contractor name - set text arrangement property](BAS-LCAP-Present-Contractor-Name-2-.png)

3. You can see that in the application's tab the ID is replaced by the name of the contractor.

    !![bas lcap list page present contractor name - preview app](BAS-LCAP-Present-Contractor-Name-3-.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Customize the app: Change titles in the object page)]

1. In the SAP Business Application Studio for low-code development, choose the **Page Map** option.

    !![bas lcap object page change titles - show page map](BAS-LCAP-Update-Object-Page-Titles-1-.png)

2. Choose the **edit** option of the **Object Page**.

    !![bas lcap object page change titles - launch object page edit](BAS-LCAP-Update-Object-Page-Titles-2-.png)

3. Expand **General Information** -> **Form** -> **Fields**.

    !![bas lcap object page change titles - expand](BAS-LCAP-Update-Object-Page-Titles-3-.png)

4. Repeat previous steps to make the following changes to the **Fields** **Label** property.

    >Hint: Trigger the **Label** update, by pressing [ENTER] or clicking somewhere on the screen.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | description | **Description** |
    | B | `total_cost` | **Total Cost** |
    | C | `contractor_contracotr` | **Contractor** |

    !![bas lcap object page change titles - change titles](BAS-LCAP-Update-Object-Page-Titles-4-.png)

5. You can see that in the app's tab the titles are changed as per your updates.

    !![bas lcap object page change titles - preview app](BAS-LCAP-Update-Object-Page-Titles-5-.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Customize the app: Add contractor section to the object page)]

1. In the **Page Map** editor of SAP Business Application Studio for low-code development, hover over **Sections**, and choose the **Add Sections** option.

    !![bas lcap object page add section - add section](BAS-LCAP-Add-Contractor-Section-1-.png)

2. Choose **Add Form Section**.

    !![bas lcap object page add section - add form section](BAS-LCAP-Add-Contractor-Section-2-.png)

3. Set the **Label** to **Contractors** and choose **Add**.

    !![bas lcap object page add section - set form section label](BAS-LCAP-Add-Contractor-Section-3-.png)

4. Expand **Contractors** -> **Form**.

    !![bas lcap object page add section - expand contractor form section](BAS-LCAP-Add-Contractor-Section-4-.png)

5. Drag and drop **Contractor** from **General Information -> Form -> Fields** to **Contractors -> Form**.

    !![bas lcap object page add section - move contractor between form sections](BAS-LCAP-Add-Contractor-Section-5-.png)

6. You can see that in the application's tab, a new **Contractor** section appears with the **Contractor** field.

  !![bas lcap object page add section - app preview](BAS-LCAP-Add-Contractor-Section-6-.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Customize the app: Change object page title)]

1. In the **Page Map** editor of SAP Business Application Studio for low-code development, choose **Header**.

    !![bas lcap object page change title - select header](BAS-LCAP-Change-Object-Page-Title-1-.png)

2. For the **Title** property choose **description**.

    !![bas lcap object page change title - change title property](BAS-LCAP-Change-Object-Page-Title-2-.png)

3. You can see that in the application's tab the object page title changed as per your updates.

    !![bas lcap object page change title - app preview](BAS-LCAP-Change-Object-Page-Title-3-.png)

[DONE]
[ACCORDION-END]


**Congratulations!**

With this, you have successfully completed developing a business application using low-code capabilities of SAP Business Application Studio.

During the development you have modelled your business data. You have created a service through which data in the data model is accessed. You have created a user interface, which is actually an app, from which an application user can interact with the data. Throughout the development process, you continually test run the app, which is a development best practice to make sure the development is on-track.

And in this tutorial, you learned about high-productivity tools that are available out-of-the-box in SAP Business Applications Studio, such as: home page, project explorer, data model editor, service editor, templates and wizards, application editors, application preview, and much more.


---
