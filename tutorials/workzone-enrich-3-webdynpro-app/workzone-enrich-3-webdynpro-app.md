---
title: Add a Web Dynpro ABAP App to your SAP Work Zone
description: Add a classic SAP application (like those typically run by SAP customers on the data center backends), to your SAP Work Zone.
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-work-zone]
primary_tag: software-product>sap-work-zone
author_name: Lindsay Bert
---

## Prerequisites
 - You have created a destination in SAP BTP cockpit to the SAP Gateway Demo System.
 - You have an environment set up and you can access your SAP Work Zone.
 - You have opened SAP Work Zone.

## Details
### You will learn
  - How to expose a Web Dynpro ABAP backend application in SAP Work Zone.
  -


To add business apps to SAP Work Zone, you configure them in a tool called the **Content Manager**, located in the Work Zone Manager. The **Content Manager** includes various editors that you can use to configure apps, groups, roles, and catalogs. Once configured, the app appears as a tile in the **Applications** page of your work zone - a single page that acts as an embedded launchpad. The page is accessed from the top-level menu of SAP Work Zone and displays the business apps that the user has permissions to launch.

---

In this tutorial you're going to add one of SAP's classic applications, a Web Dynpro ABAP app, to your work zone using the **Content Manager**. SAP's classic applications typically run on the backend of a data center.

[ACCORDION-BEGIN [Step 1: ](Open the Content Manager)]

1. Access the **Administration Console** from the user actions dropdown menu under your avatar.

    !![Access the Admin Console](1-access-admin-console.png)

2. Go to the **External Integrations** section, expand it, and click **Business Content**.

    !![Open Business Content entry](2-open-business-content.png)

3. Click **Content Manager** to open it.

    !![Access Content Manager](3-access-content-manager.png)

The **Content Manager** opens with the **My Content** tab in focus.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure the Web Dynpro ABAP app)]

1. In the **Content Manager**, click **+ New** and select **App** from the dropdown list.

    !![Add new app](4-new-app.png)

    The app editor opens with the **PROPERTIES** tab in focus.

2. Enter the following values:

    |  Field     | Value
    |  :------------- | :-------------
    |  Title           | `Search POs` If you're taking part in a workshop, please add your unique identifier to the app name like this: `<your unique identifier>_Search POs`.
    |  System          | Select the value `ES5`
    |  App UI Technology    | Select `Web Dynpro ABAP`
    |  Application ID           | `S_EPM_FPM_PO`

    !![App properties](5-app-properties.png)

3. In the **NAVIGATION** tab, enter these values:

    |  Field     | Value
    |  :------------- | :-------------
    |  Semantic Object           | `S_EPM_FPM_PO`
    |  Action          | `Display`

    !![Add navigation properties](6-navigation-properties.png)

4. In the **VISUALIZATION** tab, enter these values:

    |  Field     | Value
    |  :------------- | :-------------
    |  Subtitle           | `Search for Purchase Orders`
    |  Icon          | Click the browse icon, type `activity-2`. You'll see two icons. Click the first one and then click **OK**.

    !![Add visualization properties](7-visualization-properties.png)

5. Click **Save**



[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](View the app in the Content Manager)]

Click the icon in the left side panel to open the **Content Manager**.

  !![Back to Content Manager](8-back.png)

You can see your Web Dynpro ABAP app in the list:

  !![View app in list](9-view-app.png)

To view the app in runtime, you must assign the app to a role. You also need to assign the app to a group so that it'll be visible in the **Applications** page of your work zone. This is described in the following steps.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Assign the app to the Everyone role)]

>The `Everyone` role already exists in the **Items** list of the **Content Manager** by default. Content assigned to the `Everyone` role is visible to all users.

1. In the **Content Manager** in the **Items** list, click the `Everyone` role to open up the Role editor.

    !![Select everyone role](10-everyone-role.png)

2. Click **Edit**.

    !![Edit everyone role](11-edit-role.png)

3. Click the search box in the **Assignments** panel on the right, any available apps are shown in the list below.

4. Click the **+** icon next to the `Search POs` app to assign the app to the `Everyone` role. You'll see that the icon changes.

5. Then click **Save**.

    !![Assign app to role](12-assign-app-to-role.png)

The `Search POs` app is now assigned to the `Everyone` role. In the next step you'll assign the app to a group.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Assign the app to a group)]

>A group is a set of one or more apps displayed together in the **Applications** page of your work zone. Assigning apps to groups, makes them visible to users.

1. Click the icon in the side panel to navigate back to the **Content Manager**.

2. Click **+ New** and select **Group** to open the Group editor.

    !![Create a new group](13-new-group.png)

3. Name the group `Purchase Orders`.

4. In the **Assignments** panel, click inside the search box on the right of the screen, to show all available apps. You should see the  `Search POs` app.  

5. In the list, click **+** to assign the `Search POs` app to your group.

    You'll see that the icon changes.

6. Click **Save**.

    !![Assign app to group](14-assign-app-to-group.png)

    Now that the app is configured, it is automatically added to the **Applications** page in SAP Work Zone. You can also add it to your home page or to any workspace overview page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](View the app in the Applications page)]

1. Click the **Work Zone Directory** icon in the side panel.

    !![Open Work Zone directory](15-open-workzone-directory.png)

2. Click the **Open work zone** icon.

    !![Open work zone](16-open-workzone.png)

3. In the top-level menu of your work zone, click **Applications**.

    !![Open Applications page](17-open-applications-page.png)

You'll see your app is displayed as a tile in the group you assigned it to:

  !![View group with app](18-view-groupwithapp.png)


[VALIDATE_6]
[ACCORDION-END]
