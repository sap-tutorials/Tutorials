---
title: Integrate a URL App into Your Portal Site (Cloud Foundry)
description: Create a URL app and add it to the launchpad page on your Portal site on Cloud Foundry.
auto_validation: true
time: 15
tags: [  tutorial>beginner, products>sap-cloud-platform-portal ]
primary_tag: products>sap-cloud-platform-portal
---


## Details
### You will learn
  - How to create a URL app and add it to your site

You create business content such as apps, groups, catalogs, and roles, using the Portal service Site Manager on the subaccount level (and not for a specific site). Apps are consumed from specific sites according to their role assignment.


[ACCORDION-BEGIN [Step 1: ](Go to the Content Manager)]

To go to the **Content Manager**, do the following:

1. Use the back arrow to get to the Site Manager.

    ![Go back to Site Manager](0_open_site_manager.png)

2. Click the **Content Manager** icon in the left panel.

    ![Content manager icon](1-content-manager-icon.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new app)]

>Note that by default, the `Everyone` role is provided and is visible in the Content manager.

Click **New** and select **App** from the list.

![Content manager empty new app](2-content-manager-empty-new-app.png)

The App editor opens and now you can configure your new app.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Configure the URL app)]

The App editor opens on the **PROPERTIES** tab.

1. Enter the following values:

    * **Title**: `Innovation at SAP`

    * **Open App**: In place

    * **URL**:  `https://sap.io`

    ![App editor properties tab](3-app-editor-properties.png)

2. Click the **NAVIGATION** tab to specify the intent of your app.

3. Enter the following values:

    * **Semantic Object**: `Innovation`

    * **Action**:  `Display`

    ![App editor navigation tab](4-app-editor-navigation.png)

4. Click the **VISUALIZATION** tab.

    In this tab, you specify how the app will be displayed in the launchpad.

5.  Enter the following values:

    * **Subtitle**: `SAP.iO program `

    * **Information**:  `Learn about SAP.iO`

    * **Icon**: Click the browse icon, type `Visits` and click **OK**.

      You see a preview of the tile with all the properties you entered.

      ![App editor visualization tab](5-app-editor-visualization.png)

6.  Click **Save**.

You can then use the back arrow to get back to the Content Manager to see your app in the list of content items.



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Go back to Content Manager)]
Click the arrow in the top left corner of the app editor to go back to the Content Manager.

![Go to content manager icon](6-go-to-content-manager-icon.png)

In the Content Manager, you see your app in the list:

![Content manager with app](6-content-manager-with-app.png)

> If you want to open your app for editing, simply click it and click **Edit** in the App editor.

>![App editor Edit](6-app-editor-edit.png)

You have now created a URL app.

For end users to view the app in runtime, you must assign a role to the app. Any end user who needs to view this app, should be a member of the role that you have assigned to the app. In this tutorial, we use the `Everyone` role.  You also need to assign the app to a group so that it's visible in the Launchpad page.

This is described in the following steps.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Assign group to app)]

In this step, you will create a group and assign it to your app.

>You are assigning your app to a group so that your site user can see it in the `Launchpad` page. A group is a set of one or more apps displayed together in the launchpad.

1. In the Content Manager, click **+ New** and select **Group** to open the Group editor.

    ![Add a new group](7-add-group.png)

2. Enter the **Title** `SAP` and in the **Assignments** panel on the upper right of the screen, type `In` to search for your app. You should see **Innovation at SAP**.

3. In the **Results** list, click + to assign this group to your app.

    ![7 group editor assign app](7-group-editor-assign-app.png)

4. Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Assign the Everyone role)]

In this step you'll assign the **Everyone** role to your app.

>Content assigned to the **Everyone** role is visible to all users.

1. In the Content Manager, click the **Everyone** role.

    ![Click Everyone role](8-everyone-role.png)

2. Click **Edit**.

3. In the Role editor, in the **Assignments** panel, type `In` to search for your app.

4. In the **Results** list, click + to assign this role to your app:

    ![8 role editor assign app](8-role-editor-assign-app.png)

5. Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Review the site)]

1. Click the Site Directory icon in the left panel.

    ![Open the Site Directory](8a-open-site-directory.png)

2. On the `JobCore Portal` tile in the Site Directory, click the  **Go to site** icon:

    ![9a Go to site](9-go-to-site-icon.png)

The **Home** page opens. Navigate to the **Launchpad** page by clicking `My Apps`. This is what you'll see:

![9b Site with one group and one app](9-site-with-1group-1app.png)

You can now launch your application by clicking the **Innovation at SAP** tile.

You've created and added your first app to the `Launchpad` page in your Portal site!

[VALIDATE_7]
[ACCORDION-END]



---
