---
title: Add a Deployed Fiori App to Your Launchpad Page
description: Add the app that you deployed to your SAP Cloud Platform subaccount to your launchpad page.
auto_validation: true
primary_tag: products>sap-cloud-platform-portal
tags: [  tutorial>beginner, topic>sapui5, products>sap-cloud-platform, products>sap-cloud-platform-portal  ]
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
time: 10
---

## Prerequisites  
  - You've opened SAP Cloud Platform Portal and from the Site Directory, you've clicked on the tile of the `Supplier Portal` site that you created. For more information, see [Create a Portal freestyle site](cp-portal-freestyle-site-create-site).
  - You have deployed the `Product List` SAPUI5 app to your SAP Cloud Platform subaccount.

## Details
### You will learn  
 - How to configure the SAP Fiori (SAPUI5) app that you deployed to your SAP Cloud Platform subaccount and add it to your launchpad page.


[ACCORDION-BEGIN [Step 1: ](Add your app to the Portal site)]

1. From the side navigation panel in your `Supplier Portal` site, click **Content Management** and select **Apps** to open the **Manage App Configuration** editor.

    ![open content management](1-content-management-apps.png)

2. Click + at the bottom of the **Apps** panel to add a new app. The **Properties** tab is now in focus.

    ![Add new app](2-add-new-app.png)

3. Go to the **App Resource** field and click the browse icon.

    ![Browse for your app](3-browse-app.png)

4. In the **Select App Resource** screen, select the `Product List` app and click **OK**.

    ![Select Product List app](4-select-product-list-app.png)

5. In the **App Title** field, leave the name as `Product List`.

6. The next step is to define the **Intent Navigation** where you need to give a unique combination of a semantic object and an action. Enter the **Semantic Object** as `productslist` and **Action** as `Display`.

    ![Define intent navigation](5-intent-navigation.png)

7. Click **Save**.


>You've configured the basic properties of the `Product List` app. You will see that the **Catalogs** tab is red – this is because you still need to assign this app to a catalog.   

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Assign your app to a catalog)]

1.	Click the **Catalogs** tab.

    ![Select catalog tab](6-select-catalog-tab.png)

2.	Click **Edit** at the bottom right of the screen.

    ![Click edit](6a-click-edit.png)

3.	Click + in the **Assigned Catalogs** table.

    ![Assign Catalog](7-assign-catalog.png)

4.	In the **Select Catalogs** screen, select the `Everyone` catalog and click **OK**.

    ![Select the Everyone catalog](8-select-everyone-catalog.png)

5.	Click **Save**.

>You have assigned the `Product List` app to the `Everyone` catalog allowing all users who are authenticated to access this app.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Define a tile for the app)]

You now want to create an app tile for your app so that users can click the app and open it.

1. Go to the **Visualization** tab.

2. Click **Edit** at the bottom right of the screen.

    ![Visualization](9-visualization.png)

3. Select **Static App Launcher** as the **Tile Type**.

4. Leave the **Title** as `Product List`.

5. Add a **Subtitle** – call it `My Product List`.

6. Select an icon. Look for `sap-icon://Fiori2/F0401`.

    ![Select an icon](11-select-icon.png)

7. See the preview of your tile.

    ![Preview the tile](12-tile-preview.png)

8. Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add the app to a group)]

Now that you've created a tile for your `Product List` app you can add it to the group you created in the previous tutorial.

1. Go to the **Groups** tab and click **Edit**.

    ![Open groups tab](13-add-app-to-group.png)

2. Click the + icon in the **Assigned Groups** table.

    ![Add a group](13a-add-group.png)

4. In the **Select Groups** screen, select the `Company Products` group that you created previously for your launchpad page and click **OK**.

    ![Select group](14-select-group.png)

6. Click **Save**.

    ![Save group](15-save-group.png)

>Your app has been added to the `Company Products` group which is part of your launchpad page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Preview your app on the launchpad page)]

To see the Product List app in the Company Products group on your launchpad page, do the following.

1. Click **Publish** at the top right side of your screen.

    ![Publish](16-publish.png)

2. In the **Publish Site** screen, select **Publish and Open**.

    ![Publish and open](17-publish-and-open.png)

This is what you'll see:

![Preview app](18-preview.png)

>You have added a SAP Fiori (SAPUI5) app to a new group on your `Supplier Self Service` launchpad page. In the next tutorial you are going to add a SAP WEB DYNPRO ABAP app to another group on this launchpad page.

[VALIDATE_1]
[ACCORDION-END]





---
