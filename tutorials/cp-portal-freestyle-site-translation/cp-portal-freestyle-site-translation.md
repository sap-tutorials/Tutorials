---
title: Add a launchpad page with apps to your freestyle portal site
description: Add an SAP Fiori launchpad page to a freestyle portal site and then add apps to the launchpad page.
primary_tag: products>sap-cloud-platform-portal
tags: [  tutorial>beginner, topic>sapui5, products>sap-cloud-platform, products>sap-cloud-platform-portal  ]
time: 20
---

## Prerequisites  
 - You've opened SAP Cloud Platform Portal and from the Site Directory, you've clicked on the tile of the `Supplier Portal` site that you created. For more information, see [Create a Portal freestyle site].

## Details
### You will learn  
 - How to add a SAP Fiori launchpad page.
 - How to create and configure a catalog to enable role-based access to your apps.
 - How to configure apps and add them to relevant groups in your launchpad page.

[ACCORDION-BEGIN [Step 1: ](Add a SAP Fiori launchpad page)]
Before you can add apps to a freestyle site, you first have to add a launchpad page to it. Then you add apps to the launchpad page.

>The page you are adding is just like any other page in your freestyle site. The only difference is that you can add apps to it and arrange them in groups just like you would do on any SAP Fiori launchpad site.

  1. From your `Supplier Portal` click + at the bottom of the **Pages** panel.
  2. In the **Create Page** wizard, make sure that the **Template Source** shows **This Site**.
  3. Name the page `My Apps`.
  4. Select the **SAP Fiori Launchpad** page template.
    ![Create an SAP Fiori launchpad page](1-create-page.png)
  5. Click **Finish**.
    You'll see that the **My Apps** page is displayed in the **Pages** panel.
    ![My Apps Launchpad Page](2-launchpad-page.png)
  6. Select your new page and using the ˅, open the actions menu, and select **Add to Menu** to open the **Menu Editor**. This ensures that the `My Apps` page is now part of the Portal's site menu.
    ![Add page to site menu](3-add-page-to-menu.png)
  7. **Save** your settings.

>You have now created your launchpad page and ensured that it will show up in the freestyle site's menu.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create and configure a catalog)]

In this step you're going to create a catalog so that users can access the apps that are assigned to the same catalog as their role.  
Once you've created the catalog, we'll assign a role to it, and then once you've configured your apps, you'll assign your apps to this same catalog.

Let's create the catalog as follows:

  1. From the side panel of your `Supplier Portal` site, click **Content Management** and select **Catalogs**. This opens the **Manage Catalogs** editor.
     ![Open catalog editor](9-open-catalog-editor.png)
  2. Click + at the bottom of the **Catalogs** panel on the left to add a new catalog.
  3. Name the catalog `Supplier Catalog`.
     ![Add catalog](10-add-catalog.png)
  4. Now go to the **Roles** tab and click + in the **Assigned Roles** table, select the `Everyone` role, and click **OK**.

    >The `Everyone` role means that all users are authenticated and can access this catalog.

  5. **Save** your settings.

> We have created the `Supplier Catalog` and assigned the `Everyone` role to it. Any user assigned to the `Everyone` role can access any apps that are assigned to the `Supplier Catalog`.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Configure apps)]
In this step, you're going to create and configure three apps – two SAPUI5 apps and a URL app. Then you'll assign them to the `Supplier Catalog` that you created in the previous step.

  1. From the side navigation panel in your `Supplier Portal` site, click  **Content Management** and select **Apps**. This opens the **Manage App Configuration** editor.
    ![Open Manage App Configuration editor](6-content-management-apps.png)
  2. Click + at the bottom of the **Apps** panel on the left to add a new app and then do the following:
    <ol type="a"><li>The **Properties** tab is already in focus. Go to the **App Resource** field and browse your SAP Cloud Platform subaccount for an app that you want to configure.
    </li><li>Select the app resource and click **OK**.
    </li><li>In the **App Title** field, name your app `Products List`.
    </li><li>Under **App Resource Details**, make sure that the **App Type** is `SAPUI5`.
    </li><li>Now define the **Intent Navigation** where you need to give a unique combination of a semantic object and an action. Enter the **Semantic Object** as `productslist` and **Action** as `display`.
    </li><li> Enter the SAPUI5 Component name as `productsList`.
    </li><li> **Save** your settings.

     ![Properties tab](8-properties-tab.png)</li></ol>

    >The properties of the app vary depending on the app type. Some values are pre-populated when you select the app type and others you will have to enter yourself.

  3. Repeat these steps and add two more apps:
    These are the values for the first app:
    - **App Title** = `List of Suppliers`
    - **Intent Navigation**:
          - **Semantic Object** = `supplierlist`
          - **Action** = `display`
    - **App Type** = `SAPUI5`
    - **SAPUI5 Component** = `supplierlistcomponent`

    These are the values for the second app:

      - **App Title** = `Company Regulations`
      - **App Type** = `URL`
      - **URL** = `http://www.sap.com`

  4. You will notice that the **Catalogs** tab is red each time you create an app, warning you that you should assign the app to a catalog. Instead of assigning each app to the `Supplier Catalog` separately, go directly to the **Manage Catalogs** editor and do it there - like this:
  <ol type="a"><li>From the side panel, click **Content Management** and select **Catalogs**.
    ![Open Catalog Editor](9-open-catalog-editor.png)
  </li><li>Go to the **Apps** tab and click **Edit** at the bottom right of your screen.
  </li><li>Click + to open the **Select Apps** screen.
  </li><li>Select your 3 apps, and click **OK**.
     ![Select apps](11-select-apps-for-catalog.png)
     </li><li> **Save** your settings.</li></ol></li></ol>

>You've now created and configured 3 apps and assigned them to the `Supplier Catalog`. Now add your apps to groups so that users will see them on their launchpad page.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create groups)]

In this step, you'll create 2 groups and then add the apps to these groups.
Let's get started!

  1. Click **Page Management** -> **Pages** from the side panel and select the `My Apps` launchpad page. You will see that the **Manage Groups** editor is in focus.
  2. Click + at the bottom of the **Groups** panel to add a new group.
  ![Add a new group ](4-add-group.png)
  3. Name the group `Administration Department`.
  4. Now add a role to the group as follows:
  <ol type="a"><li>Go to the **Roles** tab.
  </li><li> Click **Edit** in the bottom right of the screen.
  </li><li>In the **Assigned Roles** table, click +, select the `Everyone` role, and click **OK**.
  This ensures that only users that are assigned to the `Everyone` role, can see the group in their launchpad.</li></ol>
  5. **Save** your changes.
  6. Now repeat this process and name the second group `Purchasing Department`.
  ![Add another group](5-add-second-group.png)

>You have created two groups and added a role to them. Now you'll add apps to these groups.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add apps to groups)]

In this step, you're going to add the apps to the relevant groups.

This is how you do it:

1. Still in the **Manage Groups** editor, select the `Administration Department` group in the **Groups** panel.
2. Go to the **Apps** tab.
3. Click **Edit** at the bottom right of the editor.
4. In the **Assigned Apps** table click + and select the following apps: `List of Suppliers`, `Company Regulations`, and `Products List`.
5.  Click **OK** and **Save** your settings.
5. Now select the other group called `Purchasing Department` and as you did above, add the `List of Suppliers` app to it.
6. Click **OK** and **Save** your settings.

> You have now added your apps to the relevant groups.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Preview your launchpad page)]

Now you can look at your launchpad page and see how your users will see their apps.

1. Click **Preview** at the top right of the screen.
   ![Preview your launchpad page](13-preview-site.png)
2. Click the `My Apps` page (the SAP Fiori Launchpad page that you added).

This is what you'll see:
![My Apps launchpad page](14-My-Apps-launchpad-page.png)

>In the next tutorial we will add one of the classic UIs to our SAP Fiori launchpad page.

[ACCORDION-END]





---
