---
title: Add a Fiori Launchpad Page to Your Portal Freestyle Site
description: Add an SAP Fiori launchpad page to your Portal site and add apps to it.
auto_validation: true
primary_tag: products>sap-cloud-platform-portal
tags: [  tutorial>beginner, topic>sapui5, products>sap-cloud-platform, products>sap-cloud-platform-portal  ]
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
time: 10
---

## Details
### You will learn  
 - How to add a SAP Fiori launchpad page to your freestyle site.


[ACCORDION-BEGIN [Step 1: ](Add a SAP Fiori launchpad page)]

In this tutorial you're going to create a launchpad page in your Portal freestyle site and then later you'll add apps to it.

>The page you are adding is just like any other page in your freestyle site. The only difference is that you can add apps to it and arrange them in groups just like you would do on any SAP Fiori launchpad site.

1. From the Portal Admin Space, open the **Supplier Portal** for editing in the Site Designer.

    ![Open Site in Site designer](1-open-site.png)

2. In the side panel, click **Page Management** -> **Pages**.

    ![Open page management](2-open-page-management.png)

3.	Click + in the **Pages** panel.

    ![Add a page](3-create-page.png)

4. In the **Create Page** wizard, enter the following:

      - **Page Name**: `Supplier Self Service`
      - **Template Source**: Leave as is
      - Page template to select: `SAP Fiori Launchpad`

5. Click **Finish**.

    ![Create page](4-create-page-wizard.png)

The `Supplier Self Service` page is added to the list of pages.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new group for launchpad tiles)]

In this step you will create a new tile group. Tile groups logically aggregate tiles in the SAP Fiori launchpad page.

1. From the list of pages in the left side panel, select the new **Supplier Self Service** page. This opens the **Manage Groups** editor.

2. Click the **+** icon at the footer of the (empty) list of Groups.

    ![Create new FLP group](5-add-group.png)

3. In the **New Group** wizard enter the **Group Name**: `Company Products`.

    ![Enter group name](6-group-name.png)

4. Click the **Roles** tab to assign roles to the tile group.

    ![Open Roles tab](7-roles-tab.png)

5. Click the **+** icon in the empty **Assigned Roles** table.

    ![Assign role](8-assign-role.png)

6. In the dialog box, select the **Everyone** role and click **OK**.

    ![Select group role](9-select-role.png)

7. Click **Save** to create the new group.

    ![New group is created](10-save-group.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add your page to the Portal site menu)]

In this step you will add the SAP Fiori launchpad page (named **Supplier Self Service**) to the Portal's navigation menu.

1. From the list of pages in the left side panel, select the  **Supplier Self Service** page.

2. Click the bottom-facing-arrow icon to open the page action menu and select **Add to Menu**.

    ![Add to Menu](11-add-to-menu.png)

3. Click **Save**.

    ![Save menu](12-save-menu.png)

    > The **Menu Editor** is opened with a new menu entry named **Supplier Self Service** added at the bottom of the menu list. The new entry points to the newly created page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Publish your site)]

In this step we will publish the site so that you can see how the new launchpad page is displayed in the Portal site menu.

1. Click the **Publish** icon.

    ![Publish](13-publish-icon.png)

2. Click **Publish and Open**.

    ![Publish and open](14-publish-open.png)

3. See the `Supplier Self Service` launchpad page displayed in the Portal site menu.

    ![Display launchpad page](15-display-page.png)

[VALIDATE_1]
[ACCORDION-END]







---
