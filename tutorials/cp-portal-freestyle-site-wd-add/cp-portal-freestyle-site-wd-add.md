---
title: Add a Web Dynpro ABAP App to Your Launchpad Page
description: Add an existing Web Dynpro ABAP backend app to your freestyle Portal site.
auto_validation: true
primary_tag: products>sap-cloud-platform-portal
tags: [ tutorial>beginner, topic>cloud, products>sap-cloud-platform-portal ]
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
time: 10
---

## Details
### You will learn  
  - How to expose a Web Dynpro ABAP backend application to your SAP Cloud Platform Portal site


SAP customers who run classic SAP UI applications on their data center backend can transition to the SAP Cloud Platform and continue using these applications.
 

---

[ACCORDION-BEGIN [Step 1: ](Configure a Web Dynpro app in your site)]

1. Open the `Supplier Portal` freestyle site for editing in the Site Designer

2. Click the **Content Management** icon from the left panel and select **Apps**.

    ![Content management](1-content-management.png)

    > The **Manage App Configuration** editor is opened.

3. Click the **+** icon at the bottom menu to create a new app configuration.

    ![Add new app](2-add-new-app.png)

4. In the **Properties** tab, under **General** enter the following values:

    * **App Title** = `Search POs`
    * **App Subtitle** = `Search for Purchase Orders`

5. Under Intent Navigation, enter the following values:

    * **Semantic Object** = `S_EPM_FPM_PO`
    * **Object** = `Display`

6. Under **App Resources Details** enter the following values:

    * **App Type** = `Web Dynpro ABAP`
    * **Application ID** = `S_EPM_FPM_PO`
      (the transaction ID associated with the application we wish to add)
    * **System Alias** = `ES5`
      (the name of the destination connecting to the SAP Backend)
    * **SAP Business Client URI**	= `/ui2/nwbc/~canvas;window=app/WDA/S_EPM_FPM_PO`
      (path to the SAP NetWeaver Business Client used to run the app together with transaction ID).

    ![Add properties](3-add-properties.png)

7. Now select the **Catalogs** tab and click the **+** in the **Assigned Catalogs** table.

    ![Add catalog](4-add-wd-catalog.png)

8.  Add the **Everyone** catalog and click **OK**.

    ![Select everyone catalog](5-select-everyone-catalog.png)

9. Select the **Groups** tab and then click the + in the **Assigned Groups** table.

    ![Add group](16-add-group.png)

10. In the **Select Groups** dialog click **New** to create a new tile group.

    ![Select new group](7-select-new-group.png)

11. Name the group **Purchase Orders** and click **OK** to close the **New Group** dialog

    ![Name the group](8-purchase-orders-group.png)

12. In the **Select Groups** dialog box, the `Purchase Orders` group is already selected. Click **OK**.

    ![Purchase orders group added](17-purchase-orders-group.png)

13. Select the **Visualization** tab. Keep the **Static App Launcher** tile type value as well as the existing configuration.

14. Select this icon: `sap-icon://Fiori2/F0003` and click **OK**.

    ![Select an icon](9-select-icon.png)

15. Click **Save**.

    ![Visualization properties](18-visualization-properties.png)

>At this stage you can already launch the app. In the next step, you'll set up an external access point so that the app is available externally.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up an external access portal landscape)]

1. From the left side panel, click the cog icon to open the **Site Settings** editor.

    ![Site Settings](10-open-site-settings.png)

2. Click **Edit**.

    ![Click edit](11-click-edit.png)

3. Under **System Settings** enter the following values:

    * **Access Type** = `External`
    * **Access Classic UIs** = `Yes, and show in Launchpad`
      (Allows users accessing the site from outside of the corporate network to launch the Web Dynpro app).
    * **Launch SAP GUI apps in place** = `Yes`
    * **Launch WDA apps in place** = `Yes`

4. Click **Save**.

    ![System Settings](12-system-settings.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Preview the latest changes)]

1. In the Supplier Portal site designer top level menu, click the **Preview** icon.

    ![Click preview icon](13-click-preview.png)

    > The Supplier Portal runtime is opened in the preview environment in a new browser tab.

2. In the site's navigation menu click **Supplier Self Service** to navigate to the Fiori Launchpad page.

    ![Preview FLP](14-preview.png)

    > The Fiori Launchpad page is displayed with the new Tile group **Purchase Orders** and a new tile **Search POs** under it.

3. Click the apps to open them.



[VALIDATE_1]
[ACCORDION-END]
