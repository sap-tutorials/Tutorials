---
title: Add a Web Dynpro ABAP App to Your Portal Site
description: Add a classic SAP UI app (like those typically run by SAP customers on the data center backends), to an SAP Cloud Platform Portal site.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-portal
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert 
---

## Prerequisites
- You have created a destination between SAP Cloud Platform and your backend system using the SAP Gateway Demo System.
- You have opened your trial account or other subaccount.




## Details
### You will learn
  - How to expose a Web Dynpro ABAP backend application to your SAP Cloud Platform Portal site.

---

[ACCORDION-BEGIN [Step 1: ](Open the Content Manager)]

1. Click **Subscriptions** in the left navigation panel of your trial account and in the search box, enter `portal` to search for the **Portal** tile.

    ![Click subscriptions](1-click-subscriptions.png)

2. Click **Go to Application** on the **Portal** tile.

    ![Open Portal Service](2-go-to-application.png)

    The **Site Manager** opens with the **Site Directory** in focus.

4. In the side panel, click the icon that opens the **Content Manager**.

    ![Open Content Manager](3-open-content-manager.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure the Web Dynpro ABAP app)]

1.  In the Content Manager, click **+ New** -> **App**.

    ![Create new app](4-new-app.png)

2. In the **PROPERTIES** tab, enter these values:

    |  Field     | Value
    |  :------------- | :-------------
    |  Title           | `Search POs`
    |  System          | Select the value `ES5`
    |  App UI Technology    | Select `Web Dynpro ABAP`
    |  Application ID           | `S_EPM_FPM_PO`

    ![Add app properties](5-app-properties.png)

3. In the **NAVIGATION** tab, enter these values:

    |  Field     | Value
    |  :------------- | :-------------
    |  Semantic Object           | `S_EPM_FPM_PO`
    |  Action          | `Display`

    ![Add navigation properties](6-navigation-properties.png)

4. In the **VISUALIZATION** tab, enter these value:

    |  Field     | Value
    |  :------------- | :-------------
    |  Subtitle           | `Search for Purchase Orders`
    |  Icon          | Select the `activity-2` icon. Just type the icon name in the search bar.

    Click **Save**.

    ![Add visualization properties](7-visualization-properties.png)

You've configured the app. Now you need to assign it to a group so that users will be able to see the app on the launchpad page of your Portal site. You also need to assign it to the **Everyone** role so that users can access the app.  

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Assign the app to a group)]

In this step, we will first create a new group and then assign the app to it.

1. Go back to the Content Manager.

    ![Go back to Content Manager](8-go-to-content-manager.png)

2. Click **+ New** and select **Group**.

    ![Add new Group](9-add-group.png)

3. Name the group `Purchase Orders`.

4. In the **Assignments** panel on the right, type in `se` to find the `Search POs` app.  Then next to the app name, click the **+** icon to assign the app to the group.

5. Click **Save**.

    ![Assign app to group](10-assign-to-group.png)


6. Go back to the Content Manager to see your new group in the list.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Assign the app to a role)]

In this step you will assign the app to the `Everyone` role so that users can access and launch the app in the launchpad page.

1. In the Content Manager, click the `Everyone` role.

    ![Click everyone role](11-click-everyone-role.png)

2. Click **Edit** in the Role editor.

3. In the **Assignments** panel on the right, enter `se` in the search bar to look for the `Search POs` app.

3. Click the **+** icon next to the app name to assign the app to the `Everyone` role.

3.  Click **Save**.

    ![Assign app to role](12-assign-role.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Launch the app)]

1. From the side panel, click the icon that opens the Site Directory.

    ![Open site directory](13-open-site-directory.png)

2. On the `JobCore Portal` site (or another site that you created), click the **Go to site** icon.

    ![Go to site](14-go-to-site.png)

3. Enter your logon details if prompted to do so.

4. In the `Purchase Orders` group, click the Web Dynpro ABAP app called `Search POs` to launch it.

    ![View my apps](15-my-apps.png)

And this is what you'll see.

  ![View app](16-web-dynpro-app.png)

[VALIDATE_7]

[ACCORDION-END]
