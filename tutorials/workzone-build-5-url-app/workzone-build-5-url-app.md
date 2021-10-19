---
title: Add a URL App to Your SAP Work Zone Page
description: Create a URL app and add it to a page in SAP Work Zone.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-launchpad-service]
primary_tag: products>sap-work-zone
author_name: Lindsay Bert
---

## Prerequisites
 - You've already designed the **Home** page where you'll add this app.



## Details
### You will learn
  - How to configure a URL app and add it to your **Home** page in SAP Work Zone


You've already added a SAPUI5 app to your **Home** page under the **Most Popular Apps** header. Now in this tutorial, you'll use the **Content Manager** to add a URL app under the same header.

---

[ACCORDION-BEGIN [Step 1: ](Open the Content Manager)]

1. Access the **Administration Console** from the user actions dropdown menu under your avatar.

    !![Open the Admin Console](1-open-admin-console.png)

2. Go to the **External Integrations** section, expand it, and click **Business Content**.

    !![Open Business Content](2-open-business-content.png)

3. Click **Content Manager**.

    !![Open Content Manager](3-open-content-manager.png)

    The **Content Manager** opens with the **My Content** tab in focus.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create and configure a new app)]

1. From the **Content Manager**, click **+ New** and select **App** from the list.

    !![Create new app](4-create-new-app.png)

    The App editor opens with the **PROPERTIES** tab in focus.

2. Enter the following values:

    * **Title**: `Innovation at SAP`

    * **Open App**: In place

    * **URL**: `https://sap.io`

      !![Define app properties](5-app-properties.png)

3. Click the **NAVIGATION** tab to specify the intent of your app.

    >The unique combination of a semantic object and an action is called an intent. It is used to define navigation to an application.

4. Enter the following values:


      * **Semantic Object**: `Innovation`

      * **Action**: `Display`

        !![Define navigation properties](6-navigation-properties.png)

5. Click the **VISUALIZATION** tab. In this tab, you specify how the app tile will appear in the **Applications** page of your work zone.

6. Enter the following values:

      * **Subtitle**: `SAP.iO program `

      * **Information**:  `Learn about SAP.iO`

      * **Icon**: Click the browse icon, type `visits`, click on the displayed icon, and click **OK**.

7.  On the right, you can see a preview of the tile with all the properties you entered.
    Click **Save**.

      !![Define visualization properties](7-vizualization-properties.png)

You've configured the URL app and in the next step you'll go back to the **Content Manager** to see it in the list of content items.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](View the app in the Content Manager)]

Click the **Content Manager** icon in the left side panel to navigate back to it.

  !![Go back to the Content Manager](8-back-to-content-manager.png)

You can see your app in the list of content items in the **My Content** screen:

  !![View app in my content](9-view-app.png)

To view the app in the **Applications** page of your work zone, you must assign the app to a role and to a group. This is described in the following steps.



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Assign the app to the Everyone role)]

The `Everyone` role already exists in the **Items** list of the **Content Manager** by default. Content assigned to the `Everyone` role is visible to all users.

1. In the **Items** list, click the **Everyone** role to open the Role editor.

    !![Click Everyone role](10-click-everyone-role.png)

2. Click **Edit**.

    !![Click Edit](11-click-edit.png)

3. Click the search box in the **Assignments** panel on the right. Any available apps are shown in the list below.

    >If you have many apps, you can type some letters of your app name in the search bar, (for example, `In`) to search for the app.

4. In the **Results** list, click the **+** icon next to the `Innovation at SAP` app to assign this role to your app. You'll see that the icon changes.

5. Click **Save**.

    !![Assign app to role](12-assign-app-to-role.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a group and assign the app to it)]

A group is a set of one or more apps displayed together on the **Applications** page of your work zone. Assigning apps to groups, makes them visible to the user on this page. In this step you'll create a new group and assign the app to it.

1. Click the icon in the side panel to navigate back to the **Content Manager**.

    !![Go back to the Content manager](13-back-to-content-manager.png)

2. Click **+ New** and select **Group** to create a group.

    !![Add new group](14-add-new-group.png)

3. Enter `SAP` as the **Title** and in the **Assignments** panel, click inside the search box on the right of the screen, to show all available apps.

4. In the **Results** list, click **+** next to the `Innovation at SAP` app to assign it to your group.

5. Click **Save**.

    !![Assign app to group](15-assign-app-to-group.png)

Your app is configured. Now you'll go to the SAP Work Zone **Applications** page to see that the app appears there.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](View the app in the Applications page of your work zone)]


1. Click the **Work Zone Directory** icon in the side panel.

    !![Go to Site Directory](16-go-to-site-directory.png)

2. Click the **Open work zone** icon.

    !![Open work zone](17-open-work-zone.png)

3. In the top-level menu of your work zone, click **Applications**.

    !![Open Applications page](18-open-applications-page.png)

You'll see your app displayed as a tile in the new group you assigned it to:

!![View new app in new group](19-view-second-app.png)

Now you're going to add this app to the **Home** page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Open the Page Designer)]


1. Click the back arrow to go back to the **Home** page.

    !![Back to Home](20-go-to-home.png)

    > You can also navigate back to the **Home** page from the top navigation bar.  Click **Home** and then select **Home** next to the little house icon.

2. Click the **Expand page settings** wand on the right of the page to expose various icons and then click the pencil icon directly under the wand to open the Page Designer. You can close the tips screen.

    !![Open page designer](21-open-page-designer.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add the URL app to the Home page)]

In the previous tutorial, you added a header to your **Home** page and underneath it, you added a SAPUI5 app. You're now going to add the URL app under the existing app.

1. Scroll down to the bottom of the page and in the last row under the `New Orders` app, click **Add Widget** to open the widget gallery.

    !![Add Widget](22-add-widget.png)

3. Under **Standard Tools**, select the **Applications** widget.

    !![Add the Applications widget](23-applications-widget.png)

4. In the Search box, start typing in the name of your app `Innovation at SAP`, select the app, and then click **Select**.

    !![Select the app](24-select-app.png)

6. Scroll to the top of the screen and click **Publish**.

    This is how your **Home** page looks now with the 2 added apps:

    !![View home page with 2 apps](25-homepage-apps2.png)

7. Click the apps to open them.



[VALIDATE_7]
[ACCORDION-END]
