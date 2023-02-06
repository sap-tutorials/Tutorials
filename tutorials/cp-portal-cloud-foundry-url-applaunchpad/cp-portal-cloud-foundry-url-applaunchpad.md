---
parser: v2
auto_validation: true
time: 15
tags: [  tutorial>beginner, products>sap-business-technology-platform, products>sap-build-work-zone--standard-edition ]
primary_tag: software-product>sap-build-work-zone--standard-edition
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---


# Add a URL App to Your Site
<!-- description --> Create a URL app and add it to your site.

## You will learn
  - How to create a URL app and add it to your site

## Intro
Use the **Content Manager** to add a URL app to your site.

### Open the Content Manager


Click the Content Manager icon in the left panel to open the **Content Manager**.

<!-- border -->![Content manager icon](1-content-manager-icon.png)


### Create and configure new app



1. Click **+ New** and select **App** from the list.

    <!-- border -->![Content manager empty new app](2-content-manager-empty-new-app.png)

    The app editor opens on the  **PROPERTIES** tab.

2. Enter the following values:

    * **Title**: `Innovation at SAP`  

    * **Open App**: In place

    * **URL**:  `https://sap.io`

    <!-- border -->![App editor properties tab](3-app-editor-properties.png)

3. Click the **NAVIGATION** tab and enter the intent of your app.

    > The unique combination of a semantic object and an action is called an intent. It is used to define navigation to an application.

4. Enter the following values:

     * **Semantic Object**: `Innovation`

     * **Action**:  `Display`

    <!-- border -->![App editor navigation tab](4-app-editor-navigation.png)

5. Click the **VISUALIZATION** tab.

    In this tab, you specify how the app will be displayed in your launchpad site.

6.  Enter the following values:

    * **Subtitle**: `SAP.iO program `

    * **Information**:  `Learn about SAP.iO`

    * **Icon**: Click the browse icon, type `visits`, select the displayed icon, and click **OK**.

      You see a preview of the tile with all the properties you entered.

      <!-- border -->![App editor visualization tab](5-app-editor-visualization.png)

7.  Click **Save**.

You have configured the URL app and in the next step you'll go back to the **Content Manager** to see it in the list of content items.




### View the app that you created


Click the Content Manager icon in the left side panel to open the **Content Manager**.

<!-- border -->![Go to content manager icon](6-go-to-content-manager-icon.png)

In the **Content Manager**, you can see your app in the list.

<!-- border -->![Content manager with app](6-content-manager-with-app.png)


To view the app in runtime, you must assign the app to a role. You also need to assign the app to a group so that it'll be visible in your site.

This is described in the following steps.


### Assign the app to the Everyone role



>Content assigned to the `Everyone` role is visible to all users.

1. In the **Content Manager**, click the **Everyone** role.

    <!-- border -->![Click Everyone role](8-everyone-role.png)

2. Click **Edit**.

    ![Edit](8a-click-edit.png)

3. Click the search box in the **Assignments** panel on the right, any available apps are shown in the list below.

4. Click **+** to assign the `Innovation at SAP` app to this role.

    <!-- border -->![Role editor assign app](8-role-editor-assign-app.png)

5. Click **Save**.


### Create a group and assign the app to it



>A group is a set of one or more apps displayed together in a site. Assigning apps to groups, makes them visible to users.

1. Open the **Content Manager** and click **+ New** and select **Group** from the list.

    <!-- border -->![Add a new group](7-add-group.png)

2. Enter the **Title** `SAP` and in the **Assignments** panel on the right of the screen, click the search box to see any available apps in the list below.

3. Click **+** to assign the `Innovation at SAP` app to this group.

    <!-- border -->![7 group editor assign app](7-group-editor-assign-app.png)

4. Click **Save**.




### Review the site


1. Click the Site Directory icon in the left panel to open the **Site Directory**.  

    <!-- border -->![Open the Site Directory](8a-open-site-directory.png)

2. On the `JobCore` tile, click the **Go to site** icon:

    <!-- border -->![Go to site](9-go-to-site-icon.png)

You'll see that your new app is displayed in the `SAP` group.

<!-- border -->![Site with one group and one app](10-site-with-groups.png)

You can now launch your application by clicking the **Innovation at SAP** tile.

Congratulations you've created a site with two apps!





---
