---
parser: v2
auto_validation: true
time: 15
tags: [  tutorial>beginner, products>sap-business-technology-platform, products>sap-build-work-zone--standard-edition ]
primary_tag: software-product>sap-build-work-zone--standard-edition
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---


# Integrate a URL App to SAP Build Work Zone
<!-- description --> Create a URL app and add it to your site.

## You will learn
  - How to create a URL app and add it to your site

## Intro
Use the **Content Manager** to add apps to SAP Build Work Zone.

### Open the Content Manager


Click the Content Manager icon in the left panel to open the **Content Manager**.

<!-- border -->![Content manager icon](1-content-manager-icon.png)


### Create and configure new app



1. Click **Create** and select **App** from the list.

    <!-- border -->![Content manager new app](2-new-app.png)

2. In the header of the app editor, enter a title `External Community`.

    <!-- border -->![Add title for app](2a-add-title.png)


3. Under the **Configuration** tab, enter the following values (some of them are already there by default):

    * **Open App**: In a new tab

        >It's preferable to open apps in place, but this time, we'll open the app in a new tab.

    * **System**: No System

    * **App UI Technology**: URL

    * **URL**:  `https://community.sap.com/topics/work-zone`

    <!-- border -->![App editor properties tab](3-app-editor-properties.png)

4. Click the **Navigation** tab and enter the intent of your app.

    > The unique combination of a semantic object and an action is called an intent. It is used to define navigation to an application.

5. Enter the following values:

     * **Semantic Object**: `Home`

     * **Action**:  `Display`

    <!-- border -->![App editor navigation tab](4-app-editor-navigation.png)

6. Click the **Visualization** tab.

    In this tab, specify how the app will be displayed in your site.

7.  Enter the following values:

    * **Subtitle**: `About Us `

    * **Information**:  `Learn about JobCore`

    * **Icon**: Click the browse icon, type `visits`, and click on the displayed icon to add it to your tile.

      You see a preview of the tile with all the properties you entered.

      <!-- border -->![App editor visualization tab](5-app-editor-visualization.png)

8.  Click **Save**.

You have configured the URL app and in the next step you'll go back to the **Content Manager** to see it in the list of content items.




### View the app that you created

1. Go back to the Content Manager using the breadcrumbs.

    <!-- border -->![Go to content manager icon](6-go-to-content-manager.png)

2. In the **Content Manager**, you can see your app in the list.

    <!-- border -->![Content manager with app](7-view-app.png)


To view the app in runtime, you must assign the app to a role. 


### Assign the app to the Everyone role.


>Spaces must be assigned to a role so that users assigned to a specific role are able to access the space and see the relevant pages assigned to it. Content assigned to the `Everyone` role is visible to all users. 

1. In the **Content Manager**, click the **Everyone** role.

    <!-- border -->![Click Everyone role](8-everyone-role.png)

2. Click **Edit**.

    <!-- border -->![Edit](8a-click-edit.png)


3. All available apps are shown in the editor. In the **Assignment Status** column you can see that the `External Community` app is not assigned. Click the toggle to assign the app to the `Everyone` role.

    <!-- border -->![Assign app to role](9-assign-role.png)

4. Click **Save**.


Congratulations you've created a site with two apps.

In the next tutorial, you're going to add these apps to a page.



---
