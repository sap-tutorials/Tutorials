---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-build-work-zone--advanced-edition]
primary_tag: software-product>sap-build-work-zone--advanced-edition
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---

# Create a Site Menu in Your SAP Build Work Zone, advanced edition Site
<!-- description --> Use the versatile SAP Build Work Zone, advanced edition menu editor to easily create a fully customized menu bar in your site.

## Prerequisites
- The onboarding steps for SAP Build Work Zone, advanced edition have been completed in your subaccount that includes a subscription to the SAP Build Work Zone, advanced edition service. Note that SAP Build Work Zone, advanced edition is not available in a trial account
- You are a company administrator 


## You will learn
  - How to add menu items to your site menu

  In this tutorial you'll create a workpage and a workspace and add both to the site menu so that you can directly link to them. You will also add an app to your site menu and access it directly from the menu.


## Intro
When you open your site for the first time, the menu bar displays a number of predefined menu items, namely **Home**, **My Workspace**, **Applications**, **Workspaces**, and **Tools**. In this tutorial, we’re going to design a completely different menu bar using some of these predefined menu items as well as adding our own custom menu items.

---

### Download image for your workspace

Before you start, download this image file so that it's on your computer ready for you to add to your workpage that you'll create later in this tutorial.  [`workspace_row2_image1.jpg`](Workspace_Images/workspace_row2_image1.jpg)

### Open the Menu Editor


1.	Hover over the top default menu bar of your site to expose the edit icon and click it to open the Menu Editor.

     <!-- border -->![Open menu editor](1-open-menu.png)

    > On the left of the screen, you'll see the **Menu Editor** panel displaying a list of menu items. These are the same menu items displayed in your top menu bar. As you add and remove menu items from the panel, the menu bar updates to reflect your changes.

    <!-- border -->![View side panel](2-view-side-panel.png)

### Remove default menu items

In this step, we'll hide the default menu items that we don't need.

1. Next to **My Workspace**, click the 3 dots to open a list of actions and select **Hide**.

    <!-- border -->![Hide menu items](3-hide-menu-item.png)

2. Do the same for the **Applications** and **Tools** menu items.

When you're done, you'll see that your menu now only has 2 menu items and the others are grayed out. You can always put them back if you want. You can also delete them if you're sure you won't need them again. In the next step, you're going to add your own menu content.

### Create a workpage in the Menu Content screen

In this step, you're going to add a workpage to the site menu that users of your site can access. If the workpage doesn't already exist in your site, you can create it directly in the **Menu Content** screen of the **Menu Editor** and then reference it from the menu.

>You can create other types of content in the **Menu Content** screen too such as blog posts, wiki pages, and much more.


1. Click the **Content** icon in the top area of the **Menu Editor** panel.

    <!-- border -->![Add content icon](4-add-content.png)

2. In the **Menu Content** screen, select **+ Create** and select **Workpage** to open a workpage editor.

    <!-- border -->![Create a workpage](5-create-workpage.png)

3. Enter a title for the workpage: `Our Story` and then click **+** to add a section to the workpage.

    <!-- border -->![Design the workpage](6-design-workpage.png)

4. In the section, click **Add Widget**.

    <!-- border -->![Add widget](7-add-widget.png)

5. Select the **Image** widget.

    <!-- border -->![Select image widget](8-image-widget.png)

6. Click the link in the widget to browse for the image that you've saved on your computer and open it: `workspace_row2_image 1`

    <!-- border -->![Add image](9-add-image.png)

7. Hover over the image to expose the settings at the top right and click the cog icon to edit the image widget.

    <!-- border -->![Change settings](10-image-settings.png)

8. Add the following settings to the **Image** widget and then click **Save**:

    |  :-------------     | :-------------
    | Caption             | `Carla Grant, Sales Manager`
    | Caption Layout      | Inline
    | Caption Alignment   | Left

    <!-- border -->![Image settings](11-image-settings.png)

9. Now in the same way, add a **Text** widget under the image widget and add this text using **14pt** text size and the **Ariel** font: 

    `I'm Carla Grant, sales manager of JobCore Enterprises. We've been selling computer accessories for the last 25 years and we have clients stationed all over the world. Our sales teams are active and are ready to show you our good quality and appealing products to help you do your work efficiently. `


    <!-- border -->![Add text widget](12-add-text-widget.png)

10. Click **Publish** to publish your workpage. You'll get another popup and if you want to publish to your feed, click **Publish** again - this is optional.

    <!-- border -->![Publish workpage](13-publish-workpage.png)

11. Use the breadcrumbs at the top of your screen to go back to the **Menu Content** screen, where you can now see the workpage you created.

    <!-- border -->![View workpage](14-view-workpage.png)

### Add the new workpage to the site menu

In this step we're going to reference the new workpage from the site menu:

1. Click the pencil icon again to open the site menu.

2. Click **+** in the top area of the **Menu Editor**. 

    <!-- border -->![Add menu item](15-add-menu-item1.png)


    You'll get a dropdown list of possible content that you can add. Select **Custom**. 

    <!-- border -->![Content list](16-content-list.png)

3. In the **Add Menu Item** dialog, in the **Name** field, enter `Our Story` and select **Workpage**. Select the `Our Story` workpage you just created. Then click **OK**.

     <!-- border -->![Select workpage](17-select-workpage.png)

4. See that the workpage is now part of the site menu.  Click **Save Draft** and then you can continue adding other menu items.

    <!-- border -->![Save workpage as draft](18-save-site-menu.png)

    >Note the `Our Story` workpage also displays in the left menu panel.

    

### Add workspace to the site menu

In this step, you'll create a workspace that you can reference from the site menu.  
> Note that you can add a new workspace or any existing workspace to the site menu, but for the purpose of this tutorial, you'll create one for our sales department.

1. From the **Workspaces** menu item, select **New Workspace** from the dropdown menu.

    ![Select workpage](19-add-workspace.png)

2. From the **New Workspace** dialog, enter the following values and then click **Create**:

    |  :-------------     | :-------------
    | **Template**                   |No Template
    | **Title**                      |Sales Department News.
    | **Workspace Permissions**       |Public 

    <!-- border -->![Workspace settings](19-workspace-settings.png)

3. Now add a workpage to the workspace. Click **+** in the workspace navigation bar and select **Workpage**.  Enter `Sales Summary` as the **Title** and click **Add**.

    <!-- border -->![Add workpage](19a-add-workpage.png)

4. Publish the workspace.  

    <!-- border -->![Publish workpage](19b-publish-workpage.png)

    You will now add this workspace to the site menu.

5. Open the **Menu Editor** again by hovering over the pencil ion on the right.


6. Click **+** at the top of the menu editor panel and select **Workspace**.

    <!-- border -->![Add menu item](21-add-menu-item2.png)

7. From the **Add Menu Item** dialog, enter a name that you want displayed in the site menu.  Let's call it `Sales Department Workspace`. Then select the `Sales Department News` workspace from the dropdown list and leave **This menu item is clickable** selected, and click **OK**.

    >If a workspace is clickable, when clicking the workspace in the menu the workspace opens. If the workspace isn't clickable, you'll only be able to navigate to its sub-pages.

    <!-- border -->![Add workspace to menu](22-add-workspace-to-menu.png)

8. Click **Save Draft** and see that the workspace has been added to the site menu. Note that if you click the workspace in the menu, it opens and you can see the workpage you added in the workspace navigation bar. If you had removed the clickable option, only the workpage would be clickable.

    <!-- border -->![View menu item in site](23-view-menu.png)

### Add an application to the site menu

>You can select any application that you have permissions to view and access and launch it directly from the site menu.

1. Open the **Menu Editor** and click **+**.

2. Select **Application**.

    <!-- border -->![Add an application](24-add-application.png)

3. Select an application that you want to add to the site menu.  Choose the `Calendar` and click **OK**.

    <!-- border -->![Select an application](25-select-application.png)

4. Publish the menu. 


    >Once you've published the menu, it will look like this. Note that you can launch the app directly from the site menu.

    <!-- border -->![Launched app](26-final-menu.png)


