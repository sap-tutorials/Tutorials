---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, software-product>sap-build-work-zone--advanced-edition]
primary_tag: software-product>sap-build-work-zone--advanced-edition
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---

# Add a Workspace to SAP Build Work Zone, advanced edition
<!-- description --> Create a workspace and add content to it that you can share with other members of your workspace.

## Prerequisites
 - You have an environment set up and you can access your SAP Build Work Zone, advanced edition.
 - You have created an app in SAP Build Apps and a process in SAP Process Automation by following this mission: [Create Sales Order Workflow with SAP Build](https://developers.sap.com/mission.sap-build-apps-create-trigger-process.html)
&nbsp;
>**SAP Build Work Zone, advanced edition is not available in a trial account.**


## You will learn
  - How to create a new workspace
  - How to add content to it and design the layout

## Intro
In this tutorial, you're going to create a workspace where employees who are part of the sales team can interact with each other, ask questions, and find information, tools, and assets to help them close more deals.


### Download images for your workspace


Before you start, download these image files so that they're on your computer ready for you to add to your workpage for the workspace that you will create.

- [`workspace_row2_image1.jpg`](Workspace_Images/workspace_row2_image1.jpg)
- [`workspace_row2_image2.jpg`](Workspace_Images/workspace_row2_image2.jpg)
- [`workspace_header_sales.jpg`](Workspace_Images/workspace_header-sales.jpg)
- [`workspace_image_with_icon1.png`](Workspace_Images/workspace_image_with_icon1.png)
- [`workspace_image_with_icon2.png`](Workspace_Images/workspace_image_with_icon2.png)
- [`workspace_image_with_icon3.png`](Workspace_Images/workspace_image_with_icon3.png)
- [`workspace_image_with_icon4.png`](Workspace_Images/workspace_image_with_icon4.png)



### Create a workspace


Workspaces are the building blocks of SAP Build Work Zone, advanced edition. You can add all kinds of content to it and you can invite other users to join as members.

1. In your site, click **Workspaces** in the top-level menu, and then click **New Workspace**.

    <!-- border -->![Create workspace](1-create-new-workspace.png)

2. Select **No Template** from the dropdown list of available templates.

    <!-- border -->![Select template](2-select-template.png)

    >You can also select one of SAP's out-of-the-box templates. In this case, you'll get a professionally designed workpage with widgets including everything you need to get started quickly. You can then customize the content to suit your needs. For this tutorial you won't use a template.

3. Enter this name for your workspace: `Sales Management`. In workshops, use: `<your unique identifier>_Sales Management`.

    >**Tip:** Always use a name that would help users distinguish your workspace from others.


4. Copy and paste this description for your workspace: `Use this workspace to manage our team sales orders and collaborate!`

5. Select `Private`.

    >Selecting private makes your workspace available to a specific set of members that you've invited to the workspace. If you later want to make it public so that everyone in the company can see it, click **Edit Workspace Settings** at the top of your screen and change the setting to `Public`.

6. Click **Create**.

    <!-- border -->![New workspace settings](3-new-workspace-settings.png)

Your workspace is created.



### Configure your workspace notifications


When you create your workspace, you may be prompted to define the frequency of email notifications.

1. Select **Daily** to get any updates to the workspace every day.

2. Click **Accept** to ensure that you've joined the workspace.

  <!-- border -->![Define workspace settings](3a-created-workspace.png)

You can now start building your workspace.

<!-- border -->![Start building a workspace](3c-start-building.png)

Now, let's add content to the workspace.


### Change the image in the workspace header


When you create a new workspace, the header has a dark and light blue background. You can replace this background with your own.

1. Hover over the background in the workspace header to expose the  **Edit cover photo** button and select **Upload Photo**.

    <!-- border -->![New cover photo](4-new-header-image.png)

2. Browse for the following image that you downloaded: `workspace-header-sales`.

3. Reposition your photo to display it as you want, and then click **Save**.

    <!-- border -->![Save cover image](5-save-cover-image.png)





### Create a workpage for your workspace


In this step, you'll add a workpage to your workspace - this workpage will describe the purpose of the workspace and will feature relevant content.

1. Click **+** to add a page tab.

    <!-- border -->![Add page tab](6-add-page-tab.png)

2. From the **Add New Page** screen, select the **Workpage** tile.

    <!-- border -->![Add overview page](7-select-overview-page.png)

3. Select the blank layout at the bottom right of the screen and click **OK**. The Page Designer is now active.

    <!-- border -->![Select layout](8-select-layout.png)


5. Enter a title for the workpage: `Sales Orders`.

    <!-- border -->![Title of overview page](9-name-overview-page.png)




### Design the first section of your workpage


In this step, you'll add two text widgets next to each other in the first section of the workpage.

1. Click **Add Widget** in the first section to open the widget gallery.

    <!-- border -->![Add widget](10-add-widget1.png)

2. Select the **Text** widget.

    <!-- border -->![Add text widget](11-select-text-widget.png)

3. Design the **Text** widget as follows:

    |  :------------- | :-------------
    | Text            | Type in the following: `Verify your sales orders with our sales managers`.
    | Font            | Select the text and change to Arial 14pt.
    | Color           | With the text already selected, choose black from the chart.

    <!-- border -->![Design text widget](12-design-text-widget.png)

4. On the right side of the widget that you've just added, click the  **+** to add another column.

    <!-- border -->![Add second widget](13-add-second-widget.png)

5. Click **Add Widget** in the second column and select the **Text** widget.

6. Design the **Text** widget as follows:

    |  :------------- | :-------------
    | Text            | Type in the following: `More Information`
    | Font            | Select the text and change to Arial 14pt.
    | Color           | With the text already selected, choose black from the chart.



### Design the second section of your workpage


In this step, you'll add images to the second section.

1. Click the **+** under the first section to add a new section.

    <!-- border -->![Add a second row](14-add-second-row.png)

2. In the second section, click **Add Widget**.

    <!-- border -->![Add widget to row 2](15-add-widget-row2.png)

3. Select the **Image** widget.

    <!-- border -->![Add image widget](16-add-image-widget.png)

4. Click the link in the widget to browse for the image that you saved on your computer: `workspace_row2_image 1`

    ![Empty image widget](16a-empty-image-widget.png)

5. Hover over the image to expose the settings at the top right of the image and click the cog icon to edit the image widget.

    <!-- border -->![Open image settings](17-open-settings.png)

6. Design the **Image** widget as follows:

    |  :-------------     | :-------------
    | Caption             | `Carla Grant, Sales Manager`
    | Caption Layout      | Inline
    | Caption Alignment   | Left

7. Click **Save**.

    <!-- border -->![Define widget settings](18-define-widget-settings.png)

8. On the right side of this widget, click the **+** to add a new column.

    <!-- border -->![Add column](19-add-column.png)

9. Click **Add Widget** in the new column, select the **Image** widget, and then click the link in the widget to add this image that you saved on your computer: `workspace_row2_image2`.

10. On the top right of the widget that you've just added, click the cog icon to edit your image widget.

    <!-- border -->![Open settings for second image widget](20-define-settings-image2.png)

11.  Design the second **Image** widget as follows and then click **Save**.

    |  :-------------     | :-------------
    | Caption             | `Michael Hill, Sales Fulfillment Manager `
    | Caption Layout      | Inline
    | Caption Alignment   | Left

12. Click **+** next to the second image widget to open a third column.

    <!-- border -->![Add another column](21-add-another-column.png)

13. Add the following 4 image widgets one under each other in the third column. Keep clicking **Add Widget** and select an image each time until you've added all these images:

    |  :-------------     | :-------------
    | First image         | `workspace_image_with_icon1`
    | Second image        | `workspace_image_with_icon2`
    | Third image         | `workspace_image_with_icon3`
    | Fourth image        |`workspace_image_with_icon4`

    <!-- border -->![Add widget 4 times](21a-add-widgetx4.png)


14. Click the cog icon at the top right of the first image widget and design as follows. When you're done, click **Save**.

    |  :-------------     | :-------------                
    | Caption             |`Sales Invoices`
    | Caption Layout      |Overlay
    | Background Opacity  |0%
    | Caption Alignment   |Left

15. In the same way, design the other 3 images as follows. When you're done click **Save** each time:

    |  :-------------     | :-------------                
    | Caption             | Second image: `Product Documentation`<div>&nbsp;</div><div>Third image: `Consider Your Sales Pitch`<div>&nbsp;</div><div>Fourth image: `Stay Involved`
    | Link To        |Add this link to the **second** image widget that you named `Product Documentation`: <https://help.sap.com/viewer/product/WZ/Cloud/en-US> <div>&nbsp;</div><div> Once you've published your site you can click on this image to open the SAP Work Zone documentation product page.
    | Caption Layout        |All images: Overlay
    | Background Opacity    |All images: 0%
    | Caption Alignment     |All images: Left


16. Use the dividers between the 3 columns to align all the images. Also make sure to adjust the `More Information` heading in line with the thrid column.

    <!-- border -->![First preview](22-first-preview.png)

17. To increase the spacing between this section and the first section, hover over the section to expose the settings on the right, and click the cog icon to edit the spacing.

    ![Edit section settings](22a-edit-row-settings.png)

18. Under **Padding Top**, add the value `30` pixels and click **Save**.

    <!-- border -->![Edit row pixels](22b-edit-pixels.png)


### Design the third section of your workspace


In this step, you'll add an **Action** widget, a **Forum** widget, and a **Feed** widget.

1. Click **+** at the bottom of the second section to add another section.

    <!-- border -->![Add the third row](22a-add-third-row.png)

2. Click **Add Widget**.

3. Select the **Action** widget.

    >Adding an **Action** widget enables workspace members to choose from a selection of commonly used action types. In this tutorial, we want the workspace members to ask questions or add ideas.

    <!-- border -->![Add an action widget](23-add-action-widget.png)

4. Enter the **Widget Title**: `Add Your Questions & Ideas`.

5. Uncheck all options except for **Ask a Question** and **Add an Idea**. Click **Save** to add the widget to your workpage.

    <!-- border -->![Design actions widget](24-design-actions-widget.png)

    Now let's create the **Forum** widget.

6. Click the **+** next to the widget you just created to add another column.

    <!-- border -->![Add another column](25-add-another-column.png)

7. Click **Add Widget**.

    <!-- border -->![Add a widget to 2nd column](25a-add-widget.png)

8. Select the **Forum** widget.

    > Adding a **Forum** widget enables a workspace member to view questions, ideas, and discussions submitted by their workspace members.

    <!-- border -->![Add a forum widget](26-add-forum-widget.png)

9. Under **Maximum Items to Display**, move the slider to number **2**, change the title to `What would you like to know?`, and click **Save**.

    > On the workpage, only 2 of the questions, ideas or discussions from the forum will be displayed.  

    <!-- border -->![Design the forum widget](27-design-forum-widget.png)


    Now let's add the **Feed** widget.

10.  Add another section.

    <!-- border -->![Add another section](27-add-another-widget.png)

11. Click **Add Widget**

      <!-- border -->![Add another section](27a-add-widget.png)

12. Select the **Feed** widget and keep the default setting to show all events in the feed.  Click **Save**.

    You've now added all three widgets.

13. To increase the spacing between this section and the section above, hover over the section to expose the settings on the right, and click the cog icon to edit the spacing.

    <!-- border -->![Edit row settings](28-edit-row-settings.png)

14. Under **Padding Top**, add the value `30`pixels and click **Save**.

15. Now **Publish** the workpage to make it visible to members of the workspace. Without doing this, the workpage is only visible to you.

    <!-- border -->![Publish](28-publish.png)

16. In the popup, leave the **Show in Feed Updates** selected and click **Publish**.

    <!-- border -->![Publish again](28a-publish-again.png)

    > By leaving the **Show in Feed Updates** checked, all workspace members will be informed of your changes. In the future, if the changes you make to your workspace aren't significant, you can uncheck this option.


### Add a forums tab and ask a question


Apart from the **Forum** widget, you're going to add a dedicated **Forum** page tab. From this page, workspace members can see all the questions, ideas and discussions in the forums that have been created for the workspace.

1. Click the **+** icon.

    <!-- border -->![Add new page tab](28-add-new-page.png)

2.  In the **Add New Page** screen, under the **Content** category, select **Forums** to add the **Forums**  page tab to your workspace. The forum page tab opens a screen displaying all the different forum topics.

    In the next steps, we ask a question in the forum.

3. Go back to your workpage by clicking the **Sales Orders** workpage.

    <!-- border -->![Go back to overview page](29-go-to-overview-page.png)

4. Scroll down to your **Actions** widget that you added previously and click **Ask a question**.

    <!-- border -->![Add new question](35-add-a-question.png)

2. Add this question: `What is our sales forecast for the coming year?` Then click **Publish** so that members will be able to see your question. You'll get a confirmation message that your question was created.

    <!-- border -->![Add first questions](35-first-question.png)

3. Go back to your workspace by using the breadcrumbs at the top.

    <!-- border -->![Go back to Forums page](35a-go-back.png)

You can see that your question has been added to your workpage as well as to your feed.

<!-- border -->![View question](36-view-questions.png)




### View your workspace


This is how your workspace looks:

<!-- border -->![Final workspace](37-final-workspace.png)

Closing
