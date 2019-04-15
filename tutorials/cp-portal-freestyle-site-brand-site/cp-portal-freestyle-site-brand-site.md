---
title: Create a Theme for Your Portal Freestyle Site
description: Create and customize a theme and then import it into your Portal freestyle site.
auto_validation: true
primary_tag: products>sap-cloud-platform-portal
tags: [  tutorial>beginner, topic>cloud, products>sap-cloud-platform-portal ]
time: 15
---


## Details
### You will learn  
  - How to create a theme
  - How to customize the theme
  - How to publish the theme

A default theme, provided by SAP, is assigned to each Portal site. You can create a custom theme in the UI Theme Designer, and then use the Theme Manager to import it so that you can assign this theme to the site, or make it available for user selection.

---

[ACCORDION-BEGIN [Step 1: ](Save the site URL)]

>You will be needing the site URL later on and so we'll start off by finding it and saving it.

1. Navigate to the Site Directory and find the Supplier Portal tile.

2. Hover over the tile.

    ![Find tile](0-find-site.png)

3. Right-click the URL and select **Copy link address**.

    ![Save link](0a-save-link.png)

4. Save this link - you'll be needing it later.


[DONE]

[ACCORDION-END]



[ACCORDION-BEGIN [Step 2: ](Access the UI theme designer tool)]

1. From the side panel of your Supplier Portal site, select **Services and Tools**.

2. Open the UI Theme Designer tool by hovering over the tile and clicking **Configure**.

    ![Open UI Theme Designer](1-launch-theme-designer.png)


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a theme)]

1. Click **Create a New Theme**.

    ![Create new theme](3-create-theme.png)

2. Select SAP Belize as the **Base Theme** and click **Step 2** to proceed.
    ![Base theme](4-base-theme.png)

3. Enter the following values:

    |Field |Value |
    |----|-----|
    |**Theme ID** |**`velotics`** |
    |**Title** |**`velotics`** |

4. Click **Create Theme**.

    ![Theme values](5-theme-values.png)

>A new theme named **`velotics`** is created as a copy of the SAP Belize theme with the custom properties that you entered.


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Customize the theme)]
To design and modify the theme, you need to first define a Target Page that will serve as a canvas upon which you can perform these changes. You will then use the quick theming mode to easily change color scheme, background image, or logo.

1. Enter the **Link to Application**. This is the runtime URL of your site that you copied previously in Step 1.

2. Click **Add** to create the target page.

    ![Add target page](6-target-pages.png)

3. From the panel on the far right, select the paint brush icon to switch to quick theming.

    ![Quick theming](7-quick-theming.png)

4.	Change the company logo by clicking on the right of the **Company Logo** field.

    ![Add logo](7a-add-logo.png)

5.	In the **Assign Image** dialog, open the file system browser.

6.	Select the `logo-velotics.png` image file from our resources. Notice that that image has been added to the list of available images.

    ![Assign image](8-save-image.png)

7.	Click the image to select it.

8.	Click **OK** to apply your changes. Notice that the change has been applied and the icon has changed.

    ![Preview logo](9b-preview-logo.png)

>To apply the new theme to your Portal site, you will need to publish it from the UI Theme Designer and then apply it to your site from the Portal Theme Manager.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Publish the theme)]

1. In the UI Theme Designer top level menu select **Theme** and then **Save & Publish**.

    ![Save and Publish](9-save-and-publish.png)

2. From the **Save & Publish** dialog, verify the theme parameters.

3. Click **Save & Publish** to publish the **`Velotics`** theme.

    ![Click Save and Publish](9a-click-publish.png)

>Publishing the theme might take several moments.

[DONE]


  [ACCORDION-END]

  [ACCORDION-BEGIN [Step 6: ](Apply the theme to your site)]

1. Go back to the Site Designer of your site.

2. From **Services and Tools** launch the **Theme Manager**.

    ![Launch Theme Manager](10-launch-theme-manager.png)

3. Click **OK** on the message.

4. Notice that the **`velotics`** theme that you created and published is available in the site's **Theme Manager**.

    ![Velotics Theme](10a-velotics-theme.png)

5. Select the **`velotics`** theme.

6. Click **Assign to Site**.

    ![Assign to site](10b-assign-to-site.png)

7. Click **Publish** to publish the latest changes.

    ![Publish changes](11-publish-changes.png)

8. Click **Publish** to only apply the changes. Select **Publish and Open** to publish your site and review the latest changes you have made to your site.

    ![Publish and Open](12-publish-and-open.png)

9. View your branded Site

    ![Branded Site](13-branded-site.png)

[VALIDATE_1]


  [ACCORDION-END]
