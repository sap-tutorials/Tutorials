---
title: Create a Portal Site
description: Create a Portal site with a launchpad and other pages that will contain apps and web content.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-platform-portal, topic>cloud, products>sap-fiori ]
primary_tag: products>sap-cloud-platform
author_name: Lindsay Bert
---

## Prerequisites
You have subscribed to the Portal service.


### You will learn
  - How to create a Portal site
  - How to add a page to your site and make it the home page
  - How to create a site menu

In this group of tutorials our goal is to create an attractive Portal site for a recruiting company called `JobCore Recruiting`. The site will include apps and web content. The site will contain two pages: a launchpad page and another page. Both pages will be displayed in the site menu.

Let's get started!

[ACCORDION-BEGIN [Step 1: ](Create a site)]

When you access the Portal service, the Site Directory is in focus. From here you will create your new Portal site.


1. In the Site Directory, click **Create New Site**.

    ![Create site](5_create_new_site.png)

2. Enter `JobCore Portal` as the site name and click **Create**.

    ![Name site](6_name_site.png)

    The Site Editor opens with the **Site Settings** screen in focus.

3. Click the **Pages** tab.

    ![Open Pages](7a_open_pages_tab.png)

In your new site, you can already see the default **Launchpad** page in the **Pages** panel on the left.
> Note that the page is marked with an eye icon with a cross. This icon indicates that the page is not part of the site menu and is not yet visible in your site.

In the next step, you are going to add a second page to your Portal site.

  ![View Site Manager](7_view_launchpad_page.png)

>For your information: Once you've created your site, you can go back to the Site Directory. There you can see that your new site is now represented by a new tile. In the future, each time you want to edit your site, you can click the pencil icon from this tile. You can also delete the site from here or preview it to see how your site users will see your site at runtime.

![View Site Tile](8_view_site_tile.png)


[VALIDATE_6]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Add a page to the site)]


1. Click **+** at the bottom of the  **Pages** panel of the Site Manager.

    ![Add a page](8a_add_page.png)

2. Select the **Anchor Navigation** page template.

3. Enter `JobCore` as the **Page Name**.

4. Click **Add**.

    ![Create a page](9_create_freestyle_page.png)

Now we have a page called `JobCore` and we have the default `Launchpad` page that you can see in the **Pages** panel. Let's add them both to the site menu.   

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Design the site menu)]


1. Click the **Menu** tab to open the **Site Menu** editor.

    ![Open site menu](10_open_site_menu.png)

2. Click  **Click here to add an item** to open the **Add Menu Item** screen.

    ![Add a menu item](11_add_menu_item.png)

3. Enter the following values:

    |  :------------- | :-------------
    |  **Display Name** | **`Home`**
    |  **Item Type**    | **`Page`**
    |  **Select Page**  | **`JobCore`**

4. Click **Add**.  

    ![Menu item details](11a_edit_menu.png)

    This takes you back to the **Menu Editor**. You'll see that the page you called `Home` has been added to the site's menu structure.

5. Now add the `Launchpad` page to the site menu.

    Hover over the `Home` menu item and click the + icon on the right.

    ![Hover over menu item](13_hover.png)

6. Click **Add Item** to open the **Add Menu Item** screen.

    ![Add new item](13a_add_new_item.png)

7. Enter the following values:


    |  :------------- | :-------------
    |  **Display Name**           | **`My Apps`**
    |  **Item Type**           | **`Page`**
    |  **Select Page**    | **`Launchpad`**

8. Click **Add**.

    ![Add launchpad to menu](14_add_launchpad_tomenu.png)

9. Click **Save** in the editor.

    ![Save menu settings](15_save_menu_launchpad.png)

>Note that both the `Launchpad` and `Home` pages appear in the site menu structure.

[VALIDATE_7]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Set the site's home page)]


1. Click the **Pages** tab to go back to the page editor.

    ![Open Pages panel](17_pages_panel.png)

2. Click the downward facing arrow next to the `JobCore` page, and then click **Set as home page**.

    ![Set as home page](18_set_home_page.png)

Now you have a Portal site called the `JobCore Portal` with 2 pages. Both pages are part of the site menu and the `JobCore` page (your `Home` page) is set as the first page that opens when a user logs on to your site.

In the next tutorial you're going to add web content to the `Home` page.

[DONE]
[ACCORDION-END]

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=cp-portal-cloud-foundry-create-site" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
