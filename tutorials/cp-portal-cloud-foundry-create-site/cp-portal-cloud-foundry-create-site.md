---
title: Create a Portal Site on SAP Cloud Platform on Cloud Foundry
description: Create a Portal site with a launchpad and other pages that will contain apps and web content.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-platform-portal, topic>cloud, products>sap-fiori]
primary_tag: products>sap-cloud-platform
---

## Prerequisites
You have subscribed to the Portal service.


### You will learn
  - How to create a Portal site
  - How to add a page to your site and make it the home page
  - How to create a site menu

In this group of tutorials our goal is to create an attractive Portal site for a recruiting company we'll call `JobCore Recruiting`. The site will include apps and web content as well as two pages: a launchpad page and another page. Both pages will be displayed in the site menu.

Let's get started!

[ACCORDION-BEGIN [Step 1: ](Create a site)]

When you access the Portal service, the Site Directory is in focus. From here you will create your new Portal site.


1. In the Site Directory, click **Create New Site**.

    ![Create site](5_create_new_site.png)

2. Enter `JobCore Portal` as the site name and click **Create**.

    The Site Editor opens in a new tab.

    >When you create a new site, by default, there is already a **Launchpad** page available for you. You can see the default **Launchpad** page in focus.

    ![View Site Manager](7_view_site_manager.png)

Once you've created your site, you can go back to the Site Directory to see that your new site is now represented by a tile. In future, each time you want to edit your site, you can click the pencil icon from this tile. You can also delete the site from here and you can also preview it to see how it looks at runtime.

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

Now we have a page called `JobCore` and we have the default `Launchpad` page. Let's add them both to the site menu.   

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Design the site menu)]


1. Click the **Menu** icon in the side panel.

    ![Open site menu](10_open_site_menu.png)

2. The **Menu Editor** opens. Click  **Click here to add an item** to open the **Add Menu Item** screen.

    ![Add a menu item](11_add_menu_item.png)

3. Set the following values:


    |  Field Name     | Value
    |  :------------- | :-------------
    |  **Display Name**           | **`Home`**
    |  **Item Type**           | **`Page`**
    |  **Select Page**    | **`JobCore`**

4. Click **Add**.  This takes you back to the **Menu Editor**. You will see that the page you called `Home` has been added to the site's menu.

    ![Menu item details](11a_edit_menu.png)

5. Click **Save**.

    ![Save menu](12_save_menu.png)

6. Now add the `Launchpad` page to the site menu. Hover over the `Home` menu item.

    ![Hover over menu item](13_hover.png)

7. Click the + icon on the right and then click **Add Item** to open the **Add Menu Item** screen.

    ![Add new item](13a_add_new_item.png)

8. Enter the following values:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  **Display Name**           | **`My Apps`**
    |  **Item Type**           | **`Page`**
    |  **Select Page**    | **`Launchpad`**

9. Click **Add**.

    ![Add launchpad to menu](14_add_launchpad_tomenu.png)

8. Click **Save** in the editor.

    ![Save menu settings](15_save_menu_launchpad.png)

>Note that both the `Launchpad` and `Home` pages appear in the site menu.

[VALIDATE_7]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Set the site's home page)]


1. Click the **Pages** icon to go back to the **Pages** panel.

    ![Open Pages panel](17_pages_panel.png)

2. Click the downward facing arrow next to the `JobCore` page, and then click **Set as home page**.

    ![Set as home page](18_set_home_page.png)

    Now you have a Portal site called the `JobCore Portal` with 2 pages. Both pages are part of the site menu and the `JobCore` page (your `Home` page) is set as the first page that opens when a user logs on to your site.

    In the next tutorial you're going to add web content to the `Home` page.

[DONE]
[ACCORDION-END]
