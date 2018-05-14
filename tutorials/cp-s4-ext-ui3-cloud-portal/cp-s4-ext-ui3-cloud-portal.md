---
title: Create SAP Fiori launchpad on SAP Cloud Platform
description: SAP S/4HANA on-premises UI extension (Part 3) - Create SAP Fiori launchpad that will work as central access point for further extension applications to be developed later.
primary_tag: topic>cloud
tags: [ tutorial>intermediate, products>sap-s-4hana, products>sap-cloud-platform ]
---

## Prerequisites
- **Proficiency:** Intermediate
- **Tutorials:** [Expose OData services to SAP Cloud Platform](https://www.sap.com/developer/tutorials/cp-s4-ext-ui2-cloud-connector.html)
- **Systems, Tools, Services:**
    - SAP Web IDE
    - SAP CP cockpit
    - SAP CP Portal Service

## Next Steps
    -  [Extend an SAPUI5 application](https://www.sap.com/developer/tutorials/cp-s4-ext-ui4-build-ui.html)

## Details
### You will learn
You will create a SAP Fiori Launchpad on SAP Cloud Platform, which provides access to an application you will develop later. Acting as SAP Cloud Platform administrator, you first prepare the Portal Service inside your SAP Cloud Platform trial account and then add a first test application (SAP Web IDE) to a new launchpad with two groups, Product Management and Equipment Tracking.

### Time to Complete
**30 Mins**

---

[ACCORDION-BEGIN [Step 1:](Enable Portal Service in trial account)]

In SAP S/4HANA, the Fiori Launchpad is your central entry point for all sorts of SAP Fiori apps. You may operate a launchpad on-premises, or you might make use of the SAP Cloud Platform Portal service, which allows you to host the launchpad in your SAP Cloud Platform account. The Launchpad offers features such as navigation, personalization, and configuration options. The apps are displayed as various tiles and groups.

Administrators must first set up their SAP Cloud Platform account prior to working in SAP Fiori launchpad. In this step you will set up the Portal Service in your SAP CP Trial Account.

1.  In Google Chrome browser open the SAP CP Trial cockpit page in a new tab.

2.  Open the **Services** pane and enter query string **portal** in the search field. Click the **Portal Service** tile to enter the admin page.

    ![SAP CP Cockpit 1](./images/w2-u4-s1/pic01-hcpcockpit-find-portalservice.png)

3.  Press the **Enable** button to provide a Portal to your SAP CP trial account.

    ![SAP CP Cockpit 2](./images/w2-u4-s1/pic02-hcpcockpit-enable-portalservice.png)

    After few seconds a green icon indicates the enabled SAP CP Portal service.

    ![SAP CP Cockpit 3](./images/w2-u4-s1/pic03-hcpcockpit-portalservice-enabled.png)

4.  Click the link **Go to Service**.

    ![SAP CP Cockpit 4](./images/w2-u4-s1/pic04-hcpcockpit-portalservice-goto.png)

> **Result:**  You entered the **SAP HANA Cloud Portal Site Directory**. This is where you will create your new launchpad site.
>
> ![FLP Config](./images/w2-u4-s1/pic05-fiori-launchpad-config-cockpit.png)

[ACCORDION-END]




[ACCORDION-BEGIN [Step 2:](Create new launchpad site)]

Before end users can work in an SAP Fiori launchpad, you as administrator must first set it up by configuring a new launchpad site with content such as app tiles, tile groups, catalogs, and roles that act as the building blocks of a launchpad.

To create a new `launchpad site` in the Site Directory of SAP HANA Cloud Portal apply the following steps:

1.  Choose **Create New Site**.

    ![Portal Service 1](./images/w2-u4-s2/pic01-portalservice-create-new-site.png)

2.  Choose **SAP Fiori Launchpad** as template and enter **Procurement Launchpad** in input field `Site Name`.

    ![Portal Service 2](./images/w2-u4-s2/pic02-portalservice-create-site-form.png)

3.  Choose **Create**.

    ![Portal Service 3](./images/w2-u4-s2/pic02-portalservice-create-site-form-create.png)

Another browser tab gets opened with the `Fiori Configuration Cockpit` for the new launchpad site **Procurement Launchpad**.

![Portal Service 4](./images/w2-u4-s2/pic03-portalservice-procurement-launchpad-fcctab.png)

> **Result:**  You added a new launchpad site with name **Procurement Launchpad** to the Site Directory of SAP HANA Cloud Portal. In the browser tab where you added the new launchpad site it gets displayed as a new tile.
>
> ![Portal Service 5](./images/w2-u4-s2/pic03-portalservice-procurement-launchpad.png)
>
> **Related Resources:** For more details see SAP Cloud Platform Documentation:
>
> -   [SAP Fiori Launchpad on Cloud](https://help.hana.ondemand.com/cloud_portal_flp/frameset.htm)
> -   [SAP Fiori Launchpad Sites](https://help.hana.ondemand.com/cloud_portal/frameset.htm?69000b4a09b54f33bef1b58a1dbb4001.html  )

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3:](Configure the launchpad site)]

To configure the new launchpad site `Procurement Launchpad` You now enter the **SAP Fiori launchpad configuration cockpit**, a browser-based tool used by administrators to create new and maintain existing content for SAP Fiori launchpads.

1.  In `Site Directory` click **Edit** at the bottom right of the `Procurement Launchpad` tile or select the browser tab `Welcome to SAP Fiori Configuration Cockpit` that was opened in the previous step.

    ![Portal Service 6](./images/w2-u4-s2/pic04-portalservice-procurement-launchpad-edit.png)

    In the `Fiori Configuration Cockpit` you will see a navigation menu on the left hand side. From here you can access the various tools, editors, and services provided by the configuration cockpit.

    ![FCC 1](./images/w2-u4-s2/pic05-fcc-dashboard.png)

2.  To configure the launchpad catalog open menu item **Content Management** | **Catalogs** or click the **Catalogs** tile.

    ![FCC 2](./images/w2-u4-s2/pic06-fcc-edit-catalog.png)

3.  To edit the `Sample Catalog` click the **Edit** button in the footer toolbar.

    ![FCC 3](./images/w2-u4-s2/pic07-fcc-edit-catalog-form.png)

4.  In the `Properties` tab change name and description to **Procurement Applications**.

    ![FCC 4](./images/w2-u4-s2/pic08-fcc-edit-sample-catalog-prop.png)

5.  Choose `Roles` tab and assign role **Everyone**.

    > **Hint:** In case the **Roles** tab does not open via mouse-click press the **Return** keyboard button instead.

6.  Click **Save**.

    ![FCC 5](./images/w2-u4-s2/pic09-fcc-edit-sample-catalog-role.png)

7.  Configure the launchpad groups:

8.  Go to `Groups` and click on **Edit**.

    ![FCC 6](./images/w2-u4-s2/pic10-fcc-edit-catalog-groups.png)

9.  Edit the `Sample Group`:
    -   Rename it to **Product Management**.
    -   Click **Save**.

    ![FCC 7](./images/w2-u4-s2/pic11-fcc-edit-catalog-group-rename.png)

10. Create a new group named `Equipment Tracking` with the **+** button in the footer toolbar.

    ![FCC 8](./images/w2-u4-s2/pic12-fcc-edit-catalog-new-group.png)

    > **Result:**  You configured the `Procurement` launchpad site to contain two groups named `Product Management` and `Equipment Tracking`:
    >
    > ![FCC 9](./images/w2-u4-s2/pic13-fcc-edit-catalog-two-groups.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4:](Add application to launchpad site)]

Add SAP Web IDE as first application tile inside the `Procurement` launchpad.

1.  In the left navigation sidebar expand **Content Management** and go to **Apps**.

2.  Click **+** button in the footer toolbar to create a new app.

    ![FCC 10](./images/w2-u4-s2/pic14-fcc-edit-add-app.png)

3.  In the `Properties` form enter the following values:
    -   Enter **General** | **App Title**: **`Demo Application`**.
    -   Under **App Resources Details** choose **App Type** menu item **URL**.
    -   Copy & paste the URL of your SAP Web IDE browser bookmark: `https://webide-p1942XXX.dispatcher.hanatrial.ondemand.com/` (replace placeholder **`p142XXX`** with your SAP Cloud Platform trial account user).
    -   Assign Catalogs: **`Procurement Applications`**
    -   Assign Groups: **`Product Management`**.
    -   Click **Save**.

    ![FCC 11](./images/w2-u4-s2/pic15-fcc-edit-add-app-webide.png)

4.  Preview the new launchpad:

5.  Click on the **Site Preview** icon from the black toolbar bar in the top right corner.

      ![FCC 12](./images/w2-u4-s2/pic16-fcc-site-preview.png)

6.  Verify that your `Demo Application` tile correctly works by navigating to the SAP Web IDE that starts in a new browser tab.

      ![FCC 13](./images/w2-u4-s2/pic18-fcc-site-preview-sapwebide.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5:](Publish the launchpad site)]

In this page you will publish the configured SAP Fiori Launchpad in your SAP Cloud Platform trial account.

1.  Click **Home** to view the existing parts of the new site:
    - 1 Application
    - 1 Catalog
    - 2 Groups
    - 2 Roles

    ![FCC 14](./images/w2-u4-s3/pic01-fcc-site-home.png)

2.  Go to **Settings**.

3.  In the header toolbar click the **globe** icon to publish the site.

4.  Confirm the popup dialog and click **Publish and Open**.

    ![FCC 15](./images/w2-u4-s3/pic02-fcc-site-publish.png)

5.  Add a browser bookmark to the new site with name **SAP CP FLP**.

    ![FCC 16](./images/w2-u4-s3/pic03-fcc-site-bookmark.png)

> **Result of Step 5:** You have successfully published your configured SAP Fiori Launchpad in your SAP Cloud Platform trial account.

[ACCORDION-END]

You have now successfully completed this tutorial. In the next tutorial you will learn how to import a SAPUI5 application from your SAP NetWeaver backend, register the application to the SAP Fiori Launchpad, and then extend the application by implementing sorting and filtering capabilities: [Extend an SAPUI5 application](https://www.sap.com/developer/tutorials/cp-s4-ext-ui4-build-ui.html)
