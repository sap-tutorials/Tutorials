---
title: Integrate External Applications into SAP Jam Collaboration
description: Demonstrates how to integrate ESPM Web Shop records into SAP Jam Collaboration.
primary_tag: products>sap-jam-collaboration
tags: [  tutorial>intermediate, products>sap-jam, products>sap-cloud-platform, topic>cloud ]
---

## Prerequisites  
 - **Proficiency:** Intermediate

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
### You will learn
In this tutorial you will learn how to integrate ESPM Web Shop records into SAP Jam Collaboration to create a work pattern that enables sales people to collaborate on each item from the [ESPM web shop](https://espmrefapps.hana.ondemand.com/espm-cloud-web/webshop/).

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create an External Application in SAP Jam Collaboration)]

An External Application in SAP Jam Collaboration defines the connection between SAP Jam Collaboration and an external application to access the data in that application exposed via its API. Use the following steps to create an External Application in SAP Jam Collaboration:

1.  In SAP Jam Collaboration, click on the **cog settings icon** and select **Admin**. The _Admin_ page displays.
2.  Select **Integrations** \> **External Applications** from the sidebar menu. The _External Applications_ page displays.
3.  Click **Add Application** and select **SAP Cloud Platform** from the dropdown menu.
4.  In the **Name** text box, enter `ESPM`.
5.  Click on the **Select Authentication Type** drop-down menu, and select **Common User**.
6.  Press **Save** to create the External Application.
7.  You will see the _ESPM_ External Application you created in the _External Applications_ list.


[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Import the ESPM Web Shop Records into SAP Jam Collaboration)]

Record types in SAP Jam Collaboration define how the data from the external application will be displayed within SAP Jam Collaboration. Use the following steps to import the _ESPM Web Shop_ records into SAP Jam Collaboration:

1.  Click on the **Action** drop-down menu for the _ESPM_ External Application created in the previous steps and select **Manage Record Types**.
2.  Download the External Application configuration file from the following URL:
[https://sapjamsamplecode.github.io/Import\_ExtApps/webshop.json](https://sapjamsamplecode.github.io/Import_ExtApps/webshop.json)
3.  Click the **Import From File** button.
4.  Click the **Browse...** button.
5.  Navigate to the External Application configuration file.
6.  Click the **Open** button.
7.  Click the **Import From File** button. _All default record types imported successfully_ displays.
8.  Click **Back to Manage ESPM Record Types**.
9.  The _Products_ and _Customer Reviews_ record types displays on the _Manage ESPM Record Types_ page.


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Import the ESPM Reviews Group Template into SAP Jam Collaboration)]

SAP Jam Collaboration groups act as the collaborative workspaces for work patterns. Group templates organize the content in a way that provides guidance to users on the best way to approach the work required to support a particular repeatable business process. Use the following steps to import the **ESPM Reviews** group template into SAP Jam Collaboration:

1.  On the Admin page select **Product Setup** \> **Group Templates** from the sidebar menu. The **Group Templates** page displays.
2.  Download the group template zip file from the following URL:
[https://sapjamsamplecode.github.io/GroupTemplates/ESPM\_Reviews-Products.zip](https://sapjamsamplecode.github.io/GroupTemplates/ESPM_Reviews-Products.zip)
3.  Click the **Import a template** button.
4.  Navigate to the group template zip file.
5.  Click the **Open** button.
6.  Click the **Import** button. The _Import a template_  dialog displays once the template has been imported.
7.  Click the **OK** button.
8.  Refresh the web page and you will see a new group template titled _ESPM Reviews - Products_. If you do not see the new group template, wait 30 seconds and try again.
9.  Click the **Slider** button next to this group template and set it to **Enabled** (shown as a blue check mark). This group template is now active.


[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Create a new ESPM Reviews group in SAP Jam Collaboration from an ESPM Web Shop Record)]

Groups are a membership of users who can upload, create, or reference material specific to a department, project, or team. Group members can participate in discussions, forums and much more. Use the following steps to create a new _ESPM Reviews Group_ in SAP Jam Collaboration for an _ESPM Web Shop Record_:

1.  Select **Business Records** from the top menu-bar. The _Business Records_ screen will appear.
2.  Select **ESPM** in the _Name_ column.
3.  Select **Products** in the _Type_ column. A list of IDs from _ESPM_ will be displayed.
4.  Copy the ID value next to _Notebook Basic 15_ in the _ID_ column.
5.  Hover over **Notebook Basic 15** in the _Product Name_ column. A quick view panel of the product displays.
6.  Click the **Create Group** button in the quick view panel. The _Create a Group_ dialog displays.
7.  Click the **No Template** drop-down list and select the group template **ESPM Reviews - Products**.
8.  In the **Type a Group Name** text box, enter `ESPM Product Reviews -` and paste the ID value.
9.  Set the Group Permissions by selecting **Public**.
10. Select the **Activate this group now** checkbox.
11. Click the **Create** button. The new group has now been created.


[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Reconfigure the ESPM Reviews Group Widgets)]

Group templates lose some of the their widget configuration information after being imported into SAP Jam Collaboration. Use the following steps to reconfigure the widgets in the ESPM Reviews SAP Jam Collaboration Group:

1.  Select **Overview**. The _Overview_ screen displays.
2.  Select **Edit**.
3.  Configure the _Customer Reviews_ widget:
    1.  Hover the mouse cursor over the top right corner of the **Customer Reviews** widget. The _Edit_ and _Remove_ menu displays.
    2.  Select **Edit**.
    3.  Select **ESPM:Customer Reviews** from the **Update Widget** drop-down list.
    4.  Select **Related** from the **Show** drop-down list.
    5.  Click the **OK** button. The _Customer Reviews_ widget information displays.
4.  Configure the _Top Rated Customer Reviews_ widget:
    1.  Hover the mouse cursor over the top right corner of the **Top Rated Customer Reviews** widget. The _Edit_ and _Remove_ menu displays.
    2.  Select **Edit**.
    3.  Select **ESPM:Customer Reviews** from the **Update Widget** drop-down list.
    4.  Select **Related** from the **Show** drop-down list.
    5.  Select **Top Rated Reviews** from the **Filter by** drop-down list.
    6.  Select **Rating** from the **Sort by** drop-down list.
    7.  Select the **Descending** radio button.
    8.  Click the **OK** button. The _Top Rated Customer Reviews_ widget information displays.
5.  Configure the _Bottom Rated Customer Reviews_ widget:
    1.  Hover the mouse cursor over the top right corner of the **Bottom Rated Customer Reviews** widget. The _Edit_ and _Remove_ menu appears.
    2.  Select **Edit**.
    3.  Select **ESPM:Customer Reviews** from the **Update Widget** drop-down list.
    4.  Select **Related** from the **Show** drop-down list.
    5.  Select **Bottom Rated Reviews** from the **Filter by** drop-down list.
    6.  Select **Rating** from the **Sort by** drop-down list.
    7.  Select the **Ascending** radio button.
    8.  Click the **OK** button. The _Bottom Rated Customer Reviews_ widget information displays.
6.  Click the **Publish** button.
7.  Click the **Publish** button.

The _Overview page_ now displays rich information about the product and its reviews.

Start collaborating with other users on this ESPM Review group\! Try creating other sales collaboration groups for other items from the _ESPM web shop_ business records as well.


[ACCORDION-END]

---

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
