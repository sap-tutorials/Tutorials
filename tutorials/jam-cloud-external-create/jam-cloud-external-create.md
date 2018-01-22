---
title: Integrate External Applications into SAP Jam Collaboration
description: Demonstrates how to integrate ESPM Web Shop records into SAP Jam Collaboration.
primary_tag: products>sap-jam-collaboration
tags: [  tutorial>intermediate, products>sap-jam, products>sap-cloud-platform, topic>cloud ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorials:** [Register for SAP Cloud Platform and SAP Jam Collaboration developer access](https://www.sap.com/developer/tutorials/jam-cloud-setup.html)

## Next Steps

## Details
### You will learn
In this tutorial you will learn how to integrate ESPM Web Shop records into SAP Jam Collaboration to create a work pattern that enables sales people to collaborate on each item from the [ESPM web shop](https://espmrefapps.hana.ondemand.com/espm-cloud-web/webshop/).

To learn how to integrate SAP Jam Collaboration and your own ESPM instance with Machine Learning go to the [Enterprise Sales Procurement Model (ESPM) Application](https://github.com/SAP/cloud-espm-v2/blob/ML/README.md).

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create an External Application in SAP Jam Collaboration)]

An External Application in SAP Jam Collaboration defines the connection between SAP Jam Collaboration and an external application to securely access the data in that application exposed via its API. Use the following steps to create an External Application in SAP Jam Collaboration:

1.  In SAP Jam Collaboration, click on the **cog settings icon** and select **Admin**. The _Admin_ page displays.
2.  Select **Integrations** \> **External Applications** from the sidebar menu. The _External Applications_ page displays.
3.  Click **Add Application** and select **SAP Cloud Platform** from the dropdown menu.
4.  In the **Name** text box, enter `ESPM`.
5.  Click on the **Select Authentication Type** drop-down menu, and select **Common User**.
6.  Press **Save** to create the External Application.
7.  The _ESPM_ External Application appears in the _External Applications_ list.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Import the ESPM Web Shop Records into SAP Jam Collaboration)]

Record types in SAP Jam Collaboration define how the data from the external application will be displayed within SAP Jam Collaboration. Use the following steps to import the _ESPM Web Shop_ records into SAP Jam Collaboration:

1. Import the _Product_ Records
    a. From the _External Applications_ page, select **Action > Manage Record Types** next to the ESPM application.
    b. Click the **Add Record Type** button.
    c. In the **Name** text box, enter `Products`.
    d. In the **External Type** text box enter
`https://espmrefapps.hana.ondemand.com/espm-cloud-web/espm.svc/$metadata#Products`. This data includes the information on the products included in the ESPM application. It is exposed as OData.
    e. In the **Annotation URL** text box enter
`https://espmrefapps.hana.ondemand.com/espm-cloud-web/webshop/reviews_annotations.xml`. This file is used by SAP JAM Collaboration to render the data. The `annotations.xml` file, together with the available data, exposed via OData, enables you to create an external application in SAP Jam Collaboration that brings in products and product reviews data from the ESPM application into SAP Jam Collaboration. The OData annotations file specifies how each type of data is displayed in a specified UI element.
    f. Click the **Import External Resources** button. A _Success_ page will be displayed. Choose **Done**.
    g. Click the **Create** button.
    h. `Products` appears as an entry in the _Manage ESPM Record Types_ page.
2. Import the _Customer Reviews_ Records
    a. To import the _Customer Reviews_ records repeat _steps b to g_ again, but change the **Name** value in _step c_ to `CustomerReviews` and the **External Type** value in _step d_ to
`https://espmrefapps.hana.ondemand.com/espm-cloud-web/espm.svc/$metadata#CustomerReviews`
    b. `CustomerReviews` appears as an entry in the _Manage ESPM Record Types_ page.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create a new ESPM Reviews group in SAP Jam Collaboration from an ESPM Web Shop Record)]

Groups are a membership of users who can upload, create, or reference material specific to a department, project, or team. Group members can participate in discussions, forums and much more. Use the following steps to create a new _ESPM Reviews Group_ in SAP Jam Collaboration for an _ESPM Web Shop Record_:

1.  Select **Business Records** from the top menu-bar. The _Business Records_ screen will appear.
2.  Select **ESPM** in the _Name_ column.
3.  Select **Products** in the _Type_ column. A list of IDs from _ESPM_ will be displayed.
4.  Copy the ID value next to _Notebook Basic 15_ in the _ID_ column.
5.  Hover over **Notebook Basic 15** in the _Product Name_ column. A quick view panel of the product displays.
6.  Click the **Create Group** button in the quick view panel. The _Create a Group_ dialog displays.
7.  In the **Type a Group Name** text box, enter `ESPM Product Reviews -` and paste the ID value.
8.  Set the Group Permissions by selecting **Public**.
9.  Select the **Activate this group now** checkbox.
10. Click the **Create** button. The new group has now been created.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Create the ESPM Reviews Group Template)]

SAP Jam Collaboration groups act as the collaborative workspaces for work patterns. Group templates organize the content in a way that provides guidance to users on the best way to approach the work required to support a particular repeatable business process. Use the following steps to create the **ESPM Reviews** group template in your SAP Jam Collaboration group:

1.  Select **Overview**. The _Overview_ screen appears.
2.  Click the **Create an Overview Page** button. The _Add New Page_ screen appears.
3.  Click the **OK** button. The _Add an Overview Page_ screen appears.
4.  Enter `Current ESPM Product` in the **Title** field.
5.  Setup the `Current Item` widget:
    1.  Click the **Add Widget** button for the widget in the top middle of the page. The _Add Widget_ dialog appears.
    2.  Select the _suitcase_ icon in the bottom row. The _Business Record Widget_ dialog appears.
    3.  Select **Item Detail** from the **Show** drop-down list.
    4.  Select the **Widget Title** field and enter `Current Item`.
    5.  Click the **OK** button. The item information appears in the widget.
6.  Setup the `Highest Reviews` widget:
    1.  Click the **Add Widget** button for the widget in the lower left side of the page. The _Add Widget_ dialog appears.
    2.  Select the _suitcase_ icon in the bottom row. The _Business Record Widget_ dialog appears.
    3.  Select **Related** from the **Show** drop-down list.
    4.  Select **`CustomerReviews`** from the **Type** drop-down list.
    5.  Select **Rating** from the **Sort by** drop-down list.
    6.  Select the **Widget Title** field and enter `Highest Reviews`.
    7.  Click the **OK** button. The top 3 product reviews appear in the widget.
7.  Setup the `Lowest Reviews` widget:
    1.  Click the **Add Widget** button for the widget in the lower left side of the page. The _Add Widget_ dialog appears.
    2.  Select the _suitcase_ icon in the bottom row. The _Business Record Widget_ dialog appears.
    3.  Select **Related** from the **Show** drop-down list.
    4.  Select **`CustomerReviews`** from the **Type** drop-down list.
    5.  Select **Rating** from the **Sort by** drop-down list.
    6.  Select the **Ascending** radio button.
    7.  Select the **Widget Title** field and enter `Lowest Reviews`.
    8.  Click the **OK** button. The lowest reviews appear in the widget.
8.  Click the **Publish** button.
9.  Click the **Publish** button. Your _group template_ is published into your group.

The _Overview page_ now displays rich information about the product and its reviews.

Start collaborating with other users on this ESPM Review group\! Try creating other sales collaboration groups for other items from the _ESPM web shop_ business records as well.

[DONE]

[ACCORDION-END]

---
