---
title: Integrate an UI5 Integration Card to Your SAP Work Zone Home Page
description: Add a custom UI5 integration card to your SAP Work Zone home page.
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product>sap-work-zone, topic>sapui5]
primary_tag: software-product>sap-work-zone
author_name: Lindsay Bert
---

## Prerequisites
 - You have deployed a UI5 integration card to SAP Work Zone.

## Details
### You will learn
  -  How to make a deployed UI5 integration card available in SAP Work Zone and add it to the SAP Work Zone home page or workspace.

You've already deployed a UI5 integration card. Now in this tutorial, you'll enable it to make it available in SAP Work Zone and later add it to your workspace.


---



[ACCORDION-BEGIN [Step 1: ](Enable your deployed UI5 integration card )]

1. Access the **Administration Console** from the user actions dropdown menu under your avatar.

    !![Access the admin console](1-access-admin-console.png)

2. Go to the **UI Integration** section, expand it, and click **Cards**.  

    !![Open Cards section](2-open-card-section.png)

3. Scroll down to view the **Uploaded Cards** section.

    >Note: if you don't see your deployed card, you may need to refresh the browser.

4. Click the slider button to enable the card and make it available for users who can then add it to Work Zone pages.

    !![Enable card](3-enable-card.png)

The card is now enabled and can be added to SAP Work Zone pages.

>Note: you can click on **Configure** to select a different destination for this UI5 integration card. As we used the same ES5 destination in this SAP BTP subaccount and also when we developed the card, this step is not required to complete this tutorial.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add your UI5 integration card to your SAP Work Zone workspace)]

1. Click the **Workspaces** menu and select `Employee Innovation Hackathon` to navigate to your workspace.

    !![Access workspace](4-select-workspace.png)


2. Click the **Expand page settings** wand on the right of the page to expose various icons and then click the pencil icon directly under the wand to open the Page Designer.

    !![Open in edit mode](5-open-page-designer.png)

3. Click the **+** below the **Forum** widget and above the **Feed** widget to add a new row to your workspace.

    !![Add new row](6-add-new-row.png)

4. In the added row, click **Add Widget**.

    !![Add widget](7-add-widget.png)

5. Under **Cards**, select the `Products by Vendor Card` widget that you previously created.

    !![Add the card](8-select-card.png)

6. Click **Add**.

    !![Add card](9-add-card.png)

The card you created was added to your workspace. Let's add a title to it.

>Note: you can also add UI integration cards to a SAP Work Zone Home page.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Finalize the design of your workspace)]

1. Click **Add Widget** below the card widget you just added.

    !![Add another widget](10-add-another-widget.png)

2. Under **Standard Tools**, select the **Text** widget.

3. Design the **Text** widget as follows:

    |  :------------- | :-------------
    | Text            | Type in the following: `Products of vendors sponsoring the hackathon, see what you can win!`.
    | Font            | Select the text and change to Arial 14pt.
    | Color           | With the text already selected, choose black from the chart.

4. Click outside of the widget row to see how your page looks so far.

5. Select the text widget you just added, and drag it above the card widget that you added before.

    !![Drag widget](11-drag-widget.png)

6. Verify that your workspace looks like this:

7. Click **Publish** to publish your workspace.

    !![Publish workspace](12-publish.png)

[VALIDATE_7]
[ACCORDION-END]
