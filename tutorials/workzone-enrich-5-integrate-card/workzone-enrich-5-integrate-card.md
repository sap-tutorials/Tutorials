---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product>sap-work-zone, topic>sapui5]
primary_tag: software-product>sap-work-zone
author_name: Lindsay Bert
---

# Integrate a UI5 Integration Card to Your SAP Work Zone
<!-- description --> Add a custom UI5 integration card to your SAP Work Zone home page.

## Prerequisites
 - You have deployed a UI5 integration card to SAP Work Zone.

## You will learn
  -  How to make a deployed UI5 integration card available in SAP Work Zone and add it to the SAP Work Zone home page or workspace.

## Intro
You've already deployed a UI5 integration card. Now in this tutorial, you'll enable it to make it available in SAP Work Zone and later add it to your workspace.


---



### Enable your deployed UI5 integration card


1. Access the **Administration Console** from the user actions dropdown menu under your avatar.

    <!-- border -->![Access the admin console](1-access-admin-console.png)

2. Go to the **UI Integration** section, expand it, and click **Cards**.  

    <!-- border -->![Open Cards section](2-open-card-section.png)

3. Scroll down to view the **Uploaded Cards** section.

    >If you don't see your deployed card, you may need to refresh the browser.

4. Click the slider button to enable the card.

    <!-- border -->![Enable card](3-enable-card.png)

The card is now enabled and can be added to the workpage in your workspace.

>In this step, for workshops, use your unique identifier `<your unique identifier>_Products by Vendor Card`.

>You can click on **Configure** to select a different destination for this UI5 integration card. As you used the same ES5 destination in this SAP BTP subaccount and also when you developed the card, this step is not required to complete this tutorial.


### Configure the card settings


Before you add the card to your workspace, let's configure one of the settings of the card.

1. On the `Products by Vendor Card`, click **Configure**.

    <!-- border -->![Click Configure](3a-click-configure.png)

2. Under **General Settings**, change the **Maximum Items** to 2 and select the destination. If you're in a workshop, you can select the destination that you created:  `<your unique ID_ES5`>. Then click **Save changes**.

    <!-- border -->![Change setting](3b-change-setting.png)

    > On the **Configure** screen, when you click the **More settings** icon there are more settings that you can configure for the end user.
    <!-- border -->![More settings](14-more-settings.png)

In the next step you'll add the card to your workspace.



### Add your UI5 integration card to your workspace


1. Click the **Workspaces** menu and select `Employee Innovation Hackathon` to navigate to your workspace.

    <!-- border -->![Access workspace](4-select-workspace.png)


2. Click the pencil icon on the right side of the screen to open the Page Designer.

    <!-- border -->![Open in edit mode](5-open-page-designer.png)

3. Click the **+** below the **Forum** widget and above the **Feed** widget to add a new section to your workspace.

    <!-- border -->![Add new row](6-add-new-row.png)

4. In the added section, click **Add Widget**.

    <!-- border -->![Add widget](7-add-widget.png)

5. Click **Cards**.

    ![Click cards](7a-click-cards.png)

6. Select the `Products by Vendor Card` widget that you previously created. In a workshop, use your unique identifier `<your unique identifier>_Products by Vendor Card`.

    <!-- border -->![Add the card](8-select-card.png)

7. In the **Add Widget** screen, click **Save**.

    <!-- border -->![Add card](9-add-card.png)

The card you created was added to the workpage of your workspace.


Now let's add a title for the card.



### Finalize the design of your workspace


1. Click **Add Widget** below the card widget you just added.

    <!-- border -->![Add another widget](10-add-another-widget.png)

2. Select the **Text** widget.

3. Design the **Text** widget as follows:

    |  :------------- | :-------------
    | Text            | Type in the following: `Products of vendors sponsoring the hackathon, see what you can win!`.
    | Font            | Select the text and change to Arial 14pt.
    | Color           | With the text already selected, choose black from the chart.

4. Click outside of the widget row to see how your page looks so far.

5. Select the text widget you just added, and drag it above the card widget that you added before.

    <!-- border -->![Drag widget](11-drag-widget.png)

6. Click **Publish** to publish your workspace.

    <!-- border -->![Publish workspace](12-publish.png)

Your workspace should look like this - note that the card is only showing 2 out of 20 products because you configured it that way.

<!-- border -->![Final workspace](13-final-workspace.png)
