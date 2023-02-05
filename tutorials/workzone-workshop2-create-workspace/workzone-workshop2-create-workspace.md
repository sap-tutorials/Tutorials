---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-build-work-zone--advanced-edition]
primary_tag: software-product>sap-build-work-zone--advanced-edition
author_name: Lindsay Bert
---

# Create a Workspace with Apps and Content
<!-- description --> Create a workspace and add apps and other content to it.

## Prerequisites
 You have accessed SAP Build Work Zone, advanced edition


## You will learn
  - How to add a workspace to SAP Build Work Zone, advanced edition
  - How to connect to the SAP Gateway system and add a Web Dynpro app to your workspace
  - How to create a UI5 integration card that consumes data from the SAP Gateway demo system
  - How to integrate the UI5 integration card to a workpage in your workspace

## Intro
In this tutorial we'll provide links to existing tutorials that you need to follow. Please note that in each step, there may be instructions for you to carry out.

---

### Add a workspace to SAP Build Work Zone, advanced edition


In this step, you're going to create a workspace where employees who joined the company hackathon challenge, can interact with other members of the workspace, ask questions, and find out all that they need to know.

1. For step-by-step guidance how to do this, follow this tutorial: [Add a Workspace to SAP Build Work Zone, advanced edition](workzone-build-2-workspace).

2. In step 2.3, give the workspace a unique name by adding your User ID as a prefix to the name: For example: `<User ID>_Employee Innovation Hackathon`.


### Create an account on the SAP Gateway Demo System


For step-by-step guidance how to do this, follow this tutorial: [Create an account on the SAP Gateway Demo System](gateway-demo-signup).



### Connect SAP BTP to your Gateway Demo System account


In this step you'll create a connection between SAP BTP, Cloud Foundry environment and the SAP Gateway Demo System (ES5).

1.  For step-by-step guidance how to do this, follow this tutorial: [Connect the SAP BTP Training Subaccount to Your SAP Gateway Demo System Account (ES5)](workzone-connect-gateway).

2. Instead of step 1 explaining how to access your subaccount, use the link above.

3. In step 2.3 give the destination a unique name. For example: `<User ID>_ES5`.


### Add a Web Dynpro ABAP app to your SAP Build Work Zone, advanced edition site


In this step, you're going to add one of SAP's classic applications, a Web Dynpro ABAP app, to your site using the Content Manager. SAP's classic applications typically run on the backend of a data center.

1. For step-by-step guidance how to do this, follow this tutorial: [Add a Web Dynpro ABAP App to your SAP Work Zone](workzone-enrich-3-webdynpro-app).

2. In step 2.2, give your app a unique name by adding your User ID as a prefix to the name: For example, `<User ID>_Search POs`.



### Create a dev space for SAP Fiori apps


1. For step-by-step guidance how to do this, follow this tutorial: [Create a Dev Space for SAP Fiori Apps](appstudio-devspace-fiori-create).

2. To access your subaccount, use the SAP BTP link above instead of Step 1 of the tutorial.

3. Use your IAS user and password to log in to SAP Business Application Studio.  


### Create a UI5 integration card that consumes data from the SAP Gateway Demo System


In this step, you'll create a UI5 integration card to display data from the backend SAP Gateway Demo System.

1. For step-by-step guidance how to do this, follow this tutorial: [Create a UI5 Integration Card that Consumes Data from the SAP Gateway Demo System](appstudio-sapui5-integrationcard-create)

2. Skip step 1.

3. In step 2.4, give your card a unique name by adding your User ID as a prefix to the name: For example, `<User ID>_products_by_vendor_card`.

    > Note: if you run out of time, please skip steps 4, 5, and 6 and create a more simple card. You can later read the steps you missed.  


### Add your UI5 integration card to an SAP Build Work Zone, advanced edition workspace


In this step, you'll add your deployed UI5 integration card to a workpage in your workspace.

1. For step-by-step guidance how to do this, follow this tutorial: [Integrate a UI5 Integration Card to Your SAP Work Zone](workzone-enrich-5-integrate-card).

2. In step 1.4, select your card with your own User ID: `<User ID>_products_by_vendor_card` and add it to your workspace in step 3.5. Also, you can use your own ES5 destination in the card settings.

Congratulations! You're done!

If you would like to do this tutorial using your own subaccount, you can follow this tutorial mission: [Create Your First Digital Workplace Using SAP Work Zone](mission.workzone-first).









---
