---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, topic>artificial-intelligence, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

# Use Boosters for Use of SAP AI Core and SAP AI Launchpad
<!-- description --> Use Boosters to set up for SAP AI Core and SAP AI Launchpad.
## Prerequisites
- A BTP global account
If you are an SAP Developer or SAP employee, please refer to the following links ( **for internal SAP stakeholders only** ) - 
[How to create a BTP Account (internal)](https://me.sap.com/notes/3493139)
[SAP AI Core](https://help.sap.com/docs/sap-ai-core?version=INTERNAL&locale=en-US&state=PRODUCTION)
If you are an external developer or a customer or a partner kindly refer to this [tutorial](https://developers.sap.com/tutorials/btp-cockpit-entitlements.html)
- A BTP global account, For more details, refer to: [BTP global account](https://statics.teams.cdn.office.net/evergreen-assets/safelinks/1/atp-safelinks.html)

## You will learn
- How to access your SAP BTP global account
- How to use boosters to get started with SAP AI Core and SAP AI Launchpad

## Intro
#### About SAP AI Core and SAP AI Launchpad
SAP AI Core and SAP AI Launchpad are services which you can link to your BTP global account. SAP AI Core offers a powerful AI runtime which is natively integrated with SAP AI Launchpad. The launchpad offers an easy-to-use interface to manage AI workflow administration, processes, and tasks.

---

### Provision SAP AI Core in your global account

Open the SAP BTP cockpit and access your global account.

![BTP Cockpit](img/btpcockpit.png)

Check the entitlements for your account by clicking `Entitlements` and searching for SAP AI Core.

Click `Configure Entitlements` > `Add Service Plans`.

![Set SAP AI Core as an entitlement](img/configureentitlements.png)
![Set SAP AI Core as an entitlement](img/addserviceplan.png)

Select SAP AI Core and the `Standard` service plan.

![Set SAP AI Core as an entitlement](img/aicoreentitlement.png)

Save your new entitlement.

![Save](img/saveentitlement.png)

### Run the booster for SAP AI Core

Choose `Boosters` from the navigation pane. Find and choose the booster for `SAP AI Core` from the selection. 

![Locate the SAP AI Core booster](img/boostercore.png)

The booster tile contains information about SAP AI Core.  Click `Start` when you are ready. 

When you start a booster, a wizard opens up which guides you through the required steps.

![Start the booster](img/coreboosterstart.png)

Choose the scenario `Select Subaccount` and click `Next`
![Choose Scenario select subaccount](img/aicore_booster_select_scenario.png)
In the Configure Subaccount, select `standard` plan and your subaccount in which you want to provision SAP AI Core and click `Next`.

![configure entitlements and subaccount](img/aicore_booster_configure_subaccount.png)

Review your configuration and click `Finish`.
![review configuration](img/aicore_booster_review_selections.png)

![booster execution inprogess](img/aicore_create_instance_inprogress.png)

After the execution completes, follow the steps shown to navigate to your subaccount.

![booster execution success](img/aicore_booster_successful_execution.png)

### View your instances and create your keys

In the subaccount section of SAP BTP Cockpit, choose `Services` from the left navigation menu and `Instances and subscriptions` from the page. 

![View instances and subscriptions](img/instancesandsubscriptions.png)

To see the details of your new instance, click the chevron on the entry.

To create the keys that you need to access your instance, click the three dots > `Create Service Key`.

![Create keys](img/createkeys1.png)

Enter a `Key Name` of your choice and click `Create`.

![Create keys](img/createkeys2.png)
![View new keys](img/keys.png)

Once your keys have been created, you can view or download them at any time by locating the key and clicking the three dots and choosing from the available options.

![Locate the keys in SAP AI Cockpit](img/viewkeys.png)

### Provision SAP AI Launchpad in your global account

> **Note:** SAP AI Launchpad is optional, but is the recommended interface for use with SAP AI Core.

Configure your entitlement as before, but select `SAP AI Launchpad`.

![SAP AI Launchpad account information](img/lpentitlement.png)
### Run the booster for SAP AI Launchpad

Choose `Boosters` from the navigation pane, and then choose the booster for `SAP AI Launchpad` from the selection. 

![Locate the booster for SAP AI Launchpad](img/boosterailp.png)

Click `Start` when you are ready.

![Start the booster](img/lpboosterstart.png)

In the Scenario Section, choose `Select Subaccount` option and click `Next`.
![choose select subaccount scenario](img/AIL_select_scenario.png)
In the Configure Subaccount, choose `standard` plan, your subaccount and space where you want to provision SAP AI Launchpad and click `Next`
![configure entitlement](img/AIL_configure_subaccount_std_plan.png)
Review the subaccount and entitlement details you have selected previously and click `Finish`.
![review configuration](img/AIL_review.png)
![execution in progress](img/AIL_creation_inprogress.png)
wait until setup completes. Once completed, navigate to your subaccount.

### View your instances

View your SAP AI Launchpad instance by navigating to `Instances and subscriptions`. To see the details of your new instance, click the chevron on the entry.

![Success screen](img/ail_subscription_success.png)

