---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Subscribe to the SAP Build Work Zone, Standard Edition
description: This tutorial shows you how to subscribe to the SAP SAP Build Work Zone, standard edition.
keywords: cap
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Prepare SAP Build Work Zone, Standard Edition Setup](btp-app-work-zone-setup)


## Details
### You will learn
 - How to subscribe to SAP Build Work Zone, standard edition
 - How to test your SAP Build Work Zone, standard edition site

---

[ACCORDION-BEGIN [Step 1: ](Overview)]
In this tutorial, you will use the SAP Build Work Zone, standard edition to access your CAP service and its UI. Additionally, the SAP Build Work Zone, standard edition provides features like personalization, role-based visibility, theming, and more. You can add multiple applications to one launchpad, including subscribed ones and applications from SAP S/4HANA or SAP BTP.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Subscribe to SAP Build Work Zone, standard edition)]
1. Enter your **Global Account**. If you are using a trial account, choose **Go To Your Trial Account**.

2. Choose **Account Explorer**.

3. In the **Subaccounts** tab, choose the subaccount where you have deployed your service and application.

    !![Choose Subaccount](choose_subaccount.png)

2. Choose **Services** &rarr; **Service Marketplace** on the left.

3. Search for the **SAP Build Work Zone, standard edition** tile and choose **Create**.

    !![Create SAP Build Work Zone, standard edition](create_workzone_instance.png)

4. Keep the default setting for **Service** and choose `standard - Subscription` for **Plan**.

    !![Choose SAP Build Work Zone, standard edition plan](choose_workzone_plan.png)

    > SAP Build Work Zone, standard edition offers two types of `standard` plans. The `standard - Subscription` plan is an application plan that lets you access your applications from a central entry point. This is the plan you need for the purposes of this tutorial. The `standard - Instance` plan is a service plan that will let you integrate with other services using APIs. You do not need this plan for the scope of this tutorial.

5. Choose **Create**.

    !![New Instance or Subscription](new_instance_dialog.png)

You have now subscribed to the SAP Build Work Zone, standard edition.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Assign SAP Build Work Zone, standard edition role collection)]
You need to assign your user to the `Launchpad_Admin` role collection, so you don't get an error accessing the **SAP Build Work Zone, standard edition** site later on.

1. Choose **Security** &rarr; **Users** on the left.

2. Choose your user.

4. Under **Role Collections** on the right, choose **Assign Role Collection** and assign the `Launchpad_Admin` role collection to your user.

    !![Add role](add_launchpad_admin_role.png)

5. Open another browser or clear your browser's cache.

> See section [Initial Setup](https://help.sap.com/viewer/8c8e1958338140699bd4811b37b82ece/Cloud/en-US/fd79b232967545569d1ae4d8f691016b.html) in the SAP Build Work Zone, standard edition's documentation for more details.



[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Create your SAP Build Work Zone, standard edition site)]
1. Choose **Services** &rarr; **Instances and Subscriptions** on the left.

2. Locate the **SAP Build Work Zone, standard edition** under **Subscriptions** and choose **Go to Application**.

    !![Instances and Subscriptions](instances_and_subscriptions.png)

3. Choose **Channel Manager** on the left and refresh the `HTML5 Apps` entry there.

    !![Refresh HTML5 Apps](refresh_html5_apps.png)

    > Content providers aren't reloaded automatically when you push an app, so it's important to manually refresh.

4. Choose **Content Manager** **&rarr;** **Content Explorer** and open the content provider `HTML5 Apps`.

    !![HTML5 Apps Content Provider](html5_apps_content_provider.png)

5. Add the `Risks` and `Mitigations` to **My Content**.

    !![Add Apps to My Content](add_apps_to_my_content.png)

6. Choose **Content Manager** &rarr; **My Content**.

7. In the item list, choose the item `Everyone`.

    !![Role Everyone](role_everyone.png)

    > `Everyone` is a role that has to be assigned to the `Risks` and `Mitigations` apps so all users can access them.

8. Choose **Edit**, click on the search field, assign the `Risks` and `Mitigations` apps to the role, and choose **Save**.

    !![Add Apps to Role](apps_to_role_everyone.png)

9. Navigate back to **My Content**.

10. Choose **New** &rarr; **Group**.

    !![New Group](new_group.png)

11. Type in `Risk Management` as the title of the group and assign the `Risks` and `Mitigations` apps to it.

    !![Create Group](create_group.png)

    > This way, you are telling the SAP Build Work Zone, standard edition to display the `Risks` and `Mitigations` apps in a group called `Risk Management`.

12. Choose **Site Directory** &rarr; **Create Site**.

    !![Create Site](create_site.png)

13. Type in `Risk Management Site` for the site name and choose **Create**.

    > The new site gets the `Everyone` role by default, so you don't have to assign it explicitly. The default site properties are sufficient for the purposes of this tutorial.



[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Test your SAP Build Work Zone, standard edition site)]
1. Choose **Go to site**.

    !![Go to site](go_to_site.png)

    You can see the `Risk Management` group that includes the `Mitigations` and `Risks` apps.

2. Open the `Risks` app.

    !![Risk Management Site](risk_management_site.png)

You have launched your `Risks` app through the SAP Build Work Zone, standard edition.

  !![Risks App](risks.png)

> If you choose **Go**, you will get an error because you haven't assigned a role collection to your user yet. We'll do it in the next tutorial.

> Do you want to change your Risk Management Site's default theme? Under your avatar, in the User Actions menu, select the Theme Manager.

>     !![Theme Manager](theme-manager.png)

[VALIDATE_1]
[ACCORDION-END]
---