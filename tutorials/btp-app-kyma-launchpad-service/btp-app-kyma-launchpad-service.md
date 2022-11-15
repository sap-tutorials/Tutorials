---
parser: v2
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
keywords: cap
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp\\, kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

# Add the SAP Launchpad Service
<!-- description --> Learn how to add the SAP Launchpad service.

## Prerequisites
 - [Prepare SAP Launchpad Service Setup](btp-app-kyma-launchpad-service-setup)

## You will learn
 - How to subscribe to SAP Launchpad service
 - How to test your SAP Launchpad site


---

### Subscribe to SAP Launchpad service

1. Enter your **Global Account**. If you are using a trial account, choose **Go To Your Trial Account**.

2. Choose **Account Explorer**.

3. In the **Subaccounts** tab, choose the subaccount where you have deployed your service and application.

    <!-- border -->![Choose Subaccount](choose_subaccount.png)

2. Choose **Services** &rarr; **Service Marketplace** on the left.

3. Search for the **Launchpad Service** tile and choose **Create**.

    <!-- border -->![Create Launchpad Service](create_launchpad_service.png)

4. Keep the default settings for **Service** and **Plan** and choose **Create**.

    <!-- border -->![New Instance or Subscription](new_instance_dialog.png)

You have now subscribed to the SAP Launchpad service.

---
### Assign SAP Launchpad role collection

You need to assign your user to the `Launchpad_Admin` role collection, so you don't get an error accessing the **Launchpad Service** site later on.

1. Choose **Security** &rarr; **Users** on the left.

2. Choose your user.

4. Under **Role Collections** on the right, choose **Assign Role Collection** and assign the `Launchpad_Admin` role collection to your user.

    <!-- border -->![Add role](add_launchpad_admin_role.png)

5. Open another browser or clear your browser's cache.

> See section [Initial Setup](https://help.sap.com/viewer/8c8e1958338140699bd4811b37b82ece/Cloud/en-US/fd79b232967545569d1ae4d8f691016b.html) in the SAP Launchpad service's documentation for more details.



---
### Create your SAP Launchpad site

1. Choose **Services** &rarr; **Instances and Subscriptions** on the left.

2. Locate the **Launchpad Service** under **Subscriptions** and choose **Go to Application**.

    <!-- border -->![Instances and Subscriptions](instances_and_subscriptions.png)

3. Choose **Channel Manager** on the left and refresh the `HTML5 Apps` entry there.

    <!-- border -->![Refresh HTML5 Apps](refresh_html5_apps.png)

    > Content providers aren't reloaded automatically when you push an app, so it's important to manually refresh.

4. Choose **Content Manager** **&rarr;** **Content Explorer** and open the content provider `HTML5 Apps`.

    <!-- border -->![HTML5 Apps Content Provider](html5_apps_content_provider.png)

5. Add the `Risks` and `Mitigations` to **My Content**.

    <!-- border -->![Add Apps to My Content](add_apps_to_my_content.png)

6. Choose **Content Manager** &rarr; **My Content**.

7. In the item list, choose the item `Everyone`.

    <!-- border -->![Role Everyone](role_everyone.png)

    > `Everyone` is a role that has to be assigned to the `Risks` and `Mitigations` apps so all users can access them.

8. Choose **Edit**, click on the search field, assign the `Risks` and `Mitigations` apps to the role, and choose **Save**.

    <!-- border -->![Add Apps to Role](apps_to_role_everyone.png)

9. Navigate back to **My Content**.

10. Choose **New** &rarr; **Group**.

    <!-- border -->![New Group](new_group.png)

11. Type in `Risk Management` as the title of the group and assign the `Risks` and `Mitigations` apps to it.

    <!-- border -->![Create Group](create_group.png)

    > This way, you are telling the SAP Launchpad service to display the `Risks` and `Mitigations` apps in a group called `Risk Management`.

12. Choose **Site Directory** &rarr; **Create Site**.

    <!-- border -->![Create Site](create_site.png)

13. Type in `Risk Management Site` for the site name and choose **Create**.

    > The new site gets the `Everyone` role by default, so you don't have to assign it explicitly. The default site properties are sufficient for the purposes of this tutorial.



---
### Test your SAP Launchpad site

1. Choose **Go to site**.

    <!-- border -->![Go to site](go_to_site.png)

    You can see the `Risk Management` group that includes the `Mitigations` and `Risks` apps.

2. Open the `Risks` app.

    <!-- border -->![Risk Management Site](risk_management_site.png)

You have launched your `Risks` app through the SAP Launchpad service.

  <!-- border -->![Risks App](risks.png)

> If you choose **Go**, you will get an error because you haven't assigned a role collection to your user yet. We'll do it in the next tutorial.


The result of this tutorial can be found in the [`kyma-launchpad-service`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/kyma-launchpad-service) branch.
---