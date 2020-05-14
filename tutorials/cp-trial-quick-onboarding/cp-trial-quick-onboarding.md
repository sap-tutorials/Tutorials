---
title: Get Started with SAP Cloud Platform Trial
description: Get familiar with the SAP Cloud Platform basics to make the most of your trial experience.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform
author_name: Jessica Katz
author_profile: https://github.com/Jessica-Katz
---

## Prerequisites
 - You must have registered for a trial account on SAP Cloud Platform.
[Sign up](https://developers.sap.com/tutorials/hcp-create-trial-account.html) for a free trial.

## Details
### You will learn
  - What SAP Cloud Platform trial offers
  - The relationship between global accounts, subaccounts, orgs and spaces
  - How to create a trial account in the Cloud Foundry environment
  - How to use services in the Cloud Foundry environment

---

[ACCORDION-BEGIN [Step: ](Welcome to SAP Cloud Platform Trial)]

So you've signed up for a trial on SAP Cloud Platform but are not quite sure where to start? This tutorial is here to help!

A trial account on SAP Cloud Platform enables you to experience it for free, offering access to a comprehensive set of platform services, as well as early access to beta functionality.

Trial accounts are intended for personal exploration, and not for production use or team development. They allow restricted use of the platform resources and services.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step: ](Global accounts, subaccounts, orgs and spaces)]

Now that you know what to expect from your trial account, let's learn about how things are structured on SAP Cloud Platform.

SAP Cloud Platform uses two levels of accounts:  **global accounts** and **subaccounts**. Your global account and subaccounts in SAP Cloud Platform are the way in which all of your activities in SAP Cloud Platform are structured.

A **global account** is the realization of a contract you made with SAP (be it enterprise or trial). It's region-independent, and it's used to manage subaccounts, members, and quotas. You receive quotas to use platform resources per global account and then distribute the quotas to the subaccount for actual consumption. A global account can contain one or more subaccounts in which you deploy applications and use services.

**Subaccounts** let you structure a global account according to your project's requirements with regards to members, authorizations, and quotas. Subaccounts in a global account are independent from each other. Each subaccount is associated with a **region**, which is the physical location where applications, data, or services are hosted. The region assigned to your subaccount doesn't have to be directly related to your location. You could be located in the United States, for example, but operate your subaccount in Europe.

### Cloud Foundry

When you register for a trial account on SAP Cloud Platform, your Cloud Foundry trial global account is not automatically created. You must create one yourself from the cockpit (more details on how to do that later).

In addition to global accounts and subaccounts, Cloud Foundry includes another hierarchical level represented by **orgs** and **spaces**.

**Each Cloud Foundry subaccount contains exactly one Cloud Foundry org.** They have a 1:1 relationship. When you create a Cloud Foundry trial account, you get a global account with one Cloud Foundry subaccount in it, where the org is created automatically. You have the option of creating additional Cloud Foundry subaccounts within your trial global account, however in those you must create an org yourself by clicking on the **Enable Cloud Foundry** button.

**In your org you can then create one or more spaces, where you deploy apps and use services.** Similar to subaccounts, spaces enable you to once again structure and sub-divide quota if you want to. When you create your Cloud Foundry trial global account in the cockpit, one space is also automatically created within your org. You also have the option of adding additional ones if you like.

![Cloud Foundry trial account model](cf-trial-account-model.png)

[VALIDATE_2]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Navigation in the cockpit)]

There are 2 ways of navigating in the cockpit:

- using the left hand-side navigation
- using the breadcrumbs at the top of the page

There are certain actions you can perform at each hierarchical level (global account, subaccount, space etc.) - we will refer to the sum of these actions as a **scope**. For example, when you are in a global account you can add global account members, create subaccounts and add entitlements to subaccounts. They represent the **global account scope**.

This scope is reflected in both the left hand-side navigation and the breadcrumbs. This step helps you understand how to use both of them together to navigate through the cockpit in the most efficient way possible.

### Left hand-side navigation

 The left hand-side navigation changes depending on where you are in the cockpit. It reflects the actions possible in that scope. You can see some examples below:

**Cloud Foundry Subaccount Scope:**

![Cloud Foundry subaccount scope](cf-sa-scope.png)

**Cloud Foundry Space Scope:**

![Cloud Foundry space scope](cf-space-scope.png)


### Breadcrumbs

You may ask yourself - if the left hand-side navigation changes depending on where you are in the cockpit,  then how can you navigate back? Simple - you use the breadcrumbs at the top.

The breadcrumbs coincide with the hierarchical path you've travelled in order to reach your current scope in the cockpit. For example, to reach an application in the Cloud Foundry environment you will have to go to your global account, then to the subaccount, then to the space and then finally to the application.

The last item in your breadcrumbs always represents the scope you find yourself in at that moment. To go back, simply follow the breadcrumbs to retrace your steps.

Let's have a look at the following screenshot, taken from an application deployed in the Cloud Foundry dev space:

![Breadcrumbs](cf-breadcrumbs.png)

1. **Trial Home**: Here you can find your trial homepage.

2. **Global account**: This contains all your subaccounts and entitlements.

3. The 3rd item represents your **subaccount**.

4. Afterwards, you have your **space** as the 4th item.

5. Finally, you have your **deployed application**, which is the current scope.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Services in the Cloud Foundry environment)]

In the Cloud Foundry environment, you create service instances in your Cloud Foundry space and bind them to applications within that space.

A service instance is a single instantiation of a service running on SAP Cloud Platform. Service instances are created using a specific service plan. A service plan is a configuration variant of a service. For example, a database may be configured with various "t-shirt sizes", each of which is a different service plan.

To integrate services with applications, the service credentials must be delivered to the application. To do so, you can bind service instances to your application to automatically deliver these credentials to your application. Alternatively, you can use service keys to generate credentials to communicate directly with a service instance. As shown in the figure below, you can deploy an application first and then bind it to a service instance:

![Using services in Cloud Foundry](using-cf-services.png)

#### To create a service instance:

1. Navigate to your space in the Cloud Foundry environment where you want to create the instance and choose **Service Marketplace** from the left hand-side navigation.

    ![Cloud Foundry space](cf-space-navigate.png)

2. Choose the service for which you want to create an instance (let's choose **Application Logging** as an example).

    ![Cloud Foundry services](cf-service-selection.png)

3. In the left hand-side navigation, choose **Instances**.

    ![Cloud Foundry service overview](cf-service-overview.png)

4. Click on the **New Instance** button.

    ![Cloud Foundry service instances](cf-new-instance.png)

5. Follow the wizard that appears to finalize the creation of the instance. This includes choosing a service plan and optionally adding parameters and an application to bind the instance to.

    ![Cloud Foundry new instance wizard](cf-create-instance.png)

6. Finally, enter a name for your instance and choose **Finish**.

    ![Cloud Foundry instance name](cf-instance-name.png)

7. Your new instance will appear in the table:

    ![Cloud Foundry service instances table](cf-instance-created.png)

8. You can then click on it to bind it to an application or create a service key.

    ![Cloud Foundry service instances table](cf-bind-instance.png)

[VALIDATE_4]
[ACCORDION-END]
---
