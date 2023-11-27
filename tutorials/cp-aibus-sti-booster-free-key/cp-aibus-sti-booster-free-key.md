---
parser: v2
auto_validation: true
time: 5
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>service-ticket-intelligence, tutorial>free-tier]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

# Use Free Tier to Set Up Account for Service Ticket Intelligence and Get Service Key
<!-- description --> Use the Free Tier service plan and an SAP BTP booster to automatically create a service instance, and the associated service key for Service Ticket Intelligence.

## Prerequisites
- You have created an account on SAP BTP to try out free tier service plans: [Get an Account on SAP BTP to Try Out Free Tier Service Plans](btp-free-tier-account)
- You are entitled to use the Service Ticket Intelligence service: [Manage Entitlements Using the Cockpit](btp-cockpit-entitlements).

## You will learn
  - How to access your SAP BTP account
  - What are interactive guided boosters
  - How to use the **Set up account for Service Ticket Intelligence** booster to assign entitlements, update your subaccount (or create a new one), create a service instance and the associated service key for Service Ticket Intelligence
---

### Go to your SAP BTP account


1. Open the [SAP BTP cockpit](https://account.hana.ondemand.com/cockpit#/home/allaccounts).

2. Access your global account.

    <!-- border -->![global account](global-account.png)    



### Run booster


SAP Business Technology Platform creates interactive guided boosters to automate cockpit steps, so users can save time when trying out the services.

Now, you will use the **Set up account for Service Ticket Intelligence** booster to automatically assign entitlements, update your subaccount, create a service instance and the associated service key for Service Ticket Intelligence.

1. On the navigation side bar, click **Boosters**.

    <!-- border -->![Service Key](access-booster.png)

2. Search for **Service Ticket Intelligence** and click the tile to access the booster.

    <!-- border -->![Service Key](access-booster-tile.png)

3. Click **Start**.

    <!-- border -->![Service Key](booster-start.png)

4. Click **Next**.

    <!-- border -->![Service Key](booster-next.png)

5. If you want to create a dedicated subaccount for the service instance, choose **Create Subaccount**. If you want to use an already created subaccount, choose **Select Subaccount** (the selection comes in the next step). For this tutorial, we'll create a dedicated subaccount. When you're done with the selection, click **Next**.

    <!-- border -->![Service Key](booster-scenario.png)

6. Choose the **free** plan. You can also rename the subaccount to `sti-free-tier-service-plan-tutorial`, for example. Choose the region closest to you. For this tutorial, we'll use **Europe (Frankfurt)**. Click **Next**.

    <!-- border -->![Service Key](booster-subaccount.png)

    >You can also perform this tutorial series using the `blocks_of_100` service plan. For that, choose the `blocks_of_100` plan in this step (instead of free). For more information on the service plans available for Service Ticket Intelligence, see [Service Plans](https://help.sap.com/docs/SERVICE_TICKET_INTELLIGENCE/934ccff77ddb4fa2bf268a0085984db0/12e4b770e0d741a3911a5b196bf383f8.html).

7. Click **Finish**.

    <!-- border -->![Service Key](booster-finish.png)

    Follow the progress of the booster automated tasks.

    <!-- border -->![Service Key](booster-progress.png)

    When the automated tasks are done, see the **Success** dialog box.

    <!-- border -->![Service Key](booster-success.png)



### Get service key


You have successfully used the booster **Set up account for Service Ticket Intelligence** to create a service key for Service Ticket Intelligence.

Click **Download Service Key** to save the service key locally on your computer.

<!-- border -->![Service Key](booster-success-key.png)

>If you face any issue with the booster **Set up account for Service Ticket Intelligence**, you can alternatively follow the steps in [Use Free Tier to Create a Service Instance for Service Ticket Intelligence](cp-aibus-sti-free-service-instance) to manually create the service instance and service key for Service Ticket Intelligence using the free tier service plan.

Step 4 is optional. If you're not interested, you can set it to **Done** and go directly to the next tutorial.




### Access service instance and service key (optional)


> This is an optional step. Use it only if you want to access the service instance and service key, you created with the **Set up account for Service Ticket Intelligence** booster, without having to run it once again.

Do the following to access your service instance and service key, without having to run the **Set up account for Service Ticket Intelligence** booster once again:

1. Close the booster **Success** dialog box.

    <!-- border -->![Service Key](leave-success.png)

2. Access your global account.

    <!-- border -->![Service Key](access-global-account.png)

3. Click **Account Explorer** on the navigation side bar and access the subaccount you used to create the service instance and service key for Service Ticket Intelligence.

    <!-- border -->![Service Key](subaccounts.png)

4. Click **Instances and Subscriptions** on the navigation side bar. You see the service instance you created with the **Set up account for Service Ticket Intelligence** booster.

    <!-- border -->![Service Key](service-instance.png)

5. Click the navigation arrow to open the details of your service instance. Select **Service Keys (1)**. Then, click the dots to **View**, **Download** or **Delete** your service key.

    <!-- border -->![Service Key](service-key.png)

Congratulations, you have completed this tutorial.

