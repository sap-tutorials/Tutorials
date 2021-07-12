---
title: Deploy an SAP Conversational AI Chatbot to Microsoft Teams
description: Enable an SAP Conversational AI chatbot to be run within Microsoft Teams.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-conversational-ai, products>sap-business-technology-platform]
primary_tag: products>sap-conversational-ai
author_name: Daniel Wroblewski
author_profile: https://github.com/thecodester

---

## Prerequisites
 - You have created a chatbot to deploy. This tutorial uses the bot created in the tutorial [Consume API Services and Call Webhooks from Your Chatbot](conversational-ai-webhook-api-call).

## Details
### You will learn
  - How to enable your chatbot within Microsoft Teams

This tutorial shows you how to enable and test your chatbot in Microsoft teams, but does not include information on adding the chatbot to teams and channels within Microsoft Teams. For more information on this, see [Manage your apps in the Microsoft Teams admin center](https://docs.microsoft.com/en-us/MicrosoftTeams/manage-apps).

---

[ACCORDION-BEGIN [Step 1: ](Create Microsoft Azure account)]

1. Create a trial account at [Microsoft Azure](https://azure.microsoft.com/en-in/free/).

2. After creating a trial account, make sure your subscription is set up properly.

    - Go to the Microsoft Azure [portal](https://portal.azure.com/).

    - Click **Subscriptions**.

    - Click your subscription (mine was called `Azure subscription 1`).

    - Click **Resource Providers** (menu on bottom left).

    Make sure the `microsoft.insights` is registered.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Azure bot channel)]
1. Go to the Microsoft Azure [portal](https://portal.azure.com/).

2. Click **Create a Resource**.

    ![Create a Resource](1-create-resource.png)

3. Search for and then select **Bot Channels Registration**.

    ![Bot Registration](1-bot-registration.png)

    Click **Create**.

    ![Link text e.g., Destination screen](1-bot-registration-create.png)

4. Fill in the registration form.

    ![Link text e.g., Destination screen](1-registration-form.png)
    <div>&nbsp;</div>

    | Field Name | Input Value
    |------------|-------------
    | Bot Name	 | A unique display name for the bot (which will appear in channels and directories -- this can be changed later)
    | Subscription	| Your Azure subscription	(in my trial, I had only one)
    | Resource Group	| Select a resource group. If you don't have one -- which you probably won't have -- then create a new one.
    | Location	| Choose a location near where your bot is deployed
    | Pricing Tier	| `F0	(10K Premium Messages)`
    | Messaging endpoint	| This will be filled out later
    | Application Insights	| On
    | Application Insights Location | Choose a location near where your bot is deployed

    Click **Create**.

>It may take a few minutes for the registration to take effect and be listed in your resources list.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Get your app ID and secret)]

1. Go to the Microsoft Azure [portal](https://portal.azure.com/), and then go to the **Dashboard**.

    ![Dashboard](3-dashboard.png)

2. In the dashboard, click the resource you just created.

3. Click **Settings**, and scroll down.

    - Generate a **Client Secret** by clicking **Manage**, then **New Client Secret**, then **Add**.

    - Copy the **Microsoft App ID** and **Client Secret**, for use later.

    ![ID and secret](3-IDandSecret.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Connect bot to Azure)]

1. Go back to [SAP Conversational AI](https://cai.tools.sap/).

2. Open your bot, and go to the **Connect** tab.

    ![Connect tab](4-connect.png)

3. Click on the row for Skype and Teams, and in the popup, enter the app ID and client secret from Azure.

    ![Copy endpoint](4-appID-secret.png)

    An endpoint will be created. Copy it.

    ![Link text e.g., Destination screen](4-endpoint.png)

4. Go back to your resource in Azure, and under **Settings**, paste the endpoint into the **Messaging Endpoint** field.

    ![Paste endpoint](4-endpoint-Azure.png)

    Click **Save** (at top).


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test bot in Azure)]

1. Still in your resource in Azure, go to **Test in Web Client**.

    ![Open test](5-test-open.png)

    At the bottom of the screen, there is a box for talking with your bot.

2. Test your bot.

    ![Test](5-test.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test bot in Teams)]

1. Still in your resource in Azure, select **Channels**.

2. Click the Microsoft Teams icon.

    ![Channels](6-channels.png)

3. Click **Save**.

    ![Publish](6-channels-teams.png)

4. Navigate back to the **Channels** main page (you may have to navigate away or refresh the page).

5. Click on **Microsoft Teams** to open the Teams application.

    ![Open Teams](6-teams-test.png)

6. Test your bot in the Teams application.

    ![Test in Teams](6-test-in-teams.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test yourself)]




[VALIDATE_7]
[ACCORDION-END]

---

### Take It Further

- [Adding the bot as a Microsoft Teams app](https://docs.microsoft.com/en-us/microsoftteams/platform/bots/how-to/create-a-bot-for-teams)
- [Distributing a Microsoft Teams app](https://docs.microsoft.com/en-us/microsoftteams/platform/concepts/deploy-and-publish/overview)
