---
title: Access SAP S/4HANA Cloud using SAP S/4HANA Cloud SDK
description: Retrieve business partners from an SAP S/4HANA system using the SAP S/4HANA Cloud SDK and its Java virtual data model (VDM).
primary_tag: products>sap-s-4hana-cloud-sdk
tags: [  tutorial>beginner, topic>cloud, topic>java, topic>odata, products>sap-s-4hana-cloud-sdk ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  
  - How to integrate SAP S/4HANA Cloud into any Cloud-native Java application.
  - How SAP S/4HANA Cloud SDK helps build Cloud-native applications and access SAP S/4HANA APIs.
  - Why SAP Cloud Platform is the premier choice for extending SAP solutions.

### Time to Complete
**20 Min**

---

Did you know that SAP customers produce 81% of the world's food? SAP software is used in different areas across the enterprise: while manufacturing goods, delivering orders to customers, paying employees, keeping track of the finances, and much more. As so-called ERP software, SAP S/4HANA Cloud runs the core of the business.

How can you build Cloud-native applications that benefit from the rich set of business-critical functionality provided by SAP S/4HANA Cloud and from the broad adoption of SAP software? In this tutorial, you will get a short glimpse of the tools SAP provides to developers to achieve this.

### Overview
You will use the Java virtual data model (VDM) of the SAP S/4HANA Cloud SDK to access business data stored in an SAP S/4HANA system. In the background, this will call an OData API.

The machine has already been prepared and you will find the source code for an Address Manager application in the development environment.

There is just one thing missing: The app needs to retrieve the business partners (for example, customers) from an SAP S/4HANA system. This tutorial will walk you through the steps to implement the missing functionality using the SAP S/4HANA Cloud SDK, and to deploy the application on SAP Cloud Platform.


[ACCORDION-BEGIN [Step 1: ](Reset to initial state)]

To begin, open the integrated development environment (IDE) `IntelliJ`. Look for this icon in the Windows task bar.
![IntelliJ icon](0-0-intellij-icon.png)

Someone else may have done the tutorial on this machine before. So, let's first make sure that everything is in the initial state required for this tutorial.

From the menu bar, choose **VCS > Git > Revert** (or press **Ctrl+Alt+Z**).
![Open revert dialog](0-1-open-revert.png)

Click the checkbox next to **Default** so that all changes are marked for reverting, and click **Revert**.
![Revert all changes](0-2-revert.png)

Check if the local server is still running by looking at the run icon with the green arrow next to **Run local server** in the upper right part of the screen. If the icon has a small green dot like in the following screenshot, the server is already running.
![Check local server](0-3-check-server.png)

If the server is not already running, start the local server by clicking the green run icon.
![Run local server](0-4-run-server.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test the initial state)]

We have already a set of tests in place that check the existing functionality of the app. Your task is to get the last failing test to pass. Let's first run the tests based on the current state.

Locate the folder `address-manager/integration-tests` in  the project pane on the left.

Right-click `integration-tests` and select **Run 'All Tests'**.
![Run all tests](1-1-run-tests.png)

While the tests are running, visit `http://localhost:8080/address-manager/` to see the currently implemented application in action. You will notice that the list of business partners on the left is still empty, because you have not yet implemented this functionality.
![Initial state of app](1-2-empty-app.png)

Return to the development environment, where you should now see the test results.

The `testGetAll` test will have failed, because it tests the not yet implemented functionality of retrieving the list of all business partners.
![Test results](1-3-test-results-failed.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Retrieve all business partners)]

Let's now implement the missing piece of retrieving business partners. A business partner is a person or company that our company does business with, for example, as customer or vendor. Business partners are stored in an ERP system like SAP S/4HANA Cloud, from where we want to retrieve them.

In the project pane on the left, expand the folder structure as follows:
`address-manager > application > src > main > java > com.sap.cloud.s4hana.examples.addressmgr.commands`

Open the file `GetAllBusinessPartnersCommand` by double-clicking it.
![Open the Java file](2-1-open-command.png)

Look out for the method called `runCacheable` and select the text `Collections.emptyList()`.
![Select text in method](2-2-select-text.png)

Replace the highlighted text with the following code snippet. Either copy and paste the code, or try typing it into the IDE to experience the discover the capabilities of the SAP S/4HANA Cloud SDK:
```java
service.getAllBusinessPartner()
    .select(BusinessPartner.BUSINESS_PARTNER,BusinessPartner.LAST_NAME, BusinessPartner.FIRST_NAME)
    .execute()
```
![Insert code snippet](2-3-code-inserted.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test and re-run the project)]

Let's see if we correctly implemented the requirements by running our tests again.

Re-run the tests by clicking on the green run icon in the left side bar of the bottom pane.
![Re-run tests](3-1-re-run-tests.png)

While the tests are running, wait a few seconds for the local server to refresh. Then you can already take a look at the updated app at `http://localhost:8080/address-manager/`.

You will now see the list of business partners on the left, and you can explore the app. Select a business partner to see the details of this business partner.
![App displaying business partner](3-2-app-with-results.png)

When you return to the development environment, the tests should now have all succeeded, including the previously failing `testGetAll` test.
![Successful tests](3-3-successful-tests.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Filter for persons)]

If you look closely in the app, there is one business partner for which no first or last name is displayed. This is because this business partner is not a natural person, but a company.

Because we want our app to only display persons, we are now going to implement a filter.

In the development environment, position the cursor directly in front of the `.execute()`.
Either copy and paste the following code, or enter it step-by-step into the IDE.
```java
.filter(BusinessPartner.BUSINESS_PARTNER_CATEGORY.eq(CATEGORY_PERSON))
```

![Implement filter](4-1-filter.png)

With this, our app is complete for now, so we are now going to deploy it on SAP Cloud Platform.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Compile and package the app)]

In order to push the application to SAP Cloud Platform, we need to package it.

In the development environment, open the terminal.
![Open terminal](5-1-open-terminal.png)

Copy and paste the following command into the terminal and execute it by hitting **Enter**.
```
mvn package -pl .\application -am
```

While the command is running, you can already continue with the next step.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create SAP Cloud Platform trial account)]

To later deploy the app to SAP Cloud Platform Cloud Foundry, you need a trial account on SAP Cloud Platform. If you already have an account, you can skip this step.

> You need to have access to your e-mail account.

Visit <https://cloudplatform.sap.com> and click **Start your free trial**.
![Start free trial](6-1-start-free-trial.png)

Enter your personal information. Make sure to enter a valid e-mail address so that you can later activate the account.
> **CAUTION**: Choose a password that you do *not* use for any other account.
> You later need to enter the password on the shared laptop.

Click **Submit** to finish the registration.
![Register](6-2-registration.png)

On a separate device like your smartphone, check your e-mail account for a mail from SAP Cloud Platform that contains an activation link. Visit this link to activate your account.
![Successful activation](6-3-activation.png)

On <https://cloudplatform.sap.com>, click **Login**. You may need to choose a username, which has to be unique, and confirm the terms and conditions.

Afterwards, click **Register**, submit your confirmation for the end user license agreement, and click the button to start your trial.

A second form appears where you need to confirm the terms and conditions and click **Register**.
![Upgrade account](6-4-account-upgrade.png)

You now see the SAP Cloud Platform cockpit. Click the **Cloud Foundry Trial** tile.
![SAP Cloud Platform cockpit](6-5-cockpit-homepage.png)

In the dialog, choose **US Central (IA) - GCP** from the **Region** dropdown menu and click **OK**. The creation of the trial account may take a few seconds.
![Start trial](6-6-start-trial.png)
![Creating trial](6-7-trial-processing.png)

Afterwards, click **Go to Space**.
![Go to space](6-8-go-to-space.png)

Return to the development environment.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Push application to SAP Cloud Platform)]

Look at the terminal of the development environment. The packaging of the application should have finished by now with a success message.
![Successful build](7-1-build-success.png)

You can now push the packaged application to your Cloud Foundry account on SAP Cloud Platform.

Type `cf login` into the terminal and provide your e-mail address and the password for your SAP Cloud Platform account that you have just created.
You will see that your trial account and space have automatically been targeted.
![Login to Cloud Foundry](7-2-cf-login.png)

Push the application to your account and space by typing `cf push` into the terminal. This command will create the app on Cloud Foundry and upload the package.
![Push to Cloud Foundry](7-3-cf-push-begin.png)

As the command will take one or two minutes, feel free to browse the source code of the application in the development environment in the mean time.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Browse source code of application (optional))]

While the application is starting, take a look at the other Java classes in the project, for example:

  - `CreateAddressCommand` to see how easy it is to create business objects in SAP S/4HANA using the SAP S/4HANA Cloud SDK
  - `GetSingleBusinessPartnerByIdCommand` to see a more advanced OData request

Once the `cf push` command has completed, you can continue with the next step.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Visit app on SAP Cloud Platform)]

After the `cf push` command has finished, you can see the URL of the newly created application in the output of the command.

Select the URL and copy it to the clipboard.
![Application started](9-1-cf-push-completed.png)

Open the browser and paste the URL that you have just copied. This will display the start page of your application.

Click the link to visit the **Address Manager**.
![Start page on SAP Cloud Platform](9-2-start-page.png)

You now see the Address Manager app that was previously launched locally running on SAP Cloud Platform.

If you want to receive your gift, look for the special business partner and note down his address. Present this information to our staff at the booth to redeem your gift.
![App running on SAP Cloud Platform](9-3-app-on-scp.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Log out from all accounts)]

This completes the tutorial.

At the end, make sure to log out from your SAP account in the browser and close the browser.

In the terminal of the development environment, type `cf logout` to log out from your SAP Cloud Platform account.

![Logout from SAP Cloud Platform](10-1-cf-logout.png)

[ACCORDION-END]

---
