---
title: Create Universal Links for iOS Apps
description: Modify an SAP Cloud Platform SDK for iOS app to accept universal links. This allows users to navigate to your app via a URL sent by email or from a website.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>intermediate, operating-system>ios, topic>mobile, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---

## Prerequisites  
 - **Proficiency:** intermediate
 - **Tutorials:** [Sign up for a free trial account on SAP Cloud Platform](https://developers.sap.com/tutorials/hcp-create-trial-account.html) and [Enable SAP Cloud Platform mobile service for development and operations](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html)
 - **Development environment:** Apple iMac, MacBook or MacBook Pro running Xcode 9.3 or higher
 - **SAP Cloud Platform SDK for iOS:** Version 2.2

## Details
### You will learn  
In this tutorial, you will learn how to create universal links for use in an iOS app created using the SAP Cloud Platform SDK for iOS Assistant. With universal links, iOS users can tap a link on a website or in an email, and get redirected to your app immediately without opening the link in Safari.

To prepare your app to support universal links, you will create a new (or modify an existing) app ID on [Apple Developer Portal](https://developer.apple.com/), modify the application definition on SAP Cloud Platform mobile service for development and operations, and modify the app's entitlements in Xcode.

For this tutorial, you can use any iOS app you have previously created with the SAP Cloud Platform SDK for iOS Assistant. However, the screenshots and parameters used in this tutorial are taken from the `OfflineDemo` app created in tutorial [Create an iOS App with Offline Capabilities](https://developers.sap.com/tutorials/fiori-ios-scpms-offline-odata.html). If you want to follow this tutorial to the letter, make sure you have finished creating the `OfflineDemo` app.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Register the app on Apple Developer Portal)]

Log in to the Apple Developer Portal at [https://developer.apple.com/](https://developer.apple.com/) and navigate to **Certificates, Identifiers & Profiles**. From the pane on the left, navigate to **Identifiers > App IDs**:

![Apple Developer Portal](fiori-ios-scpms-universal-link-01.png)

Click the **Plus** button in the top right to register an App ID.

Provide the following details:

| Field Name | Value |
|----|----|
| App ID Description | `OfflineDemo` |
| App ID Prefix | This is already filled in. Copy and store this value as you will need it later. |
| Explicit App ID | selected |
| Bundle ID | `com.sap.tutorials.demoapp.offlinedemo` |

![Apple Developer Portal](fiori-ios-scpms-universal-link-02.png)

The **App ID Description** can be anything. The **Bundle ID** can be found in your Xcode project settings, under the **General** tab at the **Identity** panel:

![Apple Developer Portal](fiori-ios-scpms-universal-link-03.png)

After you have provided the App ID Description and Bundle ID, scroll down to **App Services**.

Enable the **Associated Domains** service:

![Apple Developer Portal](fiori-ios-scpms-universal-link-04.png)

Click **Continue** to proceed.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Confirm the new App ID)]

In the next screen, review the settings you have provided:

![Apple Developer Portal](fiori-ios-scpms-universal-link-05.png)

Scroll down and click **Register** when done.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Complete the App ID registration)]

The registration is now complete:

![Apple Developer Portal](fiori-ios-scpms-universal-link-06.png)

Scroll down and click **Done** to dismiss the wizard.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Define application link on SAP Cloud Platform)]

Log on to SAP Cloud Platform mobile service for development and operations cockpit at `https://hcpmsadmin-<your_user_id>trial.dispatcher.hanatrial.ondemand.com/` and navigate to **Mobile Applications > Native/Hybrid**:

![Define application link](fiori-ios-scpms-universal-link-07.png)

Click on the application you want to have universal link support, and from the tab bar, click **APIs**:

![Define application link](fiori-ios-scpms-universal-link-08.png)

By default, there should be just one QR code listed.

Now navigate to **Application Links**:

![Define application link](fiori-ios-scpms-universal-link-09.png)

Click the **Pencil** button in the top right to edit the entry, and provide the following details:

| Field Name | Value |
|----|----|
| Enabled | checked |
| Team ID | `<your_team_id>` |
| Bundle ID | `com.sap.tutorials.demoapp.offlinedemo` |

![Define application link](fiori-ios-scpms-universal-link-10.png)

The Team ID you have copied in **Step 1**. You can also find it by going to the **Apple Developer Portal** and navigate to **Membership**.

Click **OK** to save your settings.

Navigate back to **APIs**. You should now see two QR codes, as well as the generated deep link:

![Define application link](fiori-ios-scpms-universal-link-11.png)

Leave the page open, you will need the generated URL later.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Enable Associated Domains capability to your app)]

Open your app project in Xcode. In your project's settings, click the **Capabilities** tab, and enable **Associated Domains**.

Enter the following domain:

| Field Name | Value |
|----|----|
| Domain | `applinks:hcpms-<your_user_id>trial.hanatrial.ondemand.com` |

Please note the `applinks:` prefix. It is important you enter it exactly as stated:

![Define application link](fiori-ios-scpms-universal-link-12.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Build and deploy the app)]

If you now build the app, you will notice an `<app_name>.entitlements` file is created, containing the just added capability:

![Build and deploy the app](fiori-ios-scpms-universal-link-13.png)

Continue and deploy the app to your physical device.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Check the AASA file)]

Once the app is deployed onto your device, it is now running with the added capability.

The way the universal link work is by the associated domain.

In **Step 4** you have defined the application link on SAP Cloud Platform mobile service for development and operations. This creates an `apple-app-site-association` file. This file associates your SAP Cloud Platform mobile service for development and operations account domain with your iOS app.

The generated `apple-app-site-association` file contains a JSON structure with the App ID and URL path which is used as universal link.

Open a browser to `https://hcpms-<your_user_id>trial.hanatrial.ondemand.com/.well-known/apple-app-site-association` and you should see the contents of the file:

![Build and deploy the app](fiori-ios-scpms-universal-link-14.png)

> Note the URL to the `apple-app-site-association` file starts with `https://hcpms-<your_user_id>trial`, and **not** `https://hcpmsadmin-<your_user_id>trial`

The path listed is the path you will use as universal link in the next step. The asterisk at the end denotes anything. You can for instance add a product id which could directly open an entity with that product ID.

It is up to the app developer to do something meaningful with the provided context. In the next step, you will simply detect the app is launched via a universal link, and log the URL context to the console.

[VALIDATE_7]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Process the universal link context)]

Open your app's `AppDelegate.swift` file, and add the following method:

```swift
func application(_ application: UIApplication, continue userActivity: NSUserActivity, restorationHandler: @escaping ([Any]?) -> Void) -> Bool {

    logger.info("SCPms Universal Link called: ")

    if userActivity.activityType == NSUserActivityTypeBrowsingWeb {
        let url = userActivity.webpageURL!
        logger.info(url.lastPathComponent)
        // here you can do whatever you want with the URL components
    }
    return true
}
```

When you click on a universal link, this hook will be called. The `userActivity` object contains a `webpageURL` property, which holds the universal link you clicked.

If you provide some context to the universal link URL, it will then be printed to the console by retrieving the `lastPathComponent` property of the URL.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Build and deploy the app)]

Build and deploy the app to your physical device. After it has started, you can dismiss it to the background.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Try the universal link)]

The easiest way of testing the universal link is to send it via email.

Send an email to yourself containing the link `https://hcpms-<your_user_id>trial.hanatrial.ondemand.com/mobileservices/deeplinks/com.sap.tutorials.demoapp.OfflineDemo/start`.

In this example, you simply provide the context `start`, but it can be anything more meaningful of course.

Open the email on your device and click the link:

![Try the universal link](fiori-ios-scpms-universal-link-15.png)

If you now click it, it should directly open your app without going to Safari first. If you look in the Xcode console, you see it logged the `start` context it received via the `NSUserActivity` hook:

![Try the universal link](fiori-ios-scpms-universal-link-16.png)


[DONE]
[ACCORDION-END]


---
