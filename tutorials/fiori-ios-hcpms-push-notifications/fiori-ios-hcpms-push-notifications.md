---
title: Push notifications
description: Implement push notifications into your application with SAP Cloud Platform mobile service for development and operations.
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>intermediate, topic>mobile, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Development machine:** Access to a Mac computer
 - **Apple ID:** A paid Apple developer account is required
 - **Tutorials:** [List Report Floorplan](https://www.sap.com/developer/tutorials/fiori-ios-scpms-floorplan.html)

## Next Steps
 - [Application logging and tracing](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-application-logging.html)

## Details
### You will learn  
In this tutorial, you will implement push notifications into your application, and use the SAP Cloud Platform mobile service for development and operations push notification configuration settings.

### Time to Complete
**30 Min**.

---

You can use the Apple Push Notification Service to propagate information from the backend to the device. In this tutorial you use the native iOS push services to enable APNS for your iOS application. To enable your application for push notifications, you need to carry out the following tasks:

*  Create CSR (Certificate Signing Request) file
*  Create an App ID
*  Create provisioning profile
*  Update your application to use Push Notifications
*  Configure APNS in SAP Cloud Platform cockpit

> In order to implement Push Notifications, a paid Apple developer account is required. Students or other developers with a personal Apple ID for their team will not be able to use push notifications, because they won't have access to the Developer Portal to generate the required certificate.

[ACCORDION-BEGIN [Step 1: ](Create CSR (Certificate Signing Request) file)]

First, we create the **CSR file**.

On your Mac, open the **Keychain Access** application, and navigate to **Keychain Access > Certificate Assistant > Request a Certificate From a Certificate Authority...**

![Keychain Access menu](fiori-ios-hcpms-push-notifications-01.png)

In the dialog, enter the email address you use which is associated with your Apple Developer account. Also, make sure you tick the **Request is saved to disk** option.

![Keychain Access dialog](fiori-ios-hcpms-push-notifications-02.png)

Click **Continue**.

Choose a folder to store the certificate -- it is good practice to store generated files in a separate folder for each project -- and click **Save**.

Once you see a dialog saying the certificate is saved successfully, click **Done** to finish.

![Keychain Access dialog](fiori-ios-hcpms-push-notifications-03.png)

The certificate you just saved will be used to sign other certificates in the Apple Developer website.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an App ID)]

Now, you need to create a new App ID in the Apple Developer Member Center. This App ID is used to identify your app and ensures the Apple Push Notification Servers properly push notifications to your app.

Open a browser, go to [https://developer.apple.com/membercenter/](https://developer.apple.com/membercenter/), and click the **Certificates, IDs and Profiles** link.

![App ID creation](fiori-ios-hcpms-push-notifications-04.png)

Navigate to **Identifiers > App IDs** and click the **Plus-sign** button on the top right:

![App ID creation](fiori-ios-hcpms-push-notifications-05.png)

Enter the following details:

|Field|Value|
|----|----|
| App ID Description | `SAP Cloud Platform for iOS Demo` |
| App ID Prefix | This should be prefixed with your Apple Developer's Team ID |
| Explicit App ID > Bundle ID | `com.sap.tutorial.demoapp.Demo` |

![App ID creation](fiori-ios-hcpms-push-notifications-06.png)

> You must set **Explicit App ID** to enable Push Notifications

Now, scroll down to the **App Services** area, and tick **Push Notifications**:

![App ID creation](fiori-ios-hcpms-push-notifications-07.png)

Click **Continue**. In the next screen, review your settings, and click **Register**.

You should now see a **Registration Complete** message. Click **Done**. Your App ID should now be listed:

![App ID creation](fiori-ios-hcpms-push-notifications-09.png)

Click on the **SAP Cloud Platform for iOS Demo** App ID. The panel should expand, and list the **Push Notification** as **Configurable** for both Development as well as Distribution (Productive) use.

![App ID creation](fiori-ios-hcpms-push-notifications-10.png)

> In this tutorial, you will configure it for Development use. The steps for Distribution (productive) use are similar.

Click the **Edit** button at the bottom of the panel, and scroll to the **Push Notification** section. Click the **Create Certificate...** button on the **Development SSL Certificate** panel.

![App ID creation](fiori-ios-hcpms-push-notifications-11.png)

In the screen that follows, the steps to create a CSR are outlined. Since you already created a CSR, you can click **Continue**.

In the next screen, click the **Choose File...** button and browse to the **CSR** file you created in Step 1 of this tutorial.

![App ID creation](fiori-ios-hcpms-push-notifications-12.png)

Click **Continue**.

In the next screen, you must download the generated certificate:

![App ID creation](fiori-ios-hcpms-push-notifications-13.png)

Click the **Download** button and save the resulting `aps_development.cer` file. Open the file, and it will be added to the **Keychain Access** in the **login** keychain:

![App ID creation](fiori-ios-hcpms-push-notifications-14.png)

With the certificate selected, from the top menu choose **File > Export items...**

![App ID creation](fiori-ios-hcpms-push-notifications-15.png)

In the dialog, save it as `.p12` format to the folder you saved the CSR file.

![App ID creation](fiori-ios-hcpms-push-notifications-16.png)

If it asks for specifying a password, provide one and click **OK**.

![App ID creation](fiori-ios-hcpms-push-notifications-17.png)

> While you have the option to leave the password empty, you must provide a password when configuring the certificate for use with SAP Cloud Platform mobile service for development and operations.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import certificate into SAP Cloud Platform mobile service for development and operations)]

Open your **SAP Cloud Platform Cockpit** and navigate to **Services > Development and Operations > Configure Development & Operations > Roles**

Assign your user the **Notification User** role

![App ID creation](fiori-ios-hcpms-push-notifications-31.png)

After you have assigned the role, navigate back to to **Development & Operations** and click **Go to Service** to open **SAP Cloud Platform mobile service for development and operations**

Navigate to **Applications**, select your application and from the context menu select **Configure** and switch to the **Push** tab.

Scroll down a bit to the **Apple** panel and provide the following details:

| Field | Value |
|----|----|
| APNS Endpoint | Select `Sandbox` |
| Certificate | Browse to the `.p12` certificate you just exported |
| Password | Enter the password you provided during the export |

![App ID creation](fiori-ios-hcpms-push-notifications-32.png)     

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create Provisioning Profile)]

Next, you need to create a **Provisioning Profile** for Development usage which is used to code sign your application.

Open a browser to the **Apple Developer** page, and navigate to **Certificates, IDs and Profiles > Provisioning Profiles > Development** and click the **Plus** button in the top-right.

Under **Development**, select **iOS App Development** and click **Continue**.

![App ID creation](fiori-ios-hcpms-push-notifications-18.png)

Select your demo application from the drop-down and click **Continue**.

![App ID creation](fiori-ios-hcpms-push-notifications-19.png)

Select your **iOS Development Certificate** from the list and click **Continue**.

![App ID creation](fiori-ios-hcpms-push-notifications-20.png)

Select the **device you registered for development** from the list and click **Continue**.

![App ID creation](fiori-ios-hcpms-push-notifications-21.png)

Enter a name for your **Provisioning Profile** and click **Continue**.

![App ID creation](fiori-ios-hcpms-push-notifications-22.png)

Your **Provisioning Profile** is now ready. Click the **Download** button and save the file locally.

![App ID creation](fiori-ios-hcpms-push-notifications-23.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Examine your application's Push Notifications code)]

Open your application in Xcode, select the project name from the **Project Navigator** and switch to the **General** tab. Your team, provisioning profile and signing certificate should be listed:

![App ID creation](fiori-ios-hcpms-push-notifications-25.png)

Switch to the **Capabilities** tab, and enable **Push Notifications** by flipping the switch:

![App ID creation](fiori-ios-hcpms-push-notifications-26.png)

Open the `AppDelegate.swift` file and locate the `func application(_ application: UIApplication, willFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]? = nil) -> Bool` function.

Examine the contents of this function:

```swift
func application(_ application: UIApplication, willFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]? = nil) -> Bool {
    UIApplication.shared.registerForRemoteNotifications()
    let center = UNUserNotificationCenter.current()
    center.requestAuthorization(options: [.alert, .badge, .sound]) { granted, error in
        // Enable or disable features based on authorization.
    }
    center.delegate = self
    return true
}
```

First, the device registers for remote notifications by calling the method `registerForRemoteNotification`. Then, you have specified the types of notifications your app will support, as well as added a reference to the notification settings class, which enables your application for the push notifications to be received. This will result in your application to display the **"App Name" Would Like to Send You Notifications** confirmation dialog.

Let's look at the `registerForRemoteNotification` method. In this method, a reference to the SDK Foundation class `SAPcpmsRemoteNotificationClient` is made. With this API, you will upload the received device token to SAP Cloud Platform mobile service for development and operations:

```swift
func registerForRemoteNotification() -> Void {
    guard let deviceToken = self.deviceToken else {
        // Device token has not been acquired
        return
    }

    self.remoteNotificationClient = SAPcpmsRemoteNotificationClient(sapURLSession: self.urlSession!, settingsParameters: Constants.configurationParameters)
    self.remoteNotificationClient.registerDeviceToken(deviceToken, completionHandler: { (error: Error?) -> Void in
        if error != nil {
            self.logger.error("Register DeviceToken failed")
        } else {
            self.logger.info("Register DeviceToken succeeded")
        }
    })
}
```

With these two methods, you can receive notifications, but your application also needs to respond to them in a useful manner. Therefor, two delegate methods are implemented:

```swift
// Called to let your app know which action was selected by the user for a given notification.
func userNotificationCenter(_ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse, withCompletionHandler completionHandler: @escaping() -> Void) {
    self.logger.info("App opened via user selecting notification: \(response.notification.request.content.body)")
    // Here is where you want to take action to handle the notification, maybe navigate the user to a given screen.
    completionHandler()
}

// Called when a notification is delivered to a foreground app.
func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification, withCompletionHandler completionHandler: @escaping(UNNotificationPresentationOptions) -> Void) {
    self.logger.info("Remote Notification arrived while app was in forground: \(notification.request.content.body)")
    // Currently we are presenting the notification alert as the application were in the backround.
    // If you have handled the notification and do not want to display an alert, call the completionHandle with empty options: completionHandler([])
    completionHandler([.alert, .sound])
}
```

If you now run your application from your device, you will notice it will first ask permission to display notifications:

![App ID creation](fiori-ios-hcpms-push-notifications-29.PNG)

Go back to your **SAP Cloud Platform mobile service for development and operations Cockpit** and navigate to **Applications**. Click the **Action** icon next to your application, and select **Push** from the context menu.

In the **Push Desk**, find your device using the filter options, and enter the following content in the **Message** area:

```
SAPcpms says: 'How do you like the tutorials so far?    
```

If you click **Send**, your device should now receive a new push notification:

![App ID creation](fiori-ios-hcpms-push-notifications-30.PNG)

> If you happen to have an Apple Watch, and have it configured to display push notifications received on your iPhone, the push notification you have sent via SAP Cloud Platform mobile service for development and operations will also be displayed on your Apple Watch:

> ![App ID creation](fiori-ios-hcpms-push-notifications-34.png)

[DONE]
[ACCORDION-END]

## Next Steps
- [Application logging and tracing](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-application-logging.html)
