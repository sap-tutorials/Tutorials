---
author_name: Haoyue Feng
author_profile: https://github.com/Fenghaoyue
title: Enable Multi-User for Your Android Application
description: Learn how to configure a wizard-generated application to support multi-user feature.
auto_validation: true
time: 60
tags: [ tutorial>beginner, operating-system>android, topic>mobile, products>sap-business-technology-platform]
primary_tag: products>sap-btp-sdk-for-android
---

## Prerequisites
- You completed [Try Out the SAP BTP SDK Wizard for Android](cp-sdk-android-wizard-app).
- You completed [Offline-Enable Your Android Application](cp-sdk-android-wizard-app-offline).

## Details
### You will learn
  - How the Android SDK supports multi-user feature
  - How to enable multi-user feature for an online app
  - How to enable multi-user feature for an offline app
  - Several multi-user related APIs

---

[ACCORDION-BEGIN [Step 1: ](Intro to Android SDK multi-user feature)]

The Flows component of SAP BTP SDK for Android provides the following functions to enable multi-user feature for your application:

- Handle onboarding process for multiple users.

- Handle user information and user data management.

- To enable multi-user for an online application developed using Flows component, you only need need turn on `multipleUserMode` in `FlowContext` class.

Following cases are not supported in multi-user mode:

- Biometric authentication is not supported. The biometric screen will not be shown in onboarding or unlock process.

- Authentication type `NoAuth` is not supported. Even if multi-user mode is turned on, the application with `NoAuth` authentication type will go to single user mode.

- `No passcode policy` is not supported. A default passcode policy is used if server disables it.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable multi-user for wizard generated online application)]

[OPTION BEGIN [Java]]

1.  Open the previously created project from wizard generation.

2.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WelcomeActivity`**, to open `WelcomeActivity.java`.

3.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`startFlow`**, to move to the `startFlow` method. For the **`FlowContext`** instance, change the parameter of the method **`setMultipleUserMode`** from **`false`** to **`true`**.:

    ```Java
    FlowContext flowContext = new FlowContextBuilder()
                .setApplication(appConfig)
                .setMultipleUserMode(true)
                .setFlowStateListener(new WizardFlowStateListener(
                        (SAPWizardApplication) context.getApplication()))
                .build();
    ```

    Notice that the setting will only take effect with the very first user onboarding. Once a user is onboarded,  
    this setting will be saved into local database, all flows afterwards will use the same setting from the database and ignore the one inside **`flowContext`**. To change this setting, you need reset application to bring up onboarding process, and the new setting will be updated into local database after onboarding.

4.  Re-run (quit first) the app and notice that the onboarding process is same as single user mode, except that no biometric authentication screen is shown. After onboarding, put the app in background until the unlock screen appears. In multi-user mode, there is a button **SWTICH OR ADD USER** at the bottom of the screen.

    !![Sign in screen](sign-in-screen.png)

    Click the button, the user list is shown. You can either select existing user from the list or click the **ADD USER** icon on the right top of the screen. This will start the onboarding process for the new user.

    !![User list screen](user-screen.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Open the previously created project from wizard generation.

2.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WelcomeActivity`**, to open `WelcomeActivity.kt`.

3.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`startFlow`**, to move to the `startFlow` method. For the **`FlowContext`** instance, change the parameter of the method **`setMultipleUserMode`** from **`false`** to **`true`**.:

    ```Kotlin
    val flowContext =
                FlowContextBuilder()
                    .setApplication(appConfig)
                    .setMultipleUserMode(true)
                    .setFlowStateListener(WizardFlowStateListener(activity.application as SAPWizardApplication))
                    .build()
    ```

    Notice that the setting will only take effect with the very first user onboarding. Once a user is onboarded, this setting will be saved into local database, all flows afterwards will use the same setting from the database and ignore the one inside **`flowContext`**. To change this setting, you need reset application to bring up onboarding process, and the new setting will be updated into local database after onboarding.

4.  Re-run (quit first) the app and notice that the onboarding process is same as single user mode, except that no biometric authentication screen is shown. After onboarding, put the app in background until the sign in screen appears. In multi-user mode, there is a button **SWTICH OR ADD USER** at the bottom of the screen.

    !![Sign in screen](sign-in-screen.png)

    Click the button, the user list is shown. You can either select existing user from the list or click the **ADD USER** icon on the right top of the screen. This will start the onboarding process for the new user.

    !![User list screen](user-screen.png)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enable multi-user for wizard generated offline app)]

[OPTION BEGIN [Java]]

1.  For offline application, enable **Allow Upload of Pending Changes from Previous User** option on server side first. Go to the [SAP Mobile Services cockpit](https://mobile-service-cockpit-web.cfapps.eu10.hana.ondemand.com/) and select your application from the application list. Click **Mobile Settings Exchange** in the assigned features list:

    !![Cockpit app screen](cockpit-app.png)

    Scroll down to the bottom of **Client Configuration** tab and enable  **Allow Upload of Pending Changes from Previous User** option

    !![Cockpit app setting screen](cockpit-app-setting.png)

2.  Open the previously created project from wizard generation. Enable **`setMultipleUserMode`** for **`FlowContext`** instance same as online app.

3.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`OfflineWorkerUtil`**, to open `OfflineWorkerUtil.java`.

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`initializeOffline`**, to move to the `initializeOffline` method. Notice that for the **`OfflineODataParameters`** instance, value of parameter **`isForceUploadOnUserSwitch`** is set based on the value of `runtimeMultipleUserMode`. This value is retrieved from the API call **`UserSecureStoreDelegate.getInstance().getRuntimeMultipleUserModeAsync()`**

    !![Parameter setting Java](offline-para-java.png)

5.  Different from single user scenario, encryption key is not generated by client code, but retrieved from server by SDK. Client code can get the key by API call **`UserSecureStoreDelegate.getInstance().getOfflineEncryptionKey()`**

6.  Re-run (quit first) the app. Notice that the onboarding process and add/switch user process are same as online app.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  For offline application, enable **Allow Upload of Pending Changes from Previous User** option on server side first. Go to the [SAP Mobile Services cockpit](https://mobile-service-cockpit-web.cfapps.eu10.hana.ondemand.com/) and select your application from the application list. Click **Mobile Settings Exchange** in the assigned features list:

    !![Cockpit app screen](cockpit-app.png)

    Scroll down to the bottom of **Client Configuration** tab and enable  **Allow Upload of Pending Changes from Previous User** option

    !![Cockpit app setting screen](cockpit-app-setting.png)

2.  Open the previously created project from wizard generation. Enable **`setMultipleUserMode`** for **`FlowContext`** instance same as online app.

3.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`OfflineWorkerUtil`**, to open `OfflineWorkerUtil.kt`.

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`initializeOffline`**, to move to the `initializeOffline` method. Notice that for the **`OfflineODataParameters`** instance, value of parameter **`isForceUploadOnUserSwitch`** is set based on the value of `runtimeMultipleUserMode`. This value is retrieved from the API call **`UserSecureStoreDelegate.getInstance().getRuntimeMultipleUserMode()!!`**

    !![Parameter setting Kotlin](offline-para-kotlin.png)

5.  Different from single user scenario, encryption key is not generated by client code, but retrieved from server by SDK. Client code can get the key by API call **`UserSecureStoreDelegate.getInstance().getOfflineEncryptionKey()`**

6.  Re-run (quit first) the app. Notice that the onboarding process and add/switch user process are same as online app.

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Handle offline pending changes when switch users)]

[OPTION BEGIN [Java]]

1.  In offline multi-user scenario, when a user made changes to local offline store, the changes may not be uploaded to server when the device is handed over to another user. After current user clicking **SWTICH OR ADD USER** button to do sign in or onboarding, the pending changes will be uploaded to server automatically.

2.  If there is error during synchronization, a screen will be shown to notify the synchronization failure.

    If synchronization failed due to no internet connection, a network error screen will be shown.

    !![Offline network error screen](offline-network-error.png)

    If synchronization failed due to a transaction issue, a transaction error screen will be shown.

    !![Offline transaction error screen](offline-transaction-error.png)

3.  SDK provides UI component for the two screens and application can reference the screens for error handling.

    Open the previously created offline project from wizard generation, press **`Shift`** twice and type **`activity_main_business.xml`** to open `res\layout\activity_main_business.xml`. To reference the two screens, the layout XML file includes **`com.sap.cloud.mobile.fiori.onboarding.OfflineNetworkErrorScreen`** and **`com.sap.cloud.mobile.fiori.onboarding.OfflineTransactionIssueScreen`**.

    !![Layout XML file](main-activity-xml.png)

4.  For **`OfflineNetworkErrorScreen`**, client code implements the logic for button click events.   

    On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`MainBusinessActivity`** to open `MainBusinessActivity.java`.

    On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`offlineNetworkErrorAction`**, to move to the `offlineNetworkErrorAction` method. When the network error happens, make the **`OfflineNetworkErrorScreen`** visible and provide your logic for button click event.

    !![Network error Java](main-activity-network-java.png)

5.  For **`OfflineTransactionIssueScreen`**, client code needs to set the user information of previous user and implement the logic for button click events.

    On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`offlineTransactionIssueAction`**, to move to the `offlineTransactionIssueAction` method. When the transaction error happens, make the **`OfflineTransactionIssueScreen`** visible, set information of previous user and provide your logic for button click event. To get the information of previous user, call the `getPreviousUser` method of class `OfflineODataProvider` to get user id and then call the `getUserInfoByIdAsync` method of class `UserSecureStoreDelegate` to get user information.

    !![Transaction error Java](main-activity-transaction-java.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  In offline multi-user scenario, when a user made changes to local offline store, the changes may not be uploaded to server when the device is handed over to another user. After current user clicking **SWTICH OR ADD USER** button to do sign in or onboarding, the pending changes will be uploaded to server automatically.

2.  When switch user for an offline application, the pending changes to local offline store created by previous user will be uploaded to server automatically. But if there is error during synchronization, a screen will be shown to notify the synchronization failure.

    If synchronization failed due to no internet connection, a network error screen will be shown.

    !![Offline network error screen](offline-network-error.png)

    If synchronization failed due to a transaction issue, a transaction error screen will be shown.

    !![Offline transaction error screen](offline-transaction-error.png)

3.  SDK provides UI component for the two screens and application can reference the screens for error handling.

    Open the previously created offline project from wizard generation, press **`Shift`** twice and type **`activity_main_business.xml`** to open `res\layout\activity_main_business.xml`. To reference the two screens, the layout XML file includes **`com.sap.cloud.mobile.fiori.onboarding.OfflineNetworkErrorScreen`** and **`com.sap.cloud.mobile.fiori.onboarding.OfflineTransactionIssueScreen`**.

    !![Layout XML file](main-activity-xml.png)

4.  For **`OfflineNetworkErrorScreen`**, client code implements the logic for button click events.   

    On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`MainBusinessActivity`** to open `MainBusinessActivity.kt`.

    On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`offlineNetworkErrorAction`**, to move to the `offlineNetworkErrorAction` method. When the network error happens, make the **`OfflineNetworkErrorScreen`** visible and provide your logic for button click event.

    !![Network error Kotlin](main-activity-network-kt.png)

5.  For **`OfflineTransactionIssueScreen`**, client code needs to set the user information of previous user and implement the logic for button click events.

    On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`offlineTransactionIssueAction`**, to move to the `offlineTransactionIssueAction` method. When the transaction error happens, make the **`OfflineTransactionIssueScreen`** visible, set information of previous user and provide your logic for button click event. To get the information of previous user, call the `getPreviousUser` method of class `OfflineODataProvider` to get user id and then call the `getUserInfoByIdAsync` method of class `UserSecureStoreDelegate` to get user information.

    !![Transaction error Kotlin](main-activity-transaction-kt.png)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](APIs to help customize multi-user app)]

[OPTION BEGIN [Java]]

1.  Flows component exposes two APIs in **`UserSecureStoreDelegate`** class for you to get user information by ID, such as user name and email:

    **`fun getUserInfoByIdAsync(userId: String) : DeviceUser?`**

    **`fun getUserInfoById(userId: String): DeviceUser?`**

    The function **`getUserInfoByIdAsync`** is mainly for Java code to use. Notice that this function can only be called after onboarding or restore flow.

2.  After onboarding, the setting for multi-user enablement is saved into local database. To get this setting, **`UserSecureStoreDelegate`** class exposes following API:

    **`suspend fun getRuntimeMultipleUserMode(): Boolean?`**

    **`fun getRuntimeMultipleUserModeAsync(): Boolean?`**

    The function **`getRuntimeMultipleUserModeAsync`** is mainly for Java code to use.

3.  Flows component exposes two APIs in **`FlowActionHandler`** class for you to obfuscate user name and email displayed on sign in screen:

    **`open fun obfuscateUserName(name: String): String`**

    **`fun obfuscateEmail(email: String): String`**

    Notice that a default obfuscate algorithm is provided in the APIs. You can override the APIs to provide your own obfuscate algorithm.

4.  The **`FlowStateListener`** class provides one callback **`onUserSwitched(newUser: DeviceUser, oldUser: DeviceUser?)`** for you to handle user switch event.

    As a sample implementation of this callback, you can examine a wizard generated offline app. In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WizardFlowStateListener`**, to open `WizardFlowStateListener.java`.

    On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onUserSwitched`**, to move to the `onUserSwitched` method. Examine the code and notice that it does some clean and reset work:

    !![User switch method](offline-user-switch-java.png)

    You can provide your own logic in this callback when user switch event is notified.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Flows component exposes two APIs in **`UserSecureStoreDelegate`** class for you to get user information by ID, such as user name and email:

    **`fun getUserInfoByIdAsync(userId: String) : DeviceUser?`**

    **`fun getUserInfoById(userId: String): DeviceUser?`**

    The function **`getUserInfoByIdAsync`** is mainly for Java code to use. Notice that this function can only be called after onboarding or restore flow.

2.  After onboarding, the setting for multi-user enablement is saved into local database. To get this setting, **`UserSecureStoreDelegate`** class exposes following API:

    **`suspend fun getRuntimeMultipleUserMode(): Boolean?`**

    **`fun getRuntimeMultipleUserModeAsync(): Boolean?`**

    The function **`getRuntimeMultipleUserModeAsync`** is mainly for Java code to use.

3.  Flows component exposes two APIs in **`FlowActionHandler`** class for you to obfuscate user name and email displayed on sign in screen:

    **`open fun obfuscateUserName(name: String): String`**

    **`fun obfuscateEmail(email: String): String`**

    Notice that a default obfuscate algorithm is provided in the APIs. You can override the APIs to provide your own obfuscate algorithm.

4.  The **`FlowStateListener`** class provides one callback **`onUserSwitched(newUser: DeviceUser, oldUser: DeviceUser?)`** for you to handle user switch event.

    As a sample implementation of this callback, you can examine a wizard generated offline app. In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WizardFlowStateListener`**, to open `WizardFlowStateListener.kt`.

    On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onUserSwitched`**, to move to the `onUserSwitched` method. Examine the code and notice that it does some clean and reset work:

    !![User switch method](offline-user-switch-kotlin.png)

    You can provide your own logic in this callback when user switch event is notified.

[OPTION END]

[DONE]
[ACCORDION-END]

---
