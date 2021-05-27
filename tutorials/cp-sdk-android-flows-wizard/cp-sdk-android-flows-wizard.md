---
author_name: Haoyue Feng
author_profile: https://github.com/Fenghaoyue
title: Get Familiar with Flows Component by Wizard Generated Application
description: Use the application generated from SAP BTP Android SDK Wizard to demonstrate the usage of Flows component.
auto_validation: true
time: 60
tags: [ tutorial>beginner, operating-system>android, topic>mobile, products>sap-business-technology-platform]
primary_tag: products>sap-btp-sdk-for-android
---

## Prerequisites
- You completed [Try Out the SAP BTP SDK Wizard for Android](cp-sdk-android-wizard-app).

## Details
### You will learn
  - How to import Flows component in the application project
  - How the Flows APIs are used in wizard-generated application
  - Which screens are provided by Flows component

The [Flows](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/onboarding/android/newflows/Overview.html) component is a new feature of the SAP BTP SDK for Android 3.1 and new features are continually added to the library.  It can reduce and simplify the code in an application related to onboarding process, passcode management, restore and reset scenario. Besides default implementation, it also provides options for UI customization and extension points for client code to contribute its own logic. A great overview is provided at [Onboarding Using the Flow Component of the SAP SDK for Android](https://blogs.sap.com/2021/02/02/onboarding-using-the-flow-component-of-the-sap-sdk-for-android/).

---

[ACCORDION-BEGIN [Step 1: ](Examine dependency to Flows component)]

1.  Open the project you previously created using the SAP BTP SDK Wizard for Android.

2.  Press **`Shift`** twice and type **`build.gradle`** to open the `build.gradle` file of the app module.

3.  Go to the `dependencies` part and find the dependency declaration to Flows component. **`onboarding`** contains the UI screens and **`flowsv2`** contains the Flows API and logic.

    !![App gradle file](app-gradle-file.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Explore usage of Flows API in wizard-generated application)]

[OPTION BEGIN [Java]]

1.  Open the project you previously created using the SAP BTP SDK Wizard for Android.

2.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WelcomeActivity`** to open `WelcomeActivity.java`.

3.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`startFlow`** to move to the `startFlow` method. To start the onboarding process, firstly create a **`FlowContext`** instance which contains the information required by the flow, such as, the application information, the status of multi-user mode, flow state listener, etc. Then call the `start` method of the `Flow` class to start the onboarding flow, and the entire onboarding process can be handled automatically.

    !![Flow starting method](flow-starting-java.png)

    Notice that by default, the type of a flow is **`FlowType.ONBOARDING`**. To start other type of flow, such as a `Change Passcode` flow, client code must specify the flow type in the **`FlowContext`** instance. Besides the parameters mentioned above, there are also many other parameters in the **`FlowContext`** class for client code to customize a flow. When the value of the parameters not specified in client code, default value will be used. The details of all parameters in the **`FlowContext`** class will be explained in subsequent tutorials.

4.  **`FlowStateListener`** is a parameter in the **`FlowContext`** class. With the callbacks defined in the listener, client code can insert logic at certain points during the flow process, for example, after client policy is retrieved, client code can get the information that will be used by the app. In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WizardFlowStateListener`** to open `WizardFlowStateListener.java`. This class is a sample implementation for the callbacks defined in **`FlowStateListener`** class. The details of **`FlowState`** and **`FlowStateListener`** will be explained in subsequent tutorials.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Open the project you previously created using the SAP BTP SDK Wizard for Android.

2.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WelcomeActivity`** to open `WelcomeActivity.kt`.

3.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`startFlow`** to move to the `startFlow` method. To start the onboarding process, firstly create a **`FlowContext`** instance which contains the information required by the flow, such as, the application information, the status of multi-user mode, flow state listener, etc. Then call the `start` method of the `Flow` class to start the onboarding flow, and the entire onboarding process can be handled automatically.

    !![Flow starting method](flow-starting-kotlin.png)

    Notice that by default, the type of a flow is **`FlowType.ONBOARDING`**. To start other type of flow, such as a `Change Passcode` flow, client code must specify the flow type in the **`FlowContext`** instance. Besides the parameters mentioned above, there are also many other parameters in the **`FlowContext`** class for client code to customize a flow. When the value of the parameters not specified in client code, default value will be used. The details of all parameters in the **`FlowContext`** class will be explained in subsequent tutorials.

4.  **`FlowStateListener`** is a parameter in the **`FlowContext`** class. With the callbacks defined in the listener, client code can insert logic at certain points during the flow process, for example, after client policy is retrieved, client code can get the information that will be used by the app. In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`WizardFlowStateListener`** to open `WizardFlowStateListener.kt`. This class is a sample implementation for the callbacks defined in **`FlowStateListener`** class. The details of **`FlowState`** and **`FlowStateListener`** will be explained in subsequent tutorials.
[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Display UI screens of Flows component in wizard-generated application)]

1.  The Flows component provides necessary UI screens for onboarding process and passcode management, such as, EULA screen for user to understand and agree to the terms, QR scan screen to get application configuration, sign-in screen for user authentication and passcode screens for user to create and confirm passcode.

2.  There are different combinations of the screens based on different application configuration and user options in the **`FlowContext`** instance. The following sequence of screens demonstrate the onboarding process of the project previously created from wizard generation. The details for how to customize the screens and include or exclude a screen in a flow will be explained in subsequent tutorials.

    ![Onboarding steps screen](onboarding-steps.png)

Congratulations! You have got basic concept of Flows component and examined its usage in the project created using the SAP BTP SDK Wizard for Android!

[VALIDATE_2]
[ACCORDION-END]

---
