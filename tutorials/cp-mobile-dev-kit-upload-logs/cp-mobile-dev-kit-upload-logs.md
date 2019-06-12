---
title: Upload Logs from an MDK App
description: Allow users to upload logs from an MDK app to SAP Cloud Platform Mobile Services.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 20
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to enable log upload feature in Mobile Services
  - How to upload logs from the app
  - How to examine log details for troubleshooting

---

[ACCORDION-BEGIN [Step 1: ](Define client log policy in Mobile Services cockpit )]

SAP Cloud Platform Mobile Services provides administrators, developers and support staff with extensive logs and traces functionality to troubleshoot application issues. You can control the amount of information that is captured by setting the log level for individual logging components.

In this step, you will enable client log upload policy in **SAP Cloud Platform Mobile Services Cockpit** for a given MDK app.

Login to [Mobile Services Cockpit](fiori-ios-hcpms-setup), click `com.sap.mdk.demo` | **Client Policies**.

![MDK](img_001.png)

**Enable Client Log Upload** option and click **Save**.

![MDK](img_002.png)

>Other policy parameters like Log Level, Delete Uploaded Log After,  Maximum Number of Logs and Maximum Log Size currently have no effect.

>You can find more details about [Client Log Upload feature](https://help.sap.com/viewer/38dbd9fbb49240f3b4d954e92335e670/Cloud/en-US/36178b64b6bd4c9392cd421f6aa9ef12.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create log actions)]

Logs help you trace events that occur while your application is running. You can create logging actions, set their priority levels and upload stored log entries.

You will create 3 actions:

* Log action of Type `SetState`: to turn the logger On, Off or Toggle

* Log action of Type `SetLevel`: to set log level (Debug, Info, Warn or Error)

* Log action of Type `Upload`: to upload logs from app to Mobile Services

>You can find more details about [MDK Log Actions](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/f906d3ed1919484c8f54050dfd0143ae.html).

![MDK](img_003.png)

First, create a log action of type `SetState`:

Right-click on the **Actions** folder | **New MDK Action** | **Log Action** | **Next**.

![MDK](img_004.png)

Provide the below information:

| Property | Value |
|----|----|
| `Action Name`| `LogSetState` |
| `Type` | `SetState` |
| `Log State`| On |

![MDK](img_005.png)

Click **Next** and then **Finish** on the confirmation step.

Next, create a log action of type `SetLevel`:

Right-click on the **Actions** folder | **New MDK Action** | **Log Action** | **Next**.

Provide the below information:

| Property | Value |
|----|----|
| `Action Name`| `LogSetLevel` |
| `Type` | `SetLevel` |
| `Log Level`| Debug |

>Setting log level to debug is not recommended for productive environment.

![MDK](img_006.png)

Click **Next** and then **Finish** on the confirmation step.

Next, create a log action of type `Upload`:

Right-click on the **Actions** folder | **New MDK Action** | **Log Action** | **Next**.

Provide the below information:

| Property | Value |
|----|----|
| `Action Name`| `LogUpload` |
| `Type` | `Upload` |

![MDK](img_007.png)

Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Bind Set Level log action to the success of Set State action)]

Double click on the `LogSetState.action` file | expand **Common Action Properties** and select `LogSetLevel.action` for **Success Action**.

![MDK](MDK_007.2.png)

Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add Set State log action at app OnLaunch)]

You will add `LogSetState.action` at application on launch event so that when your app can start gathering logs on launch.

Double click on the `Application.app` file | click on **+** icon to add required action.

![MDK](img_008.png)

Double-click  `LogSetState.action` and click **OK**.

![MDK](img_009.png)

This is how final results should look like.

![MDK](img_010.png)

Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Define success/failure messages for Log Upload action)]

You will define two message actions for displaying success or failure when _Log upload action_ is triggered. Later, you will bind these actions in **Common Action Properties** of `LogUpload.action`.

First, you will create a success message action.

Right-click on the **Actions** folder | **New MDK Action** | **Message Action** | **Next**.

Provide the below information:

| Property | Value |
|----|----|
| `Action Name`| `LogUploadSuccessful` |
| `Type` | `ToastMessage` |
| `Message`| `Log File Uploaded` |
| `MaxNumberOfLines`| 1 |
| `Duration` | 3 |
| `IsIconHidden`| `false` |
| `Animated` | `true` |

![MDK](img_011.png)

Click **Next** and then **Finish** on the confirmation step.

Next, create a failure message action.

Right-click on the **Actions** folder | **New MDK Action** | **Message Action** | **Next**.

Provide the below information:

| Property | Value |
|----|----|
| `Action Name`| `LogUploadFailure` |
| `Type` | `Message` |
| `Message`| `Uploading log file failed` |
| `Title`| `Log Upload Failed` |
| `OKCaption` | `OK` |
| `OnOK` | `--None--` |
| `CancelCaption` | leave it blank |
| `OnCancel` | `--None--` |

![MDK](img_012.png)

Click **Next** and then **Finish** on the confirmation step.

Next, bind both actions in **Common Action Properties** of `LogUpload.action`.

Double click on the `LogUpload.action` and provide the below information:

| Property | Value |
|----|----|
| `Success Action`| `LogUploadSuccessful.action` |
| `Failure Action`| `LogUploadFailure.action`
| `Show Activity Indicator` | select it |
| `Activity Indicator Text`| `Uploading Logs...` |

![MDK](img_013.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add Upload Log button to main page)]

You will add a toolbar item to the _main page_ called **Upload Logs**. You will link toolbar item to `LogUpload.action` you just created in step 2.

In `Main` page, drag and drop an **Toolbar Item** to the bottom right of the page.

![MDK](img_014.gif)

Replace **Caption** to _Upload Logs_.

![MDK](img_015.png)

In the Properties pane, click the **Events** tab, click the **link icon** for the `OnPress` property to open the object browser.

Double Click on the `LogUpload.action` action and click **OK** to set it as the `OnPress` Action.

![MDK](img_016.png)

Save the changes to the `Main` page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Deploy, activate and test the application)]

Deploy the updated application to your MDK client.

Right-click on the MDK Application in the project explorer pane and select **MDK Deploy and Activate**, click **Next** and deploy to Mobile Services.

>Make sure to select same App ID as you chose in previous tutorial.

Re-launch the app on your device, you may asked to authenticate with passcode or Touch ID or Fingerprint. You will see a _Confirmation_ pop-up, click **OK**.

At `OnLaunch` event, app starts gathering logs.

Click **Upload Logs** to upload client logs from app to SAP Cloud Platform Mobile Services.

![MDK](img_017.png)

![MDK](img_018.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Examine uploaded logs in Mobile Services cockpit)]

Open SAP Cloud Platform Mobile Services Cockpit, click **Analytics** | **Logs**.

![MDK](img_019.png)

Navigate to **Technical Logs** tab. You may set criteria to filter some specific log entries.
For example, choose **Application ID** as `com.sap.mdk.demo`
and **Type** as `Client Log`.

![MDK](img_020.png)

You may select required log entries and then either view them directly in Mobile Services Cockpit or download them locally.

![MDK](img_021.png)

>You can find more details about [Uploading and Viewing Client Logs](https://help.sap.com/viewer/DRAFT/7f3bded2efb9424eb0affaee75bd58ce/1.1/en-US/7dfa70b6be6f4d5ebb6845e7f1e4ae82.html).

[VALIDATE_1]
[ACCORDION-END]

---
