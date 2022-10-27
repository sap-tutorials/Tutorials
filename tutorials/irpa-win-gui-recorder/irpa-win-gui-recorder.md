---
author_name: Ilyes Yamoun
author_profile: https://github.com/shielddz
title: Create an automation using SAP WinGUI Recorder
description: Automate Sales order Creation in SAP GUI for Windows
keywords: RPA
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-cloud-sdk]
primary_tag: software-product>sap-intelligent-robotic-process-automation
---

## Prerequisites
  - [Subscribe to SAP Intelligent RPA Service in SAP BTP](irpa-setup-1-booster-subscription)
  - [Install SAP Intelligent RPA On-Premise Components](irpa-setup-2-onpremise-installation)
  - [Scripting enabled for SAP GUI for Windows](https://help.sap.com/viewer/8e71b41b9ea043c8bccee01a10d6ba72/Cloud/en-US/f0fe92f292c946bca1269f826cd682b3.html) on the Client and the Server.

## Details
### You will learn
  - How to use Recorder in **SAP GUI for Windows**

---
[ACCORDION-BEGIN [Step 1: ](Prepare the Recording)]

What is the Recorder?

You can automate complex workflows easily using the **Recorder** in **Cloud Studio**. It automatically captures applications and designs automations accurately at the same time. The **Recorder** records the steps you perform across the screens of an application. Then you can export the recording in the automation designer of the **Cloud Studio** where a workflow is built.

Make sure your screen display settings (Scale and layout) are set to 100%.

1.  Switch to your desktop, right-click and select **Display Settings**.

    !![Open Display Settings](step1-display-settings-1.png)

2.  Select **100%** in **Scale and layout**.

    !![Display Settings](step1-display-settings-2.png)

Create a project and use **Recorder** to record a **SAP GUI for Windows** Application.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a project in the Cloud Studio)]
Create a project in the **SAP Intelligent RPA Cloud Factory** by choosing **Projects** &rarr; **New Project**.

!![Create a project](step2-create-a-project.png)

A new project is created in **SAP Intelligent RPA Cloud Studio**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create application artifact)]
1. Create a new **Application** artifact in your project.

    !![Create an application](step3-create-application.png)

2. Select the **SAP GUI for Windows** application to record.
3. Select **Launch Recorder**.
4. Set up an **Application Name** for your application.
5. Select **Record**.

  !![Select an application](step3-select-application-and-configure.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Record an application)]
1. Choose **Record** !![Record icon](step4-record-icon.PNG) to initiate the recording. You will be directed to the **SAP GUI for Windows** application you want to record.
2. Select **Client** and enter your credentials.
3. Hit <kbd>Enter</kbd> button.

  !![Login](step4-login.png)

**Capture: SAP Easy Access**

The screen has changed and warning at the bottom of the **Recorder** appeared.

1. Choose **New Screen Capture** to capture the new screen and wait for the **Recorder** to load the new screen's elements. Once the new screen is captured, it will appear in the **Recorder**.

2. Enter the Sales Order transaction ID **VA01**.

3. Hit <kbd>Enter</kbd> button.

  !![Select a transaction](step4-select-transaction.png)

**Capture: Create Sales Order: Initial Screen**

1. Choose **New Screen Capture**.

2. Fill in the details.

    |  Field Name           | Value
    |  :------------------- | :-------------
    |  Order Type           | AVC
    |  Sales Organization   | 0001
    |  Distribution Channel | 01
    |  Division             | 01

3. Hit <kbd>Enter</kbd> button.

  !![Fill order type](step4-order-type.png)

**Capture: Create Standard Order: Overview**

1. Choose **New Screen Capture**.

2. Fill in the order details.

3. Hit <kbd>Enter</kbd> button.

    > You can notice that the **Recorder** has generated the corresponding activities from the previous screen.

    !![Fill Order details](step4-order-details.png)

4. Choose **Save** to complete the process of recording the sales order application.

5. Choose **Stop** to stop the recording.

  !![Stop recording](step4-stop-recording.png)

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Export recording)]
Select **Export** to export the recording to your project.

  !![Export recording](step5-export.png)

As a result of your recording:
 - A **screen** is captured in the **Cloud Studio**
 - An **automation** is generated in the **Cloud Studio**

 Your Project now consists of an application named *Create Sales Order* and automation named **Create Sales Order Automation**.

  !![Generated automation](step5-resultant-automation.png)

In the automation you will have the choice to customize your automation as per your requirements.

The password is recorded as **asterisks**. You need to change its value to the real password.

1.  Find and Select the activity **Set Element** of password that sets the password value.

2.  Change the value to the real password.

    !![Change Password field Value](step5-change-password.png)

The automation is ready to be tested.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test your Automation)]
To test your automation, choose **Test**.

  !![Test button in Cloud Studio](step5-test.png)

**SAP Intelligent RPA Cloud Studio** starts the automation by calling the **Desktop Agent** using the **SAP Intelligent RPA Browser Extension**.

The process operates as follows:

1.  SAP ERP system is opened, enters the credentials and navigates to Sales order Transaction to create a Sales Order.

2.  The **Desktop Agent** fills all the details in the screen.

3.  Information is validated to move to the next detected screen.

4.  These steps are repeated for all the screens that were captured.

5.  The Sales order is created successfully.

[DONE]
[ACCORDION-END]
---
