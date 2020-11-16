---
title: Define a Validation Rule in an MDK App
description: Write a JavaScript logic to validate an email address format in an MDK app.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services, products>sap-business-application-studio ]
time: 10
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to write a rule to validate an UI field

---

![MDK](img_1.gif)

[ACCORDION-BEGIN [Step 1: ](Create a validation message)]

When allowing end-users to make updates to data, it is important to add validation rules to verify that they are entering valid information.
If the Update action fails due to the validation rule, the application will display a validation failure message to the end-user. You can customize this message by creating a Message action and adding the action to the validation Failure Message field in the Update action.

1. Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    !![MDK](img_1.1.png)

2. Provide the below information:

    | Field | Value |
    |----|----|
    | `Action Name`| `ValidationFailure` |
    | `Type` | Select `Message` from the dropdown |
    | `Message`| `Email address is not in the correct format recipient @ domain . domaintype` |
    | `Title` |  `Validate Email` |
    | `OKCaption`| `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | leave it blank |
    | `OnCancel` | `--None--` |

    !![MDK](img_1.2.png)

3. Click **Next** and then **Finish** on the Confirmation step.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create a new rule)]

In the MDK editor, you will create a new JavaScript file called `EmailValidation.js` to handle validation if a **@** symbol exists in the email address. In the validation rule, you will grab the data entered by the end-user, validate it and check for the **@** symbol then return true if the email address is of a valid format or false if it is not. The returning result of the validation rule can be used in the Update action to determine whether the action succeeds or fails.

>You can find more details about [writing a Rule](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/development/rules.html).

1. Right-click the **Rules** folder | click **MDK: New Rule**.

    !![MDK](img_2.1.png)

2. Enter the Rule name `EmailValidation.js`, press `Enter`.

    !![MDK](img_2.2.png)

3. Replace the generated snippet with below code.

    ```JavaScript
    export default function EmailValidation(context) {
        //The following evaluateTargetPath will retrieve the current value of the email control
        if ((context.evaluateTargetPath('#Control:FCEmail/#Value').indexOf('@')) === -1) {
            //If email value does not contain @ display a validation failure message to the end-user
            context.executeAction('/DemoSampleApp/Actions/ValidationFailure.action');
        } else {
            //If @ is present in the email value, return true to indicate validation is successful
            return true;
        }
    }
    ```

4. Save the changes.

    >The [`indexOf()` method](https://www.w3schools.com/jsref/jsref_indexof.asp) returns the index within the calling String object of the first occurrence of the specified value and -1, if no occurrence is found.

    >In above code there is a reference to `ValidationFailure.action` , you can navigate directly from here to the MDK file by right clicking on it.
    !![MDK](img_2.3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add the validtion rule to the Update entity)]

Now, that you have created the validation rule, you will add it to the Update action. This will tell the Update action to run the validation rule before saving any data. If the validation rule is successful, the Update action will save the changes as expected. If the validation rule fails, the end-user receives the validation failure message telling them useful information so they can fix the problem before continuing.

1. Open `Customers_UpdateEntity.action` by double clicking on the action in the project explorer pane.

2. Expand the **Common Action Properties** and select `EmailValidation.js` rule for **Validation Rule** field.

    !![MDK](img_3.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy and activate the application)]

Deploy the updated application to your MDK client.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img_4.1.png)

2. Select deploy target as **Mobile Services**.

    !![MDK](img_4.2.png)

    You should see **Deploy succeeded** message.

    !![MDK](img_4.3.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test the application)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, click **OK**.

2. Update a record with no contain of **@** in Email property throws a validation failure message.

    ![MDK](img_5.2.1.png)
    ![MDK](img_5.2.2.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, click **OK**.

2. Update a record with no contain of **@** in Email property throws a validation failure message.

    ![MDK](img_5.2.3.png)
    ![MDK](img_5.2.4.png)

[OPTION END]

**Congratulations!** You have successfully completed **Create Your First Mobile App with the Mobile Development Kit (MDK)** mission and you are now all set to [Enhance Your First MDK App with Additional Functionalities](mission.mobile-dev-kit-enhance) mission.

[VALIDATE_1]
[ACCORDION-END]

---
