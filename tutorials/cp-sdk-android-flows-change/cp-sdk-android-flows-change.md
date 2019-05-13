---
title: Add a Change Flow
description: Add a change flow enabling a user to trigger the action to change the app's passcode.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-platform, operating-system>android, topic>mobile]
primary_tag: products>sap-cloud-platform-sdk-for-android
---

## Prerequisites
 - Completed the previous tutorial in this mission which added the reset flow

## Details
### You will learn
  - How to add a change flow to an app

In the restore tutorial, it was shown how the application responded to a change made in the management cockpit to the client policy. In the below steps, the user will trigger the action to change their passcode using the change flow.

---

[ACCORDION-BEGIN [Step 1: ](Add the code)]

Add the following method to **`MainActivity.java`**

```Java
private void startChangeFlow() {
    LOGGER.debug("starting Change flow");
    // Creating flow and configuring steps
    Flow flow = new Flow("change");
    flow.setSteps(new Step[] {
        new ChangePasscodeStep()  // shows the enter, new, and confirm passcode screens
    });

    flowManagerService.execute(flow, flowContext, new FlowActionHandler() {
        @Override
        public void onFailure(Throwable t) {
            LOGGER.debug("onFailure in change called" + t);
            showAlertDialog("Change", t);
        }

        @Override
        public void onSuccess(FlowContext result) {
            LOGGER.debug("Change flow completed successfully.");
        }
    });
}
```

The change flow is unique in the sense that it simply uses the existing `flowContext` from the onboard or restore flow and then adds another step that enables users to change their passcode.

Add the following line to the **`onChange`** method.

```Java
startChangeFlow();
```

Ensure that `settingsDownloadStep.passcodePolicy = null;` is commented out so that a passcode exists for the app.  Otherwise `onFailure` will be called.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Try it out)]

To test out the changing passcode functionality, tap the **Change** button.

![Change button](change-button.png)

A screen to prompt for the current passcode will be shown.  

![Enter passcode screen](original-passcode.png)

Then a screen to create a new passcode will appear.

![New passcode screen](new-passcode.png)

Finally, the verify passcode screen will be shown.

![Confirm passcode screen](confirm-passcode.png)

Congratulations!  The code to perform a change flow has been added enabling a user to change their app's passcode.

[VALIDATE_1]
[ACCORDION-END]


---
