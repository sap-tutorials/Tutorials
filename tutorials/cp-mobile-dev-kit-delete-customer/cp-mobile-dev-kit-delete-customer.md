---
title: Delete a customer record in an MDK App
description: Allow deleting of a customer record in an MDK app.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 20
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- [Define a Validation Rule in an MDK App](cp-mobile-dev-kit-add-validation)

## Details
### You will learn
  - How to delete a customer record
  - How to store changes locally
  - How to sync local changes with backend

---

[ACCORDION-BEGIN [Step 1: ](Store the deleted record locally)]

The next step is to store deleted record locally for an offline application or delete directly back to the backed for online applications. You will now create an OData delete action to delete a customer record. You will also show a failure message if the delete action fails.

First, define a failure message action for displaying a message in case deleting of a customer fails.

Right click on the **Actions** folder | **New MDK Action** | **Message Action** | **Next**.

![MDK](img_020.png)

Provide the below information:

| Property | Value |
|----|----|
| `Action Name`| `DeleteCustomerFailure` |
| `Type` | select `Message` |
| `Message` | `Delete entity failure - {{#ActionResults:delete/#Property:error}}` |
| `Title` | `Delete Customer` |
| `OK Caption` | `OK` |

>In this expression `{{#ActionResults:delete/#Property:error}}`, _delete_ is a reference to the **Action Result** name in the _Delete Entity action_ (you will create this action in following step). This reference is used to pass the results to subsequent actions in the chain. These actions can reference the action result as needed. In this case if there is a failure, you access the error property of the action result to display the OData failure message.

>This is the standard Binding Target Path (also called Dynamic Target Path) syntax used when you need to include a binding with other bindings or within a string as used in the message here.

>You could exclude above expression and can just display a generic message.

![MDK](img_021.png)

Click **Next** and then **Finish** on the Confirmation step.

Next, you will create the **OData Delete action** to delete a customer record.

>You can find more details about [Delete Entity Action](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/a7fb91f8f07148c4bcadc4774da5e114.html).

Right click on the **Actions** folder | **New MDK Action** | **OData Action** | **Next**.

![MDK](img_023.png)

Provide the below information:

| Property | Value |
|----|----|
| `Action Name`| `DeleteCustomer` |
| `Type` | `DeleteEntity` |
| `Service`| `SampleServiceV2` |
| `EntitySet` | `Customers` |
| `Read Link` | `{@odata.readLink}` |

![MDK](img_024.png)

>The `readlink` is a direct reference to an individual entity set entry.

Click **Next** and **Finish** on the confirmation screen. The action editor will open with the `DeleteCustomer` action loaded.

Next, set **Common Action Properties** for `DeleteCustomer` action.

Provide the below information:

| Property | Value |
|----|----|
| `Action Result`| `delete` |
| `Success Action` | `ClosePageComplete.action` |
| `Failure Action` | `DeleteCustomerFailure.action` |

>When `DeleteCustomer` action gets executed successfully then `ClosePageComplete` action will be triggered or if `DeleteCustomer` action fails then `DeleteCustomerFailure` action will be triggered.

>`delete` value for **Action Result** is reference to `DeleteCustomerFailure.action` created in step 1.

>You could also show a success message for `Success Action` or chain a message to success of `ClosePageComplete.action`.

![MDK](img_028.png)

Save the changes to the `DeleteCustomer` action.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new dialog confirmation)]

In the MDK editor, you will write a rule in JavaScript called `Customers_DeleteConfirmation.js` to display a dialog to confirm if user wants to delete current record. On it's confirmation, _customer delete entity action_ is executed.

>You can find more details about [writing a Rule](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/ef1e3404ff5f4ca68676acbda10e4bd0.html).

Right click on the **Rules** folder | **New** | **File**.

![MDK](img_001.png)

Enter the file name `Customers_DeleteConfirmation.js`, click **OK**.

Copy and paste the following code.

```JavaScript
export default function DeleteConfirmation(clientAPI) {
	let dialogs = clientAPI.nativescript.uiDialogsModule;
	return dialogs.confirm("Delete current record?").then((result) => {
		if (result === true) {
			return clientAPI.executeAction('/DemoSampleApp/Actions/DeleteCustomer.action').then(
				(success) => Promise.resolve(success),
				(failure) => Promise.reject('Delete entity failed ' + failure));
		} else {
			return Promise.reject('User Deferred');
		}
	});
}
```

>You can navigate from a JavaScript file to the MDK file by right clicking on it.
![MDK](img_001.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add a trash button to customer details page)]

You will add a button to the _Customer Details page_ called **Trash**. You will link this button to the `Customers_DeleteConfirmation.js` rule you just created. This event will display a dialog when the **Trash** button is pressed by the end-user.

In `CustomerDetail` page, drag and drop an **Action Bar Item** to the upper right of the action bar.

![MDK](img_016_1.gif)

>**Action Bar Item** is a button that users can use to fire actions when pressed. You can add an Action Bar Item only to the Action Bar (normally at the top of the page).

Next, In the Properties pane, click the **link icon** to open the object browser for the **System Item** property.

Double click on the **Trash** type and click **OK**.

![MDK](img_017.png)

Next, click the **Events** tab, click the **link icon** for the `OnPress` property to open the object browser.

Double Click on the `Customers_DeleteConfirmation.js` rule and click **OK** to set it as the `OnPress` Action.

>You could link `OnPress` property directly to OData delete action directly instead to this JavaScript file. Idea of linking to  JavaScript file is to let you understand another way to achieve similar functionality.

![MDK](img_019.1.png)

Save the changes to the `CustomerDetail` page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy, activate and test the application)]

Deploy the updated application to your MDK client.

Right click on the MDK Application in the project explorer pane and select **MDK Deploy and Activate**, click **Next** and deploy to Mobile Services.

>Make sure to select same App ID as you chose in previous tutorial.

Re-launch the app on your device, you may asked to authenticate with passcode or Touch ID. You will see a _Confirmation_ pop-up, click **OK**.

>By default, there is a standard 25-35 minutes time duration for App update. In case, you don't see _Confirmation_ pop-up, kill the MDK client app from the background and open it again.

Click Customer List | click any record | click trash icon.

![MDK](img_032.png)

A confirmation dialog appears for user action, click **OK**.

![MDK](img_033.png)

Since this is an Offline application, record has been removed from local store and deletion request has been added to request queue. This has to be sent or uploaded to the backend explicitly.  

>MDK base template has added a **Sync** button on main page of the app to upload local changes from device to the backend and to download the latest changes from backend to the device. Actions | Service | `UploadOffline.action` & `DownloadOffline.action`.

On Main page, click **Sync**, a successful message will be shown.

![MDK](img_036.png)

You can cross verify if this record has been deleted in the backend. For trial account,

`https://hcpms-YOURIDtrial.hanatrial.ondemand.com/mobileservices/origin/hcpms/ESPM.svc/v2/Customers`

>This URL can be found in Mobile Services Cockpit. Mobile Applications | Native/Hybrid | Click on the MDK App | Connectivity

>As Sync is pressed, `UploadOffline.action` gets trigger to upload local changes from device to the backend and on success of this call, `DownloadOffline.action` is being called.

[VALIDATE_1]
[ACCORDION-END]

---
