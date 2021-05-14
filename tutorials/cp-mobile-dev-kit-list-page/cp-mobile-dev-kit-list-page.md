---
title: Create a Customer List Page in an MDK App
description: Use the mobile development kit page editor to create a new page for displaying a customer list.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>mobile-development-kit-client, products>sap-mobile-services, products>sap-business-application-studio ]
time: 10
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to create a new page and add some controls to display customer information
  - How to navigate from one page to another page

---

To enhance your MDK app with customer list information, you need to carry out the following tasks:

*  Create a new customer list page
*  Add Contact Cell Table control to the page
*  Create a new navigation action to the customer list page
*  Add a new button on main page and navigate to customer list page when user clicks it
*  Deploy the app metadata to SAP Mobile Services & Cloud Foundry
*  Update the app with new changes

![MDK](img_1.gif)

[ACCORDION-BEGIN [Step 1: ](Create a new page for displaying customer list)]

This page is a searchable list that displays all customers.

To create the Customer List page, you will create a **Section page** and drag the Customer **Contact Cell Table** control onto the page. In the property palette, you will link the control to the Customer Collection and then map data and actions to different areas of the cell object in the property palette. One nice feature about the **Contact Cell Table** control is that it has icons that can activate device functionality such as phone, email, video and more.


1. In SAP Business Application Studio project, right-click the **Pages** | **MDK: New Page**.

    !![MDK](img-1.1.png)

2. Select | **Section Page** and click **Next**.

    !![MDK](img_1.2.png)

    >You can find more details about [Section Page](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/mdk/section-page.html).

3. Enter the **Page Name** `Customers_List` and click **Next** and the **Finish** on the confirmation step.

    !![MDK](img_1.3.png)

4. In the **Properties** pane, set the **Caption** to **Customers**.

    !![MDK](img_1.4.png)

5. In the Layout Editor, expand the **Controls** | **Compound** section, drag and drop the **Contact Cell Table** control onto the Page area.

    !![MDK](img_1.5.gif)

    >A **Compound** control contains a group of other controls. Unlike in a container control where you can add your own child controls (container items), the child controls in a compound control are fixed. You can populate each of its child control by defining its data binding, depending on which the child controls are created.

6. In the Properties pane, provide the below information:

    | Field | Value |
    |----|----|
    | `Service`| select `Sample.service` from the dropdown |
    | `EntitySet` | Select `Customers` from the dropdown |
    | `QueryOptions` | `$orderby=LastName` |

    !![MDK](img-1.6.gif)

7. In the **Properties** pane, click the **link icon** to open the Object Browser for the **Description** property. Double click the `City` property of the Customer entity to set it as the binding expression and click **OK**.

    !![MDK](img-1.7.gif)

    >Be careful **not** to select `City` from `Address (ESPM.Address)`.

    >!![MDK](img-1.7.1.png)

8. Repeat the above steps for `Headline` and `Subheadline` properties binding to `LastName` and `FirstName` properties of the Customer entity respectively.

    You should have final results as below.

    !![MDK](img-1.8.png)

9. In the **Search** section of the Properties pane, change both the **Search Enabled** property and **Barcode Scanner** property to **true**.

    !![MDK](img-1.9.png)

10. In the **Activity Items** section of the Properties pane, change `VideoCall` on the first activity to **Phone**.

    !![MDK](img-1.10.gif)

11. Click the **link icon** to open the Object browser for the **Phone** activity and bind it to the `PhoneNumber` property of the Customer entity.

    !![MDK](img-1.11.png)

12. Repeat the above step for **Email** activity and bind it to `EmailAddress` property of the Customer entity.

    !![MDK](img-1.12.png)

13. Click the **trash** can icon for the Detail activity to remove it from the Activity list.

    !![MDK](img-1.13.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create navigation action)]

Now, you will create a Navigation action that opens the `Customers_List.page` when called.

1. In SAP Business Application Studio project, right-click the **Actions** | **MDK: New Action**.

    !![MDK](img-2.1.png)

2. Choose **MDK UI Actions** in **Category** | click **Navigation Action** | **Next**.

    !![MDK](img-2.2.png)

2. Provide the below information:

    | Field | Value |
    |----|----|
    | `Action Name`| `NavToCustomers_List` |
    | `PageToOpen` | Select `Customers_List.page` from the dropdown |

    !![MDK](img_2.3.png)

3. Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add a button to view customer list)]

Now, that the Customer List page is created, you will add a button to the Main page to display the Customers. You will use a **Section Button Table** control.

1. In `Main.page`, drag and drop the **Section Button Table** container control onto the Page.

    !![MDK](img-3.1.gif)

    >**Container** includes controls that act as containers for other controls, such as container items. A container is constant for all pages. The size of a container depends on the controls and contents included inside.

2. Expand the **Container Item** section of the Controls palette.

    Drag and drop a **Section Button** onto the Section Button Table container on the page.

    !![MDK](img_3.2.gif)

    >Each container type in a Section Page can contain specific controls (container items).

    >You can find more details about [Container and Container Item](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/SectionedTable/SectionedTable.schema.html).

3. In the Properties pane, set the **Title** of the button to **Customer List**.

    !![MDK](img-3.3.png)

4. Under **Events** tab, click the **link icon** for the `OnPress Handler` property to open the Object Browser. Double click the `NavToCustomers_List` action and click **OK** to set it as the `OnPress Handler` Action.

    !![MDK](img-3.4.gif)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy the application)]

Deploy the updated application to your MDK client.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-4.1.png)

2. Select deploy target as **Mobile & Cloud**.

    !![MDK](img-4.2.png)

    You should see success message for both deployments.

    !![MDK](img-4.3.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test the application)]

>Make sure you are choosing the right platform tab above.

[OPTION BEGIN [Android]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, tap **OK**.

    ![MDK](img-5.1.png)

2. You will notice, newly added button on the main page. Tap **CUSTOMER LIST**.

    ![MDK](img_5.2.png)

    Here, you will see list of all the Customers. You can search a record by by First Name or Last Name or City. Controls are rendered natively on device, you can email to the customer, make a phone call etc.

    ![MDK](img_5.3.png)

    >Here, you may notice that City is not showing up on screen, this is by design. In portrait mode, the device width is considered compact, if it was a tablet device (where both portrait and landscape are considered regular instead of compact) you would see City on either orientation.

[OPTION END]

[OPTION BEGIN [iOS]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, tap **OK**.

    ![MDK](img_6.1.png)

2. You will notice, newly added button on the main page. Tap **Customer List**.

    ![MDK](img_6.2.png)

    Here, you will see list of all the Customers. You can search a record by by First Name or Last Name or City. Controls are rendered natively on device, you can email to the customer, make a phone call etc.

    ![MDK](img_6.3.png)

    >Here, you may notice that **City** is not showing up on screen, this is by design. In portrait mode, the device width is considered _compact_ , if you change device orientation to landscape mode, you will see **City**.

    >![MDK](img_6.4.png)

    >If it was an iPad (where both portrait and landscape are considered *regular* instead of *compact*) you would see **City** on either orientation.

[OPTION END]

[OPTION BEGIN [Web]]

1. Either click the highlighted button or refresh the web page to load the changes.

    !![MDK](img-6.5.png)

    >If you see the error `404 Not Found: Requested route ('xxxxxtrial-dev-nsdemosampleapp-approuter.cfapps.xxxx.hana.ondemand.com') does not exist.` while accessing the web application, make sure that in your space cockpit, highlight applications are in started state.

    >!![MDK](img-6.8.png)

2. You will notice, newly added button on the main page. Click **Customer List**.

    !![MDK](img_6.6.png)

    Here, you will see list of all the Customers. You can search (case-sensitive) a record by by First Name or Last Name or City. The rendered Controls are UI5 web components, you can email to the customer, make a phone call etc.

    !![MDK](img_6.7.png)

[OPTION END]

[VALIDATE_3]
[ACCORDION-END]

---

Congratulations, you have successfully created a Customer List page and you are now all set to [create a Customer Detail Page](cp-mobile-dev-kit-detail-page).

---
