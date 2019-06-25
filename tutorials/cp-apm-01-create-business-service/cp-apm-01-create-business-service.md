---
author_name: René Jeglinsky
author_profile: https://github.com/renejeglinsky
title: Create a Business Service Using CDS
description: Define a data and service model using SAP Web IDE Full-Stack and the SAP Cloud Application Programming Model based on Core Data and Services (CDS).
auto_validation: true
primary_tag: software-product-function>sap-cloud-application-programming-model
tags: [  tutorial>intermediate, topic>java, products>sap-cloud-platform, products>sap-web-ide, software-product-function>sap-cloud-application-programming-model ]
time: 10
---

## Prerequisites  
 - [Get a free trial account on SAP Cloud Platform](hcp-create-trial-account)
 - [Create a Cloud Foundry Account](cp-cf-create-account)
 - [Set Up the SAP Web IDE](sapui5-webide-open-webide)
 - [Create an SAP HANA Service Instance](https://help.sap.com/viewer/cc53ad464a57404b8d453bbadbc81ceb/Cloud/en-US/21418824b23a401aa116d9ad42dd5ba6.html) (If no SAP HANA Service Instance is available, change your region)

## Details
### You will learn  
  - How to develop a simple business service on SAP Cloud Platform using the SAP Cloud Application Programming Model and SAP Web IDE Full-Stack

---

[ACCORDION-BEGIN [Step 1: ](Start a project)]

1. In SAP Web IDE choose **File** | **New** | **Project from Template**.

1. Search for **SAP Cloud Platform Business Application**.

    ![Select the project template](web-ide.png)   

    >If you do not see the template, make sure **All Categories** is selected from the **Category** drop-down menu and try again. If you still do not see the template, make sure the **SAP Cloud Platform Business Application Development Tools** are enabled. See [Developing SAP Cloud Platform Business Applications](https://help.sap.com/viewer/825270ffffe74d9f988a0f0066ad59f0/CF/en-US/99936743e1964680a0884479bfa75c8e.html).

1. Enter **`bookshop`** as the project name and choose **Next**.

1. In the **Template Customization** tab leave the default values.

1. Complete the **Project Details** tab as shown in the screenshot, ensuring that the **Include sample files in project** checkbox is checked.

    ![Complete the project details](project-details-bookshop.png)

    >You should update the **Java Package** to match the namespace used in this sample application, which is `my.bookshop`.

1. Choose **Finish**.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Define a data model)]

1. Open `db/data-model.cds` and replace the template with the following CDS definitions:

    ```CDS
    namespace my.bookshop;

    entity Books {
      key ID : Integer;
      title  : String;
      author : Association to Authors;
      stock  : Integer;
    }

    entity Authors {
      key ID : Integer;
      name   : String;
      books  : Association to many Books on books.author = $self;
    }

    entity Orders {
      key ID : UUID;
      book   : Association to Books;
      buyer  : String;
      date   : DateTime;
      amount : Integer;
    }
    ```

    ![Define the data model](define-data-model.png)

2. Save the file.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Define a service)]

1. Go to `srv/cat-service.cds` and open the context menu.

2. Replace the template with the following CDS definitions:

    ```CDS
    using my.bookshop from '../db/data-model';
    service CatalogService {
      entity Books    @readonly as projection on bookshop.Books;
      entity Authors  @readonly as projection on bookshop.Authors;
      entity Orders   @insertonly as projection on bookshop.Orders;
    }
    ```

    ![Define the service model](define-service-model.png)

3. Save the file.

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test-run the service)]

1. Click on the **`srv`** module and choose **Run** from the global toolbar.

    >This might take a few minutes because a new cloud container has to be initialized and started. Subsequent restarts are much faster, because hot deployment is used.

    ![Choose Run](run-java-app.png)

    > If the build is unsuccessful, check the console log for errors. Errors similar to this one: `Could not create the 'bookshop-bookshop-hdi-container-D0oRdR+pMxiYd6NYDr' instance of the 'hana' service type for the 'bookshop-hdi-container' resource. No hdi-shared plan available found in this space."` To fix this issue, try the following:

    > 1. Make sure your **HANA** entitlement is assigned.

    > 1. Go to **SAP Cloud Platform Cockpit | Cloud Foundry | Entitlements**.

    > 1. Click on **Edit**.
    ![Edit entitlements](entitlements-edit.png)

    > 1. Scroll down until you find **HANA** and change the value to `1`.
    ![Change hdi-shared value](entitlements-hdi-shared.png)

    > 1. **Save** your changes, and re-try to run your service.

1. Go to the **Run Console** and click on the URL.

    ![Run console](run-console.png)

    A new browser window opens containing a link to the OData service.

1. Click on the service link.

    The OData service document opens.

1. Add **`/$metadata`** to the URL and refresh.

    The OData metadata document opens in EDMX format.

1. Replace `/$metadata` with **`/Books`**.

    An error message is displayed because we have not added a database yet.

[DONE]

[ACCORDION-END]

---
