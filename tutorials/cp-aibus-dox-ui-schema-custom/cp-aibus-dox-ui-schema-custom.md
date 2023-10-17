---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>intermediate, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>document-information-extraction, tutorial>free-tier]
primary_tag: topic>machine-learning
---

# Create Custom Schema for Custom Documents
<!-- description --> Create a custom schema for custom documents (which are not supported out of the box) to extract information from similar documents using the Document Information Extraction service.

## You will learn
  - How to create a custom schema for custom documents
  - How to add standard and custom data fields for the header information of custom documents

## Intro
The core functionality of Document Information Extraction is to automatically extract structured information from documents using machine learning. The service supports extraction from the following standard document types out of the box: invoices, payment advices, and purchase orders.

You can also use the [Schema Configuration](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/3c7862e30fc2488ea95f58f1d77e424e.html) and [Template](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/1eeb08998f49409681c06a01febc3172.html) features to extract information from custom documents that are different from the standard document types. You can customize the information extracted from custom document types by creating a custom schema and adding the specific information that you have in your documents.

In this tutorial, we'll use power of attorney documents as an example of a custom document type that is not supported by Document Information Extraction out of the box. A power of attorney document is a legal instrument authorizing one to act as the attorney or agent for another person in specified or all legal or financial matters.

If you are new to the Document Information Extraction UI, first try out the tutorial: [Use Machine Learning to Extract Information from Documents with Document Information Extraction UI](cp-aibus-dox-ui).

---

### Access schema configuration


1. Open the Document Information Extraction UI, as described in the tutorial: [Use Trial to Set Up Account for Document Information Extraction and Go to Application](cp-aibus-dox-booster-app) or [Use Free Tier to Set Up Account for Document Information Extraction and Go to Application](cp-aibus-dox-free-booster-app).


    >If you **HAVE NOT** just used the **Set up account for Document Information Extraction** booster to create a service instance for Document Information Extraction and subscribe to the Document Information Extraction UI, observe the following:

    >- To access the [Schema Configuration](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/3c7862e30fc2488ea95f58f1d77e424e.html) and [Template](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/1eeb08998f49409681c06a01febc3172.html) features, ensure that you use the `blocks_of_100` plan to create the service instance for Document Information Extraction Trial.

    ><!-- border -->![Access](plan.png)


    >- And make sure you're assigned to the role collection: `Document_Information_Extraction_UI_Templates_Admin_trial` (or `Document_Information_Extraction_UI_Templates_Admin` if you're using the free tier option). For more details on how to assign role collections, see step 2 in the tutorial: [Use Trial to Subscribe to Document Information Extraction Trial UI](cp-aibus-dox-ui-sub), or step 3 in the tutorial: [Use Free Tier to Subscribe to Document Information Extraction UI](cp-aibus-dox-free-ui-sub).

    ><!-- border -->![Access](roles.png)


    >- After assigning new role collections, **Log Off** from the UI application to see all features you're now entitled to try out.

    ><!-- border -->![Access](log-off.png)


2. To create a custom schema, click the cogwheels icon and choose **Schema Configuration**.

    <!-- border -->![Access Schema Configuration](access-schema-configuration.png)

Here, you find the SAP schemas. The Document Information Extraction UI includes preconfigured SAP schemas for the following standard document types: purchase order, payment advice, and invoice. In addition, there’s an SAP schema for custom documents (`SAP_OCROnly_schema`). You can’t delete or change SAP schemas. You can use them as they are, or create copies and adapt the list of fields according to your needs.

<!-- border -->![Access Schema Configuration](sap-schemas.png)


>**CAUTION:**

>When using the free tier option for Document Information Extraction or a trial account, be aware of the technical limits listed in [Free Tier Option and Trial Account Technical Constraints](https://help.sap.com/docs/document-information-extraction/document-information-extraction/free-tier-option-and-trial-account-technical-constraints).



### Create schema


To create your own schema, click **Create** and a dialog opens.

<!-- border -->![Create Schema](create-schema.png)

In the dialog, enter a name for your custom schema, `Custom_power_of_attorney_schema`, for example. Note that the name cannot include blanks. Further, select `Custom` as your **Document Type** and `Document` for **OCR Engine Type**.

Click **Create** to create the schema.

<!-- border -->![Create Schema Dialog](create-schema-dialog.png)

Now, your schema shows up in the list. Access the schema by clicking on the row.

<!-- border -->![Access Schema](access-schema.png)




### Add header fields


To define your first header field, click **Add**.

<!-- border -->![Add Header Field](add-header-field.png)

For each field, you have to enter a name, a data type, and a setup type. Adding a description is optional. Default extractors aren't available for custom documents. The available data types are `string`, `number`, `date`, `discount`, `currency`, and `country/region`. 

The available setup types are `auto` and `manual`. The setup type `auto` supports extraction using the service’s machine learning models. You must specify a default extractor for this setup type. It can only be used in schemas created for standard document types. The setup type `manual` supports extraction using a template. It’s available in schemas created for standard or custom document types.

As your first header field, add the shipper number of your power of attorney document.

1. Enter an appropriate name for your field, `shipperNumber`, for example.

2. Select `string` for the `Data Type`. Note that a shipper number is a `string`, even though it consists of numbers, as it is an arbitrary combination of numbers without meaning. In contrast, price is an example of the data type `number`.

3. Select `manual` for the `Setup Type` and click **Add** to create the header field.

    <!-- border -->![Create Number](add-number.png)

The field now displays in your list of header fields, where you again find all the information that you have just entered. You can edit or delete the field by clicking the respective icons on the right.

<!-- border -->![View Number](added-number.png)

Click **Add** again to open the `Add Data Field` dialog.

1. Enter a name for your second header field, `fullName`, for example.

2. Select `string` for the `Data Type`.

3. Select `manual` for the `Setup Type` and click **Add** to create the field.

    <!-- border -->![Create Name](add-name.png)

Go ahead and create the list of header fields as shown in the table and image below. Pay attention to the different data types. Feel free to extend or reduce the list of header fields.

|  Field Name           | Data Type     | Setup Type   
|  :------------------- | :----------   | :----------    
|  `shipperNumber`      | string        | manual       
|  `fullName`           | string        | manual
|  `EIN`                | string        | manual           
|  `state`              | string        | manual       
|  `fullAddress`        | string        | manual       
|  `capacity`           | string        | manual       
|  `date`               | date          | manual                    


<!-- border -->![All Header Fields](all-header-fields.png)



### Activate schema


Once you have added all fields, the schema needs to be activated so that it can be used to extract information from documents. Right now, the schema has the status `DRAFT`, indicating that it cannot be used yet.

To activate the schema, click **Activate**.

<!-- border -->![Activate Schema](activate.png)

Now, the status of your schema changes to `ACTIVE`. To make changes to your schema, you have to **Deactivate** it first.

<!-- border -->![Activate Schema](active.png)

Congratulations, you have created and activated your custom schema for power of attorney documents.

In the next tutorial: [Create Custom Template for Custom Documents](cp-aibus-dox-ui-template-custom), you'll create a template that uses your schema, and associate documents with your template to show the Document Information Extraction service where each field is located in the document.

