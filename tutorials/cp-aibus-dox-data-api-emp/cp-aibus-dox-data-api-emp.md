---
title: Use Machine Learning to Enrich Employee Data
description: Create, update, list and delete enrichment data using the Data API employee entity from Document Information Extraction, one of the SAP AI Business Services in SAP Cloud Platform.
auto_validation: true
time: 15
tags: [tutorial>intermediate, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-cloud-platform, products>sap-ai-business-services, products>document-information-extraction]
primary_tag: topic>machine-learning
---

## Prerequisites
- [Set Up Account for Document Information Extraction](cp-aibus-dox-service-instance-booster)
- [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token)
- [Use Machine Learning to Extract Information from Documents with Swagger UI](cp-aibus-dox-swagger-ui) (step 1 only)

## Details
### You will learn
  - How to create, update, list and delete enrichment data using the employee entity

After completing the tutorial mission [Use Machine Learning to Process Business Documents](mission.cp-aibus-extract-document-service), you can also use Document Information Extraction to enrich the information extracted from documents with your own master data. You can, for example, match enrichment data entities, such as employee IDs, with the document [Extracted Header Fields](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/b1c07d0c51b64580881d11b4acb6a6e6.html), such as receiver contacts.

When enriching data with Document Information Extraction, you use 2 types of entities that you find in business documents. The `business entity` represents different kinds of organizations with which you deal as a company. It can represent, for example, suppliers and customers. The `employee entity` represents an employee in the company.

When you finish this tutorial, you will have explored all Data API functionalities to create, update, list and delete enrichment data using the `employee entity` type. See [Enrichment Data API documentation](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/ca4b609107dd47a78d880cb5eaceb8c8.html).

---

[ACCORDION-BEGIN [Step 1: ](Create client)]

After completing the prerequisite tutorials [Set Up Account for Document Information Extraction](cp-aibus-dox-service-instance-booster) and [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token), and performing at least step 1 in the tutorial [Use Machine Learning to Extract Information from Documents with Swagger UI](cp-aibus-dox-swagger-ui) to access and authorize the Document Information Extraction Swagger UI, you need to create a client. This client is used in most of the endpoints to distinguish and separate data.

You can either create a single client or multiple clients in the **payload** field of the **POST /clients** endpoint. The **`clientId`** values created here will be used in other service endpoints.

1. Expand the **POST /clients** endpoint.

2. Click **Try it out**.

3. Enter your **`clientId`** and **`clientName`** values in the **payload** field in the format you see in **Examples for payload parameter** (`c_27` and `client 27`, for example).

4. Click **Execute**.

![DOX](1create_clients_request.png)

You should receive a response like the following:

![DOX](1create_clients_response.png)


>**CAUTION:**

>Be aware of the following Document Information Extraction trial account limitations:​

>- Maximum 40 uploaded document pages per week​ (the documents can have more than 1 page)

>- Maximum 1 created `clientId`

>- Maximum 10 created enrichment `dataIds`


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create enrichment data)]

Use the **POST /data/jobs** endpoint to add your own master data records to the database to enrich the information extracted from documents.

1. Expand the **POST /data/jobs** endpoint.

2. Click **Try it out**.

3. Define the data in the **`payload`** field, so that the system knows which extracted field (using, for example, the employee ID from master data) should be enriched.

    ```JSON
    {
       "value":[
          {
             "id":"E0001",
             "email":"",
             "firstName":"Linda",
             "middleName":"",
             "lastName":"Owens"
          },
          {
             "id":"E0002",
             "email":"",
             "firstName":"Lin",
             "middleName":"",
             "lastName":"Owens"
          }
       ]
    }
    ```

4. Choose the enrichment data **`type`** `employee`.

5. Enter your **`clientId`** (created in the previous step).

6. Click **Execute**.

!![DOX](1post_data_jobs_request.png)

> ### What just happened?
>
> In this example, in the **`payload`** field, several master data records (full name and ID, for example) from 2 employees (Linda Owens and Lin Owens) are provided, so this additional information can be added to the document extracted fields prediction when the information matches.

You should receive a response like the following with status PENDING:

!![DOX](1post_data_jobs_response.png)

Copy the **`id`** from the **Response body** to see the result of the enrichment data status in the next step.

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](See created enrichment data status)]

Use the **GET /data/jobs/{`id`}** endpoint to see the status of the uploaded enrichment data.

1. Expand the **GET /data/jobs/{`id`}** endpoint.

2. Click **Try it out**.

3. Enter the **`id`** received in the **POST /data/jobs** endpoint as the **`id`**.

4. Click **Execute**.

!![DOX](1get_data_jobs_id_request.png)

You should receive a response like the following with status SUCCESS:

!![DOX](1get_data_jobs_id_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Upload document to get prediction with enrichment data)]

>Document Information Extraction uses a globally pre-trained machine learning model that currently obtains better accuracy results with invoices and payment advices in the languages listed in [Supported Languages and Countries](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/5bf847f7d1a848dcb3513eff9ec70412.html). The team is working to support additional document types and languages in the near future.

When enrichment data has been uploaded and fits to a certain prediction it is added to the results from the **GET /document/jobs/{`id`}** endpoint. To have the enrichment data in the prediction, you need to have the following part in the query of the **POST /document/jobs** endpoint (it is usually already there by default):

```JSON
"enrichment": {
    "sender": {
      "top": 5,
      "type": "businessEntity",
      "subtype": "supplier"
    },
    "employee": {
      "type": "employee"
    }
}
```

Do the following:

1. Expand the **POST /document/jobs** endpoint.

2. Click **Try it out**.

3. Right click [Sample Invoice 2](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-swagger-ui/data/sample-invoice-2.pdf), then click ***Save link as*** to download locally the document file for this enrich employee data example.
> You can also upload to the service and enrich any document file in PDF or single-page PNG and JPEG format that has content in headers and tables, such as an invoice. In this case, make sure the data you define in the **`payload`** field, in step 2, matches your document fields.

4. Upload the document file you want to enrich.

5. In **options**, enter the list of fields to be extracted from the uploaded file (`documentNumber`, `taxId`, `purchaseOrderNumber`, `shippingAmount`, `netAmount`, `senderAddress`, `senderName`, `grossAmount`, for example), the client id you created in step 1 (`c_27`, for example), the document type (`invoice`, for example), `receivedDate` (2020-02-17, for example) and the enrichment data type `employee`.

    ```JSON
    {
       "extraction":{
          "headerFields":[
             "documentNumber",
             "taxId",
             "purchaseOrderNumber",
             "shippingAmount",
             "netAmount",
             "senderAddress",
             "senderName",
             "grossAmount",
             "currencyCode",
             "receiverContact",
             "documentDate",
             "taxAmount",
             "taxRate",
             "receiverName",
             "receiverAddress"
          ],
          "lineItemFields":[
             "description",
             "netAmount",
             "quantity",
             "unitPrice",
             "materialNumber"
          ]
       },
       "clientId":"c_27",
       "documentType":"invoice",
       "receivedDate":"2020-02-17",
       "enrichment":{
          "employee":{
             "type":"employee"
          }
       }
    }
    ```

6. Click **Execute**.

This is how the request should look like:

!![DOX](1post_document_jobs_request.png)

And that's how the response looks like:

!![DOX](1post_document_jobs_response.png)

Copy the **`id`** from the **Response body** to get enrichment data prediction in the next step.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Get enrichment data prediction)]

When enrichment data has been uploaded and fits to a certain prediction it is added to the results from the **GET /document/jobs/{`id`}** endpoint.

> Enrichment data is refreshed automatically every 4 hours. It might take up to 4 hours until the enrichment data prediction is available in the response.

1. Expand the **GET /document/jobs/{`id`}** endpoint.

2. Click **Try it out**.

3. Set **`returnNullValues`** and **`extractedValues`** to `true`.

4. Enter the **`id`** received in the **POST /document/jobs** endpoint as the **`id`**.

5. Click **Execute**.

The endpoint request and response look as follows:

!![DOX](1get_document_jobs_id_request.png)

!![DOX](1get_document_jobs_id_response.png)

> ### What just happened?
>
> In this example, in the response, one of the extracted fields is the receiver contact Linda Owens. This information is enriched with the employee ID enrichment data created in step 2. The prediction suggests the employee ID from Linda Owens (E0001) with 100% probability. The employee ID from Lin Owens (E0002) is not even considered by the machine leaning model.

This is an example of a full prediction including the enrichment data part:

```JSON
{
  "status": "DONE",
  "id": "47dc278a-f329-4925-9344-48ac7ffda67a",
  "fileName": "sample-invoice-2.pdf",
  "documentType": "invoice",
  "created": "2020-11-13T09:22:46.069362+00:00",
  "finished": "2020-11-13T09:22:55.708792+00:00",
  "country": "XX",
  "extraction": {
    "headerFields": [
      {
        "name": "taxAmount",
        "category": "amounts",
        "value": null,
        "type": "number",
        "page": null,
        "confidence": null,
        "coordinates": {
          "x": 0,
          "y": 0,
          "w": 0,
          "h": 0
        },
        "group": 1
      },
      {
        "name": "taxRate",
        "category": "amounts",
        "value": null,
        "type": "number",
        "page": null,
        "confidence": null,
        "coordinates": {
          "x": 0,
          "y": 0,
          "w": 0,
          "h": 0
        },
        "group": 1
      },
      {
        "name": "documentDate",
        "category": "document",
        "value": "2020-02-20",
        "type": "date",
        "page": 1,
        "confidence": 0.654179429306703,
        "coordinates": {
          "x": 0.863306451612903,
          "y": 0.286121402108863,
          "w": 0.0681451612903226,
          "h": 0.0088344257623254
        }
      },
      {
        "name": "senderAddress",
        "category": "sender",
        "value": "Cupertino,CA 95014",
        "type": "string",
        "page": 1,
        "confidence": 0.595586925575679,
        "coordinates": {
          "x": 0.067741935483871,
          "y": 0.0929039612425192,
          "w": 0.138306451612903,
          "h": 0.0102593331433457
        }
      },
      {
        "name": "senderName",
        "category": "sender",
        "value": "Apple Store One Infinite Loop",
        "type": "string",
        "page": 1,
        "confidence": 0.597478873833562,
        "coordinates": {
          "x": 0.0665322580645161,
          "y": 0.0544314619549729,
          "w": 0.127016129032258,
          "h": 0.030777999430037
        }
      },
      {
        "name": "grossAmount",
        "category": "amounts",
        "value": 1998,
        "type": "number",
        "page": 1,
        "confidence": 0.549836874008179,
        "coordinates": {
          "x": 0.793548387096774,
          "y": 0.447420917640353,
          "w": 0.0681451612903226,
          "h": 0.00997435166714167
        }
      },
      {
        "name": "documentNumber",
        "category": "document",
        "value": "9001321",
        "type": "string",
        "page": 1,
        "confidence": 0.654770247993015,
        "coordinates": {
          "x": 0.863306451612903,
          "y": 0.268167569108008,
          "w": 0.0584677419354839,
          "h": 0.00854944428612142
        }
      },
      {
        "name": "receiverContact",
        "category": "receiver",
        "value": "Linda Owens",
        "type": "string",
        "page": 1,
        "confidence": 0.622583129314276,
        "coordinates": {
          "x": 0.150403225806452,
          "y": 0.320604160729553,
          "w": 0.0943548387096774,
          "h": 0.00797948133371335
        }
      },
      {
        "name": "receiverAddress",
        "category": "receiver",
        "value": "5584 Nickel Road KINTA, Oklahoma 74552",
        "type": "string",
        "page": 1,
        "confidence": 0.587670087023011,
        "coordinates": {
          "x": 0.14758064516129,
          "y": 0.275577087489313,
          "w": 0.170564516129032,
          "h": 0.0287831290966087
        }
      },
      {
        "name": "receiverName",
        "category": "receiver",
        "value": "Future Inc.",
        "type": "string",
        "page": 1,
        "confidence": 0.605180115394649,
        "coordinates": {
          "x": 0.148387096774194,
          "y": 0.257623254488458,
          "w": 0.0770161290322581,
          "h": 0.00883442576232546
        }
      },
      {
        "name": "taxId",
        "category": "amounts",
        "value": null,
        "type": "string",
        "page": null,
        "confidence": null,
        "coordinates": {
          "x": 0,
          "y": 0,
          "w": 0,
          "h": 0
        },
        "group": 1
      },
      {
        "name": "shippingAmount",
        "category": "amounts",
        "value": null,
        "type": "number",
        "page": null,
        "confidence": null,
        "coordinates": {
          "x": 0,
          "y": 0,
          "w": 0,
          "h": 0
        }
      },
      {
        "name": "currencyCode",
        "category": "amounts",
        "value": "USD",
        "type": "string",
        "page": 1,
        "confidence": 0.962176322937012,
        "coordinates": {
          "x": 0,
          "y": 0,
          "w": 0,
          "h": 0
        }
      },
      {
        "name": "purchaseOrderNumber",
        "category": "details",
        "value": null,
        "type": "string",
        "page": null,
        "confidence": null,
        "coordinates": {
          "x": 0,
          "y": 0,
          "w": 0,
          "h": 0
        }
      },
      {
        "name": "netAmount",
        "category": "amounts",
        "value": 1998,
        "type": "number",
        "page": 1,
        "confidence": 0.227824148204592,
        "coordinates": {
          "x": 0.793548387096774,
          "y": 0.447420917640353,
          "w": 0.067741935483871,
          "h": 0.00968937019093763
        }
      }
    ],
    "lineItems": [
      [
        {
          "name": "description",
          "category": "details",
          "value": "Phone 11 Pro 256GB Gold",
          "type": "string",
          "page": 1,
          "confidence": 0.618215461879066,
          "coordinates": {
            "x": 0.0758064516129032,
            "y": 0.376460530065546,
            "w": 0.181451612903226,
            "h": 0.00854944428612137
          }
        },
        {
          "name": "quantity",
          "category": "details",
          "value": 1,
          "type": "number",
          "page": 1,
          "confidence": 0.623341202735901,
          "coordinates": {
            "x": 0.631048387096774,
            "y": 0.37674551154175,
            "w": 0.0108870967741935,
            "h": 0.00797948133371329
          }
        },
        {
          "name": "netAmount",
          "category": "amounts",
          "value": 1149,
          "type": "number",
          "page": 1,
          "confidence": 0.634690215774611,
          "coordinates": {
            "x": 0.794758064516129,
            "y": 0.376460530065546,
            "w": 0.0681451612903226,
            "h": 0.00997435166714161
          }
        },
        {
          "name": "unitPrice",
          "category": "details",
          "value": 1149,
          "type": "number",
          "page": 1,
          "confidence": 0.619598222275575,
          "coordinates": {
            "x": 0.709677419354839,
            "y": 0.376460530065546,
            "w": 0.0681451612903226,
            "h": 0.00997435166714161
          }
        },
        {
          "name": "materialNumber",
          "category": "details",
          "value": null,
          "type": "string",
          "page": 1,
          "confidence": null,
          "coordinates": {
            "x": 0,
            "y": 0,
            "w": 0,
            "h": 0
          }
        }
      ],
      [
        {
          "name": "description",
          "category": "details",
          "value": "Apple Watch Edition GPS + Cellular, 44mm Space Black Titanium Case with Anchor Gray Sport Loop",
          "type": "string",
          "page": 1,
          "confidence": 0.615273810685171,
          "coordinates": {
            "x": 0.0737903225806452,
            "y": 0.402678825876318,
            "w": 0.524193548387097,
            "h": 0.0290681105728128
          }
        },
        {
          "name": "quantity",
          "category": "details",
          "value": 1,
          "type": "number",
          "page": 1,
          "confidence": 0.624722316861153,
          "coordinates": {
            "x": 0.631048387096774,
            "y": 0.403248788828726,
            "w": 0.0108870967741935,
            "h": 0.00826446280991738
          }
        },
        {
          "name": "netAmount",
          "category": "amounts",
          "value": 849,
          "type": "number",
          "page": 1,
          "confidence": 0.613978360380445,
          "coordinates": {
            "x": 0.794758064516129,
            "y": 0.402678825876318,
            "w": 0.0552419354838709,
            "h": 0.00968937019093757
          }
        },
        {
          "name": "unitPrice",
          "category": "details",
          "value": 849,
          "type": "number",
          "page": 1,
          "confidence": 0.591678272073086,
          "coordinates": {
            "x": 0.71008064516129,
            "y": 0.402678825876318,
            "w": 0.0548387096774193,
            "h": 0.00968937019093757
          }
        },
        {
          "name": "materialNumber",
          "category": "details",
          "value": null,
          "type": "string",
          "page": 1,
          "confidence": null,
          "coordinates": {
            "x": 0,
            "y": 0,
            "w": 0,
            "h": 0
          }
        }
      ]
    ]
  },
  "fileType": "pdf",
  "enrichment": {
    "employee": [
      {
        "id": "E0001",
        "confidence": 1
      }
    ]
  }
}
```

You have now successfully used the employee entity to get enrichment data predictions for the document you uploaded to Document Information Extraction.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](See all enrichment data entries)]

To see a list of the enrichment data entries you have created:

1. Expand the **GET /data** endpoint.

2. Click **Try it out**.

3. Choose the enrichment data **`type`** `employee` and enter your **`clientId`**.

4. Click **Execute**.

![DOX](1get_data_request.png)

You should receive a response like the following:

![DOX](1get_data_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Delete enrichment data)]

To delete enrichment data which has been uploaded before:

1. Expand the **DELETE /data** endpoint.

2. Click **Try it out**.

3. Define the data in the **`payload`** field, so that the system knows which data entry (using, for example, the data entry ID) should be deleted.

    ```JSON
    {
       "value":[
          {
             "id":"E0001"
          }
       ]
    }
    ```

4. Choose the enrichment data **`type`** `employee` and enter your **`clientId`**.

5. Click **Execute**.

![DOX](1delete_data_request.png)

You should receive a response like the following:

![DOX](1delete_data_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Delete client)]

If you want to delete a client you created in Step 1, use the **DELETE /clients** endpoint.

1. Expand the **DELETE /clients** endpoint.

2. Click **Try it out**.

3. Enter in the **payload** field the client id or multiple client ids (`c_27`, for example) you want to delete.

4. Click **Execute**.

![DOX](1delete_clients_request.png)

You should receive a response like the following:

![DOX](1delete_clients_response.png)

Congratulations, you have completed this tutorial.

[DONE]
[ACCORDION-END]
