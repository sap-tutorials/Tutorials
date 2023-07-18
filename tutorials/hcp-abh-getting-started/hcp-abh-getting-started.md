---
parser: v2
primary_tag: products>sap-business-technology-platform
author_name: Prathibha GC
author_profile: https://github.com/Prathibha-Mouli
auto_validation: true
time: 15
tags: [  tutorial>beginner, products>sap-business-technology-platform, topic>sap-api-business-hub  ]

---

# Tour the SAP Business Accelerator Hub
<!-- description --> Explore the various features of the SAP SAP Business Accelerator Hub, and learn how to find and test APIs.

## You will learn
- How to get started with SAP Business Accelerator Hub

## Intro
SAP Business Accelerator Hub is a website to discover, explore, and test APIs from various SAP products such as SAP S/4HANA, SAP SuccessFactors, SAP Customer Experience, SAP BTP, and selected partners. Apart from various product APIs, you can find detailed information on business processes and partner assets. The new workspace feature also enables you to consume integration and workflow content.

Want to learn more about the new SAP SAP Business Accelerator Hub? Not sure where to get started? Go to [api.sap.com] (https://api.sap.com), and begin your journey.

---

### Go to the SAP Business Accelerator Hub
In your browser, open [SAP Business Accelerator Hub](https://api.sap.com/).

<!-- border -->![SAP Business Accelerator Hub page](1.png)

You can explore products, business processes as well as different categories of assets, along with partner and featured content. Choose **Explore** for quick access to the various sections of the SAP Business Accelerator Hub.

### Explore API content
On the SAP Business Accelerator Hub homepage, choose **Categories**.

<!-- border -->![SAP Business Accelerator Hub homepage API tile](2.png)

Choose APIs from the different categories available.

<!-- border -->![SAP Business Accelerator Hub APIs section](2a.png)

### Search for an API

Different APIs, policy templates, and API packages are available under the API category. An API package contains one or more APIs. You can browse through different types of APIs such as REST, OData, and SOAP APIs.

<!-- border -->![Searching for types of APIs APIs](3.png)

You can explore APIs either by selecting a type of an API or by selecting an API package. Navigate to the Packages tab. You can choose any package for testing.

<!-- border -->![API Hub Packages search](4.png)

For the purpose of this tutorial, we are going to choose the `SAP Cloud for Customer` package (find it using the search facility).

<!-- border -->![SAP Cloud for Customer Package Search](4a.png)

### Explore the API

On choosing the **SAP Cloud for Customer** package, you can view the available APIs in the package. Select the Campaign API to explore it in detail.

<!-- border -->![SAP Cloud for Customer API package details](5.png)

The **`Overview`** section briefs you about the API. From here you can download the API reference file, view the documentation available, and check for the API version.

The **`API Reference`** section provides information and functions that are offered to consume the API.

<!-- border -->![Campaign API References](5a.png)

The API can contain one or more endpoints which are organized into groups. On the left-hand side of selected API - **Campaign**, you can see six groups of endpoints.

<!-- border -->![Campaign API](5b.png)

Various HTTP methods of API are used to address the functionality of endpoints. While exploring the endpoints, you may wish to collapse the information by selecting the `GET` operation row. You can explore the available endpoints for the API to understand it in detail.

### Test an API endpoint

The SAP Business Accelerator Hub allows you to test APIs. A sandbox environment is provided to test the Campaign API. To take advantage of this, ensure you are logged on. Select Login at the top of the page.

<!-- border -->![Login](6.png)

Once logged on, choose the Try Out button in the **`Overview`** section.

<!-- border -->![The "Try out" button](6a.png)

Selecting this will open the Try Out View.

For the purpose of this tutorial the **`GET /CampaignCollection`** operation has been selected for testing.

<!-- border -->![Get method for Campaign API](6b.png)

Calling the `GET` operation on the `/CampaignCollection` endpoint will return a list of entities from Campaign Collection. The `$top` parameter can be used to retrieve only the first _n_ results once all the other filtering is done. Specify a value of **20** for this parameter.

<!-- border -->![editable fields on User endpoint](7.png)

You can also select the `$orderby` to sort the display order based on `Object ID`, `Object ID description`, `Campaign Type`, and so on.

Once the required inputs are entered, Choose `Run` at the top right corner to view the response.

<!-- border -->![API Run](8.png)

The response section displays various entities fetched calling the `GET` method.

<!-- border -->![Response Body](9.png)


Feel free to explore other endpoints in this Campaign API.

Congratulations, you have just taken your first steps with the SAP Business Accelerator Hub and should now be able to navigate it and the content within, confidently.
