---
title: Getting started with the SAP API Business Hub
description: Learn how to find and test the API Business Hub.
primary_tag: products>sap-cloud-platform
author_name: Marius Obert
author_profile: https://github.com/IObert
auto_validation: true
time: 15
tags: [  tutorial>beginner, products>sap-cloud-platform ]

---

## Details
### You will learn  
Want to learn more about the new SAP API Business Hub? Not sure where to get started? Find more about how to find and enable the SAP API Business Hub in your SAP Cloud Platform instance. Once you are in the SAP API Business Hub, start learning about and testing one of the many available APIs.

---

[ACCORDION-BEGIN [Step : ](Open SAP API Business Hub)]
Go to the [SAP API Business Hub](https://api.sap.com/).

![SAP API Business Hub page](1.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Find the APIS in the Hub)]
On the SAP API Business Hub homepage, select **APIs** under **Content Types** to see the available packages in the API Business Hub.

![SAP API Business Hub homepage API tile](2.png)

This will take you to the APIs section on the Homepage. Select the **View More** link to see all available APIs.

![API listing on homepage](3.png)

This will take you to API listing on the SAP API Business Hub.

![Searchable API list page](4.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Select an API to use)]

In the search box, type **`SuccessFactors`** to find the all Success Factors APIs. Click the **Search Icon** or hit **Enter** to search.

![Searching the discover all APIs](5.png)

This will bring up all the APIs relating to SuccessFactors.

![Success Factors API search results](6.png)

Click on the **SAP Success Factors Foundation/Platform** package.

![Selecting the SuccessFactors API](7.png)

This will bring up the SAP SuccessFactors Foundation/Platform API artifact details.

![SAP SuccessFactors API overview page](8.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Test the API in the Hub)]
Let's select an API to test and see some results. Select the **User Management** tile.

![User Management tile on page](9.png)

In order to test the API in the API Business Hub, you need to login. Click the **Log On** button at the top of the page to do so.

![Log On button in SAP API Business Hub](10.png)

When you look at the API Documentation again for the `GET /User` endpoint, you will see a **Parameters** section with a bunch of fields. To test different query parameters, you need to click the **Try Out** button on the API endpoint.

![try out button on API](11.png)

This will enable to Parameter fields to make them editable. You can test different query parameters by providing a value in the input of the specified data type for that parameter.

![editable fields on User endpoint](12.png)

The `GET /User` method will return a list of users from Success Factors. The top parameter tells the query to retrieve only the top _n_ results once all the other filtering is done. We don't want to see all a users data, only selected data points. In the `$select` parameter, using the _control_ key for Windows or _command_ key for MacOS, select `firstName`, `lastName`, and `jobTitle`. This will limit the data set returned to this 3 fields for the matching results and therefore reduce the size of the resulting data set. Also set an [OData Filtering Expression](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html#_Toc445374625) to show only employees named `Sami`: **`firstName eq 'Sami'`**.

![Query parameters populated in the API Hub](13.jpg)

Once your parameters are all set, click the **Execute** button at the bottom of the documentation for that method.

![Try it out button location](14.png)

If there aren't any issues, you will see the resulting request URL and the response body (in `JSON`) on your page.

Feel free to keep playing around in the SAP API Business Hub to better understand the methods available in the User Management API.

[VALIDATE_4]
[ACCORDION-END]
