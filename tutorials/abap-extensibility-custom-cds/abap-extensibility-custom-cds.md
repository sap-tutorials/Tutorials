---
title: Custom CDS View and its exposure as a Web Service  
description: Create a custom CDS view from an existing data source and expose it as a Web Service.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>abap-extensibility ]
---

## Prerequisites  
The Prerequisites for the usage of CDS Views is the assignment of the Business Catalog **`SAP_CORE_BC_EXT`** to your user.
Your user needs to assign Business Catalog **`SAP_CORE_BC_COM`** to be able to use the Communication Management Application, your user needs Business Catalog **`SAP_CORE_BC_COM`**.

## Next Steps
 (coming soon).
## Details
If you need access to data in your system in a way the system does not offer so far, you can create your own data access by making use of a Custom Core Data Services view (Custom CDS view). Usually, the data of an application is distributed across several database tables. Using CDS views allows you to model the data access without changing underlying database tables. In general, CDS views are intended to be consumed by a UI, analytics or other systems. CDS View can be built on CDS Views as well. To simplify the data access there are pre-delivered CDS Views, Custom CDS Views can only be built on.
### You will learn  
In this tutorial the use case for the custom CDS view is to minimize the data set gotten by a pre-delivered CDS View for Customers. Afterwards, you will expose the custom CDS View as a Web Service so that external systems can make use of it to read the Customer data it provides.

### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Open Custom CDS Views Application)]
Navigate to the **Extensibility** category and press the **Custom CDS Views** tile to start the application.

![start Custom CDS View App](ExtensibilityFLP1-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Select Data Source)]
This brings you to the data source list of the available data sources for Custom CDS Views.
Search for a data source you want to use as a basis and **select** the data source **(1)** `I_Customer`.

This is the primary data source of the new Custom CDS view.

![Select Data Source](dataSourceList2-0.png)

Now execute **create (2)** to create a new Custom CDS view.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create Custom CDS View)]
On the general tab enter name `YY1_New_Customer` and label `New Customer`.

![Select Data Source](createCustomCDSView1-0.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Select Fields)]
This step is used to `taylor` a given CDS View to the needs of a customer.
Now switch to the Field Selection tab and **select** following **fields (1)**:
- `Customer`
- `CustomerName`
- `CustomerFullName`
- `Country`
- `CityName`
- `PostalCode`
- `StreetName`
These fields will be available in the Custom CDS View.

![Select Data Source](publish2.png)
Now **save** and **publish** the view.

Result:
This saves and activates the view.
In case the primary data source is access protected, the new view is protected, too.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Generate OData Service)]
Switch back to the general tab to generate an OData Service by `checkmarking` it.
Afterwards **save** and **publish** your change. Now your OData Service is generated.

![Select Data Source](odataservice.png)
Result:
Now new service `<ViewName>_CDS` got created and the service is registered for further usage in the communication scenario.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Open Custom Communication Scenarios Application)]
Go back to Press the **Custom Communication Scenarios** tile to start the application.


![Select Data Source](ExtensibilityFLP2-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add New Scenario)]
In **Custom Communication Scenario** add a new Scenario by clicking on **new** button.

![Select Data Source](new.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add Name and Description)]
A new window will open where you can define the **Name of the New Scenario** and its **Description**.
![Select Data Source](newname.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Add Services)]
First you have to click the **add** button. Afterwards search for New Customer and add `YY_NEW_CUSTOMER_CDS` as a service.
![Select Data Source](service.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Publish Communication Scenario)]
After adding the service `YY_NEW_CUSTOMER_CDS`, the result will be shown in the following way:
![Select Data Source](service2.png)
Now you can **publish** your Communication Scenario.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Use Communication System and User)]
If you already have a communication system and user then use them in the following steps.
Otherwise you can create a new communication system and user by making use of the following example.
Therefore go to Step 2: Creating a Communication System and User.
https://blogs.sap.com/2017/05/17/part-viii-custom-business-object-exposure-as-external-web-service-bonus-plan/

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Create Communication Arrangement)]
Navigate to the **Communication Management** category and press the **Communication Arrangements** tile to start the application.

![Select Data Source](arrangement.png)
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Add New Communication Arrangement)]
Click on the **new** button to create a new Communication Arrangement.

![Select Data Source](maintain.png)
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Select Scenario and Describe Arrangement Name)]
You can choose `YY1_New_Customer` as Scenario and select the **create** button by clicking the value help.

![Select Data Source](arrangementnewcustomer.png)
[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 15: ](Save Communication Arrangement)]
Now select your Service, like `EXTERNAL_SYSTEM` as Communication System and **save** the Communication Arrangement.

![Select Data Source](save.png)
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Generating URL)]
After you have saved your Communication Arrangement, the URL will be generated.

![Select Data Source](odata.png)

Result:
The role containing the authorizations for the service(s) is assigned to a communication user. After this step, you can call the service in a web browser and logon with the communication user. You can consume this service also in tools like Excel using the comm. user logon.

Note: the communication arrangement is a configuration object that is created in test and production system separately.

[DONE]
[ACCORDION-END]


## Next Steps
(coming soon)
