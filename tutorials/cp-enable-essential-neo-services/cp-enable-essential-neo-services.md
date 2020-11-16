---
title: Enable Essential Neo Services
description: Learn how to enable an essential service, like the Portal service, in the SAP Cloud Platform Neo data center that you can use later for additional tutorials.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [ tutorial>beginner, tutorial>license ]
time: 5
---

## Prerequisites
 - Learn how to [sign up](https://developers.sap.com/tutorials/hcp-create-trial-account.html) for a free  SAP Cloud Platform trial.

## Details
### You will learn
  - Where to find the services section and the associated categories
  - How to enable a new service for your development environment

[ACCORDION-BEGIN [Step 1: ](Navigate to the services section)]

In the Neo environment, you enable services in the SAP Cloud Platform cockpit.  From the [SAP Cloud Platform Trial welcome page](https://account.hanatrial.ondemand.com/#/home/welcome) homepage select the Neo tile.

![Neo Access](neo-access.png)

Once logged in, you will find the services in the Neo Environment can be found on the left side menu by clicking on **Services**:

![Services menu](click-services-neo.png)

The cockpit will now list all services grouped by service category.

![choose category or service to enable](choose-category-or-service-to-enable.png)

Some of the services are basic services, which are provided with SAP Cloud Platform and are ready-to-use. In addition, extended services are available. A ***green*** or ***gray*** label on the tile for a service indicates if this service is enabled/not enabled.

You must first enable the service and apply the service-specific configuration (for example, configure the corresponding roles and destinations) before any `subaccount` members can use it.

The term ***essential*** may mean different things to different developers, but this is a short list I would start with.

- User Management - Identity Provisioning
- Data Management - SAP HANA
- Data Privacy & Security - OAuth 2.0
- Developer Experience - SAP Web IDE Full-Stack
- Integration `OData` Provisioning
- Mobile - Development & Operations, `std`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable a service)]

Now let's enable one of the services listed above.

Click on the tile for **Portal** *Service* from the tile in the *User Experience section*, or from the services menu drop down for *User Experience*. You can also search for any service by name.

The service description will give you a definition of what the service you are going to enable does.

When ready to enable the service simply click the **Enable** button.

![enable button portal service](enable-button-portal-service.png)

The status will change to ***processing***

![enable service processing](enable-service-processing.png)

Now after a few moments, you should see the **Enabled** button has changed to green and the action items for the service are now active.

![enable service button green](enable-service-button-green.png)

[VALIDATE_1]
[ACCORDION-END]
