---
parser: v2
author_name: Madeline Schaefer
author_profile: https://github.com/Madeline-Schaefer
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product-function>sap-btp-cockpit, software-product>sap-business-technology-platform, tutorial>license, software-product-function>sap-btp-command-line-interface, software-product-function>sap-private-link-Service]
primary_tag: software-product-function>sap-btp-cockpit
---

# Connect SAP Private Link Service to Google Cloud Private Service Connect

**Requires Customer/Partner License** | **Beginner** | **10 min.**
**Tags:** SAP Private Link service, Beginner, SAP BTP cockpit, Tutorial, SAP Business Technology Platform, SAP BTP command line interface

Connect SAP Private Link service to Google Cloud Private Service Connect with Cloud Foundry CLI and bind the service instance to your app or create a service key.

## You will learn

* How to create a SAP Private Link service instance to connect to your Google Cloud Private Service Connect using Cloud Foundry CLI
* How to bind the service instance to your application using Cloud Foundry CLI

**Created by:** Madeline Schaefer (December 5, 2022)
**Contributors:** June 24, 2021

---

## PREREQUISITES

* You have a global account and subaccount on SAP Business Technology Platform with SAP Private Link service entitlement: [Set Up SAP Private Link service](https://developers.sap.com/tutorials/private-link-onboarding.html).
* You have created a Google Cloud Private Service Connect in the Google Cloud console. You only have to create the Load Balancer resources and the private link service. The section “Create a private endpoint” can be skipped, as SAP Private Link service will establish the connection for you. See [Create a Private Link service by using the Google Cloud portal](https://docs.cloud.google.com/vpc/docs/private-service-connect).
    When creating the Google Cloud Private Service Connect, make sure you allowlist the SAP BTP Cloud Foundry accounts as described in [Only allow requests from SAP BTP Cloud Foundry and Kyma environment’s accounts](../docs/best-practices-for-secure-endpoint-approval-on-gcp.md).
* You have installed Cloud Foundry CLI. See [Install the Cloud Foundry Command Line Interface (CLI)](https://developers.sap.com/tutorials/cp-cf-download-cli.html).

SAP Private Link service establishes a private connection between applications running on SAP BTP and selected services in your own IaaS provider accounts. By reusing the private link functionality of our partner IaaS providers, you can access your services through private network connections to avoid data transfer via the public internet.

---

## Step 1: Check offerings of Private Link service

After you’ve logged in as described in [Install the Cloud Foundry Command Line Interface (CLI)](https://developers.sap.com/tutorials/cp-cf-download-cli.html), access the Service Marketplace of SAP BTP. Open a command prompt on your computer and type in the following:

```shell
cf marketplace
```

You can now see the offering, the plan, and the description, as is shown in this example:

```shell
$ cf marketplace
Getting all service offerings from marketplace in org ... / xy… trial as admin...

offering      plans      description
privatelink   standard    Link service establishes a private connection between selected SAP BTP services and selected services in your own IaaS provider accounts.
```

Make sure you can see `privatelink` in the sample output.

## Step 2: Get Resource-ID for Google Cloud Private Service Connect

To create and enable a private link, you need to define the connection to the service first. To do so, you need the Service attachment:

1. Go to the Google Cloud console.
2. Navigate to the *Private Service Connect* for which you want to find out the Service Attachment, for example: Private Service Connect > Published services.
3. Select a published service and copy its **Service Attachment** path.

## Step 3: Create private link service

Currently, you do not have any service instances enabled. Therefore, you need to create one. To create a new private link, you need the following information:

* offering (`privatelink`),
* plans (`standard`),
* a unique name (for instance, `privatelink-test`),
* and the Service Attachment path from Google Cloud console (for example, `projects/<gcp-project>/regions/<gcp-region>/serviceAttachments/<my-service-attachment>`).

Enter `cf create-service` and add that information. Your command should look like this:

```shell
cf create-service privatelink standard privatelink-test -c '{"resourceId": "Service-Attachment"}'
```

> Example:
> `cf create-service privatelink standard privatelink-test -c '{"resourceId":"projects/<gcp-project>/regions/<gcp-region>/serviceAttachments/<my-service-attachment>"}'`
If the creation of the service instance was accepted, you receive a success message telling you to proceed.
> Tip: You can add an optional description to your CF CLI `cf create service` command, for example `"requestMessage": "Please approve ASAP."` to provide some extra context.

## Step 4: Check status of private link

To check the current status of the newly created service instance, you need the name of your service instance (in this example `privatelink-test`). Type in the following:

```shell
cf service privatelink-test
```

You should see the following success message:

```shell
status:    create succeeded

message:    Private Endpoint to ResourceID 'service-attachment' successfully provisioned, ready for binding.
started:   <date>
updated:   <date>
```

## Step 5: Bind application to service instance

Upon the creation of a binding between a CF application and a private link service instance, Private Link service creates a space-scoped Cloud Foundry application security group that enables network access to the IP address associated with the Private Endpoint.

To bind the service instance to your application, You need to know the name of your application and your service instance (in this example `privatelink-test`). Then, execute the following command:

```shell
cf bind-service "app-name" "privatelink-test"
```

After the creation of your service binding, your application receives the information on how to connect via the binding credentials:

```json
{
    "privatelink": [
        {
            "instance_name": "privatelink-test",
            "label": "privatelink", // can be used to look up the bound instance programmatically
            "credentials": {
                "hostname": "<private-link hostname>", // internal hostname to connect to the service
                "additionalHostname": "<private-link additional hostname>" // additional internal hostname to connect to the service
            },
            "tags": [
                "privatelink",
                "privatelinkservice"
            ]
        }
    ]
}
```

> **Note:** You need to restage the application for the `/etc/hosts` changes to take effect: `cf restage "app-name"`.
If you do not have an app that you’d like to bind to your service instance, you can create a service key by running `cf create-service-key <service-instance-name> <key-name>`. The service key credentials look as follows:

```json
{
    "endpoints": [
        "<IP address>"
    ]
}
```

---

Congratulations! You have successfully completed the tutorial. 

---
