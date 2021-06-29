---
title: Connect SAP Private Link Service to Microsoft Azure Private Link Service
author_profile: https://github.com/AnnikaGonnermann
description: Connect SAP Private Link service (Beta) to Microsoft Azure Private Link Service with Cloud Foundry CLI and bind the service instance to your app or create a service key.
auto_validation: validation
time: 10
tags: [tutorial>beginner, software-product-function>sap-btp-cockpit, tutorial>license, software-product-function>sap-private-link-service, software-product-function>sap-btp-command-line-interface]
primary_tag: software-product-function>sap-private-link-service
---

## Prerequisites
 - You have a global account and subaccount on SAP Business Technology Platform with SAP Private Link service (Beta) entitlement: [Set Up SAP Private Link Service](private-link-onboarding).
  - You have created a Microsoft Azure Private Link Service in the Azure Portal. You only have to create the Load Balancer resources (pool and rules) and the private link service. The section "Create a private endpoint" can be skipped, as SAP Private Link service (Beta) will establish the connection for you. See [Create a Private Link service by using the Azure portal](https://docs.microsoft.com/en-us/azure/private-link/create-private-link-service-portal).
 - You have installed Cloud Foundry CLI. See [Install the Cloud Foundry Command Line Interface (CLI)](cp-cf-download-cli).


## Details
### You will learn
  - How to create a SAP Private Link Service (Beta) instance to connect to your Microsoft Azure Private Link Service using Cloud Foundry CLI
  - How to bind the service instance to your application using Cloud Foundry CLI

SAP Private Link service (Beta) establishes a private connection between applications running on SAP BTP and selected services in your own IaaS provider accounts. By reusing the private link functionality of our partner IaaS providers, you can access your services through private network connections to avoid data transfer via the public internet.

!![Overview of SAP Private Link service functionality](private-endpoint.png)

---

[ACCORDION-BEGIN [Step 1: ](Check offerings of SAP Private Link Service )]

After you've logged in as described in [Install the Cloud Foundry Command Line Interface (CLI)](cp-cf-download-cli), access the **Service Marketplace** of SAP BTP. Open a command prompt on your computer and type in the following:

```Shell/Bash
cf marketplace
```

You can now see the offering, the plan, and the description, as is shown in this example:

```Shell/Bash
$ cf marketplace
Getting all service offerings from marketplace in org ... / xy… trial as admin...

offering      plans      description                                                                                                                                                    
privatelink   standard   SAP Private Link service (BETA) establishes a private connection between selected SAP BTP services and selected services in your own IaaS provider accounts.
```

Make sure you can see `privatelink` in the sample output.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get Resource-ID for Azure Private Link Service)]

To create and enable a private link, you need to define the connection to the Microsoft Azure Private Link Service first. To do so, you need the Resource-ID of your Microsoft Azure Private Link Service:

1. Go to the Azure portal and navigate to **Private Link Center** > **Private link services**.
2. Click on the desired Azure Private Link service that you created as part of the prerequisites and select **Properties**.
3.	Copy the **Resource ID** and save it for later use.

!![Get Resource-ID](private-endpoint-resource-ID.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create private link service)]

Currently, you do not have any service instances enabled. Therefore, you need to create one. To create a new private link, you need the following information:

- offering (`privatelink`)
- plans (`standard`)
- a unique name (for instance, `privatelink-test`)
- and the Resource-ID from Microsoft Azure (for instance, `/subscriptions/<subscription>/resourceGroups/<rg>/providers/Microsoft.Network/privateLinkServices/<my-private-link-service>`)

Enter `cf create-service` and add that information. Your command should look like this:

```Shell/Bash
cf create-service privatelink standard privatelink-test -c '{"resourceId": "Resource-ID"}'
```

> **Example**:
`cf create-service privatelink standard privatelink-test -c '{"resourceId":"/subscriptions/<subscription>/resourceGroups/<rg>/providers/Microsoft.Network/privateLinkServices/<my-private-link-service>"}'`

If the creation of the service instance was accepted, you receive a success message telling you to proceed.

> **Tip**: You can add an optional description to your CF CLI `cf create service` command, for example `"requestMessage": "Please approve ASAP."` to provide some extra context.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check status of private link)]

To check the current status of the newly created service instance, you need the name of your service instance (in this example `privatelink-test`). Type in the following:

```Shell/Bash
cf service privatelink-test
```

Under "message", you can see the current status. Renew the command after approximately one minute. You should see the following message:

```Shell/Bash
Showing status of last operation from service verify-privatelink...

status:    create in progress
message:   Please approve the connection for Private Endpoint 'endpoint-name' in your Azure portal
```

Copy the *endpoint-name* from the success message. You need it in the next step.

> Execute this command again, in case there's no change in the current status. If you receive an error message, go back to the previous steps.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Approve connection in Azure)]

Return to Microsoft Azure portal:

1. Select **Settings > Private endpoint connections**.
2. Search for the name of the private endpoint you received from the success message in the previous step.
3. Select the private end point and click **Approve**.

!![Approve your private endpoint](Private-endpoint-approve-connection-azure.png)

You should now receive a success message that the approval is pending.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check status of private link)]

To check the current status of the newly created service instance, you need the name of your service instance (in this example `privatelink-test). Type in the following:

```Shell/Bash
cf service privatelink-test
```

You should see the following success message:

```Shell/Bash
status:    create succeeded
message:   Endpoint ready for binding
started:   <date>
updated:   <date>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Bind application to service instance)]

Upon the creation of a binding between a CF application and a private link service instance, SAP Private Link service creates a space-scoped [Cloud Foundry application security group](https://docs.cloudfoundry.org/concepts/asg.html) that enables network access to the IP address associated with the Private Endpoint.

To bind the service instance to your application, You need to know the name of your application and your service instance (in this example ```privatelink-test```). Then, execute the following command:

```Shell/Bash
cf bind-service "app-name" "service-instance"
```

> If you do not have an app that you'd like to bind to your service instance, you can create a service key by running ```cf create-service-key <service-instance-name> <key-name>```.
 After the creation of your service binding, your application receives the information on how to connect via the binding credentials. See the following example for binding credentials:

> ```JSON
{
  "privatelink": [
      {
        "instance_name": "privatelink-test",
        "label": "privatelink", // can be used to look up the bound instance programmatically
        "credentials": {
          "hostname": "<private-link-IP>" // internal IP which needs to be used to connect to the service
        }
      }
  ]
}
```

[VALIDATE_1]
[ACCORDION-END]

---

Congratulations! You have successfully completed the tutorial.

---
