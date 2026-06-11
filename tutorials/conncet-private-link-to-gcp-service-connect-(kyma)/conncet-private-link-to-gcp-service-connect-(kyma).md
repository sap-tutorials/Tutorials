---
parser: v2
author_name: Madeline Schaefer
author_profile: https://github.com/Madeline-Schaefer
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product-function>sap-btp-cockpit, software-product>sap-business-technology-platform, tutorial>license, software-product-function>sap-btp-command-line-interface, software-product-function>sap-private-link-Service]
primary_tag: software-product-function>sap-btp-cockpit
---

# Connect SAP Private Link Service to Google Cloud Private Service Connect (Kyma)

**Requires Customer/Partner License** | **Beginner** | **10 min.**
**Tags:** SAP Private Link service, Beginner, SAP BTP cockpit, Tutorial, SAP Business Technology Platform, SAP BTP command line interface

Connect SAP Private Link service to Google Cloud Private Service Connect and bind the service instance on a Kyma cluster.

## You will learn

* How to create a SAP Private Link service instance to connect to your Google Cloud Private Service Connect using `kubectl` CLI
* How to bind the service instance to your Kyma cluster using `kubectl` CLI

**Created by:** Madeline Schaefer (February 12, 2024)
**Contributors:** Madeline-Schaefer (February 7, 2024)

---

## Prerequisites

* You have a global account and subaccount on SAP Business Technology Platform with SAP Private Link service entitlement: [Set Up SAP Private Link service](https://developers.sap.com/tutorials/private-link-onboarding.html).
* You have created a Google Cloud Private Service Connect in the Google Cloud console. You only have to create the Load Balancer resources and the private link service. The section “Create a private endpoint” can be skipped, as SAP Private Link service will establish the connection for you. See [Create a Private Link service by using the Google Cloud console](https://docs.cloud.google.com/vpc/docs/private-service-connect).
    When creating the Google Cloud Private Service Connect, make sure you allowlist the SAP BTP Kyma accounts as described in [Only allow requests from SAP BTP Cloud Foundry and Kyma environment's accounts](../docs/best-practices-for-secure-endpoint-approval-on-gcp.md).
* You have enabled Kyma Runtime and configured `kubectl` CLI. See [Enable SAP BTP, Kyma Runtime Using the Command Line](https://developers.sap.com/tutorials/btp-cli-setup-kyma-cluster.html).

SAP Private Link service establishes a private connection between applications running on SAP BTP and selected services in your own IaaS provider accounts. By reusing the private link functionality of our partner IaaS providers, you can access your services through private network connections to avoid data transfer via the public internet.

---

## Step 1: Check offerings of Private Link service

After you've logged in as described in [Enable SAP BTP, Kyma Runtime Using the Command Line](https://developers.sap.com/tutorials/btp-cli-setup-kyma-cluster.html), you can check all available entitlements for your subaccount. Open a command prompt and enter the following command:

```shell
btp list accounts/entitlements
```

You can now see a list of service names and service plans, as shown in this example:

```shell
$ btp list accounts/entitlements
Showing entitlements for subaccount 9be57735-1234-1234-1234-0123456789ab:
service name service plan quota
...
privatelink standard 8
...
```

Make sure you can find `privatelink` under the service name column in the output.

## Step 2: Get Resource-ID for Google Cloud Private Service Connect

To create and enable a private link, you need to define the connection to the service first. To do so, you need the Service attachment:

1. Go to the Google Cloud console.
2. Navigate to the *Private Service Connect* for which you want to find out the Service Attachment, for example: Private Service Connect > Published services.
3. Select a published service and copy its **Service Attachment** path.

## Step 3: Make sure SAP BTP Operator module is enabled

SAP BTP Operator module `btp-operator` must be enabled before creating and managing SAP Private Link Service Instances in Kyma. Otherwise, you get the following error message: `resource mapping not found for {...} ensure CRDs are installed first`. Additionally, it is required that `btp-operator` module is version 1.1.0 or newer. To enable the `btp-operator` module, follow the procedure described in [Enable and Disable a Kyma Module](https://help.sap.com/docs/btp/sap-business-technology-platform/enable-and-disable-kyma-module).

## Step 4: Create private link service

Currently, you do not have any service instances enabled. Therefore, you need to create one. To create a new private link, you need the following information:

* offering (`privatelink`),
* plans (`standard`),
* a unique name (for instance, `privatelink-test`),
* and the Service Attachment path from Google Cloud console (for example, `projects/<gcp-project>/regions/<gcp-region>/serviceAttachments/<my-service-attachment>`).

Enter the following command and fill in the necessary information:

```yaml
kubectl create -f - <<EOF
apiVersion: services.cloud.sap.com/v1
kind: ServiceInstance
metadata:
  name: privatelink-test
spec:
  serviceOfferingName: privatelink
  servicePlanName: standard
  parameters:
    resourceId: "projects/<gcp-project>/regions/<gcp-region>/serviceAttachments/<my-service-attachment>"
    requestMessage: Please Approve
EOF
```

## Step 5: Ensure private link was created successfully

To check the current status of the newly created service instance, you need the name of your service instance (in this example `privatelink-test`). Type in the following:

```shell
kubectl get ServiceInstance privatelink-test -o wide
```

You should see the following success message:

```shell
NAME                   OFFERING      PLAN       STATUS     READY   AGE   ID            MESSAGE
privatelink-test       privatelink   standard   Created    True    2m    <some UUID>   ServiceInstance provisioned successfully
```

## Step 6: Create a service binding for the service instance

When service binding is created Private Link service enables network access to the IP address associated with the Private Endpoint. To create a new binding, you need the following information:

* unique name of the binding (for example `privatelink-binding-test`)
* the name of the service instance (`privatelink-test`)
* unique name of the secret that will be created (for example `privatelink-secret-test`)

Enter the following command to create a new binding with the example information:

```yaml
kubectl create -f - <<EOF
apiVersion: services.cloud.sap.com/v1
kind: ServiceBinding
metadata:
  name: privatelink-binding-test
spec:
  serviceInstanceName: privatelink-test
  secretName: privatelink-secret-test
EOF
```

If the command is successful, a kubernetes secret with the same name as specified in `secretName` is created. The secret stores information about the private link endpoint in its `hostname` field. Use the following command to print the `hostname` field from the secret:

```shell
kubectl get secret privatelink-secret-test -o jsonpath='{.data.hostname}' | base64 --decode
```

As an example, the output might look like the following:

```shell
000000-000000-0000-0000-07a4762eceb6.pls.sap.internal
```

Follow the steps in [Configure DNS on Kyma](https://help.sap.com/docs/private-link/private-link1/configure-dns-on-kyma).

---

Congratulations! You have successfully completed the tutorial.

---
