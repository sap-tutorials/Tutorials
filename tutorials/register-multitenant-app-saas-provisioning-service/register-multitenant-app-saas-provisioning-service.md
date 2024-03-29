---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-btp\, kyma-runtime
---

# Register a Multitenant Application to the SAP SaaS Provisioning Service
<!-- description --> Register a Node.js application to the SAP SaaS Provisioning Service (saas-registry) in the Kyma runtime to make it available for subscription to SaaS consumer tenants.

## Prerequisites
- You have finished the tutorial [Implement Subscription Callbacks for a Multitenant Application](implement-subscription-callback-multitenant)

## You will learn
- How to create SaaS Provisioning Service instance for multitenancy in the Kyma runtime
- How to consume SaaS Provisioning Service instance credential from the backend application


---

### Create SAP SaaS Provisioning Service Instance and Credential


**1.** Create an instance and binding of SAP SaaS Provisioning Service by adding the following part to the deployment file `k8s-deployment-services.yaml`:

```YAML
################### SaaS Provisioning Service ###################
---
apiVersion: services.cloud.sap.com/v1alpha1
kind: ServiceInstance
metadata:
  name: saas-registry-service
spec:
  serviceOfferingName: saas-registry
  servicePlanName: application
  parameters:
    # the xsappname refers to the one defined in xsuaa service
    xsappname: multitenant-kyma-demo
    displayName: Multitenancy Sample in Kyma
    description: A NodeJS application to show how to use the SaaS registry to build a multi-tenant application on BTP Kyma Runtime'
    category: 'Provider: TIA'
    appUrls:
      # url registered in the kyma-broker which handles SaaS provisioning (subscription/deletion of saas instances)
      onSubscription: https://<subaccount-subdomain>-node.<cluster-domain>/callback/v1.0/tenants/{tenantId}
      onSubscriptionAsync: false
      onUnSubscriptionAsync: false
---
apiVersion: services.cloud.sap.com/v1alpha1
kind: ServiceBinding
metadata:
  name: saas-registry-service-binding
spec:
  serviceInstanceName: saas-registry-service
  secretName: saas-registry-service-binding
```

**2.** Specify the following parameters:

| Parameters            | Description                                                  |
| --------------------- | ------------------------------------------------------------ |
| `xsappname`             | The `xsappname` configured in the security descriptor file used to create the XSUAA instance (see [Develop the Multitenant Application](https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/ff540477f5404e3da2a8ce23dcee602a.html??locale=en-US&version=Cloud)). |
| `getDependencies`       | (Optional) Any URL that the application exposes for GET dependencies. If the application doesn't have dependencies and the callback isn't implemented, it shouldn't be declared.</br> The JSON response of the callback must be encoded as either UTF8, UTF16, or UTF32, otherwise an error is returned. |
| `onSubscription`        | Any URL that the application exposes via PUT and DELETE subscription. It must end with `/{tenantId}`. The tenant for the subscription is passed to this callback as a path parameter. You must keep `{tenantId}` as a parameter in the URL so that it's replaced at real time with the tenant calling the subscription. This callback URL is called when a subscription between a multitenant application and a consumer tenant is created (PUT) and when the subscription is removed (DELETE). |
| `displayName`           | (Optional) The display name of the application when viewed in the cockpit. For example, in the application's tile. If left empty, takes the application's technical name. |
| `description`           | (Optional) The description of the application when viewed in the cockpit. For example, in the application's tile. If left empty, takes the application's display name. |
| `category`              | (Optional) The category to which the application is grouped in the **Subscriptions** page in the cockpit. If left empty, gets assigned to the default category. |
| `onSubscriptionAsync`   | Whether the subscription callback is asynchronous.If set to true, `callbackTimeoutMillis` is mandatory. |
| `callbackTimeoutMillis` | The number of milliseconds the SAP SaaS Provisioning service waits for the application's subscription asynchronous callback to execute, before it changes the subscription status to FAILED. |
| `allowContextUpdates`   | Whether to send updates about the changes in contextual data for the service instance.For example, when a subaccount with which the instance is associated is moved to a different global account.Defaut value is false. |






### Access Instance Credential from Backend Application 


Mount the Secret as a volume to the pod in the `k8s-deployment-backend.yaml`:

```YAML[3-5,9-11]
        volumeMounts:
        ......
        - name: saas-registry-volume
          mountPath: "/etc/secrets/sapcp/saas-registry/saas-registry-service"
          readOnly: true
      ......
      volumes:
      ......
      - name: saas-registry-volume
        secret:
          secretName: saas-registry-service-binding
```




---
