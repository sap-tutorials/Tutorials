---
parser: v2
time: 20
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-btp\\, kyma-runtime
---

# Trigger a Microservice with an Event
<!-- description --> Trigger a microservice to run when an event is published into the Kyma runtime.

## Prerequisites
 - [`kubectl` configured to KUBECONFIG downloaded from the Kyma runtime](cp-kyma-download-cli)
 - [Deploy a Go MSSQL API Endpoint in the Kyma Runtime](cp-kyma-api-mssql-golang) tutorial completed
 - [Deploy the Commerce Mock Application in the Kyma Runtime](cp-kyma-mocks) tutorial completed

## You will learn
  - How to trigger a microservice with an event

## Intro
This tutorial relies on the Commerce mock application to publish events into the Kyma runtime. After binding the Commerce mock application to the `dev` Namespace, we will create a service instance of the SAP Commerce Cloud - Events. The service instance will allow for any microservice or lambda function within the `dev` Namespace to subscribe to these events by defining an event subscription. The subscription pairs an event source, the Commerce mock application, and the event type, **order.created**, to a subscriber which in this case will be the Go MSSQL API microservice.

---

### Clone the Git repository


1. Copy the repository URL.

  In your browser, navigate to [kyma-runtime-extension-samples](https://github.com/SAP-samples/kyma-runtime-extension-samples).

  Choose the **Code** button and choose one of the options to download the code locally, or simply run the following command using your CLI at your desired folder location:

```Shell/Bash
git clone https://github.com/SAP-samples/kyma-runtime-extension-samples
```




### Explore the sample


1. Open the `api-mssql-go` directory in your desired editor.

2. Explore the content of the sample.

3. Within the `internal/api/events.go` file you can find the code that handles the consumption of the event data. This code expects to receive a JSON payload containing an `orderCode`. This function is exposed on the path `/orderCodeEvent`, which is defined in `cmd/api/main.go`.

4. Within the `k8s` directory you can find the file `event.yaml` containing the event subscription definition which defines how an event will be consumed. The fields of interest include the following which would have to be altered if a different namespace or application name was used:

| Property                                | Description                                                   | Value                                                         |
|-----------------------------------------|---------------------------------------------------------------|---------------------------------------------------------------|
| spec.filter.filters.eventType.value     | The event source and version to subscribe to                  | sap.kyma.custom.mp-commerce-mock.order.created.v1             |
| spec.sink                               | The URI endpoint of the service that receives the event       | `http://api-mssql-go.dev.svc.cluster.local:80/orderCodeEvent` |





### Apply an event subscription


In this step you will define an event subscription which is used to create a subscriber of an event. This will allow you to specify that your **api-mssql-go** API application, by referencing its service, should receive the payload of the **order.created** event.

1. Apply the subscription by running the following command in the CLI:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/event.yaml
    ```

2. Verify that the subscription was created successfully by running this command:

    ```Shell/Bash
    kubectl get subscription api-mssql-go-event-sub -n dev -o yaml
    ```

    > After the event subscription definition, find a status object indicating the status of the related resources:

    ```yaml
    status:
      conditions:
      - lastTransitionTime: "2021-05-06T14:54:50Z"
        reason: NATS Subscription active
        status: "True"
        type: Subscription active
      emsSubscriptionStatus: {}
      ready: true
    ```



### Test scenario


With the configuration steps completed, you can now test the scenario to validate that it is working as intended.

1. Open the mock application in the browser by choosing **Discovery and Network > `API Rules`** from the menu.

2. Choose the **Host** entry for the **commerce-mock** `APIRule` to open it in the browser. This URL should be similar to:
`https://commerce-mock.*******.kyma.ondemand.com`

3. Choose the **Remote APIS** tab.

    ![Test the Scenario](test-scenario-1.png)

4. Choose the **SAP Commerce Cloud - Events** option.

5. For the **Event Topics**, choose **order.created.v1**.

6. Modify the `orderCode` value as desired and choose **Send Event**.

    ![Test the Scenario](test-scenario-2.png)

7. With the Kyma console, choose **Discovery and Network > `API Rules`** from the menu.

8. Choose the **Host** entry for the **fe-ui5-mssql** `APIRule` to open the application in the browser. This should be similar to:
`https://fe-ui5-mssql.*******.kyma.ondemand.com`

9. You should now see the data received by the event as shown below:

    ![Test the Scenario](test-scenario-3.png)


---
