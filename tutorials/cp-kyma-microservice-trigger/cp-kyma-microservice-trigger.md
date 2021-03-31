---
title: Trigger a Microservice with an Event
description: Trigger a microservice to run when an event is published into the Kyma runtime.
time: 20
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, products>sap-business-technology-platform]
primary_tag: products>sap-btp\\, kyma-runtime
---

## Prerequisites
 - [`kubectl` configured to KUBECONFIG downloaded from the Kyma runtime](cp-kyma-download-cli)
 - [Deploy a Go MSSQL API Endpoint in the Kyma Runtime](cp-kyma-api-mssql-golang) tutorial completed
 - [Deploy the Commerce Mock Application in the Kyma Runtime](cp-kyma-mocks) tutorial completed

## Details
### You will learn
  - How to trigger a microservice with an event

This tutorial relies on the Commerce mock application to publish events into the Kyma runtime. After binding the Commerce mock application to the `dev` Namespace, we will create a service instance of the SAP Commerce Cloud - Events. The service instance will allow for any microservice or lambda function within the `dev` Namespace to subscribe to these events by defining a Trigger. The Trigger pairs an event source, the Commerce mock application, and the event type, **order.created**, to a subscriber which in this case will be the Go MSSQL API microservice.

---

[ACCORDION-BEGIN [Step 1: ](Clone the Git repository)]

1. Copy the repository URL.

  In your browser, navigate to [kyma-runtime-extension-samples](https://github.com/SAP-samples/kyma-runtime-extension-samples).

  Choose the **Code** button and choose one of the options to download the code locally, or simply run the following command using your CLI at your desired folder location:

```Shell/Bash
git clone https://github.com/SAP-samples/kyma-runtime-extension-samples
```



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Explore the sample)]

1. Open the `api-mssql-go` directory in your desired editor.

2. Explore the content of the sample.

3. Within the `internal/api/events.go` file you can find the code that handles the consumption of the event data. This code expects to receive a JSON payload containing an `orderCode`. This function is exposed on the path `/orderCodeEvent`, which is defined in `cmd/api/main.go`.

4. Within the `k8s` directory you can find the file `event-trigger.yaml` containing the Trigger definition which defines how an event will be consumed. The fields of interest include:

| Property                                | Description                                                   | Value            |
|-----------------------------------------|---------------------------------------------------------------|------------------|
| spec.filter.attributes.eventtypeversion | The event version to subscribe to                             | v1               |
| spec.filter.attributes.source           | The name of the application that publishes the events         | mp-commerce-mock |
| spec.filter.attributes.type             | The event type to subscribe to                                | order.created    |
| spec.filter.subscriber.ref.name         | The name of the service to receive the event                  | api-mssql-go     |
| spec.filter.subscriber.uri              | The URI endpoint of the service that receives the event | `/orderCodeEvent`  |



[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Bind Commerce mock to Namespace)]

In this step you will bind the mock application to the `dev` Namespace. This process is used to enable the usage of the APIs and events of the mock application to the bounded Namespace. The API and events will then appear within the **Service Catalog** of the Namespace.

1. Open the Kyma runtime console.

2. Choose the **Integration > Applications/Systems** menu options.

3. Choose **mp-commerce-mock**.

4. Choose **Create Binding**.

5. Choose the `dev` Namespace.

6. Choose **Create**.

    ![Binding](binding.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a ServiceInstance)]

In this step you will create a service instance of the events exposed by the mock application. This is done to enable the consumption of the events within the `dev` Namespace.

1. Open the Kyma runtime console.

2. Choose the `dev` Namespace.

3. Choose the **Service Management > Catalog** menu options.

4. Choose **mp-commerce-mock**.

    ![Service Instance](service-instance-1.png)

5. Choose the **SAP Commerce Cloud - Events** service plan.

    ![Service Instance Plan](service-instance-2.png)

6. Choose **Add**.

    ![Service Instance Plan](service-instance-3.png)

7. Choose **Create** to create an instance.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Apply a trigger)]

In this step you will define a Trigger which is used to create a subscriber of an event. This will allow you to specify that your **api-mssql-go** API application, by referencing its service, should receive the payload of the **order.created** event.

1. Apply the Trigger by running the following command in the CLI:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/event-trigger.yaml
    ```

2. Verify that the Trigger was created successfully by running this command:

    ```Shell/Bash
    kubectl get trigger api-mssql-go-trigger -n dev -o yaml
    ```

    > After the Trigger definition, find a status object indicating the status of the related resources:

    ```yaml
    status:
      conditions:
      - lastTransitionTime: "2020-10-07T16:09:44Z"
        status: "True"
        type: BrokerReady
      - lastTransitionTime: "2020-10-07T16:09:26Z"
        status: "True"
        type: DependencyReady
      - lastTransitionTime: "2020-10-07T16:09:44Z"
        status: "True"
        type: Ready
      - lastTransitionTime: "2020-10-07T16:09:27Z"
        status: "True"
        type: Subscribed
      - lastTransitionTime: "2020-10-07T16:09:26Z"
        status: "True"
        type: SubscriberResolved
    ```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test scenario)]

With the configuration steps completed, you can now test the scenario to validate that it is working as intended.

1. Open the mock application in the browser by choosing **Configuration > `APIRules`** from the menu.

2. Choose the **Host** entry for the **commerce-mock** `APIRule` to open it in the browser. This URL should be similar to:
`https://commerce.*******.kyma.shoot.live.k8s-hana.ondemand.com`

3. Choose the **Remote APIS** tab.

    ![Test the Scenario](test-scenario-1.png)

4. Choose the **SAP Commerce Cloud - Events** option.

5. For the **Event Topics**, choose **order.created.v1**.

6. Modify the `orderCode` value as desired and choose **Send Event**.

    ![Test the Scenario](test-scenario-2.png)

7. With the Kyma console, choose **Configuration > `APIRules`** from the menu.

8. Choose the **Host** entry for the **fe-ui5-mssql** `APIRule` to open the application in the browser. This should be similar to:
`https://fe-ui5-mssql.*******.kyma.shoot.live.k8s-hana.ondemand.com`

9. You should now see the data received by the event as shown below:

    ![Test the Scenario](test-scenario-3.png)

[VALIDATE_2]
[ACCORDION-END]


---
