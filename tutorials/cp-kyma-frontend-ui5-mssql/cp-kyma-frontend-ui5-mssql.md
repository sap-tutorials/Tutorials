---
title: Deploy the SAPUI5 Frontend in the Kyma Runtime
description: Develop and deploy the SAPUI5 frontend app in the Kyma runtime.
auto_validation: true
time: 40
tags: [ tutorial>intermediate, topic>cloud, products>sap-business-technology-platform]
primary_tag: products>sap-btp\\, kyma-runtime
---

## Prerequisites
  - [Docker](https://www.docker.com/) installed with a valid public account
  - [kubectl configured to KUBECONFIG downloaded from the Kyma runtime](cp-kyma-download-cli)
  - [GIT](https://git-scm.com/downloads)
  - [Node.js](https://nodejs.org/en/download/)
  - [UI5 Tooling](https://sap.github.io/ui5-tooling/)
  - [Deploying a Go MSSQL API Endpoint in the Kyma Runtime](cp-kyma-api-mssql-golang) tutorial completed


## Details
### You will learn
  - How to configure and build SAPUI5 Docker image
  - How to create a development Namespace in the Kyma runtime
  - How to deploy the SAPUI5 Docker image to the Kyma runtime

This tutorial expects that the [Deploy a Go MSSQL API Endpoint in the Kyma Runtime](cp-kyma-api-mssql-golang) tutorial has been completed and relies on the database running within the Kyma runtime.

Deploying the SAPUI5 Docker image to the Kyma runtime includes:

- A Kubernetes `Deployment` of the frontend image with the `ConfigMap` mounted to a volume
- A Kubernetes `ConfigMap` containing the URL to the backend API
- A Kubernetes Service used to expose the application to other Kubernetes resources
- A Kyma `APIRule` to expose the frontend application to the Internet

---

[ACCORDION-BEGIN [Step 1: ](Clone the Git repository)]

1. Copy the repository URL.

  In your browser, go to [kyma-runtime-extension-samples](https://github.com/SAP-samples/kyma-runtime-extension-samples). This repository contains a collection of Kyma sample applications which will be used during the tutorial.

  Choose the **Code** button and choose one of the options to download the code locally or simply run the following command within your CLI at your desired folder location:

```Shell/Bash
git clone https://github.com/SAP-samples/kyma-runtime-extension-samples
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Explore the sample)]

1. Open the `frontend-ui5-mssql` directory in your desired editor.

2. Explore the content of the sample.

3. You can find many directories that relate to the UI5 application.

4. The `docker` directory contains the Dockerfile used to generate the Docker image. The image is built in two stages to create a small image. In the first stage, a `NodeJS` image is used which copies the related content of the project into the image and builds the application. The built application is then copied into an `nginx` image with the default setup exposing the application on port 80.

5. Within the `k8s` directory you can find the Kubernetes/Kyma resources you will apply to your Kyma runtime.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Run application locally)]

1. Within the `frontend-ui5-mssql` directory, run the following command using your CLI to install the application dependencies:

    ```Shell/Bash
    npm install
    ```

2. Within the project, open the `webapp/config.json` file and adjust `API_URL` by replacing `<cluster domain>` to match the Kyma runtime cluster domain. This should be the value of the API Rule created in step 6 of the [Deploying a Go MSSQL API Endpoint in the Kyma Runtime](cp-kyma-api-mssql-golang) tutorial.

    ```Text/Javascript
    {
      "API_URL": "https://api-mssql-go.*******.kyma.shoot.live.k8s-hana.ondemand.com"
    }
    ```

3. To start the application, run the following command. This should automatically load the application in your browser.

    ```Shell/Bash
    npm run-script start
    ```
    > Pressing `control-c` will stop the running application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build Docker image)]

> Run the following commands from the `frontend-ui5-mssql` directory using your CLI. Make sure to replace the value of `<your-docker-id>` with your Docker account ID.

1. To build the Docker image, run this command:

    ```Shell/Bash
    docker build -t <your-docker-id>/fe-ui5-mssql -f docker/Dockerfile .
    ```

2. To push the Docker image to your Docker repository, run this command:

    ```Shell/Bash
    docker push <your-docker-id>/fe-ui5-mssql
    ```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Apply resources to Kyma runtime)]

You can find the resource definitions in the `k8s` folder. If you performed any changes in the configuration, these files may also need to be updated. The folder contains the following files that are relevant to this tutorial:

- `apirule.yaml`: defines the API endpoint which exposes the application to the Internet. This endpoint does not define any authentication access strategy and should be disabled when not in use.  
- `configmap.yaml`: defines `API_URL` which represents the endpoint used for the orders API. It must be adjusted to reflect the domain of the Kyma runtime.
- `deployment.yaml`: defines the Deployment definition for the SAPUI5 application as well as a service used for communication. This definition references the `configmap.yaml` by name. It is used to overwrite the `webapp/config.json` of the application.


1. Start by creating the `dev` Namespace if it doesn't already exist:

    ```Shell/Bash
    kubectl create namespace dev
    ```

2. Within the project, open the `k8s/configmap.yaml` file and adjust `API_URL` by replacing `<cluster domain>` to match the Kyma runtime cluster domain:

    ```
    kind: ConfigMap
    apiVersion: v1
    metadata:
      name: fe-ui5-mssql
      labels:
        app: fe-ui5-mssql
    data:
      config.json: |-
        {
          "API_URL": "https://api-mssql-go.*******.kyma.shoot.live.k8s-hana.ondemand.com"
        }
    ```

3. Apply the `ConfigMap`:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/configmap.yaml
    ```

4. Within the `deployment.yaml`, adjust the value of `spec.template.spec.containers.image` to use your Docker image. Apply the Deployment:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/deployment.yaml
    ```

5. Apply the `APIRule`:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/apirule.yaml
    ```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Open the UI application)]

To access the application we can use the `APIRule` we created in the previous step.

1. Open the Kyma runtime console

2. Choose the `dev` Namespace.

3. From the menu, choose **Discovery and Network > `APIRules`**.

4. Choose the **Host** entry for the **fe-ui5-mssql** `APIRule` to open the application in the browser. This should be similar to:
`https://fe-ui5-mssql.*******.kyma.shoot.live.k8s-hana.ondemand.com`

[VALIDATE_2]
[ACCORDION-END]
---
