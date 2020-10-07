---
title: Deploy an SAPUI5 Frontend in the Kyma Runtime
description: Develop and deploy a frontend SAPUI5 app to the Kyma runtime.
auto_validation: true
time: 40
tags: [ tutorial>intermediate, topic>cloud, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform\, kyma-runtime
---

## Prerequisites
  - [Docker](https://www.docker.com/) installed with a valid public account
  - [kubectl configured to KUBECONFIG downloaded from Kyma Runtime](cp-kyma-download-cli).
  - [GIT](https://git-scm.com/downloads)
  - [Node.js](https://nodejs.org/en/download/)
  - [UI5 Tooling](https://sap.github.io/ui5-tooling/)
  - Complete the [Deploying a Go MSSQL API Endpoint in the Kyma Runtime](cp-kyma-api-mssql-golang) tutorial.


## Details
### You will learn
  - How to configure and build a SAPUI5 Docker image
  - How to create a development namespace in Kyma Runtime.
  - How to deploy the SAPUI5 Docker image to the Kyma runtime

This tutorial expects that the tutorial [Deploy a Go MSSQL API Endpoint in the Kyma Runtime](cp-kyma-api-mssql-golang) has been completed and relies on the database running within the Kyma runtime.

Deploying the SAPUI5 Docker image to the Kyma runtime includes:

- A Kubernetes deployment of the frontend image with the `configmap` mounted to a volume
- A Kubernetes `configmap` containing the URL to the backend API
- A Kubernetes Service used to expose the application to other Kubernetes resources
- A Kyma `APIRule` to expose the frontend application to the internet

---

[ACCORDION-BEGIN [Step 1: ](Clone the Git repository)]

1. Copy the repository URL.

  In your browser, go to [kyma-runtime-extension-samples](https://github.com/SAP-samples/kyma-runtime-extension-samples). This repository contains a collection of Kyma sample applications which will be used during the tutorial.

  Choose the `Code` button and choose one of the options to download the code locally or simply run the following command within your CLI at your desired folder location.

```Shell/Bash
git clone https://github.com/SAP-samples/kyma-runtime-extension-samples
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Explore the sample)]

1. Open the directory `frontend-ui5-mssql` in your desired editor.

2. Explore the content of the sample.

3. You will find many directories that relate to the UI5 application.

4. The `docker` directory contains the `Dockerfile` used to generate the Docker image. The image is built in two stages to create a small image.  In the first stage a `NodeJS` image is used which copies the related content of the project into the image and builds the application.  The built application is then copied into a `nginx` image with the default setup exposing the application on port 80.

5. Within the `k8s` directory you will find the Kubernetes/Kyma resources you will apply to your Kyma Runtime.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Run application locally)]

1. Within the directory run the following command within your Command Line Interface(CLI) to install the application dependencies.

    ```Shell/Bash
    npm install
    ```

2. Within the project open the file `webapp/config.json` and adjust the `API_URL` by replacing `<cluster domain>` to the match the Kyma runtime cluster domain.

    ```Text/Javascript
    {
      "API_URL": "https://api-mssql-go.*******.kyma.shoot.live.k8s-hana.ondemand.com"
    }
    ```

3. To start the application run the following command.  This should automatically load the application in your browser.

    ```Shell/Bash
    npm run-script start
    ```
    > Pressing `control-c` will stop the running application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build Docker image)]

> The following commands must be ran from the directory `frontend-ui5-mssql` within your command line interface. Make sure to replace the value of <your-docker-id> with your docker account id

1. To build the Docker image run the command:

    ```Shell/Bash
    docker build -t <your-docker-id>/fe-ui5-mssql -f docker/Dockerfile .
    ```

2. To push the Docker image to your Docker repository run the command:

    ```Shell/Bash
    docker push <your-docker-id>/fe-ui5-mssql
    ```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Apply resources to Kyma runtime)]

You will find the resource definitions in the `k8s` folder. If any changes were performed on the configuration these file may also need to be updated. The folder contains the following files that are relevant to this tutorial:

- `apirule.yaml`: defines the API endpoint which exposes the application to the internet.  This endpoint does not define any authentication access strategy and should be disabled when not is use.  
- `configmap.yaml`: defines the `API_URL` which represents the endpoint used for the orders API.  This will need to be adjusted to reflect the domain of the Kyma runtime.
- `deployment.yaml`: defines the deployment definition for the SAPUI5 application as well as a service used for communication.  This definition references the `configmap.yaml` by name.  It is used to overwrite the `webapp/config.json` of the application.


1. Start by creating the namespace `dev` if it doesn't already exist.

    ```Shell/Bash
    kubectl create namespace dev
    ```

2. Within the project open the file `k8s/configmap.yaml` and adjust the `API_URL` by replacing `<cluster domain>` to the match the Kyma runtime cluster domain.

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

4. Within the `deployment.yaml` adjust the value of `spec.template.spec.containers.image` to use your Docker image.  Apply the Deployment.

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/deployment.yaml
    ```

5. Apply the API Rule:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/apirule.yaml
    ```

The API Rule will create an endpoint similar to the one below that is used to open the application.

>https\://fe-ui5-mssql.\<cluster\>.kyma.shoot.live.k8s-hana.ondemand.com

[VALIDATE_1]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Opening the UI Application)]

1. Choose the menu option **Configuration > `APIRules`**.

2. Choose the **Host** entry for the **fe-ui5-mssql** `APIRule` to open the application in the browser. This should be similar to `https://fe-ui5-mssql.*******.kyma.shoot.live.k8s-hana.ondemand.com`.

[DONE]
[ACCORDION-END]
---
