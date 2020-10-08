---
title: Deploy a Go MSSQL API Endpoint in the Kyma Runtime
description: Develop and deploy an MSSQL API endpoint written in Go to the Kyma runtime.
auto_validation: true
time: 40
tags: [ tutorial>intermediate, topic>cloud, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform\, kyma-runtime
---

## Prerequisites
  - [Docker](https://www.docker.com/)
  - [Go ](https://golang.org/doc/install)
  - [GIT](https://git-scm.com/downloads)
  - [kubectl configured to KUBECONFIG downloaded from Kyma runtime](cp-kyma-download-cli).
  - Complete [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) tutorial.

## Details
### You will learn
  - How to configure and build a Go Docker image
  - How to create a development namespace in Kyma runtime.
  - How to deploy the Go Docker image to the Kyma runtime

This tutorial expects that the tutorial [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) has been completed and relies on the database running either locally or within the Kyma Runtime.  If running the database in the Kyma Runtime make sure to use the `port-forward` feature presented in the tutorial to expose the database to your local environment.

Deploying the image includes:

- A Kubernetes Secret to store the database user/password
- A Kubernetes `ConfigMap` to store the database configuration in
- A Kubernetes Service used to expose the Go application to other Kubernetes resources
- A Kyma `APIRule` to expose the API to the internet


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

1. Open the directory `api-mssql-go` in your desired editor

2. Explore the content of the sample.

3. Within the directory `cmd/api` you will find `main.go` which is the main entry point of the Go application.

4. The `docker` directory contains the `Dockerfile` used to generate the Docker image. The image is built in two stages to create a very small image.

    In the first stage, a Go image is used, which copies the related content of the project into the image and builds the application.  The built application is then copied into the Docker `scratch` image and exposed on port 8000.  The `scratch` image contains no other tools within it so obtaining a shell/Bash session is not possible.  If desired the following lines could be commented out to build an image with more included tools, but results in a larger image.

    ```Shell/Bash
    FROM scratch
    WORKDIR /app
    COPY --from=builder /app/api-mssql-go /app/
    ```

5. The `internal` directory contains the rest of the Go application, which is broken down into three packages: `api`, `config` and `db`.  Explore these contents to understand the structure and functionality.

6. Within the `k8s` directory you will find the Kubernetes/Kyma resources you will apply to your Kyma runtime.

7. Within the root you will find `go.mod` and `go.sum`, which are used to manage the dependencies the application is using.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Run the application locally)]

The following commands must be ran from the directory `api-mssql-go` within your command line interface.

1. The application expects that the following environment variables are set which are required for the database connection.  Make sure to adjust them for your environment.  Review the the tutorial [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) for the different configurations to run the database.

    ```Shell/Bash
    export MYAPP_username=sa
    export MYAPP_password=Yukon900
    export MYAPP_database=DemoDB
    export MYAPP_host=localhost
    export MYAPP_port=1433
    ```

2. To run the application use the following command:

    ```Shell/Bash
    go run ./cmd/api/main.go
    ```

3. The application will be available at <http://localhost:8000/orders>.  A tool such as Curl could be used to test the different HTTP methods, for example.

    > To GET data:
    ```Shell/Bash
    curl -i -H "Accept: application/json" -H "Content-Type: application/json" -X GET http://localhost:8000/orders
    ```

    > To POST data:
    ```Shell/Bash
    curl --data '{"order_id":"10000003","description":"test from curl"}' http://localhost:8000/orders
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build the Docker image)]

The following commands must be ran from the directory `api-mssql-go` within your command line interface.

Make sure to replace the value of `<your-docker-id>` with your Docker account ID.

1. To build the Docker image, run the command:

    ```Shell/Bash
    docker build -t <your-docker-id>/api-mssql-go -f docker/Dockerfile .
    ```

2. To push the Docker image to your Docker repository, run the command:

    ```Shell/Bash
    docker push <your-docker-id>/api-mssql-go
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Use the Docker image locally)]

The following commands must be ran from the directory `api-mssql-go` within your command line interface.

Make sure to replace the value of `<your-docker-id>` with your docker account ID.

1. Start the image locally, by running the following command:  

    The command is expecting that the database is available at `localhost:1433` on the host machine.  This is denoted by the value `host.docker.internal`.  Review the tutorial [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) for the different configurations to run the database.

    ```Shell/Bash
      docker run -p 8000:8000  --name api-mssql-go \
      -e MYAPP_username="sa" \
      -e MYAPP_password="Yukon900" \
      -e MYAPP_database="DemoDB" \
      -e MYAPP_host="host.docker.internal" \
      -e MYAPP_port="1433" \
      -d <your-docker-id>/api-mssql-go:latest
    ```

2. The application will be available at <http://localhost:8000/orders>.  A tool such as Curl could be used to test the different HTTP methods, for example.

    > To GET data:
    ```Shell/Bash
    curl -i -H "Accept: application/json" -H "Content-Type: application/json" -X GET http://localhost:8000/orders
    ```

    > To POST data:
    ```Shell/Bash
    curl --data '{"order_id":"10000003","description":"test from curl"}' http://localhost:8000/orders
    ```

3. Stop the docker container by running:

    ```Shell/Bash
    docker stop api-mssql-go
    ```

4. Some additional commands that can by used:

    > To restart the docker container, run:
    ```Shell/Bash
    docker start api-mssql-go
    ```

    > To remove the docker container, run:
    ```Shell/Bash
    docker rm api-mssql-go
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Apply resources to Kyma runtime)]

You will find the resource definitions in the `k8s` folder. If any changes were performed on the configuration these file may also need to be updated. The folder contains the following files that are relevant to this tutorial:

- `apirule.yaml`: defines the API endpoint which exposes the application to the internet.  This endpoint does not define any authentication access strategy and should be disabled when not is use.  
- `configmap.yaml`: defines the name of the database, host and port.  The host value assumes that the service for the database is named `mssql` and is defined in the `dev` namespace.  Make sure to adjust this if any changes have been done.
- `deployment.yaml`: defines the deployment definition for the Go API as well as a service used for communication.  This definition references both the `secret.yaml` which was defined in the previous tutorial, but also included in this directory, and the `configmap.yaml` by name.  

1. Start by creating the namespace `dev` if it doesn't already exist.

    ```Shell/Bash
    kubectl create namespace dev
    ```

2. Within the `deployment.yaml`, adjust the value of `spec.template.spec.containers.image` to use your Docker image.  Apply the Deployment.

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/deployment.yaml
    ```

3. Check the status of the pod by running:

    ```Shell/Bash
    kubectl -n dev get po
    ```
    This command should result in a table similar to the one below, showing a pod with the name `api-mssql-go-` ending with a random hash. Make sure to adjust this value in the subsequent commands. Notice that **STATUS** is reporting `CreateContainerConfigError`.

    ```Shell/Bash
    NAME                           READY   STATUS                       RESTARTS   AGE
    api-mssql-go-c694bc847-tkthc   1/2     CreateContainerConfigError   0          23s
    ```

    The logs can also be reviewed, which will report the same **STATUS** `CreateContainerConfigError`:

    ```Shell/Bash
    kubectl logs api-mssql-go-c694bc847-tkthc -c api-mssql-go -n dev
    ```

    To find the reason for the **STATUS** `CreateContainerConfigError`, review the pod definition:

    ```Shell/Bash
    kubectl get pod api-mssql-go-c694bc847-tkthc -n dev -o yaml
    ```

    Find the property `containerStatuses.state.waiting.message` property for the `api-mssql-go` image to determine the issue.

4. Apply the `ConfigMap`.

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/configmap.yaml
    ```

5. Verify the status of the pod by running:

    ```Shell/Bash
    kubectl -n dev get po
    ```

    The pod should now be running.

    ```Shell/Bash
    NAME                           READY   STATUS    RESTARTS   AGE
    api-mssql-go-c694bc847-tkthc   2/2     Running   0          23m
    ```

6. Apply the API Rule.

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/apirule.yaml
    ```

    The API Rule will create an endpoint similar to the one below. This can be used to test within your browser or by using a tool such as `curl`  

    `https://api-mssql-go.<cluster>.kyma.shoot.live.k8s-hana.ondemand.com/orders`

[VALIDATE_1]
[ACCORDION-END]

---
