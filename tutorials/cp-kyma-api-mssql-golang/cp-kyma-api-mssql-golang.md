---
parser: v2
auto_validation: true
time: 40
tags: [ tutorial>intermediate, topic>cloud, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-btp\\, kyma-runtime
---

# Deploy a Go MSSQL API Endpoint in the Kyma Runtime
<!-- description --> Develop and deploy an MSSQL API endpoint written in Go to the Kyma runtime.

## Prerequisites
  - [Docker](https://www.docker.com/)
  - [Go ](https://golang.org/doc/install)
  - [GIT](https://git-scm.com/downloads)
  - [kubectl configured to KUBECONFIG downloaded from the Kyma runtime](cp-kyma-download-cli)
  - [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) tutorial completed

## You will learn
  - How to configure and build a Go Docker image
  - How to create a development Namespace in the Kyma runtime
  - How to deploy the Go Docker image to the Kyma runtime

## Intro
This tutorial expects that the tutorial [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) has been completed and relies on the database running either locally or within the Kyma runtime. If you run the database in the Kyma runtime, make sure to use the `port-forward` feature presented in the tutorial to expose the database to your local environment.

Deploying the image includes:

- A Kubernetes Secret to store the database user/password
- A Kubernetes `ConfigMap` to store the database configuration in
- A Kubernetes Service used to expose the Go application to other Kubernetes resources
- A Kyma `APIRule` to expose the API to the Internet


---

### Clone the Git repository


1. Copy the repository URL.

  In your browser, go to [kyma-runtime-extension-samples](https://github.com/SAP-samples/kyma-runtime-extension-samples). This repository contains a collection of Kyma sample applications which will be used during the tutorial.

  Choose the **Code** button and choose one of the options to download the code locally or simply run the following command within your CLI at your desired folder location:

```Shell/Bash
git clone https://github.com/SAP-samples/kyma-runtime-extension-samples
```


### Explore the sample


1. Open the `api-mssql-go` directory in your desired editor.

2. Explore the content of the sample.

3. Within the `cmd/api` directory, you can find `main.go` which is the main entry point of the Go application.

4. The `docker` directory contains the Dockerfile used to generate the Docker image. The image is built in two stages to create an image with a small file size.

    In the first stage, a Go image is used. It copies the related content of the project into the image and builds the application. The built application is then copied into the Docker `scratch` image and exposed on port 8000. The `scratch` image is an empty image containing no other tools within it so obtaining a shell/bash session is not possible. If desired, the following lines could be commented out to build an image with more included tools, but this results in a larger image.

    ```Shell/Bash
    FROM scratch
    WORKDIR /app
    COPY --from=builder /app/api-mssql-go /app/
    ```

5. The `internal` directory contains the rest of the Go application, which is broken down into three packages: `api`, `config` and `db`.  Explore these contents to understand the structure and functionality.

6. Within the `k8s` directory you can find the Kubernetes/Kyma resources you will apply to your Kyma runtime.

7. Within the root you can find `go.mod` and `go.sum` files that are used to manage the dependencies the application uses.



### Run the application locally


Run the following commands from the `api-mssql-go` directory using your CLI.

1. The application expects that the following environment variables are set as they are required for the database connection. Make sure to adjust them for your environment. Review the the tutorial [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) for the different configurations to run the database.

[OPTION BEGIN [Mac and Linux]]

```Shell/Bash
export MYAPP_username=sa
export MYAPP_password=Yukon900
export MYAPP_database=DemoDB
export MYAPP_host=localhost
export MYAPP_port=1433
```

[OPTION END]

[OPTION BEGIN [Windows]]

```PowerShell
$ENV:MYAPP_username='sa'
$ENV:MYAPP_password='Yukon900'
$ENV:MYAPP_database='DemoDB'
$ENV:MYAPP_host='localhost'
$ENV:MYAPP_port=1433
```

```DOS
set MYAPP_username=sa
set MYAPP_password=Yukon900
set MYAPP_database=DemoDB
set MYAPP_host=localhost
set MYAPP_port=1433
```

[OPTION END]

2. To run the application, use the following command:

    ```Shell/Bash
    go run ./cmd/api/main.go
    ```

3. The application is available at <http://localhost:8000/orders>. You can use a tool such as curl to test the different HTTP methods, for example:

    > To GET data:
    ```Shell/Bash
    curl -i -H "Accept: application/json" -H "Content-Type: application/json" -X GET http://localhost:8000/orders
    ```

    > To POST data:
    ```Shell/Bash
    curl --data "{\"order_id\":\"10000003\",\"description\":\"test from curl\"}" http://localhost:8000/orders
    ```


### Build the Docker image


Run the following commands from the `api-mssql-go` directory within your CLI.

Make sure to replace the value of `<your-docker-id>` with your Docker account ID.

1. To build the Docker image, run this command:

    ```Shell/Bash
    docker build -t <your-docker-id>/api-mssql-go -f docker/Dockerfile .
    ```

2. To push the Docker image to your Docker repository, run this command:

    ```Shell/Bash
    docker push <your-docker-id>/api-mssql-go
    ```


### Use the Docker image locally


Run the following commands from the `api-mssql-go` directory within your CLI.

Make sure to replace the value of `<your-docker-id>` with your Docker account ID.

1. Start the image locally by running the following command:  

[OPTION BEGIN [Mac and Linux]]

```Shell/Bash
docker run -p 8000:8000  --name api-mssql-go \
-e MYAPP_username="sa" \
-e MYAPP_password="Yukon900" \
-e MYAPP_database="DemoDB" \
-e MYAPP_host="host.docker.internal" \
-e MYAPP_port="1433" \
-d <your-docker-id>/api-mssql-go:latest
```
[OPTION END]
[OPTION BEGIN [Windows]]

```PowerShell
docker run -p 8000:8000  --name api-mssql-go `
-e MYAPP_username="sa" `
-e MYAPP_password="Yukon900" `
-e MYAPP_database="DemoDB" `
-e MYAPP_host="host.docker.internal" `
-e MYAPP_port="1433" `
-d <your-docker-id>/api-mssql-go:latest  
```

```DOS
docker run -p 8000:8000  --name api-mssql-go ^
-e MYAPP_username="sa" ^
-e MYAPP_password="Yukon900" ^
-e MYAPP_database="DemoDB" ^
-e MYAPP_host="host.docker.internal" ^
-e MYAPP_port="1433" ^
-d <your-docker-id>/api-mssql-go:latest  
```
[OPTION END]

The command is expecting that the database is available at `localhost:1433` on the host machine. This is denoted by the `host.docker.internal` value. Review the tutorial [Deploying MSSQL in the Kyma Runtime](cp-kyma-mssql-deployment) for the different configurations to run/stop the database.

2. The application is available at <http://localhost:8000/orders>. You can use a tool such as curl to test the different HTTP methods, for example:

    > To GET data:
    ```Shell/Bash
    curl -i -H "Accept: application/json" -H "Content-Type: application/json" -X GET http://localhost:8000/orders
    ```

    > To POST data - make sure to use a unique value for the order_id:
    ```Shell/Bash
    curl --data "{\"order_id\":\"10000004\",\"description\":\"test from curl\"}" http://localhost:8000/orders
    ```

3. Stop the Docker container by running:

    ```Shell/Bash
    docker stop api-mssql-go
    ```

4. Here are some additional commands that you can use:

    > To restart the Docker container, run:
    ```Shell/Bash
    docker start api-mssql-go
    ```

    > To remove the Docker container, run:
    ```Shell/Bash
    docker rm api-mssql-go
    ```


### Apply resources to Kyma runtime


You can find the resource definitions in the `k8s` folder. If you performed any changes in the configuration, these files may also need to be updated. The folder contains the following files that are relevant to this tutorial:

- `apirule.yaml`: defines the API endpoint which exposes the application to the Internet. This endpoint does not define any authentication access strategy and should be disabled when not in use.  
- `configmap.yaml`: defines the name of the database, host, and port. The host value assumes that the service for the database is named `mssql` and is defined in the `dev` Namespace. Make sure to adjust this if you made any changes.
- `deployment.yaml`: defines the deployment definition for the Go API, as well as a service used for communication. This definition references both the `secret.yaml`, which was defined in the previous tutorial and also included in this directory, and the `configmap.yaml` by name.  

1. Start by creating the `dev` Namespace and enabling `Istio`:

    ```Shell/Bash
    kubectl create namespace dev
    kubectl label namespaces dev istio-injection=enabled
    ```
    > Namespaces separate objects inside a Kubernetes cluster. Choosing a different namespace will require adjustments to the provided samples.

    > Adding the label `istio-injection=enabled` to the namespace enables `Istio`. `Istio` is the service mesh implementation used by the Kyma runtime.

2. Within the `deployment.yaml`, adjust the value of `spec.template.spec.containers.image`, commented with **#change it to your image**, to use your Docker image. Apply the Deployment which will cause an error which we will further explore:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/deployment.yaml
    ```

3. Check the status of the Pod by running:

    ```Shell/Bash
    kubectl -n dev get po
    ```
    This command results in a table similar to the one below, showing a Pod with the name `api-mssql-go-` ending with a random hash. Make sure to adjust this value in the subsequent commands. Notice that **STATUS** is reporting `CreateContainerConfigError`.

    ```Shell/Bash
    NAME                           READY   STATUS                       RESTARTS   AGE
    api-mssql-go-c694bc847-tkthc   1/2     CreateContainerConfigError   0          23s
    ```

    You can also see the logs that report the same **STATUS** `CreateContainerConfigError`:

    ```Shell/Bash
    kubectl logs api-mssql-go-c694bc847-tkthc -c api-mssql-go -n dev
    ```

    To find the reason for the **STATUS** `CreateContainerConfigError`, review the Pod definition:

    ```Shell/Bash
    kubectl describe pod api-mssql-go-c694bc847-tkthc -n dev
    ```

    Within the Events of the pods you should find a message `Error: configmap "api-mssql-go" not found`.

4. Apply the `ConfigMap`:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/configmap.yaml
    ```

5. Verify the status of the Pod by running:

    ```Shell/Bash
    kubectl -n dev get po
    ```

    The Pod should now be running.

    ```Shell/Bash
    NAME                           READY   STATUS    RESTARTS   AGE
    api-mssql-go-c694bc847-tkthc   2/2     Running   0          23m
    ```

6. Apply the `APIRule`:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/apirule.yaml
    ```



### Open the API endpoint


To access the API we can use the `APIRule` we created in the previous step.

1. Open the Kyma runtime console

2. From the menu, choose **Namespaces**

3. Choose the `dev` Namespace.

4. From the menu, choose **Discovery and Network > `API Rules`**.

5. Choose the **Host** entry for the **api-mssql-go** `APIRule` to open the application in the browser which will produce a **404** error. Append `/orders` to the end of the URL and refresh the page to successfully access the API. The URL should be similar to:

    `https://api-mssql-go.<cluster>.kyma.ondemand.com/orders`

    >A tool such as `curl` can be used to test the various HTTP methods of the API.




---
