---
title: Deploy MSSQL in the Kyma Runtime
description: Configure and deploy an MSSQL database within the Kyma runtime to be used for a test or development scenario.
time: 45
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, products>sap-business-technology-platform]
primary_tag: products>sap-btp\\, kyma-runtime
---

## Prerequisites
 - [Docker](https://www.docker.com/) installed with a valid public account
 - [`kubectl` configured to KUBECONFIG downloaded from the Kyma runtime](cp-kyma-download-cli)
 - [GIT](https://git-scm.com/downloads) installed

## Details
### You will learn
  - How to configure and build a MSSQL database Docker image
  - How to create a Namespace in the Kyma runtime
  - How to deploy the MSSQL database Docker image to the Kyma runtime which includes:
    - A Kubernetes Secret to store the database user/password
    - A Kubernetes `PersistentVolumeClaim` for the storage of the database data
    - A Kubernetes Service used to expose the database to other Kubernetes resources

In this tutorial, you configure a database named `DemoDB` which contains one `Orders` table populated with two rows of sample data.

---

[ACCORDION-BEGIN [Step 1: ](Clone the Git repository)]

1. Go to the [kyma-runtime-extension-samples](https://github.com/SAP-samples/kyma-runtime-extension-samples) repository. This repository contains a collection of Kyma sample applications which will be used during the tutorial.

2. Download the code by choosing the green **Code** button, and then choosing one of the options to download the code locally.

    You can instead run the following command using your CLI at your desired folder location:

    ```Shell/Bash
    git clone https://github.com/SAP-samples/kyma-runtime-extension-samples
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Explore the sample)]

1. Open the `database-mssql` directory in your desired editor.

2. Explore the content of the sample.

3. Within the `app` folder you can find the configuration files for setting up the sample `DemoDB` database within the Docker image.  

    The process includes defining the database password as well as the creation of the `Orders` table with sample data.

4. Within the `docker` folder you can find the Dockerfile, which is a text-based set of instructions that is used to create a container image. Notice how the last command references the `entrypoint.sh` script defined within the app directory which is used to call the commands to configure the sample database.

5. Within the `k8s` folder you can find the resource definitions that will be used to deploy the sample to the Kyma runtime. This includes the `deployment.yaml` which specifies the microservice definition of the MSSQL database and also a service definition which exposes the microservice to other resources within the cluster. The `pvc.yaml` specifies a persistent volume claim which is used to request a storage location for the data of the database.  The `secret.yaml` contains the database user and password.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Build the Docker image)]

A Docker container image is a lightweight, standalone, executable package of software that includes everything needed to run an application: code, runtime, system tools, system libraries and settings. In this step, you will build a `mssql` image according to the Dockerfile definition contained in the docker folder. Make sure to run the following commands from the `database-mssql` directory using your CLI, and also replace the value of `<your-docker-id>` with your Docker account ID.

1. To build the Docker image, run this command:

    ```Shell/Bash
    docker build -t <your-docker-id>/mssql -f docker/Dockerfile .
    ```

2. To push the Docker image to your Docker repository, run this command:

    ```Shell/Bash
    docker push <your-docker-id>/mssql
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Use the Docker image locally)]

Make sure to replace the value of `<your-docker-id>` with your Docker account ID.

1. Start the image locally by running the following command. The start up will take about two minutes due to the scripts that run to initialize the database.

    ```Shell/Bash
    docker run -e ACCEPT_EULA=Y -e SA_PASSWORD=Yukon900 -p 1433:1433 --name sql1 -d <your-docker-id>/mssql
    ```

2. Open a bash shell within the image by running this command:

    ```Shell/Bash
    docker exec -it sql1 "bash"
    ```

3. Start the `sqlcmd` tool, which allows you to run queries against the database, by running this command:

    ```Shell/Bash
    /opt/mssql-tools/bin/sqlcmd -S localhost -U SA -P Yukon900
    ```

4. Enter a query by entering this command:

    ```Shell/Bash
    USE DemoDB SELECT * FROM ORDERS
    ```

5. The query can now be executed by entering this command:

    ```Shell/Bash
    GO
    ```

6. End the `sqlcmd` session by running:

    ```Shell/Bash
    exit
    ```

7. End the bash session by running:

    ```Shell/Bash
    exit
    ```

8. Shutdown the Docker container by running this command:

    ```Shell/Bash
    docker stop sql1
    ```

9. Here are some additional commands that can be used:

    > To start the container again, run:
    ```Shell/Bash
    docker start sql1
    ```
    > The container can be removed by running:
    ```Shell/Bash
    docker rm sql1
    ```

    > To list all local containers existing:
    ```Shell/Bash
    docker container ls -a
    ```

    > To list out all local images existing
    ```Shell/Bash
    docker images
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Apply resources to the Kyma runtime)]

You can find the resource definitions in the `k8s` folder. If you performed any changes in the database configuration, these files may also need to be updated. The folder contains the following files:

- `secret.yaml`: defines the database password and user which is base64-encoded.  
- `pvc.yaml`: defines a `PersistentVolume` used to store the data of the database.
- `deployment.yaml`: defines the Deployment definition for the MSSQL database as well as a Service used for communication. This definition references both the `secret.yaml` and `pvc.yaml` by name.  

Run the following commands from the `database-mssql` directory using your CLI.

1. Start by creating the `dev` Namespace if it doesn't already exist:

    ```Shell/Bash
    kubectl create namespace dev
    ```

2. Apply the `PersistentVolumeClaim`:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/pvc.yaml
    ```

3. Apply the Secret:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/secret.yaml
    ```

4. Within the `deployment.yaml`, adjust the value of `spec.template.spec.containers.image`, commented with **#change it to your image**, to use your Docker image. Apply the Deployment:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/deployment.yaml
    ```

5. Verify the Pod is up and running:

    ```Shell/Bash
    kubectl -n dev get po
    ```

    This command results in a table similar to the one below, showing a Pod with the name `mssql-` ending with a random hash. The **STATUS** will display `Running` when the Pod is up and running.

    ```Shell/Bash
    NAME                                     READY   STATUS    RESTARTS   AGE
    mssql-6df65c689d-qdj4r        2/2     Running   0          93s
    ```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Locally access the MSSQL Deployment)]

Kubernetes provides a port-forward functionality that allows you to connect to resources running in the Kyma runtime locally. This can be useful for development and debugging tasks. Make sure to adjust the name of the Pod in the following commands to match your own.

1.  Confirm the port on which the Pod is listening:

    ```Shell/Bash
    kubectl get pod mssql-6df65c689d-qdj4r -n dev --template="{{(index (index .spec.containers 0).ports 0).containerPort}}"
    ```

    This command should return:

    ```Shell/Bash
    1433
    ```

2. Apply the port-forward to the Pod:

    ```Shell/Bash
    kubectl port-forward mssql-6df65c689d-qdj4r -n dev 1433:1433
    ```

    This command should return:

    ```Shell/Bash
    Forwarding from 127.0.0.1:1433 -> 1433
    Forwarding from [::1]:1433 -> 1433
    ```

3. At this point, a tool such as `sqlcmd` or a development project running on your computer can access the database running in the Kyma runtime using `localhost:1433`.

4. To end the process, use `CTRL+C`.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Directly access the MSSQL Deployment)]

Similarly to how the Docker image can be accessed locally, you can perform the same on the Deployment running in the Kyma runtime. Make sure to adjust the name of the Pod in the following commands to match your own.

1. By default, a Pod includes the denoted Deployments defined in the `yaml` definition as well as the `istio-proxy`. For the `mssql` Deployment, this means there will be two containers in the Pod.  

    The `describe` command can be used to view this information.

    ```Shell/Bash
    kubectl describe pod mssql-6df65c689d-qdj4r -n dev
    ```

2. Run the following command to obtain a bash shell.

    If any adjustments where made to the name of the MSSQL deployment, adjust the name of the container denoted by the `-c` option.

    ```Shell/Bash
    kubectl exec -it mssql-6df65c689d-qdj4r -n dev -c mssql -- bash
    ```

    >This may output the following message which can be ignored: `groups: cannot find name for group ID 1337`

3. To run the `sqlcmd` tool, which allows you to run queries against the database, run this command:

    ```Shell/Bash
    /opt/mssql-tools/bin/sqlcmd -S 127.0.0.1 -U SA -P Yukon900
    ```

4. The commands performed in [Step 4: ](Use the Docker image locally) to query the database can now be used in the same fashion.

[DONE]
[ACCORDION-END]

---
