---
title: Deploy MSSQL in the Kyma Runtime
description: Configure and deploy an MSSQL database within the Kyma runtime to be used for a test or development scenario.
time: 45
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform\, kyma-runtime
---

## Prerequisites
 - [Docker](https://www.docker.com/) installed with a valid public account.
 - [`kubectl` configured to KUBECONFIG downloaded from Kyma Runtime](cp-kyma-download-cli)
 - [GIT](https://git-scm.com/downloads) installed.

## Details
### You will learn
  - How to configure and build a MSSQL database docker image.
  - How to create a namespace in Kyma Runtime.
  - How to deploy the MSSQL database docker image to the Kyma Runtime which includes:
    - A Kubernetes Secret to store the database user/password.
    - A Kubernetes `PersistentVolumeClaim` for the storage of the database data.
    - A Kubernetes Service used to expose the database to other Kubernetes resources.

In this tutorial, you configure a database named `DemoDB`, which contains one table `Orders` populated with two rows of sample data.

---

[ACCORDION-BEGIN [Step 1: ](Clone the Git repository)]

1. Go to the [kyma-runtime-extension-samples](https://github.com/SAP-samples/kyma-runtime-extension-samples) repo. This repository contains a collection of Kyma sample applications which will be used during the tutorial.

2. Download the code by choosing the green **Code** button, and then choosing one of the options to download the code locally.

    You can instead run the following command within your CLI at your desired folder location.

    ```Shell/Bash
    git clone https://github.com/SAP-samples/kyma-runtime-extension-samples
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Explore the sample)]

1. Open the directory `database-mssql` in your desired editor.

2. Explore the content of the sample.

3. Within the folder `app` you can find the configuration files for setting up the sample database `DemoDB` within the Docker image.  

    The process includes defining the database password as well as the creation of the `Orders` table with sample data.

4. Within the folder `docker` folder, you will find the `Dockerfile` definition.  Notice how the last command references the `entrypoint.sh` defined within the app directory -- which is used to call the commands to configure the sample database.

5. Within the folder `k8s`, you will find the resource definitions that will be used to deploy the sample to the Kyma runtime.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Build the Docker image)]

The following commands must be ran from the directory `database-mssql` within your command line interface.

Make sure to replace the value of `<your-docker-id>` with your docker account ID.

1. To build the Docker image, run the command:

    ```Shell/Bash
    docker build -t <your-docker-id>/mssql -f docker/Dockerfile .
    ```

2. To push the Docker image to your Docker repository, run the command:

    ```Shell/Bash
    docker push <your-docker-id>/mssql
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Use the Docker image locally)]

Make sure to replace the value of `<your-docker-id>` with your docker account ID.

1. Start the image locally, by running the command:

    ```Shell/Bash
    docker run -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=Yukon900' -p 1433:1433 --name sql1 -d <your-docker-id>/mssql
    ```

2. Open a Bash shell within the image by running the command:

    ```Shell/Bash
    docker exec -it sql1 "bash"
    ```

3. Start the `sqlcmd` tool (which allows you to run queries against the database), by running the command:

    ```Shell/Bash
    /opt/mssql-tools/bin/sqlcmd -S localhost -U SA -P Yukon900
    ```

4. Run a sample query by running the commands:

    ```Shell/Bash
    1> USE DemoDB
    2> SELECT * FROM ORDERS
    3> GO
    ```

5. End the `sqlcmd` session by running:

    ```Shell/Bash
    1> exit
    ```

6. End the Bash session by running:

    ```Shell/Bash
    exit
    ```

7. Shutdown the docker container by running the command:

    ```Shell/Bash
    docker stop sql1
    ```

8. Some additional commands that can by used:

    > To start the container again, run:
    ```Shell/Bash
    docker start sql1
    ```
    > The container can be removed by running:
    ```Shell/Bash
    docker rm sql1
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Apply resources to Kyma runtime)]

You will find the resource definitions in the `k8s` folder. If any changes were performed on the database configuration, these file may also need to be updated. The folder contains the following files:

- `secret.yaml`: Defines the database password and user which is base64 encoded.  
- `pvc.yaml`: Defines a persistent volume used to store the data of the database on.
- `deployment.yaml`: Defines the deployment definition for the MSSQL database as well as a service used for communication.  This definition references both the `secret.yaml` and `pvc.yaml` by name.  

The following commands must be ran from the directory `database-mssql` within your command line interface.

1. Start by creating the namespace `dev` if it doesn't already exist.

    ```Shell/Bash
    kubectl create namespace dev
    ```

2. Apply the persistent volume claim:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/pvc.yaml
    ```

3. Apply the secret:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/secret.yaml
    ```

4. Within the `deployment.yaml` adjust the value of `spec.template.spec.containers.image` to use your Docker image.  Apply the deployment:

    ```Shell/Bash
    kubectl -n dev apply -f ./k8s/deployment.yaml
    ```

5. Verify the pod is up and running:

    ```Shell/Bash
    kubectl -n dev get po
    ```

    This command should result in a table similar to the one below showing a pod with the name `mssql-` ending with a random hash.  The `STATUS` will display `Running` when the pod is up and running.

    ```Shell/Bash
    NAME                                     READY   STATUS    RESTARTS   AGE
    mssql-6df65c689d-qdj4r        2/2     Running   0          93s
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Locally access MSSQL deployment)]

Kubernetes provides a port-forward functionality that allows you to connect to resources running in the Kyma runtime locally.  This can be useful for development and debugging tasks.  Make sure to adjust the name of the pod in the following commands to match your own.

1.  Confirm the port the pod is listening on.

    ```Shell/Bash
    kubectl get pod mssql-6df65c689d-qdj4r -n dev --template='{{(index (index .spec.containers 0).ports 0).containerPort}}{{"\n"}}'
    ```

    This command should return:

    ```Shell/Bash
    1433
    ```

2. Apply the port-forward to the pod.

    ```Shell/Bash
    kubectl port-forward mssql-6df65c689d-qdj4r -n dev 1433:1433
    ```

    This command should return:

    ```Shell/Bash
    Forwarding from 127.0.0.1:1433 -> 1433
    Forwarding from [::1]:1433 -> 1433
    ```

3. At this point a tool such as `sqlcmd` or a development project running on your computer could access the database running in the Kyma runtime using `localhost:1433`.

4. To kill the process, use `CTRL+C`.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Directly access MSSQL deployment)]

Similar to how the Docker image can be access locally, we can perform the same on the deployment running in the Kyma runtime.  Make sure to adjust the name of the pod in the following commands to match your own.

1. By default, a pod will include the denoted deployments defined in the `yaml` definition as well as an `istio-proxy`.  For the `mssql` deployment, this means there will be two containers in the pod.  

    The `describe` command can be used to view this information.

    ```Shell/Bash
    kubectl describe pod mssql-6df65c689d-qdj4r -n dev
    ```

2. Run the following command to obtain a Bash shell.  

    If any adjustments where made to the name of the MSSQL deployment, you will need to adjust the name of the container denoted by the `-c` option.

    ```Shell/Bash
    kubectl exec -it mssql-6df65c689d-qdj4r -n dev -c mssql -- bash
    ```

3. To run the `sqlcmd` tool (which allows you to run queries against the database), run the command:

    ```Shell/Bash
    /opt/mssql-tools/bin/sqlcmd -S 127.0.0.1 -U SA -P Yukon900
    ```

4. The commands performed in `Step 4: Using the Docker Image Locally` to query the database, can now be used in the same fashion.

[DONE]
[ACCORDION-END]

---
