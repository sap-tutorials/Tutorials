---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-btp\, kyma-runtime
---

# Deploy a Multitenant Application to a Provider subaccount, Kyma Runtime
<!-- description --> Build a Node.js application into OCI image and push it into Docker registry. Based on that, deploy the application into the Kyma runtime.

## Prerequisites
- You have a Kyma runtime environment on SAP Business Technology Platform (BTP) and the relevant command line tools. If not, please follow the tutorials [Enable SAP BTP, Kyma Runtime](cp-kyma-getting-started) and [Install the Kubernetes Command Line Tool](cp-kyma-download-cli).
- You have installed [Docker](https://docs.docker.com/get-started/#download-and-install-docker).
- You have [Docker Hub](https://hub.docker.com/) account.
- You have finished the tutorial [Register a Multitenant Application to the SAP SaaS Provisioning Service](register-multitenant-app-saas-provisioning-service).



## You will learn
- How to build Application to OCI Image
- How to push OCI image to Docker Hub
- How to deploy applications into Kyma runtime


---

### Determine SAP BTP Subaccount Subdomain


Open your subaccount in the Cockpit. In the overview page, find the subdomain for your deployment.

For example:

<!-- border -->![image-20211214133316133](image-20211214133316133.png)


### Determine Kyma Cluster Domain


Find the full Kyma cluster domain in the downloaded `kubeconfig.yml` file. For example: `e6803e4.kyma.shoot.live.k8s-hana.ondemand.com`.





### Build Application to OCI Image


1. Install tool
<p> </p>
    In order to run your code on the Kyma Runtime (or on any Kubernetes-based platform), you need to provide an OCI image (aka Docker image) for your application. While you are in principle free to choose your image building tool, we recommend using [Cloud Native Buildpacks (CNB)](https://buildpacks.io/).  
<p> </p>
    The command-line tool `pack` supports providing a buildpack and your local source code and creating an OCI image from it. We are working on a process to provide recommended and supported buildpacks. In the meantime, you can use the community-supported [Paketo Buildpacks](https://paketo.io/).
<p> </p>
    If you followed the tutorials [Create a Basic Node.js Application with Express Generator](basic-nodejs-application-create) and [Deploy a Node.js Application in the Kyma Runtime](deploy-nodejs-application-kyma), you have installed the command-line tool `pack`, if not, please follow this official guide: [Install Pack](https://buildpacks.io/docs/tools/pack/).

    For example (macOS):  
```Shell / Bash
brew install buildpacks/tap/pack
```

2. Build image for applications
<p> </p>
    When we speak about repository name, we mean the combination of account and repo name that is usual with Docker Hub: `<docker-hub-account>/<repo-name>`. An example would be `tiaxu/multitenant-kyma-backend`.
<p> </p>
    As you can only create one private repository in a free Docker hub account, Docker images stored in Docker hub will have different tag names so that they can be stored in one repository. Thus, addressing an image will include the tag name:`<docker-hub-account>/<repo-name>:<tag-name>`. An example would be `tiaxu/multitenant-kyma-backend:v1`.
<p> </p>
    In the directory `kyma-multitenant-approuter`, build the image for the approuter app from source, for example:
```Shell / Bash
pack build <docker-hub-account>/multitenant-approuter:v1 --builder paketobuildpacks/builder-jammy-base
```

    In the directory `kyma-multitenant-node`, build the image for the approuter app from source, for example:
```Shell / Bash
pack build <docker-hub-account>/multitenant-kyma-backend:v1 --builder paketobuildpacks/builder-jammy-base
```




### Push OCI Image to Docker Hub


**1.** Log in to Docker using this command:

```Shell / Bash
docker login -u <docker-id> -p <password>
```

**2.** Push the local images into the Docker Hub:

```Shell / Bash
docker push <docker-hub-account>/multitenant-approuter:v1
docker push <docker-hub-account>/multitenant-kyma-backend:v1
```

>  For more details, see the [Kubernetes documentation](https://kubernetes.io/docs/tasks/configure-pod-container/pull-image-private-registry/).





### Create Namespace


If you followed the tutorials [Create a Basic Node.js Application with Express Generator](basic-nodejs-application-create) and [Deploy a Node.js Application in the Kyma Runtime](deploy-nodejs-application-kyma), you have created a namespace in the Kyma environment called `multitenancy-ns`. If not, create a new namespace through the Kyma dashboard or `kubectl` CLI, for example, called `multitenancy-ns`:

<!-- border -->![image-20220214150615225](image-20220214150615225.png)





### Deploy Secret for Docker Hub


Since the OCI image is stored in your Docker hub, you need to provide the access information to your Kyma cluster that you can pull the images from those repositories.

If you followed the tutorials [Create a Basic Node.js Application with Express Generator](basic-nodejs-application-create) and [Deploy a Node.js Application in the Kyma Runtime](deploy-nodejs-application-kyma), you have configured the credential of your Docker Hub as a Secret. If not, create a new Secret with the following command, replace the placeholder values according to your account:

```Shell / Bash
kubectl -n multitenancy-ns create secret docker-registry registry-secret --docker-server=https://index.docker.io/v1/  --docker-username=<docker-id> --docker-password=<password> --docker-email=<email>
```


Therefore, all deployment files contain an `imagePullSecret` entry, which should be set to `registry-secret`.

```YAML
imagePullSecrets:
        - name: registry-secret # replace with your own registry secret
```





### Deploy Application into Kyma Runtime


**1.** Deploy consumed services by executing this command:

```Shell / Bash
kubectl -n multitenancy-ns apply -f k8s-deployment-services.yaml
```

**2.** Deploy the approuter applications by executing this command:

```Shell / Bash
kubectl -n multitenancy-ns apply -f k8s-deployment-approuter.yaml
```

**3.** Deploy the backend applications by executing this command:

```Shell / Bash
kubectl -n multitenancy-ns apply -f k8s-deployment-backend.yaml
```








---
