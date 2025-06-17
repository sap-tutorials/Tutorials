---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>node-js]
primary_tag: software-product>sap-btp\, kyma-runtime
---

# Deploy a Node.js Application in SAP BTP, Kyma runtime
<!-- description --> Build a basic Node.js application into OCI image and push it into Docker registry. Besides, describe the corresponding Kubernetes objects for the application. Based on the above, deploy the application into SAP BTP, Kyma runtime.

## Prerequisites
- You have a Kyma runtime environment on SAP Business Technology Platform (BTP) and the relevant command line tools. If not, please follow the tutorials [Enable SAP BTP, Kyma runtime](cp-kyma-getting-started) and [Install the Kubernetes Command Line Tool](cp-kyma-download-cli).
- You have installed [Docker](https://docs.docker.com/get-started/#download-and-install-docker).
- You have [Docker Hub](https://hub.docker.com/) account.
- You have finished the tutorial [Create a Basic Node.js Application with Express Generator](basic-nodejs-application-create).

## You will learn
- How to build Application to OCI Image
- How to push OCI image to Docker Hub
- How to describe Kubernetes objects for web application
- How to deploy application into Kyma runtime

---

### Determine SAP BTP Subaccount Subdomain

Open your subaccount in SAP BTP cockpit. Make sure you've already enabled SAP BTP, Kyma runtime on your cluster.


### Build Application as OCI Image


In order to run your code on Kyma Runtime (or on any Kubernetes-based platform), you need to provide an OCI image (aka Docker image) for your application. While you are in principle free to choose your image building tool, we recommend using [Cloud Native Buildpacks (CNB)](https://buildpacks.io/).

The command-line tool `pack` supports providing a buildpack and your local source code and creating an OCI image from it. We are working on a process to provide recommended and supported buildpacks. In the meantime, you can use the community-supported [Paketo Buildpacks](https://paketo.io/).

**1.** Install command-line tool `pack`: [Install Pack](https://buildpacks.io/docs/tools/pack/)

For example (macOS):

```Shell / Bash
brew install buildpacks/tap/pack
```

When we speak about repository name, we mean the combination of account and repo name that is usual with Docker Hub: `<docker-hub-account>/<repo-name>`. An example would be `tiaxu/multitenant-kyma-backend`.

As you can only create one private repository in a free Docker hub account, Docker images stored in Docker hub will have different tag names so that they can be stored under one repository. Thus, addressing an image will include the tag name:`<docker-hub-account>/<repo-name>:<tag-name>`. An example would be `tiaxu/multitenant-kyma-backend:v1`.

**2.** In the directory `kyma-multitenant-node`, build the image for the approuter app from source, for example:

```Shell / Bash
pack build <docker-hub-account>/multitenant-kyma-backend:v1 --builder paketobuildpacks/builder-jammy-full
```




### Push OCI Image to Docker Hub


**1.** Log in to Docker using this command:

```Shell / Bash
docker login -u <docker-id> -p <password>
```

**2.** Push the local image into the Docker Hub:

```Shell / Bash
docker push <docker-hub-account>/multitenant-kyma-backend:v1
```

>  For more details, see the [Kubernetes documentation](https://kubernetes.io/docs/tasks/configure-pod-container/pull-image-private-registry/).



### Create Namespace


Then you are ready to deploy it into the Kubernetes cluster with Kyma runtime.

**1.** Select the `Link to dashboard` to open Kyma dashboard.

<!-- border -->![image-20220112154735200](image-20220112154735200.png)

**2.** Create a new namespace through Kyma dashboard or `kubectl` CLI, for example, called `multitenancy-ns`:

<!-- border -->![create_ns](create_ns.png)


**3.** Enable Istio Sidecar Proxy Injection  
Enabling Istio sidecar proxy injection for a namespace allows istiod to watch all Pod creation operations in this namespace and automatically inject newly created Pods with an Istio sidecar proxy. Access Kyma dashboard, switch the toggle to enable Istio sidecar proxy injection.  

 3.1 Select the namespace where you want to enable sidecar proxy injection. 
 
 3.2 Click Edit.  

 3.3 In the UI Form section, switch the toggle to enable Istio sidecar proxy injection.  

 3.4 Click Save.

 <!-- border -->![enable_ns_sidecar](enable_ns_sidecar.png)

> For more details, refer to the [Enable Istio Sidecar Proxy Injection](https://kyma-project.io/#/istio/user/tutorials/01-40-enable-sidecar-injection?id=enable-sidecar-injection-for-a-namespace)


### Deploy Secret for Docker Hub


Since the OCI image is stored in your Docker hub, you need to provide the access information to your Kyma cluster that you can pull the images from those repositories, replace the placeholder values according to your account:

```Shell / Bash
kubectl -n multitenancy-ns create secret docker-registry registry-secret --docker-server=https://index.docker.io/v1/  --docker-username=<docker-id> --docker-password=<password> --docker-email=<email>
```


Therefore, all deployment files contain an `imagePullSecret` entry, which should be set to `registry-secret`.

```yaml
imagePullSecrets:
        - name: registry-secret # replace with your own registry secret
```




### Prepare Deployment Description File


Based on the previous preparation steps, you can define the description file for deployment.

In the root directory `multitenancy-kyma-tutorial`, create a new YAML file called `k8s-deployment-backend.yaml` with the following content:

```yaml

---
apiVersion: gateway.kyma-project.io/v2
kind: APIRule
metadata:
  labels:
    app: kyma-multitenant-node-multitenancy
    release: multitenancy
  name: kyma-multitenant-node-multitenancy
spec:
  gateway: kyma-system/kyma-gateway
  hosts: 
    - kyma-multitenant-node-multitenancy
  rules:
    - path: /*
      methods: ["GET", "POST", "PUT", "PATCH", "DELETE", "HEAD"]
      noAuth: true
  service:
    name: kyma-multitenant-node-multitenancy
    port: 8080

---
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: kyma-multitenant-node-multitenancy
    release: multitenancy
  name: kyma-multitenant-node-multitenancy
spec:
  replicas: 1
  selector:
    matchLabels:
      app: kyma-multitenant-node-multitenancy
      release: multitenancy
  template:
    metadata:
      labels:
        app: kyma-multitenant-node-multitenancy
        release: multitenancy
        sidecar.istio.io/inject: "true"  # Enable Istio sidecar injection on Deployment and all Pods
    spec:
      imagePullSecrets:
        - name: registry-secret # replace with your own registry secret
      containers:
      - env:
        - name: PORT
          value: "8080"
        - name: TMPDIR
          value: /tmp
        image: <docker-hub-account>/multitenant-kyma-backend:v1  # replace with your Docker Hub account name
        name: kyma-multitenant-node-multitenancy
        ports:
        - name: http
          containerPort: 8080
          protocol: TCP
        livenessProbe:
          httpGet:
            path: /
            port: http
        readinessProbe:
          httpGet:
            path: /
            port: http
        startupProbe:
          httpGet:
            path: /
            port: http
          failureThreshold: 15
          periodSeconds: 2
        resources:
          limits:
            cpu: 100m
            memory: 256M
          requests:
            cpu: 100m
            memory: 256M
        securityContext:
          allowPrivilegeEscalation: false
          capabilities:
            drop:
            - ALL
          privileged: false
          runAsNonRoot: true
          readOnlyRootFilesystem: false
        volumeMounts:
        - mountPath: /tmp
          name: tmp
      volumes:
      - emptyDir: {}
        name: tmp

---
apiVersion: v1
kind: Service
metadata:
  creationTimestamp: null
  labels:
    app: kyma-multitenant-node-multitenancy
    release: multitenancy
  name: kyma-multitenant-node-multitenancy
spec:
  ports:
  - port: 8080
    protocol: TCP
    targetPort: 8080
  selector:
    app: kyma-multitenant-node-multitenancy
    release: multitenancy
status:
  loadBalancer: {}

---
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  creationTimestamp: null
  labels:
    release: multitenancy
  name: multitenancy
spec:
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          kubernetes.io/metadata.name: istio-system
      podSelector:
        matchLabels:
          app: istio-ingressgateway
  podSelector:
    matchLabels:
      release: multitenancy
  policyTypes:
  - Ingress
```

> Replace the placeholders with the values that you got in the previous steps.





### Deploy Application into SAP BTP, Kyma runtime



Deploy the Node.js application by executing this command under the root directory `multitenancy-kyma-tutorial`:

```Shell / Bash
kubectl -n multitenancy-ns apply -f k8s-deployment-backend.yaml
```





### Access Application in Browser



**1.** Find the URL address of your application in Kyma dashboard:

<!-- border -->![api_rule_host](api_rule_host.png)

**2.** Access it in the browser, then the application will return the message that you defined before.






### Project Repository



You can find the final Node.js project from repository: [here](https://github.com/SAP-samples/btp-kyma-runtime-multitenancy-tutorial/tree/main/Mission%20-%20Develop%20a%20Node.js%20Application%20in%20the%20SAP%20BTP%20Kyma%20Runtime).





---
