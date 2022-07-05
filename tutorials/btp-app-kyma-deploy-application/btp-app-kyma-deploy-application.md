---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Deploy Your Application to Kyma
description: Learn how to add Authorization and Trust Management service to your CAP app, build Docker images and push them to your container registry, deploy your app to your Kyma cluster, and troubleshoot it, if needed.
keywords: cap
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Set Up Local Development using VS Code](btp-app-set-up-local-development)
 - [Create a Directory for Development](btp-app-create-directory)
 - [Create a CAP-Based Application](btp-app-create-cap-application)
 - [Create an SAP Fiori Elements-Based UI](btp-app-create-ui-fiori-elements)
 - [Add Business Logic to Your Application](btp-app-cap-business-logic)
 - [Create a UI Using Freestyle SAPUI5](btp-app-create-ui-freestyle-sapui5)
 - [Use a Local Launch Page](btp-app-launchpage)
 - [Implement Roles and Authorization Checks in CAP](btp-app-cap-roles)
 - [Prepare for SAP BTP Development](btp-app-kyma-prepare-btp)
 - [Prepare Your Kyma Development Environment](btp-app-kyma-prepare-dev-environment)
 - [Setup SAP HANA Cloud for Kyma](btp-app-kyma-hana-cloud-setup)
 - [Prepare User Authentication and Authorization (XSUAA) Setup](btp-app-kyma-prepare-xsuaa)
 - [Add Helm Chart](btp-app-kyma-add-helm-chart)
 - You have created a DB secret as specified in [Setup HANA Cloud](btp-app-#setup-hana-cloud).

## Details
### You will learn
 - How to add the Authorization and Trust Management service (XSUAA) to your project
 - How to build docker images for your CAP service and database deployer
 - How to push the docker images to your container registry
 - How to deploy your app to your Kyma cluster


---

[ACCORDION-BEGIN [Step 1: ](Add Authorization and Trust Management service (XSUAA))]
The next step is to add the Authorization and Trust Management service to `values.yaml` to allow user login, authorization, and authentication checks. Open the `chart/values.yaml` file and add the following snippet:


```YAML[9-17]
srv:
  ...
xsuaa:
  serviceOfferingName: xsuaa
  servicePlanName: application
  parameters:
    xsappname: cpapp
    role-collections:
      - description: Manage Risks
        name: RiskManager
        role-template-references:
          - '$XSAPPNAME.RiskManager'
      - description: View Risks
        name: RiskViewer
        role-template-references:
          - '$XSAPPNAME.RiskViewer'
  config: xs-security.json
```

The configuration for XSUAA is read from the `xs-security.json` file that was created in the tutorial [Prepare User Authentication and Authorization (XSUAA) Setup](btp-app-kyma-prepare-xsuaa). But in the `config` element, values can be added and overwritten. The value `xsappname` gets overwritten with a space-dependent value. The name has to be unique within a subaccount. This allows multiple deployments of this tutorial in different spaces of the same subaccount. For example, different people of a team that want to try it out and don't want to create a new subaccount for each team member. For a productive application, the `xsappname` should be explicitly set to the desired value. Alternatively, role collections can be manually assigned in the SAP BTP cockpit.

> Additional Documentation:

> See section [Assigning Role Collections](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/9e1bf57130ef466e8017eab298b40e5e.html) in SAP BTP documentation for more details.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Build docker images)]

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Set the environment variable for the container registry)]
1. Open a terminal and run the following command:

     ```
     CONTAINER_REGISTRY=<Container Registry>
     ```

2. Refer to [Configure Container Image](btp-app-#configure-container-image) for details on finding `<Container Registry>`.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](CAP build)]
1. Execute the following command in your project folder:

     ```Shell/Bash
     cds build --production
     ```
     You should get an output like: `[cds] - build completed in XXX ms`.

1. (Optional) Run the following command to remove the test data:

     ```
     rm -rf gen/db/data
     ```

> Although the app will work with the test data, usually test data should be removed before deployment.

>  Test files should never be deployed to a SAP HANA database as table data. This can cause the deletion of all files of the affected database table with a change of a data file. You can find more details in [Exclude CSV files from deployment](btp-app-#exclude-csv-files-from-deployment).

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Build CAP service)]
1. Run the following command:

     ```Shell/Bash
     pack build $CONTAINER_REGISTRY/cpapp-srv --path gen/srv \
          --buildpack gcr.io/paketo-buildpacks/nodejs \
          --builder paketobuildpacks/builder:base
     ```

2. You should get an output like:

     ```Shell/Bash
     Successfully built image <Container Registry>/cpapp-srv
     ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 6: ](Build DB deployer)]
1. Run the following command:

     ```Shell/Bash
     pack build $CONTAINER_REGISTRY/cpapp-hana-deployer --path gen/db \
          --buildpack gcr.io/paketo-buildpacks/nodejs \
          --builder paketobuildpacks/builder:base
     ```

2. You should get an output like:

     ```Shell/Bash
     Successfully built image <Container Registry>/cpapp-hana-deployer
     ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 7: ](Push docker images)]
Now that we've build the docker images, let's push them to the container registry.

1. Make sure you're logged in to your container registry:

     ```Shell/Bash
     docker login
     ```

2. Push the images to container registry:

     ```Shell/Bash
     docker push $CONTAINER_REGISTRY/cpapp-srv
     docker push $CONTAINER_REGISTRY/cpapp-hana-deployer
     ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 8: ](Deploy)]
1. Deploy your app:

     ```Shell/Bash
     helm upgrade cpapp ./chart --install
     ```

2. Copy the app URL when done and paste it into a new browser window:

     ![CPAPP URL](cpappURL.png)

3. Now, you can access the CAP server:

     !![Deployed app](srv-cpapp-riskmanagement.png)

     If the error message "no healthy upstream" is shown, wait a few seconds and try again.

6. When you choose the **Mitigation** or **Risk** service entity, you will see an error message:

     !![CAP 401 error](cap_mta_403_error.png)


     The service expects a so called `JWT` (JSON Web Token) in the HTTP `Authorization` header that contains the required authentication and authorization information to access the service. In the next tutorial, you will deploy the SAP Fiori UIs, so that you can access your UIs from SAP Fiori Launchpad. The Launchpad will trigger the authentication flow to provide the required token to access the service.

7. List installed helm charts:

     ```Shell/Bash
     helm list
     ```

     The installed helm chart should be displayed:

     ```
     NAME    NAMESPACE       REVISION        UPDATED                                 STATUS          CHART           APP VERSION
     cpapp   risk-management 5               yyyy-mm-dd time timezone                deployed        cpapp-1.0.0     1.0.0
     ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 9: ](Troubleshooting)]
The Helm chart starts a job that deploys the database content and a deployment with the CAP service. After successful execution, the job is deleted.

In case you encounter an error during the deployment process, follow the instructions to troubleshoot.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 10: ](Check your database deployment)]
```
kubectl get jobs
```

You should see an empty list of jobs that indicates that the deployment job was run successfully:

```
NAME                       COMPLETIONS   DURATION   AGE
```

The Helm chart is configured to clean-up jobs after completion. If it fails or is still in progress, the job is displayed incomplete (completions `0/1`) like in this example:

```
NAME                 COMPLETIONS   DURATION   AGE
cpapp-hana-deployer  0/1           3m7s       3m7s
```

In such a case, you can check the logs for the deployer. First, print the pods:

```
kubectl get pods
```

You should see a list of pods that run on error because of failed deployment attempts:

```
NAME                           READY   STATUS    RESTARTS   AGE
cpapp-hana-deployer-6s7fl      0/1     Error     0          6m16s
cpapp-hana-deployer-n5fnq      0/1     Error     0          7m46s
cpapp-hana-deployer-plfmh      0/1     Error     0          7m16s
cpapp-hana-deployer-z2nxh      0/1     Error     0          8m8s
cpapp-hana-deployer-zc9c2      0/1     Error     0          6m56s
```

Just pick one of the pod names to receive its logs. For example:

```
kubectl logs cpapp-hana-deployer-6s7fl
```

With the `describe` command you can inspect the state of the pod:

```
kubectl describe pod cpapp-hana-deployer-6s7fl
```

This is helpful if the pod couldn't be started.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 11: ](Check SAP HANA cloud trusted IP addresses)]
A possible error cause is that your SAP HANA Cloud instance doesn't allow your Kyma's cluster IP address.

You can check and adjust the settings for [the trusted source IP addresses](https://help.sap.com/docs/HANA_CLOUD/9ae9104a46f74a6583ce5182e7fb20cb/0610e4440c7643b48d869a6376ccaecd.html) in SAP HANA Cloud Central.

You can get your clusters outbound IP address with the following command:

```
kubectl run -it --rm --restart=Never --image alpine/curl nat-ip-probe --overrides='{ "apiVersion": "v1", "metadata": {"annotations": { "sidecar.istio.io/inject":"false" } } }' -- curl https://httpbin.org/ip
```

The command takes a few second to execute and will print a JSON object with an IP address.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 12: ](Check your CAP service)]
If the deployment was successful, you should see the running CAP service in the list of pods:

```
kubectl get pods
```

```
NAME                         READY   STATUS    RESTARTS   AGE
cpapp-srv-84964965cd-5mwtm   2/2     Running   0          13m
```

You can use the `logs` and `describe` commands as described above to inspect the pods. You can find further information about debugging pods in the [Kubernetes documentation](https://kubernetes.io/docs/tasks/debug/debug-application/debug-pods/).

Your service is made externally available using the `VirtualService` resource from `Istio`. You can check your externally exposed hostname:

```
kubectl get virtualservice
```

It should look like this:

```
NAME              GATEWAYS                                         HOSTS                                                         AGE
cpapp-srv-bsbj8   ["kyma-gateway.kyma-system.svc.cluster.local"]   ["srv-cpapp-risk-management.c-abc.stage.kyma.ondemand.com"]   2d15h
```


[VALIDATE_1]
[ACCORDION-END]
---
