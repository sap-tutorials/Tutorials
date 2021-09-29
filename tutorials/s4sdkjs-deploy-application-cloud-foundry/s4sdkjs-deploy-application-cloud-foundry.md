---
title: Deploy Application to Cloud Foundry with SAP Cloud SDK for JavaScript
description: Deploy an existing application and deploy it to Cloud Foundry in SAP Cloud Platform.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-cloud-sdk, topic>javascript ]
primary_tag: products>sap-cloud-sdk
---

## Details
### You will learn
 - How to deploy your application to Cloud Foundry in SAP Cloud Platform
 - How to configure a destination in the SAP Cloud Platform cockpit
 - How to consume the destination in your application

---

[ACCORDION-BEGIN [Step 1: ](Deploy application to Cloud Foundry)]

>**Note:** If you don't have a [SAP Cloud Platform](https://account.hana.ondemand.com/) account, you need to create one.

In order to deploy our application, we first need to login to `Cloud Foundry` in `SAP Cloud Platform` using the **`cf` CLI**. First we need to set an `API` endpoint. The exact URL of this `API` endpoint depends on the region your `subaccount` is in. Open the [SAP Cloud Platform Cockpit](https://account.hana.ondemand.com/) and navigate to the `subaccount` you are planning to deploy your application to. Click on "Overview" on the left and you can see the URL of the `API` endpoint.

![API_Endpoint_in_Subaccount](subaccount_api_endpoint.png)

Copy the URL and paste it into the following command in your command line:

```Shell
cf api https://api.cf.<region>.hana.ondemand.com
cf login
```

`cf login` will prompt you for your username and your password. Should you have more then one organization or space, you will also have to select those.

Finally, if you have logged in successfully, it is time to build and deploy your application.
The `package.json` contains a few scripts that can be used for this purpose. In productive environments you would transpile the application from TypeScript to JavaScript using the `ci-build` script, package our deployment using the `ci-package` script and deploy the application using:
```Shell
cf push
```
Now you want to see your app in action without employing a pipeline and instead deploy it manually.

For manual deployments, run:
```Shell
npm run deploy
```

This command will use your local sources for transpiling, packaging and deployment, but will omit packaging your local `node_modules` as those can be system dependent. Dependencies will instead be installed automatically when deploying to `Cloud Foundry`.

The **`cf` CLI** will automatically pick up the `manifest.yml` of the project when deploying your application. The file should look like this (where `<YOUR-APPLICATION-NAME>` is replaced by the name you specified when initializing the project):

```YAML
applications:
  - name: <YOUR-APPLICATION-NAME>
    path: deployment/
    buildpacks:
      - nodejs_buildpack
    memory: 256M
    command: npm run start:prod
    random-route: true
```

Take a look at the `path` and the `command` attributes. The specified path instructs **`cf` CLI** to upload all the files from the `deployment/` folder. The command specified under the `command` attribute tells the `buildpack` what command to issue to start the application.

When everything works as expected, you should get output that looks something like this:

```Shell
Waiting for app to start...

name:              <YOUR-APPLICATION-NAME>
requested state:   started
routes:            <YOUR-APPLICATION-NAME>.cfapps.eu10.hana.ondemand.com
last uploaded:     Thu 21 Mar 14:05:32 CET 2019
stack:             cflinuxfs3
buildpacks:        nodejs

type:            web
instances:       1/1
memory usage:    256M
start command:   node index.js
     state     since                  cpu    memory        disk           details
#0   running   2019-03-21T13:05:47Z   0.0%   16M of 256M   126.8M of 1G
```

Make sure that the application works correctly by running the start command, this command can be different than the one shown above. Should the application not work for whatever reason, you can call the following command to access the logs:

```Shell
cf logs <YOUR-APPLICATION-NAME> --recent
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deploy the Business Partner mock server (optional))]

>**Note:** If you have access to an SAP S/4HANA Cloud system, you can skip this step.

If you have used the [`OData` Mock Service for the Business Partner `API`](https://github.com/SAP/cloud-s4-sdk-book/tree/mock-server) in the previous tutorial, you will now also have to deploy it to `Cloud Foundry in SAP Cloud Platform`. Navigate to the mock server's root folder, that already contains a `manifest.yml` and run:
```Shell
cf push
```

Make sure that your [Mock Server](https://github.com/SAP/cloud-s4-sdk-book/tree/mock-server) and node.js are up to date.

When the server has been pushed successfully, **`cf` CLI** will output the route where the server can be reached.

Copy this route, as we will need it in the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure destination)]

Now that we have deployed our application, we need to configure a destination in the Cloud Cockpit so that it can be used by our application.

Start by opening the [Cloud Cockpit](https://account.hana.ondemand.com) in your browser and logging in.

Next, navigate to your respective subaccount (in case of a trial account it should be called **trial**). In the menu bar on the left, there should be a section **Connectivity** with an entry called **Destinations**. Click **Destinations**. On the page that opens, click **New Destination** and fill in the details below.

![SAP_Cloud_Platform_Cockpit](sap_cloud_platform_cockpit.png)

For **Name**, choose a name that describes your system. For the tutorial, we will go with **`MockServer`**.

If you use the Business Partner mock server, enter for **URL** the URL that you have saved from the previous step and use **`NoAuthentication`** for **Authentication**. If you use an SAP S/4HANA Cloud system, enter the systems URL in the **URL** field and choose **`BasicAuthentication`** as authentication type. This will make the fields **User** and **Password** appear. Enter here the credentials of a technical user for your SAP S/4HANA Cloud system.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Bind destination service)]

In order to allow the application to use the destination you have just configured, you will need to bind an instance of the destination service and an instance of the `XSUAA service` to your application.

To create an instance of the destination service, execute the following command in your terminal:

```Shell
cf create-service destination lite my-destination
```

This tells `Cloud Foundry in SAP Cloud Platform` to create an instance of the destination service with service plan **lite** and make it accessible under the name **my-destination**. We can now use the name to bind this service to our application. To do this, open your `manifest.yml` and add a section called `services`, under which you can then add the name of the just created service.

The resulting `manifest.yml` should look like this:

```YAML
applications:
  - name: <YOUR-APPLICATION-NAME>
    path: deployment/
    buildpacks:
      - nodejs_buildpack
    memory: 256M
    command: node index.js
    random-route: true
    services:
      - my-destination
```

Secondly, we need an instance of the `XSUAA service`. The `XSUAA service` is responsible for issuing access tokens that are necessary to talk to other services, like the destination service. For this service, we will need a bit of extra configuration in the form of a configuration file. Create a file called `xs-security.json` with the following content:

```JSON
{
  "xsappname": "<YOUR-APPLICATION-NAME>",
  "tenant-mode": "shared"
}
```

The value for `xsappname` again has to be unique across the whole of `Cloud Foundry in SAP Cloud Platform`, so make sure to choose a unique name or prefix.

Now, execute the following command:

```Shell
cf create-service xsuaa application my-xsuaa -c xs-security.json
```

And, as before, add the newly created services to the services section of your `manifest.yml`.

The final `manifest.yml` should look like this:

```YAML
applications:
  - name: <YOUR-APPLICATION-NAME>
    path: deployment/
    buildpacks:
      - nodejs_buildpack
    memory: 256M
    command: node index.js
    random-route: true
    services:
      - my-destination
      - my-xsuaa
```

Finally, we need to adapt the `getAllBusinessPartners` function in `business-partner.controller.ts` to use the destination defined in the Cloud Platform Cockpit.

The new function now looks like this:

```JavaScript / TypeScript
function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      destinationName: 'MockServer'
    });
}
```

We replaced the parameter of `execute` with an object whose key `destinationName` refers to the name of the destination we defined earlier. If you followed step 5 in the previous tutorial, your code will already refer to the correct `destinationName`. If you chose a different name than `MockServer`, make sure to use it here accordingly.

Now we can recompile and redeploy the application. In your command line, run:
```Shell
npm run deploy
```

When you now call the `/business-partners` route of your app, the Business Partners will be retrieved from the defined destination!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test yourself)]

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]

[VALIDATE_3]

[ACCORDION-END]

---
