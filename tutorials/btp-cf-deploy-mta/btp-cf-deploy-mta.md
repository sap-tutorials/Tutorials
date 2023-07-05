---
parser: v2
author_name: Lilyana Rangelova
author_profile: https://github.com/lilyanarangelova
auto_validation: true
time: 30
tags: [ tutorial>intermediate, products>sap-btp--cloud-foundry-environment]
primary_tag: products>sap-business-technology-platform
---

# Deploy Your First Multitarget Application
<!-- description --> Deploy a simple multitarget application (MTA) to SAP Business Technology Platform, Cloud Foundry environment using the MultiApps CF CLI Plugin.

## Prerequisites
- If you do not have a Cloud Foundry Trial subaccount and dev space on [SAP BTP Cockpit](https://cockpit.hanatrial.ondemand.com/cockpit/) yet, create your  [Cloud Foundry Trial Account](hcp-create-trial-account) and, if necessary [Manage Entitlements](cp-trial-entitlements).
- You have downloaded and installed the [cf command line client](https://github.com/cloudfoundry/cli#downloads) for Cloud Foundry as described in the tutorial [Install the Cloud Foundry Command Line Interface (CLI)](cp-cf-download-cli).

## You will learn
  - How to deploy a multitarget application (MTA) in Cloud Foundry environment

## Intro
The example MTA demonstrates the creation of a service instance and an application, and the binding between them.

---

### Prepare a simple static application



1. Create an `index.html` file with the following content:

    ```HTML
    <h3>Hello World!</h3>
    ```

2. Archive the `index.html` file into a `content.zip` archive and place it to a directory that is called `my_first_mta`.


### Prepare the MTA deployment descriptor


Copy the example below to an `mtad.yaml` file that is located in the `my_first_mta` directory.

```YAML
_schema-version: "3.1"
ID: app
version: 1.0.0

modules:
  - name: my-first-app
    type: staticfile
    path: content.zip
    requires:
      - name: my-first-app-service
    parameters:
      memory: 64m
      disk-quota: 64m

resources:
  - name: my-first-app-service
    type: org.cloudfoundry.managed-service
    parameters:
      service: application-logs
      service-plan: lite
```

The MTA deployment descriptor comprises of an MTA module, which represents a simple static application, and an MTA resource, which represents an `application-logs` service instance with plan **lite**.



### Install the MultiApps CF CLI Plugin


Execute the following:

```Shell
cf add-plugin-repo CF-Community https://plugins.cloudfoundry.org
cf install-plugin multiapps
```


### Deploy the MTA


Execute the following:

```Shell
cd  <path to my_first_mta>
cf deploy ./
```

The result should be as follows:

```Output
Deploying multi-target app archive app.mtar in org <ORG> / space <SPACE> as <USER>...
Uploading 1 files...
<PATH_TO_MTAR>
OK
Operation ID: <MTA_OPERATION_ID>
Deploying in org "deploy-service" and space "<SPACE>"
Detected MTA schema version: "3"
No deployed MTA detected - this is initial deployment of MTA with ID "app"
Detected new MTA version: "1.0.0"
Processing service "my-first-app-service"...
Creating service "my-first-app-service" from MTA resource "my-first-app-service"...
1 of 1 done
Creating application "my-first-app" from MTA module "my-first-app"...
Binding service instance "my-first-app-service" to application "my-first-app"...
Uploading application "my-first-app"...
Started async upload of application "my-first-app"
Scaling application "my-first-app" to "1" instances...
Staging application "my-first-app"...
Application "my-first-app" staged
Starting application "my-first-app"...
Application "my-first-app" started and available at "<ORG>-<SPACE>-my-first-app.<DEFAULT_DOMAIN>"
Skipping deletion of services, because the command line option "--delete-services" is not specified.
Process finished.
Use "cf dmol -i <MTA_OPERATION_ID>" to download the logs of the process.
```

><MTA_OPERATION_ID> is a unique identifier for each MTA operation. It can be later used for troubleshooting.

In the output example above, the `application my-first-app` is deployed and started. A service called `my-first-app-service` is also created and is bound to the application. Credentials are provisioned for the service instance and delivered to the application runtime in the `VCAP_SERVICES` environment variable.

>The example above shows the deployment from a directory where the MTA deployment descriptor is available.
 If you want to deploy a ready MTA archive, with a file extension `.mtar`, execute the following command:

>``` Console command
cf deploy <PATH_TO_MTAR>
```

When triggering a deployment from a directory, the MTAR is built under the hood and it can be verified by checking the new file `app.mtar` in the current folder.


### Examine the results of deployment


To check the application, execute:

```Shell
cf apps
```

The result should be as follows:

```Output
cf apps
Getting apps in org <ORG> / space <SPACE> as <USER>...
OK

name          requested state  processes   routes
my-first-app  started          web:1/1    <ORG>-<SPACE>-my-first-app.<DEFAULT_DOMAIN>
```

To check the service, execute:
```Console command
cf services
```
The result should be as follows:

```Output
cf services
Getting services in org <ORG> / space <SPACE> as <USER>...

name                   service            plan     bound apps     last operation
my-first-app-service   application-logs   lite     my-first-app   create succeeded
```

All CF entities above compose the MTA with ID “app”. It can be displayed with the command:
```Console Command
cf mta app
Showing health and status for multi-target app app in org <ORG> / space <SPACE> as <USER>...
OK
Version: 1.0.0
Namespace:
 
Apps:
name           requested state   instances   memory   disk   urls
my-first-app   STARTED           1/1         10.9M    5.2M   <ORG>-<SPACE>-my-first-app.<DEFAULT_DOMAIN>
 
Services:
name                   service            plan   bound apps     last operation
my-first-app-service   application-logs   lite   my-first-app   create succeeded
```



### Create an extension descriptor



Extension descriptors are files complementary to the main deployment descriptor that provide additional data. They have a file extension `.mtaext` and are external to the MTA archive `.mtar`. They are used to provide deployment specific information, for example, credentials to external services.

1. Copy the example below to an `app-2-instances.mtaext` file:

    ```YAML
    _schema-version: "3.1"
    ID: app-instances-2
    extends: app

    modules:
    - name: my-first-app
      parameters:
        instances: 2
    ```


2. Place the file to the directory `my_first_mta`.

The extension descriptor is used to extend the MTA deployment descriptor (`mtad.yaml`), so that `my-first-app` will be scaled to two instances.



### Deploy the MTA and the extension descriptor


Execute the following command:
```Shell
cf deploy ./ -e app-2-instances.mtaext
```

The result should be as follows:

```Output
OK
Uploading 1 files...
<PATH_TO_MTAR>
OK
Uploading 1 files...
<PATH_TO_EXTENSION_DESCRIPTOR>
OK
Operation ID: <MTA_OPERATION_ID_2>
Deploying in org "<ORG>" and space "<SPACE>"
Detected MTA schema version: "3"
Detected deployed MTA with namespace "null", ID "app" and version "1.0.0"
Detected new MTA version: "1.0.0"
Deployed MTA version: "1.0.0"
Processing service "my-first-app-service"...
Updating application "my-first-app"...
Application "my-first-app" attributes are not modified and will not be updated
Unbinding service instance "my-first-app-service" from application "my-first-app"...
Binding service instance "my-first-app-service" to application "my-first-app"...
Uploading application "my-first-app"...
Content of application "my-first-app" is not changed - upload will be skipped.
Scaling application "my-first-app" to "2" instances...
Stopping application "my-first-app"...
Starting application "my-first-app"...
Application "my-first-app" started and available at "<ORG>-<SPACE>-my-first-app.<DEFAULT_DOMAIN>"
Skipping deletion of services, because the command line option "--delete-services" is not specified.
Process finished.
Use "cf dmol -i <MTA_OPERATION_ID_2>" to download the logs of the process.
```



### Examine the results of deployment


To verify that the application is scaled to two instances, execute:

```Shell
cf apps
```

The result should be as follows :

```Output
cf apps
Getting apps in org <ORG> / space <SPACE> as <USER>...
OK

name          requested state  processes   routes
my-first-app  started          web:2/2    <ORG>-<SPACE>-my-first-app.<DEFAULT_DOMAIN>
```


### Test yourself










---
