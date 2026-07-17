---
parser: v2
auto_validation: true
time: 20
tags: [tutorial>beginner, software-product>sap-btp--cloud-foundry-runtime-and-environment, topic>cloud]
primary_tag: software-product>sap-btp--cloud-foundry-runtime-and-environment
author_name: Beyhan Veli
author_profile: https://github.com/beyhan
---

# Delivering Service Binding Credentials to Your Application
<!-- description --> Learn how Cloud Foundry provides service credentials to your application and how to configure the credential delivery method.

## Prerequisites
- **Groups:** [Create Your First App on Cloud Foundry](group.scp-3-first-app)
- **Tutorials:** [Create a Service Instance in SAP BTP](btp-cockpit-instances)
- **Tutorials:** [Install the Cloud Foundry Command Line Interface (CLI)](cp-cf-download-cli)

## You will learn
- What service binding is and why it is important in Cloud Foundry
- The three credential delivery methods available in Cloud Foundry
- How to create a service binding and bind it to your application
- How to enable and use the file-based credential delivery option

---

### Introduction to Service Binding in Cloud Foundry

Service binding is the process by which Cloud Foundry provisions credentials for a service instance and delivers them to your application at runtime. When you **bind** a service instance to your application, the platform generates credentials (such as URLs, usernames, passwords, certificates, or API keys) and makes them available inside the application container. Your application can then use these credentials to connect to the service without hardcoding any configuration.

This approach follows the [twelve-factor app](https://12factor.net/backing-services) methodology, where backing services are treated as attached resources, configured through the environment rather than embedded in application code. The benefits include:

- **Portability:** The same application code works across development, staging, and production environments because credentials are injected externally.
- **Security:** Credentials are never stored in source code or configuration files that could be committed to version control.
- **Flexibility:** You can swap or rotate service instances without changing your application code.

Service binding credentials are usually unique for each application. Another application bound to the same service instance may receive different credentials. This behavior depends on the service implementation.

> After binding or unbinding a service instance, you must **restage** or **re-push** your application for changes to take effect.

### Overview of Service Binding Delivery Options

When a service instance is created and bound to your application, the credentials and metadata become available inside the application container. Cloud Foundry offers **three mutually exclusive** delivery methods. You must choose one, and your application must consume credentials using the method you select.

**1. Default: `VCAP_SERVICES` environment variable**

By default, credentials for all bound service instances are stored as a JSON object in the [VCAP_SERVICES](https://docs.cloudfoundry.org/devguide/deploy-apps/environment-variable.html#VCAP-SERVICES) environment variable. Your application parses this JSON at startup to extract connection details. Helper libraries are available for many frameworks.

> The `VCAP_SERVICES` environment variable has a maximum size of **130 KB**. If you bind many services or if your services provide large credential payloads, consider one of the alternative delivery methods below.

**2. File-based VCAP services**

If the [file-based-vcap-services](https://v3-apidocs.cloudfoundry.org/version/3.216.0/index.html#the-app-feature-object) app feature is enabled, Cloud Foundry writes the same JSON content to a file inside the container and exposes the file path through the [VCAP_SERVICES_FILE_PATH](https://docs.cloudfoundry.org/devguide/deploy-apps/environment-variable.html#VCAP-SERVICES-FILE-PATH) environment variable. Your application reads credentials from the file instead of the environment variable.

By default, this method supports credential payloads up to **1 MB**, although the Cloud Foundry operator can set a different limit. This is significantly higher than the 130 KB limit of the environment variable method. The file-based method is a good choice if you have many service bindings or services that return large credential objects.

**3. Service Binding for K8s**

If the [service-binding-k8s](https://v3-apidocs.cloudfoundry.org/version/3.216.0/index.html#the-app-feature-object) app feature is enabled, Cloud Foundry exposes credentials as a directory tree of files, following the [servicebinding.io](https://servicebinding.io) specification. The [SERVICE_BINDING_ROOT](https://docs.cloudfoundry.org/devguide/deploy-apps/environment-variable.html#SERVICE-BINDING-ROOT) environment variable points to the root folder of this file structure.

Each binding gets its own subdirectory named after the service. Inside each directory, every credential field is saved as a separate file. Nested structures and lists are serialized as JSON strings. For example if you have `VCAP_SERVICES` content like:

```json
{
  "foo": [
    {
      "name": "foo",
      "credentials": {
        "host": "db.example.com",
        "port": "5432",
        "deeply": { "nested": "value" }
      }
    }
  ]
}
```

This produces the following file structure:

```
$SERVICE_BINDING_ROOT/
  foo/
    name: foo
    host: db.example.com
    port: 5432
    deeply: {"nested":"value"}
```

This method also supports up to **1 MB** in total across all files. It is fully compatible with Kubernetes service binding conventions, making it ideal for applications that run on both Cloud Foundry and Kubernetes.

> These three delivery methods are **mutually exclusive**. Enabling one disables the others. If the application cannot consume credentials via the selected method, it will not be able to connect to its bound services, which may cause downtime.

### Bind a Service Instance to Your Application

Follow the [Create a Service Instance in SAP BTP](btp-cockpit-instances) tutorial to create a service instance in your Cloud Foundry environment. It does not matter which service you choose. Please name your instance `my-first-service-instance`.

To bind this service instance to your [cf-nodejs](https://github.com/SAP-samples/cf-sample-app-nodejs.git) application, update your `manifest.yml` file as shown below:

```yaml
---
applications:
  - name: cf-nodejs
    memory: 192M
    instances: 1
    services:
      - my-first-service-instance
```

Then push your application to pick up the new binding:

```bash
cf push cf-nodejs
```

Alternatively, you can create the service binding using the CF CLI. Run the following command to bind the `my-first-service-instance` service instance to your `cf-nodejs` application:

```bash
cf bind-service cf-nodejs my-first-service-instance
```

You should see output similar to:

```
Binding service instance my-first-service-instance to app cf-nodejs in org <my-org> / <my-space> test as <my-user>...
OK

TIP: Use 'cf restage cf-nodejs' to ensure your env variable changes take effect
```

Restage your application to pick up the new binding:

```bash
cf restage cf-nodejs
```

After the restage or push, verify the binding by inspecting your application's environment:

```bash
cf env cf-nodejs
```

Look for the `VCAP_SERVICES` section in the output. This section contains the credentials and metadata for all services bound to your application (since the default binding method is being used).

### Configure the File-Based Service Binding Option

By default, Cloud Foundry delivers credentials through the `VCAP_SERVICES` environment variable. If you need to work with larger credential payloads (beyond the 130 KB environment variable limit) or prefer credential access via file, you can enable the **file-based VCAP services** delivery method.

Currently, the CF CLI does not support enabling Cloud Foundry application features. Therefore, to enable file-based credential delivery for your application, you need to use the Cloud Foundry APIs by running the following commands:

```bash
cf app cf-nodejs --guid # get the guid of your applicaiton
cf curl -X PATCH "/v3/apps/<app-guid>/features/file-based-vcap-services" -d '{ "enabled": true }' # enable the "file-based-vcap-services" application feature
```

After enabling this feature, restart your application for the change to take effect:

```bash
cf restart cf-nodejs
```

Once the application is running, verify that the `VCAP_SERVICES_FILE_PATH` environment variable is set:

```bash
cf env cf-nodejs
```

The value of `VCAP_SERVICES_FILE_PATH` should be a file path such as `/etc/cf-service-bindings/vcap_services`. To inspect the contents of this file, you will need to SSH into the application container. For instructions, see [Accessing your apps with SSH](https://docs.cloudfoundry.org/devguide/deploy-apps/ssh-apps.html).

---
