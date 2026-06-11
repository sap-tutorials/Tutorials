---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-btp--cloud-foundry-runtime-and-environment, topic>cloud]
primary_tag: software-product>sap-btp--cloud-foundry-runtime-and-environment
author_name: Beyhan Veli
author_profile: https://github.com/beyhan
---

# Application Readiness Check
<!-- description --> How the Application Readiness Health Check works and how you can configure it for your own application

## Prerequisites
- **Groups** [Create Your First App on Cloud Foundry](group.scp-3-first-app)
- **Tutorials** [Install the Cloud Foundry Command Line Interface (CLI)](cp-cf-download-cli)
- **Tutorials** [Application Health Check](cp-cf-application-health-check)

## You will learn
- The difference between liveness and readiness health checks in Cloud Foundry
- How readiness health checks prevent premature routing of traffic to app instances
- How to configure readiness health checks using the app manifest

---

### Overview of Health Checks


Cloud Foundry provides two categories of health checks to keep your applications healthy and available: **liveness** health checks and **readiness** health checks. Understanding the difference between them is essential for running reliable applications.

- **Liveness health checks** validate that app instances are running. When a liveness health check fails, Cloud Foundry considers the instance crashed, stops it, and restarts it.
- **Readiness health checks** validate that app instances are ready to serve requests. When a readiness health check fails, the app instance is removed from the route pool so that it no longer receives traffic, but it is **not** restarted.

There are three types available for both liveness and readiness health checks, as explained in the tutorial for [Application Health Check](cp-cf-application-health-check):

- `http` health checks perform a GET request to an endpoint.
- `port` health checks make a TCP connection to the port (or ports) configured for the app.
- `process` health checks check that a process stays running.

The default liveness health check type is `port`, while the default readiness health check type is `process`.

For more detailed information about the types of health checks, see [Using Cloud Foundry Health Checks](https://docs.cloudfoundry.org/devguide/deploy-apps/healthchecks.html).

Readiness health checks are particularly useful when your application needs time after startup to load caches, establish database connections, or warm up before it can handle real traffic. Without a readiness health check, Cloud Foundry may route requests to an instance that is running but not yet ready, resulting in errors for your users.

Additionally, readiness health checks help in situations where an application instance becomes temporarily overloaded. If the instance determines it cannot handle new requests based on its internal metrics or logic, it can intentionally fail its readiness health check. This causes Cloud Foundry to automatically remove the overloaded instance from the routing pool, so it stops receiving traffic while still remaining running. Once the instance recovers and is able to handle new requests, its readiness health check will succeed again, and Cloud Foundry will add it back to the request queue. This mechanism helps prevent further overload, increases application stability, and allows instances to flexibly remove and re-add themselves as they become healthy.



### The Lifecycle of a Readiness Health Check


Understanding how readiness health checks fit into the app lifecycle is important for configuring them correctly. The lifecycle is similar to the one described for [Application Health Check](cp-cf-application-health-check), with key differences in how readiness and liveness are handled.

1. When you deploy an application, you can specify both a liveness and a readiness health check in the app manifest.
2. Cloud Controller creates a Long Running Process (LRP) for Diego with the specified health check definitions. If no readiness health check is provided, Cloud Controller configures a `process` readiness health check type by default.
3. When Diego starts an app instance, a **startup health check** (using the liveness health check configuration) runs every 2 seconds until a healthy response is received or the startup timeout elapses. This 2-second interval is not configurable.
4. After the startup health check succeeds, the app instance is marked as **Running** and Diego begins performing both the liveness and readiness health checks at their configured intervals (default: 30 seconds).
5. Once the readiness health check succeeds, the app instance's route is advertised and the instance starts receiving traffic.
6. If the readiness health check later fails, the route to that app instance is **removed** and the instance stops receiving traffic but continues running. Once the readiness health check succeeds again, the route is re-advertised.
7. If the liveness health check fails, Diego stops and deletes the app instance entirely, then reschedules a new one. This is reported as a crash event.

> The key distinction: a failed readiness health check removes the instance from routing (no traffic), while a failed liveness health check causes the instance to be restarted. This separation allows your app to temporarily become unavailable to traffic without being unnecessarily restarted.



### Configuring a Readiness Health Check via the App Manifest


Readiness health checks are configured through the app manifest. Unlike liveness health checks, they cannot be set using `cf set-health-check`. The following manifest attributes are available:

| Attribute | Description | Default |
|---|---|---|
| `readiness-health-check-type` | The type of readiness health check: `http`, `port`, or `process` | `process` |
| `readiness-health-check-http-endpoint` | The endpoint for `http` type readiness health checks | `/` |
| `readiness-health-check-invocation-timeout` | Timeout in seconds for individual readiness health check requests | `1` |
| `readiness-health-check-interval` | Time in seconds between readiness health check requests | `30` |

To configure an `http` readiness health check for your [cf-nodejs](https://github.com/SAP-samples/cf-sample-app-nodejs.git) application, update your `manifest.yml` as follows:

```YAML
---
applications:
- name: cf-nodejs
  memory: 192M
  instances: 1
  random-route: false
  readiness-health-check-type: http
  readiness-health-check-http-endpoint: /
```

In the example above, Cloud Foundry will perform a GET request to the `/` endpoint to determine if the app instance is ready to receive traffic. You can point this to a dedicated readiness endpoint (e.g., `/ready`) if your application provides one.

You can also combine a readiness health check with a liveness health check in the same manifest:

```YAML
---
applications:
- name: cf-nodejs
  memory: 192M
  instances: 1
  random-route: false
  health-check-type: http
  health-check-http-endpoint: /
  readiness-health-check-type: http
  readiness-health-check-http-endpoint: /ready
```

After updating your manifest, push or re-push your application for the changes to take effect:

```
cf push cf-nodejs
```


### Validate the readiness health check


To validate that the readiness health check is working, you can use the `cf logs` command:

```
cf logs cf-nodejs --recent
```

You should see a log line similar to:

```
[HEALTH/0] OUT Container passed the readiness health check. Container marked ready and added to route pool.
```

This indicates that the readiness health check has been configured and was successful. Additional log messages may also show that both the liveness and readiness health checks are being evaluated.


You can also check the current health check configuration of your app by running:

```
cf get-readiness-health-check cf-nodejs
```

This displays the configured readiness check type and endpoint. Note that readiness health check details are reflected in the manifest and the app's process configuration.


---
