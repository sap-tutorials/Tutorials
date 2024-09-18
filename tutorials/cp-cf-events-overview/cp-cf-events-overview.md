---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product>sap-btp--cloud-foundry-environment]
primary_tag: software-product>sap-btp--cloud-foundry-environment
author_name: Beyhan Veli
author_profile: https://github.com/beyhan
---

# Overview of Events and the SAP Alert Notification System
<!-- description --> Understand Cloud Foundry events and options to send alerts with the SAP Alert Notification Service

## Prerequisites
 - **Tutorials** [Understand the Cloud Foundry Application Lifecycle for Buildpack Applications](cp-cf-understand-application-lifecycle)

## You will learn
- What Cloud Foundry events are
- How to access events
- How long events are available
- How the SAP Alert Notification service for SAP BTP relates to events


---
### Cloud Foundry Events


Cloud foundry `events` refer to actions taken against Cloud Foundry resources (services, applications, etc.) whether by a user or the system itself. Events are generated e.g. when an app `starts`, `stops`, and `crashes`, among other actions. Events give more detailed information about what has happened to a resource and they can be useful for troubleshooting.

To see all events in relation to the `cf-nodejs` application you created in the previous tutorials, first make sure the application is started, and then run:

```
cf events cf-nodejs
```

You will be greeted by output showing the last 50 events that have occurred in relation to the application.

<!-- border -->![cf events example](cf-events.png)


### Events through the CF API


With the `cf events` command you see only those events related to the app you specify, but using the Cloud Foundry API you can see all the events you have access to in Cloud Foundry.

> The latest version of the [API](https://v3-apidocs.cloudfoundry.org/version/3.111.0/) is v3, which is what will be referenced in this tutorial. All other versions are deprecated.

To see all the `audit events`, which you have access to, run:

```
cf curl /v3/audit_events
```

The generated list of events from this command will be for all organizations and spaces you have access to. You can filter these results further with the options listed in the [API docs](https://v3-apidocs.cloudfoundry.org/version/3.111.0/index.html#list-audit-events). E.g. to get the audit events for a dedicated space you can run:

```
cf space <space-name> --guid
cf curl "/v3/audit_events?space_guids=<space-guid>"
```
> Be aware that there is [pagination](https://v3-apidocs.cloudfoundry.org/version/3.111.0/index.html#pagination) and you may need to iterate over the pages.

Another type of event you can check out is known as an `App Usage Event`, which is used to record a change in app and/or task usage. To see all your app usage events, run:

```
cf curl /v3/app_usage_events
```



### Events in the BTP Cockpit


The BTP Cockpit allows you to see events for your entire space, and also for each individual application.

To see the events for a space, navigate to your space, and there will be a tab on the left labeled `Events`. Click on this tab, and you will see a list of all events that have occurred in that space. You also have the option of searching and filtering on this list.

To see events for an application in the BTP Cockpit, first navigate to the `cf-nodejs` app. Once there, you'll see a tab on the left labeled `Events`. Click on this, and you will see a list of events that have occurred for that app.


### Event Availability


The retention policy for Cloud Foundry events on the SAP BTP is 14 days. As such, if you want to keep track of your events for longer you will need to forward them to another place to be stored. Events are a separate entity from Cloud Foundry logs, and as such they are not stored in the log cache or forwarded in the same way. Therefore, in order to forward events you must use the [CF API](https://v3-apidocs.cloudfoundry.org/version/3.111.0/) discussed earlier in this tutorial.

For example, you can get audit events by calling the `/v3/audit_events` API endpoint, and store the data into a system from your choice. The same can be done for the `/v3/app_usage_events` endpoint. As mentioned before, you can also fine tune what events you retrieve using the filtering options for each endpoint.



### Alert Notification Service


When a Cloud Foundry event occurs, and you want to know about it and be alerted on it, you can use the [SAP Alert Notification service for SAP BTP](https://help.sap.com/viewer/5967a369d4b74f7a9c2b91f5df8e6ab6/Cloud/en-US/086361cb02fb467993acd6f9515607d4.html). Using this service with Cloud Foundry you can alert on any of the events discussed in this tutorial, such as when an application crashes because of a failed health check, when an app is getting restarted, and more. This service provides an API for publishing and subscribing to alerts on events, and it also provides real-time notifications for alerts that you have subscribed to. The service also provides several delivery options for alerts and notifications, including e-mail, Slack, and ticket management systems. Check the [help page](https://help.sap.com/viewer/product/ALERT_NOTIFICATION/Cloud/en-US) of the service for more information.


---
