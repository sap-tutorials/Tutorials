---
title: Logging and tracing in SAP Mobile Services for development and operations
description: Logging and tracing overview in the SAP Mobile Services for development and operations Administration Cockpit.
auto_validation: true
primary_tag: products>ios-sdk-for-sap-btp
tags: [  tutorial>beginner, topic>mobile, operating-system>ios, products>sap-business-technology-platform, products>sap-mobile-services ]
---
## Prerequisites  

- **Proficiency:** Beginner
- **Development environment:** Apple iMac, MacBook or MacBook Pro running Xcode 9 or higher
- **SAP BTP SDK for iOS:** Version 2.0
- **Tutorials:** [Application logging and tracing](fiori-ios-hcpms-application-logging)

## Next Steps

- [Manage usage statistics on SAP Mobile Services for development and operations](fiori-ios-hcpms-reporting)

## Details

### You will learn  

In this tutorial, you will learn to use the logging functionality and configurations available in SAP Mobile Services for development and operations. You will also learn how to set logging levels, enable application traces, and view logs and traces.

### Time to Complete

**15 Min**.

---

SAP Mobile Services for development and operations provides administrators, developers and support staff with extensive logs and traces functionality to troubleshoot application issues. You can control the amount of information that is captured by setting the log level for individual logging components. Logs and traces are automatically synchronized with the server when an application is launched or switched to foreground or background on the client.

[ACCORDION-BEGIN [Step 1: ](Examine the SAP Mobile Services for development and operations log settings)]

Go to your [SAP BTP Cockpit](https://account.hanatrial.ondemand.com) and log on. Navigate to **Logs > Log Settings**

In this screen, at the top part, you can set the **logging level** for each type of logging component, as well as **enabling application traces**:

![SAP BTP SDK Assistant for iOS](fiori-ios-hcpms-logging-01.png)

> By default, log levels are set to `ERROR` and application traces are set to `Disable`. Lowering log levels and enabling traces can impact server performance. Generally, you should lower log levels and/or enable traces only when required for debugging or user support.

In the bottom part, you can specify the logging purge periods:

![SAP BTP SDK Assistant for iOS](fiori-ios-hcpms-logging-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Change application log settings)]

For our application, set the following logging levels and application traces:

| Component | Log Level | Trace |
|----|----|----|----|
| Connectivity | DEBUG | Enable |
| Offline | INFO | Enable |
| Security | INFO | Disable |
| Statistics | INFO | Disable |

Click **Save** to store the changes, and navigate to **Logs > Log and Trace**.

By default, the log viewer displays logs and traces occurred in the last 60 minutes. You can change the filter by specifying a different time period, as well as filter by Application ID, Status, Log Type, Logging Component, and set the maximum logging entries:

![SAP BTP SDK Assistant for iOS](fiori-ios-hcpms-logging-03.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Examine changed log details)]

Since the application you created earlier should have run without errors, there should be no log entries for your application visible.

Now you have lowered the logging levels, try run your iOS application again, and check the added log entries. You can open the log details by clicking on the **Log Trace** button in the **Log Trace** column. If all is well, you should see the `DEBUG` **Connectivity** log entries like the following:

![SAP BTP SDK Assistant for iOS](fiori-ios-hcpms-logging-04.png)

[DONE]
[ACCORDION-END]
