---
title: Application logging and tracing
description: Logging and tracing using the SAP Cloud Platform SDK for iOS.
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, topic>mobile, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Development machine:** Access to a Mac computer
 - **Tutorials:** [Push Notifications](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-push-notifications.html)

## Next Steps
 - [Logging and tracing in SAP Cloud Platform mobile service for development and operations](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-logging.html)

## Details
### You will learn  
In this tutorial, you will learn to use the logging functionality that is part of the SAP Cloud Platform SDK for iOS. You will also learn how to set logging settings in SAP Cloud Platform mobile service for development and operations which will be reflected in your application.

### Time to Complete
**15 Min**.

---

The SAP Cloud Platform SDK for iOS provides you with sophisticated functionality which allows you to implement logging and tracing in your application. In addition, you can configure specific logging settings in SAP Cloud Platform mobile service for development and operations, which can be mirrored in your application using the `SAPcpmsSettings` class.

The actual logging is provided by the `Logger` class, which is part of the `SAPCommon` SDK module.

[ACCORDION-BEGIN [Step 1: ](Referencing the Logger class)]

To make a reference to the `Logger` class in the Swift class files you want to provide with logging capabilities, you call the `Logger`'s `shared` method:

```swift
// Declaration of method 'shared' in class 'Logger'
public class func shared(withName name: String) -> Logger
```

You provide function `shared` a String with the name of your class, and it will return the `Logger` object for that class. So, in your class file `MyViewController.swift`, it would thus look like the following:

```swift
import SAPCommon

class MyViewController: UIViewController {

    private let logger = Logger.shared(withName: "MyViewController")

    // etc

}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set logging levels)]

You can define different logging levels, based on how and what you want to log.

| `Logger` method | Description |
|---|---|
| debug | For messages that are only useful during application debugging and should not be logged by default in the shipping application. Think of information that is useful for finding issues in specific 'problematic' areas of the code|
| info | For message logging that is part of the normal operation of the app. Useful for flagging what happens during execution of your app |
| warn | For messages that are concerning but not causing the operation to abort; everything is still working but something occurred that was not expected |
| error | For messages that indicate the app is behaving in a way it shouldn't and is going to abort the current operation. This shouldn't be used for user errors; instead, use for assertion failures, network problems, OData CRUD operations failing, etc. |

For instance, add a logging statement indicating the application view is loaded successfully in the `override func viewDidLoad()` function:

```swift
override func viewDidLoad() {
    super.viewDidLoad()

    self.logger.debug("Demo application is loaded successfully")
}
```

In addition to plain text, you can also supply an optional `error` object, for instance:

```swift
self.someOperation {
    if let error = error {
	      self.logger.error("Uh-oh... An error happened", error: error)
    }
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Retrieve log settings from SAP Cloud Platform mobile service for development and operations)]

Instead of hard-coding logging levels and settings, you can also retrieve these from SAP Cloud Platform mobile service for development and operations. Once you have defined logging settings for your application in SAP Cloud Platform mobile service for development and operations, you can retrieve these settings (among other settings you may have specified) via:

```swift
var urlSession = SAPURLSession()
let settingsParameters = SAPcpmsSettingsParameters(backendURL: URL(string: <#domain#>)!, applicationID: <#appid#>)
let settings = SAPcpmsSettings(sapURLSession: urlSession, settingsParameters: settingsParameters)

settings.load(for: .application)
```

[DONE]
[ACCORDION-END]

## Next Steps
 - [Logging and tracing in SAP Cloud Platform mobile service for development and operations](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-logging.html)
