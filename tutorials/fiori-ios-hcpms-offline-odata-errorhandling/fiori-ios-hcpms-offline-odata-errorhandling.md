---
title: Handle error scenarios with Offline OData
description: How to gracefully handle errors in an Offline OData scenario
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>intermediate, topic>mobile, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Development machine:** Access to a Mac computer
 - **Tutorials:** [Offline OData - Implementation](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-offline-odata-implementation.html)

## Next Steps
 - [Basic Authentication and Secure Key Store](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-secure-keystore.html)

## Details
### You will learn  
In this tutorial, you will learn how to handle errors which can occur in an application using the offline OData scenario.

### Time to Complete
**15 Min**.

---

In a standard, **online OData** application, any errors that may occur are received instantly. As such, these can be handled in a quite simple way. For instance, the SDK Assistant already creates an Xcode project with standard error handlers for the **Create** and **Update** operations.

Because an application using an **offline OData** scenario does not directly perform a create or update operation, any errors are only discovered once the **Upload** operation has been executed.

For example, someone has changed a record on the backend. Imagine your application has not yet synchronized the changes, and while being offline, you have made changes to that same record. If you now upload your changes, they might collide with the changes already made on the backend, and these needs to be corrected.

Or imagine someone has deleted a record you were editing, and the upload of the now-deleted entity fails. An update will also fail if the updated record is locked.

This tutorial will give some hints on how to implement error handling in such scenarios.

In the previous tutorial, you created an application with the offline OData store. In that tutorial, only downloads from the backend to the apps offline store were implemented. Now, uploading of local changes will have to be implemented as well. Uploading the request queue when the device is online sends up changes that have been made to the offline store but have not yet been made to the back end. Once a request is successfully received by the back end, the request is deleted from the queue, the change is made to the back end, and a new version of that object is sent to the offline store the next time the application performs a download.

> Generally, it is good practice to perform an upload before performing a download. However, since locally changed objects are not updated with an upload, this order is not absolutely required.

> In addition, since download and upload operations are not performed automatically, it is up to the application developer to decide when these operations will happen:

>   * User-action
>   * Download when the application starts
>   * Download / upload every N minutes
>   * etc.

In the previous tutorial, the download happens on user-action (when loading a collection of data). For this tutorial, the upload will happen every 60 seconds.

[ACCORDION-BEGIN [Step 1: ](Add scheduled timer)]

Open the file `AppDelegate.swift` and add the following field:

```swift
var scheduledUploadTimer: DispatchSourceTimer?
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add methods for starting & stopping Scheduled Uploads)]

Add the following methods to the `AppDelegate` class:

```swift
private func startScheduledUpload() {
    let queue = DispatchQueue(label: Constants.appId + ".scheduledUploadTimer")
    scheduledUploadTimer = DispatchSource.makeTimerSource(queue: queue)
    scheduledUploadTimer!.scheduleRepeating(deadline: .now(), interval: .seconds(60))

    scheduledUploadTimer!.setEventHandler(handler: self.eSPMContainer.performUpload)

    scheduledUploadTimer!.resume()
}

private func stopScheduledUpload() {
    scheduledUploadTimer?.cancel()
    scheduledUploadTimer = nil
}
```

The first method instantiates the timer, and defines it as a repeating schedule to run every 60 seconds. It will then execute the defined handler `self.eSPMContainer.performUpload` which will be implemented later.

The second method is used to invalidate the timer, when the application will be terminated.

To instantiate the timer, add the `startScheduledUpload()` method to the `urlSession didSet` handler:

```swift
var urlSession: SAPURLSession? {
    didSet {
        self.eSPMContainer = ESPMContainer(urlSession: urlSession!)
        self.uploadLogs()
        self.startScheduledUpload()
    }
}
```

To stop the timer when the app is inactive and resume it once active again, add the following two task delegates:

```swift
func applicationDidEnterBackground(_ application: UIApplication) {
    self.stopScheduledUpload()
}

func applicationWillEnterForeground(_ application: UIApplication) {
    if self.scheduledUploadTimer != nil {
        self.scheduledUploadTimer!.resume()
    }
    else {
        self.startScheduledUpload()
    }
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Suspend timer when application becomes inactive)]

Locate the method `applicationWillResignActive`, and add the following implementation:

```swift
scheduledUploadTimer?.suspend()
```

This will pause the timer when the application becomes inactive.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Resume timer when application becomes active)]

Locate the method `applicationDidBecomeActive`, and add the following implementation:

```swift
scheduledUploadTimer?.resume()
```

This will resume the timer once the application becomes active again.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Implement upload logic)]

Open file `ESPMContainerDataAccess.swift` and add the following methods:

```swift
func performUpload() {
    self.service.upload(completionHandler: { ( _ error: OfflineODataError?) -> Void in
        self.uploadCompletion(success: error == nil, error: error)
    })
}

func uploadCompletion(success: Bool, error: Error?) {
    if success {
        self.logger.info("Upload from offline store to backend successful")
    }
    else {
        self.logger.error("Upload from offline store failed!", error: error)
        // TODO: Implement error handling
    }
}
```

The first method calls the offline store's `upload` method. Upon finishing -- whether successful or not -- the second method `uploadCompletion` is called.

If the upload was successful, it is simply logged. If it failed, an error is logged, and some counteraction should be taken.

>   Now, for an actual collision to happen, you need to update the data on the backend -- either lock update or delete a record. If you run these tutorials against an SAP ECC or ES4 backend, you could easily do that in the backend itself. If you're connecting against the sample OData service of SAP Cloud Platform mobile service for development and operations, it takes some more steps.

>   1.  Create a new, online OData application using the SDK Assistant, using the same application namespace as the offline application for this tutorial.
>   2.  Have the offline application run on a physical device, and the new, online application run on the Xcode Simulator (or a separate device)
>   3.  Connect the physical device running the Offline OData application to Xcode, so you can examine its logs.
>   4.  Start both applications, and make sure both data is up to date.
>   5.  Switch the offline application device to Airplane Mode.
>   6.  On the online application, update a field for a specific record and save the changes.
>   7.  On the offline application, update that same field for that same record and save the changes.
>   8.  Disable Airplane Mode from the offline application's device.
>   9.  Wait for the timer to trigger the upload, and examine the log.

When an upload request fails, the request itself and relevant details about that request are stored in the `ErrorArchive`, a special entity set that can be queried using the `OfflineODataProvider`. It is up to the app developers to determine how these errors should be solved. You could choose to

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Log failed uploads)]

Replace the comment in method `uploadCompletion` with the following function call:

```swift
self.logFailedRequestsAndClearErrorArchive()
```

Add the implementation for the previous function call:

```swift
private func logFailedRequestsAndClearErrorArchive() {
    do {
        let errorArchive:   EntitySet       = try self.service.entitySet( withName: "ErrorArchive" )
        let affectedEntity: Property        = errorArchive.entityType.property( withName: "AffectedEntity" )
        let errorList:      EntityValueList = try self.service.executeQuery(
                DataQuery().selectAll().from( errorArchive ) ).entityList()

        for errorObject in errorList {
            let code    = errorArchive.entityType.property( withName: "Code" ).stringValue( from: errorObject )
            let message = errorArchive.entityType.property( withName: "Message" ).stringValue( from: errorObject )
            let body    = errorArchive.entityType.property( withName: "RequestBody" ).stringValue( from: errorObject )

            self.logger.error("Error object code: \(code), message: \(message), body: \(body)")

            try self.service.loadProperty( affectedEntity, into: errorObject )

            // Do something with the affected entity here, i.e. navigate to AffectedEntity in
            // order to correct the error

            // let errorEntity = affectedEntity.entityValue( from: errorObject )
            // ...


            // Remove error object from EntityArchive
            try self.service.deleteEntity(errorObject)
        }
    }
    catch let error {
        self.logger.error("Cleaning up ErrorArchive failed", error: error)
    }
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Trigger a failing update)]

Repeat the steps to create a failed upload as outlined in step 5. Any change you make now will result in a failed upload request. The failure is logged, and the Error Archive entry is deleted. The affected entity itself is not updated; it is reverted to its previous state.

[DONE]
[ACCORDION-END]

## Next Steps
- [Basic Authentication and Secure Key Store](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-secure-keystore.html)
