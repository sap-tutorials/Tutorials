---
title: Enable Offline OData in Your Android Application
description: Use the Offline OData library to enable data access from an on device database.
auto_validation: true
primary_tag: operating-system>android
tags: [  tutorial>beginner, operating-system>android ]
time: 20
---

## Details
### You will learn  
  - How to enable Offline OData in your Android application

---

**THIS TUTORIAL SERIES CAN ONLY BE EXECUTED AT TECHED**  as it is. Please find us at the Google booth in the `AppSpace` and we will provide everything you will need.


[ACCORDION-BEGIN [Step 1: ](Add dependency to build.gradle document)]

In Android Studio, add the following line to the list of dependencies in the app's **`build.gradle`** file to include the Offline OData framework and click on **`Sync Now`**.  After the sync completes, it will be possible to use the libraries in the `offline-odata` component.

```Java
implementation group:'com.sap.cloud.android', name:'offline-odata', version: sdkVersion
```

![Offline OData Gradle Dependency](offline-odata-gradle-file.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add code to open offline store)]

Press **`Ctrl+N`** and enter **`SAPServiceManager.java`**.

Add the following imports.  Note they will appear grey until they are used in the following steps.

```Java
import android.content.Context;
import android.util.Log;
import android.widget.Toast;
import com.sap.cloud.mobile.odata.core.AndroidSystem;
import com.sap.cloud.mobile.odata.offline.OfflineODataDefiningQuery;
import com.sap.cloud.mobile.odata.offline.OfflineODataException;
import com.sap.cloud.mobile.odata.offline.OfflineODataParameters;
import com.sap.cloud.mobile.odata.offline.OfflineODataProvider;
import java.net.MalformedURLException;
import java.net.URL;
```

In the `SAPServiceManager` class, select the variable, **`OnlineODataProvider`** and replace it with the two following variables.

```Java
//private OnlineODataProvider provider;
private OfflineODataProvider provider;
private static final String TAG = SAPServiceManager.class.getName();
```


Comment out the methods **`openODataStore`** and **`getServiceRoot`**.


Add the below methods.

```Java
public void openODataStore(Action0 callback, Context context) {
    setupOfflineOData(callback, context);
}

private void setupOfflineOData(Action0 callback, Context context) {
    try {
        if (configurationData.loadData()) {
            Toast.makeText(context, "Opening the offline store which may take a few moments the first time it is opened.", Toast.LENGTH_LONG).show();
            //Initialize application context for use by OfflineODataProvider
            AndroidSystem.setContext(context);
            //commonly set parameters include setStoreEncryptionKey, setStoreName, setStorePath
            OfflineODataParameters offParam = new OfflineODataParameters();
            String serviceUrl = configurationData.getServiceUrl();
            URL url = new URL(serviceUrl + CONNECTION_ID_ESPMCONTAINER);
            provider = new OfflineODataProvider(url, offParam, ClientProvider.get());
            //Specifies which entities we wish to include in the offline store
            provider.addDefiningQuery(new OfflineODataDefiningQuery("Customers", "Customers", false));
        }
  } catch (MalformedURLException e) {
      e.printStackTrace();
  } catch (OfflineODataException e) {
      e.printStackTrace();
  }
  //opens the offline store.  This can take a minute or so the first time it is called as it is created and populated.
  Log.d(TAG, "About to call open on the offline store.");
  provider.open(() -> {
      Log.d(TAG, "Offline store opened.");
      eSPMContainer = new ESPMContainer(provider);
      callback.call();
      syncOfflineData();  //TODO could be triggered via a menu action
  }, (OfflineODataException offlineODataException) -> {
      Log.d(TAG, "Offline store did not open.", offlineODataException);
  });
}

public void syncOfflineData() {
    //send the local changes to the back end OData service
    Log.d(TAG, "About to call provider.upload.");
    provider.upload(() -> {
        Log.d(TAG, "Successfully uploaded the changed data.");
        //get changes from the back end OData service.
        provider.download(() -> {
            Log.d(TAG, "Successfully downloaded the changed data.");
        }, (error) -> {
            Log.d(TAG, "Failed when downloading the changed data with error: " + error.getMessage());
        });
    }, (error) -> {
        Log.d(TAG, "Failed when uploading the changed data with error: " + error.getMessage());
    });
}

public String getServiceRoot() {
    if (configurationData.loadData()) {
        String serviceUrl = configurationData.getServiceUrl();
        if (serviceRoot == null) {
            serviceRoot = serviceUrl + "/" + CONNECTION_ID_ESPMCONTAINER;
        }
    }
    else {
        throw new IllegalStateException("SAPService manager configurationData.loadData() failed.");
    }
    return serviceRoot;
}
```

Press **`Ctrl+N`** and enter **`LogonActivity.java`**.

Press **`Ctrl+F`** and enter **`openODataStore`**. In the `LogonActivity.java` add the application context to the `openODataStore` call as a parameter as shown below.  Note, this can be done by replacing the second last line of the method with the below code.

![getApplicationContext() parameter added](application-context-parameter.png)

```Java
}, getApplicationContext());
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run and explore the application)]
Run the application, upon reaching the home screen, it may take a few moments for the offline store to open.

> The first time the app is opened it will take longer to open the offline store as it needs to be created and populated.  Make sure Wi-Fi is enabled. Consult the `Logcat` log and wait for the following messages to appear.

![Log Message from Opening offline store](offline-store-opened.png)

Turn on airplane mode on your device (swipe down from the top of the emulator and click on the airplane icon) which simulates having no network connection. Notice that Wi-Fi and mobile data are switched off.

![Airplane mode on your device](airplane-mode.png)

Navigate to the list of customers by tapping the **Customers** row. Now tap on a customer.

To edit the customer tap the "Edit Customer" icon.

![Edit Customer Button](edit-customer-button.png)

Change a field of a customer such as the city, and save your changes.

![Change the customer's city](edit-customer-activity.png)

To show that the data is stored offline and then synchronized turn off airplane mode and fully close the app.

![Close the app](close-app.png)

Re-open the app and check the `Logcat`. It shows the upload and download operations were successful. The previously made change is now synced with the backend OData service.

![Successful upload/download](upload-successful.png)

> This example does not handle conflicts. Details on this topic are available at <a target="_blank" href="https://help.sap.com/doc/c2d571df73104f72b9f1b73e06c5609a/Latest/en-US/docs/user-guide/odata/Offline_OData_Handling_Errors_And_Conflicts.html">Offline OData Conflicts</a>.

Congratulations! You have created an offline enabled application and have completed this TechEd App Space mission. 

See also the blog <a target="_blank" href="https://blogs.sap.com/2018/10/15/step-by-step-with-the-sap-cloud-platform-sdk-for-android-part-1/">Step by Step with the SAP Cloud Platform SDK for Android</a>.

[VALIDATE_1]
[ACCORDION-END]

---
