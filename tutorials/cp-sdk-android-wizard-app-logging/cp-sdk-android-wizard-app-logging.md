---
title: See How Logging Can Be Used in Your Android Application
description: Explore how the logging component can help make deployed applications more supportable.
primary_tag: products>sap-cloud-platform-sdk-for-android
auto_validation: true
tags: [  tutorial>beginner, operating-system>android, topic>mobile, products>sap-cloud-platform-sdk-for-android, products>sap-cloud-platform ]
time: 20
---

## Details
### You will learn  
- How to use the Logging component to log messages
- How to change the log level
- How to upload and view logs in the SAP Cloud Platform Mobile Services cockpit

---

[ACCORDION-BEGIN [Step 1: ](Use the logging component)]
In Android Studio, on Windows press **`Control+N`** or on a Mac press **`command+O`** and enter **`EntitySetListActivity`** to open `EntitySetListActivity.java`.  

On Windows press **`Ctrl+F12`** or on a Mac press **`command+F12`** and enter **`onOptionsItemSelected`** to move to the `onOptionsItemSelected` method.

Note that below method contains two LOGGER statements:
```Java
@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		LOGGER.debug("onOptionsItemSelected: " + item.getTitle());
		if (item.getItemId() == SETTINGS_SCREEN_ITEM) {
			LOGGER.debug("settings screen menu item selected.");
			Intent intent = new Intent(this, SettingsActivity.class);
			this.startActivityForResult(intent, SETTINGS_SCREEN_ITEM);
			return true;
        }
		return false;
	}
```

These message will be logged when the app's log level is set to Debug or Path and the app's Settings menu item is clicked.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Change the log level)]

Navigate to the entity list screen and open the app's menu.

![Settings menu button](settings_menu_button.png)


Choose Settings.

![Settings menu opened](settings_menu.png)


Select Log Level.

![Log level settings option](log_level_option.png)


Set the level to Debug.

![Debug log level option](debug_log_level_option.png)


Navigate back to the entity list screen, then back into the settings screen to show the effect of changing the log level.

![Settings menu opened](settings_menu.png)


Examine the `Logcat`.  In the filter add the class name that we are interested in seeing the log from.

`com.sap.wizapp.mdui.EntitySetListActivity`

Notice that the messages were logged since the log level of the app was set to DEBUG or PATH.

![Debug log level output](debug_log.png)


The SDK libraries also log output based on the app's log level.  

Change the filter to `com.sap.cloud.mobile.foundation`.

Press the back button to exit the app and notice the logged lines from the foundation library.

![Debug log level output for foundation](debug_log_foundation.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Upload and view the log)]

Navigate back to the settings menu in the app, and this time, tap the "Upload Log" button.

![Upload log button in settings menu](upload_log_button.png)


A Toast message will appear, confirming the upload succeeded.

![Log upload succeeded](log_uploaded.png)

In the Mobile Services cockpit, navigate to Analytics > Logs > Technical Logs.

![Analytics > Logs > Technical Logs](select_and_download_log.png)


You should be able to see the log you just uploaded in the list of technical logs. You can inspect the log in browser by clicking on its table entry, or you can download the text version by selecting it and clicking the download button.  If the log doesn't appear immediately, wait for a few moments then click "Go" to refresh the view.

![View log](view_log.png)

Additionally, you can access the logs locally on an emulator. The file system of an Android emulator can be browsed using the Device File Explorer to view the log files as shown below, in `/data/user/0/com.sap.wizapp/files`.

![View logs in emulator](local_log_location.png)


> **Note**: For more details on logging, check out the <a target="_blank" href="https://help.sap.com/doc/c2d571df73104f72b9f1b73e06c5609a/Latest/en-US/docs/user-guide/foundation/logging.html">Logging</a>.

Congratulations! You have explored how the logging library can be used to debug or support a deployed application.

[DONE]
[ACCORDION-END]

---
