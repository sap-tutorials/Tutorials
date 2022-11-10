---
parser: v2
author_name: Bruce Meng
author_profile: https://github.com/flyingfish162
primary_tag: software-product>sap-btp-sdk-for-android
auto_validation: true
tags: [  tutorial>beginner, operating-system>android, topic>mobile, software-product>sap-btp-sdk-for-android, software-product>sap-business-technology-platform ]
keywords: sdkforandroid
time: 15
---

# See How Logging Can Be Used in Your Android Application
<!-- description --> Explore how the logging component can help make deployed applications more supportable.

## Prerequisites
- You have [Set Up a BTP Account for Tutorials](group.btp-setup). Follow the instructions to get an account, and then to set up entitlements and service instances for the following BTP services.
    - **SAP Mobile Services**
- You completed [Try Out the SAP BTP SDK Wizard for Android](cp-sdk-android-wizard-app).

## You will learn
- How to use the Logging component to log messages
- How to change the log level
- How to upload and view logs in the SAP Mobile Services cockpit

---

### Use the logging component


[OPTION BEGIN [Java]]

1.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and enter **`EntitySetListActivity`** to open `EntitySetListActivity.java`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and enter **`onOptionsItemSelected`** to move to the `onOptionsItemSelected` method.

    Note that the following method contains two LOGGER statements:

    ```Java
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        LOGGER.debug("onOptionsItemSelected: " + item.getTitle());
        switch (item.getItemId()) {
            case R.id.menu_settings:
                LOGGER.debug("settings screen menu item selected.");
                this.startActivity(new Intent(this, SettingsActivity.class));
                return true;
    ```

    These messages will be logged when the app's log level is set to **Debug** or **Path** and the app's **Settings** menu item is opened.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and enter **`EntitySetListActivity`** to open `EntitySetListActivity.kt`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and enter **`onOptionsItemSelected`** to move to the `onOptionsItemSelected` method.

    Note that the following method contains two LOGGER statements:

    ```Kotlin
    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        LOGGER.debug("onOptionsItemSelected: " + item.title)
        return when (item.itemId) {
            R.id.menu_settings -> {
                LOGGER.debug("settings screen menu item selected.")
                Intent(this, SettingsActivity::class.java).also {
                    this.startActivity(it)
                }
                true
            }
    ```

    These messages will be logged when the app's log level is set to **Debug** or **Path** and the app's **Settings** menu item is opened.

[OPTION END]


### Change the log level


1.  Navigate to the entity list screen and open the app's menu.

    <!-- border -->![Settings menu button](settings_menu_button.png)

2.  Choose **Settings**.

    <!-- border -->![Settings menu opened](settings_menu.png)

3.  Select **Log Level**.

    <!-- border -->![Log level settings option](log_level_option.png)

4.  Set the level to **Debug**.

    <!-- border -->![Debug log level option](debug_log_level_option.png)

5.  Navigate back to the entity list screen, then back into the **Settings** screen to see the effect of changing the log level.

    <!-- border -->![Settings menu opened](settings_menu.png)

6.  Examine the **Logcat** (located at the bottom of the Android Studio screen, click it and you can see the logs). In the filter, add the name of the class that we are interested in seeing the log from: **`com.sap.wizapp.mdui.EntitySetListActivity`**.

    Notice that the messages were logged since the log level of the app was set to **Debug** or **Path**.

    <!-- border -->![Debug log level output](debug_log.png)

---

The SDK libraries also log output based on the app's log level.

1.  Change the filter to **`com.sap.cloud.mobile.foundation`**.

2.  Press **Back** to exit the app and you will see the logged lines from the foundation library.

    <!-- border -->![Debug log level output for foundation](debug_log_foundation.png)



### Upload and view the log


1.  Navigate back to the **Settings** menu in the app, and this time tap **Upload Log**.

    <!-- border -->![Upload log button in settings menu](upload_log_button.png)

    A Toast message is displayed confirming that the upload succeeded.

    <!-- border -->![Log upload succeeded](log_uploaded.png)

2.  In the [Mobile Services cockpit](https://mobile-service-cockpit-web.cfapps.eu10.hana.ondemand.com/), navigate to **Mobile Applications** > **Native/Hybrid** > **com.sap.wizapp** > **Mobile Client Log Upload**.

    <!-- border -->![Mobile Applications > Native/Hybrid > com.sap.wizapp > Mobile Client Log Upload](select_and_download_log.png)

3.  Select the **Logs** tab and you will see the log you just uploaded in the list (only the **Error** level can be viewed here). If the log doesn't appear immediately, wait for a few moments, then click **Go** to refresh the view.

    <!-- border -->![View log](view_log.png)

4.  You can inspect the log details in the browser by clicking on its table entry. The following screenshot contains the details of the first log.

    <!-- border -->![View log details](view_log_details.png)

5.  Select the **Log Files** tab and you will see the log files you just uploaded in the list. You can download the files by clicking **Download**.

    <!-- border -->![View log details](view_log_files.png)

6.  Additionally, you can access the logs locally on an emulator. You can browse the file system of an Android emulator using the **Device File Explorer** to view the log files as shown below, in **data** > **data** > **com.sap.wizapp** (or the package name of your project) > **files**.

    <!-- border -->![View logs in emulator](local_log_location.png)

7.  You can manage the initial log level of the application and the ability for mobile services to accept logs on the **Configuration** page, as shown below.

    <!-- border -->![Log Policy](client_policies.png)


>For further information on logging, see [Logging](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/logging/overview.html).

Congratulations! You have explored how you can use the logging feature to debug or support a deployed application.


---
