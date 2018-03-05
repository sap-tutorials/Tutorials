---
title: Build a geofencing app
description: Build and app with geofencing notifications using SAP Cloud Platform SDK for iOS and SAP HANA MDC databse on SAP Cloud Platform
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>how-to, tutorial>advanced, operating-system>ios, topic>mobile, topic>odata, products>sap-hana, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---
## Prerequisites  
 - **Proficiency:** Advanced
 - **Tutorials:** [Sign up for a free trial account on SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html) and [Enable SAP Cloud Platform mobile service for development and operations](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-setup.html)



## How-To Details
One of the most useful features of mobile devices is it's location-awareness. It helps you navigate, automatically switch to the timezone you're in, allow for location targeted push notifications, or give insights into whatever activities take place at a certain location.
You could say there are two types of location services:

* Active, i.e. where you perform a task based on your location (for instance turn-by-turn navigation, checking into a place on social media, find the nearest espresso bar, etc.)
* Passive, i.e. where your mobile device performs an action based on your location (for instance location based mobile advertising, notify warehouse workers a truck is about to arrive at the dock, alert people of entering a dangerous area, open your garage door when you approach your home, etc.)

One solution to be able to passively act on your current location is by using geofences. A geofence is nothing more than an area on a virtual map. And when a mobile device enters or leaves such predefined areas, this can be detected and an application can act on that: send a notification, update a backend system, trigger another hardware device to do something; basically anything that can be done manually can now be done automatically.

In this tutorial, you will first create an SAP HANA MDC database which will hold location data as geofences. You then expose this data via an OData service. Then you will build an app using this OData service, add logic to display the stored geofences, as well as perform action when your device enters these geofences.



### Time to Complete
**75 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Create the SAP HANA MDC database)]

Log on to your **SAP Cloud Platform Cockpit**, navigate to **SAP HANA / SAP ASE > Database & Schemas** and click the **New** button.

Enter the following details:

| Field Name | Value |
|----|----|
| Database ID | `sapgeo` |
| Database System | `HANA MDC (<trial>)` |
| SYSTEM User Password | `<provide a password>` |
| Web Access | `ON` |

![Create the SAP HANA MDC database](fiori-ios-scpms-geolocation-01.png)

Click **Save** when done.

The database is now being created. This might take a couple of minutes, but you can track its progress in the **Events** page for the database container:

![Create the SAP HANA MDC database](fiori-ios-scpms-geolocation-02.png)

The database has been created if you see the status **Started** in the **Overview** page for the database:

![Create the SAP HANA MDC database](fiori-ios-scpms-geolocation-03.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Log on to the SAP HANA Cockpit)]

Once the database has been created, click on the **SAP HANA Cockpit** link in the `sapgeo` **Overview** page.

At the logon screen, enter `SYSTEM` and the password you provided in **Step 1**:

![Log on to the SAP HANA Cockpit](fiori-ios-scpms-geolocation-04.png)

After you have clicked the **Log On** button, you will see the following warning:

![Log on to the SAP HANA Cockpit](fiori-ios-scpms-geolocation-05.png)

Click **OK**, and you will now see the **SAP HANA Database Administration** overview page:

![Log on to the SAP HANA Cockpit](fiori-ios-scpms-geolocation-06.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Log on to the SAP HANA Web-based Development Workbench)]

Go back to the `sapgeo` **Overview** page and click the **SAP HANA Web-based Development Workbench** link. If being asked your login credentials, provide the same **SYSTEM** user credentials you used to accessing the SAP HANA Cockpit.

Once logged in, you should see the workbench' landing page:

![Log on to the SAP HANA Web-based Development Workbench](fiori-ios-scpms-geolocation-07.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Assign administration and development roles to SYSTEM user)]

To allow creation and administration of the database and service, you need to assign the correct roles to the user.

> For the simplicity of this tutorial, you will use the `sapgeo` **SYSTEM** user to create and maintain the database. In a real-world environment, however, you would *never* use the SYSTEM user but use a dedicated user.

.

Click on the **Security** tile of the **SAP HANA Web-based Development Workbench** landing page.

In the left pane, navigate to the **SYSTEM** user:

![Assign administration and development roles to SYSTEM user](fiori-ios-scpms-geolocation-08.png)

In the **Granted Roles** tab, click the **Add** button. Add the following roles:

* `sap.hana.xs.ide.roles::Developer`
* `sap.hana.xs.debugger::Debugger`
* `sap.hana.xs.admin.roles::HTTPDestViewer`
* `sap.hana.xs.admin.roles::HTTPDestAdministrator`
* `sap.hana.xs.admin.roles::TrustStoreViewer`
* `sap.hana.xs.admin.roles::TrustStoreAdministrator`

![Assign administration and development roles to SYSTEM user](fiori-ios-scpms-geolocation-09.png)

Click **OK** once done. The roles are now assigned:

![Assign administration and development roles to SYSTEM user](fiori-ios-scpms-geolocation-10.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create database schema and tables)]

Click on the **Catalog** tile of the **SAP HANA Web-based Development Workbench** landing page. After you have provided the **SYSTEM** user credentials, the catalog workbench opens:

![Create database schema](fiori-ios-scpms-geolocation-11.png)

Click the **Open SQL Console** button in the top toolbar, and in the editor, enter the following command:

```sql
CREATE SCHEMA "SAPGEO";
```

Click the **Run** button (or press **F8**) to execute the command. The console in the bottom pane should indicate a successful execution and the newly added schema should be listed in the left pane:

![Create database schema](fiori-ios-scpms-geolocation-12.png)

Remove the SQL statement from the SQL Console, and replace it with the following:

```SQL
CREATE COLUMN TABLE "SAPGEO"."GeoLocation" (
  "ID" VARCHAR(36) NOT NULL ,
  "Title" NVARCHAR(32),
  "Description" NVARCHAR(256),
  "Latitude" DOUBLE,
  "Longitude" DOUBLE,
  "Radius" DOUBLE,
  PRIMARY KEY ("ID")
);

```

Click the **Run** button (or press **F8**) to execute the command. The console in the bottom pane should indicate a successful execution and you should see the newly added table under the `SAPGEO` schema.

The just created table is used to store the geofence data you will use in the mobile app.

> **NOTE 1**: Ultimately, geofences can be any two-dimensional polygon. However, since iOS by default only supports circular regions and it takes quite some extra coding to support polygon regions as well as a more complicated database structure to store this polygon data (see also **Note 2** below), this tutorial will use the simple circular region.

> **NOTE 2:** Instead of using separate `latitude` and `longitude` columns, SAP HANA supports geospatial columns of type `ST_POINT` and `ST_POLYGON`. These columns, however, are stored in a binary format, and are currently not supported by OData version 2 which you will be using for this tutorial (there is limited supported in OData version 4). For simplicity of the service and not doing any conversions, a simple table with separate `latitude`, `longitude` and `radius` columns is used instead to store the geofence properties, but this could be easily adapted for use with a `ST_POINT` or `ST_POLYGON` column.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add location data to the database)]

For this tutorial, it is assumed the geofence data is already stored in the database. In a real-world scenario, geofence data can be provided in many ways. For convenience of not having to run around with your mobile device to manually submit geofences to the database, but also to simplify the mobile app coding, you will just run a few SQL INSERT statements with pre-set data.

The easiest way to retrieve longitude and latitude data from a map would be to use Google Maps.

Click on an area on the map (preferably a location near or at your current location) and copy the coordinates displayed at the bottom of the screen:

![Add location data to the database](fiori-ios-scpms-geolocation-20.png)

In the SQL Console, add the following statement:

```SQL
INSERT INTO "SAPGEO"."GeoLocation"
VALUES('<some ID>', 'SAP SE WDF01', 'The Mothership', 49.293406, 8.641362, 200);
```

Using this example, this will create a geofence region at SAP's main building, with a radius of 200 meters.

Select some more points nearby, and create an additional 3 to 4 records with these coordinates.

> For best results, make sure the circular regions do not overlap and do have some significant radius (at least 10 meters or more).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create XS OData service)]

Click on the **Editor** tile of the **SAP HANA Web-based Development Workbench** landing page. After you have provided the **SYSTEM** user credentials, the editor workbench opens:

![Create XS OData service](fiori-ios-scpms-geolocation-13.png)

**Right-click** the **Content** node, and from the context menu, select **New > Package**. In the dialog, enter the following details:

| Field Name | Value |
|----|----|
| Package name | `sapgeo` |

![Create XS OData service](fiori-ios-scpms-geolocation-14.png)

Click **Create** when finished. Select the new `sapgeo` package, and from the toolbar click the **Menu** button and select **File > Create Application**. In the dialog that appears, specify the following:

| Field Name | Value |
|----|----|
| Template | `Empty application (with XSAccess and XSApp)` |
| Package | `sapgeo` |

![Create XS OData service](fiori-ios-scpms-geolocation-15.png)

Click **Create** when done. The `sapgeo` package now expands and should contain the files `.xsaccess`, `.xsapp` and `index.html`.

To allow execution of the OData service you will create later on, you first set the privilege to do so. **Right-click** the `sapgeo` package, and from the context menu, select **New > File**. Specify a file name `.xsprivileges`.

Add the following JSON code to the newly created `.xsprivileges` file:

```JSON
{
    "privileges" : [
        {
            "name" : "Execute",
            "description" : "Execute"
        }
    ]
}
```

Click the **Save** button once done. The console in the bottom pane should indicate a successful save and activation:

![Create XS OData service](fiori-ios-scpms-geolocation-16.png)

Next, you will create the XS OData service. **Right-click** the `sapgeo` package, and from the context menu, select **New > File**. Specify a file name `SAPGeoService.xsodata`:

![Create XS OData service](fiori-ios-scpms-geolocation-17.png)

Add the following code to the newly created `SAPGeoService.xsodata` file:

```
service {
    "SAPGEO"."GeoLocation" as "GeoLocation";
}
```

This exposes the `GeoLocation` table in schema `SAPGEO` as entity set `GeoLocation`. Click the **Save** button once done. The console in the bottom pane should indicate a successful save and activation:

![Create XS OData service](fiori-ios-scpms-geolocation-18.png)

Select the `SAPGeoService.xsodata`, and click the **Run** button from the top toolbar. You should now see the OData service response:

![Create XS OData service](fiori-ios-scpms-geolocation-19.png)

Take a note of the URL, because you will need it later: `https://sapgeo<your account>trial.hanatrial.ondemand.com/sapgeo/SAPGeoService.xsodata`

For fun, try to add `/GeoLocation` at the end of the URL, and you should see the entities you created in **Step 6**

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create mobile application definition)]

Log on to your **SAP Cloud Platform mobile service for development and operations** cockpit, and navigate to **Mobile Applications > Native/Hybrid**. Click the **New** button, and in the dialog, add the following information:

| Field Name | Value |
|----|----|
| Configuration Templates | `Native` |
| ID | `com.sap.tutorials.demoapp.SAPGeo` |
| Name | `SAPGeo` |

![Create mobile application definition](fiori-ios-scpms-geolocation-21.png)

Click **Save** when finished. You should now see the application definition details:

![Create mobile application definition](fiori-ios-scpms-geolocation-22.png)

The **Connectivity** feature is listed as **Incomplete**, because you haven't yet specified which OData service the application will use. Click the **Connectivity** item, and in the following screen, click the **Create Destination** button. In the dialog that appears, enter the following data:

| Field Name | Value |
|----|----|
| Type | `Mobile Destination` |
| Destination Name | `com.sap.tutorials.demoapp.SAPGeo` |

![Create mobile application definition](fiori-ios-scpms-geolocation-23.png)

Click **Next**. In the next page, specify the following data:

| Field Name | Value |
|----|----|
| URL | `<OData URL you noted at the end of Step 7>` |
| Proxy Type | `Internet` |
| Maximum Connections | `10` |
| Timeout | `0` |
| Rewrite Mode | `Rewrite URL` |

![Create mobile application definition](fiori-ios-scpms-geolocation-24.png)

Click **Next**. In the next page, specify the following data:

| Field Name | Value |
|----|----|
| SSO Mechanism | `Basic Authentication` |

![Create mobile application definition](fiori-ios-scpms-geolocation-25.png)

Click **Next**. In the next page, specify the following data:

| Field Name | Value |
|----|----|
| User Name | `SYSTEM` |
| Password | `<SYSTEM user password>` |

> For the simplicity of this tutorial, you will use the `sapgeo` **SYSTEM** user to access the database. In a real-world environment, however, you would *never* use the SYSTEM user but use a dedicated user to access the database.

.

![Create mobile application definition](fiori-ios-scpms-geolocation-26.png)

Click **Next**. In the next page, no changes are required:

![Create mobile application definition](fiori-ios-scpms-geolocation-27.png)

Click **Finish** to complete the wizard. The dialog will close, and the connection is created:

![Create mobile application definition](fiori-ios-scpms-geolocation-28.png)

Click the **Ping** button next to the destination to check whether the OData service is accessible from the destination.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create Xcode project with SDK Assistant)]

On your local machine, navigate to your `./<SAP Cloud Platform SDK for iOS>` folder. Double-click the **SAP Cloud Platform SDK for iOS Assistant** icon to start the application.

Click the **Plus** button on the top-right of the SDK Assistant. The first page of the Xcode Project generation wizard lets you define the Project Properties.

Enter the following details:

| Field | Value |
|----|----|
| Product Name | `SAPGeo` |
| Author | `<your name>` |
| Organization Name | `<your company name>` |
| Organization Identifier | `com.sap.tutorials.demoapp` |
| Destination | `<choose a local destination>` |

> Make sure `<Organization Identifier>.<Product Name>` matches the value of `Application ID` you entered in **Step 8**

.

![Create Xcode project with SDK Assistant](fiori-ios-scpms-geolocation-29.png)

Click **Next** to advance to the **SAP Cloud Platform mobile service for development and operations Configuration** step.

In the **SAP Cloud Platform mobile service for development and operations Configuration** page, select the **Use Existing** tab button.

Click the **Select from SAP Cloud Platform mobile service for development and operations** button next to **Application Identifier**.

Select the `com.sap.tutorials.demoapp.SAPGeo` data source and click **OK**.

![Create Xcode project with SDK Assistant](fiori-ios-scpms-geolocation-30.png)

The selected data source is now pre-filled in the SAP Cloud Platform mobile service for development and operations Configuration page.

Click **Next** to advance to the **OData Services** step.

In the **OData Services** page, the primary OData service connection you have specified in the previous wizard step is displayed:

![Create Xcode project with SDK Assistant](fiori-ios-scpms-geolocation-31.png)

Click **Next** to advance to the **Optional Features** step.

In the **Optional Features** page, you have the option to generate a **Master-Detail Application**, enable **logging** and **log uploads**, and enable **remote notifications**.

![Create Xcode project with SDK Assistant](fiori-ios-scpms-geolocation-32.png)

Make sure the checkboxes **Generate Master-Detail Application**, **Enable Logging** and **Enable Log Upload** are selected and click **Finish** to complete the wizard.

After you have clicked **Finish** in the previous step, the SDK Assistant now loads the OData service's metadata. This metadata describes the data model, and can be accessed via `<service URL>$metadata`. For your service, the metadata URL would be `https://sapgeo<your account>trial.hanatrial.ondemand.com/sapgeo/SAPGeoService.xsodata/$metadata`

> If you have followed the tutorial to the letter, you may now get a message the SDK Assistant could not load the metadata. This happens because the application definition created in Step 8 by default is configured with SAML authentication. If you see this warning, simply download the contents of the `https://sapgeo<your account>trial.hanatrial.ondemand.com/sapgeo/SAPGeoService.xsodata/$metadata` locally, and upload it to the SDK Assistant.

.

After the SDK Assistant has finished, **Xcode** will launch and open the just generated `SAPGeo` project.

![Create Xcode project with SDK Assistant](fiori-ios-scpms-geolocation-33.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Test the generated Xcode project)]

In Xcode, assign the appropriate development account to the project's **Team** property in the **General > Signing** panel, and then build and run the app in the simulator.

![Test the generated Xcode project](fiori-ios-scpms-geolocation-34a.png)

Once the app has started, dismiss the push notifications message by clicking **Allow**.

The on-boarding landing page is now displayed:

![Test the generated Xcode project](fiori-ios-scpms-geolocation-34b.png)

Click the blue **Start** button, and in the SAML login screen, provide your SAP Cloud Platform credentials:

![Test the generated Xcode project](fiori-ios-scpms-geolocation-34c.png)

After you click **Log on**, the **Touch ID** screen is displayed:

![Test the generated Xcode project](fiori-ios-scpms-geolocation-34d.png)

If you are running in the simulator, click **Not Now**, and the **Passcode** screen is displayed:

![Test the generated Xcode project](fiori-ios-scpms-geolocation-34e.png)

Enter an 8-digit numerical passcode, click **Next**, and confirm the passcode. Click **Done** when finished, and the single entity collection is now shown:

![Test the generated Xcode project](fiori-ios-scpms-geolocation-35a.png)

Click on the `GeoLocation` list item, and you should see the list of entities you created with the SQL INSERT statements in **Step 6**.

![Test the generated Xcode project](fiori-ios-scpms-geolocation-35b.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Add a map view)]

In this step, you will add a new View Controller which will display a map with the stored geofences. In all fairness, for the geofences to work you don't need to see the geofences in a map at all. For the purpose of the tutorial, having a visual clue of the geofence locations, it should make things a bit more clear.

Open `Main.storyboard`, and from the **Object library**, drag a **View Controller** right next to the **Collections** scene. With the new view controller selected, set its title to `Map View Controller` in the **Attributes inspector**:

![Add a map view](fiori-ios-scpms-geolocation-36.png)

Next, drag a **Map Kit View** from the **Object Library** onto the **Map View Controller**. Resize the map so its borders align with the view's dimensions:

![Add a map view](fiori-ios-scpms-geolocation-37.png)

With the map control still selected, click the little "triangle TIE-fighter" button in the lower right of the storyboard and from the context menu, select **Reset to Suggested Constraints**. This ensures that regardless of the screen dimensions your app will run, the map viewport will have the same dimensions as its parent view controller.

![Add a map view](fiori-ios-scpms-geolocation-38.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Add a custom controller class to the map view)]

In the **Project navigator** pane on the left side of Xcode, right-click the `ViewControllers` group and from the context menu, select **New File...**. In the dialog, select **Cocoa Touch Class**:

![Add a custom controller class to the map view](fiori-ios-scpms-geolocation-39.png)

Click **Next** to continue. In the next page, enter the following details:

| Field | Value |
|----|----|
| Class | `MapViewController` |
| Subclass of | `UIViewController` |

![Add a custom controller class to the map view](fiori-ios-scpms-geolocation-40.png)

Click **Next** to continue. In the next page, make sure the new class is added to the `ViewControllers` group, and click **Create** to finalize the wizard. The new class will now open:

![Add a custom controller class to the map view](fiori-ios-scpms-geolocation-41.png)

Go back to the **Storyboard**, select the **Map View Controller**, and in the **Identity inspector**, assign the newly created class to the view controller:

![Add a custom controller class to the map view](fiori-ios-scpms-geolocation-42.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Enable navigation to the map view)]

You have created the map view as well as a custom implementing class, but there's no navigation path to that view. Since the view is merely intended as feedback, it makes sense to navigate to the map via a toolbar action.

First, you need to enable the toolbar. Select the **Navigation Controller** connected to the **Collections** scene, and from the **Attributes inspector**, tick the checkbox next to **Shows Toolbar**.

![Enable navigation to the map view](fiori-ios-scpms-geolocation-43.png)

Next, drag a **Bar Button Item** onto the toolbar of the **Collections** scene. In the **Attributes inspector**, set the button's title to `Show Map`:

![Enable navigation to the map view](fiori-ios-scpms-geolocation-44.png)

Finally, **Ctrl-drag** from the toolbar button to the **Map View Controller** scene. From the action list, choose the **Show Detail** action segue.

Select the segue, and in the **Attribute inspector**, provide the **Identifier** `showMap`:

![Enable navigation to the map view](fiori-ios-scpms-geolocation-45.png)

If you now build and run the app and click the **Show Map** button in the toolbar, a map zoomed to display the country you're currently in is displayed:

![Enable navigation to the map view](fiori-ios-scpms-geolocation-46.png)

In the next steps, you will implement logic to visually show the geofence data stored in the SAP HANA MDC.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Display your current location on the map)]

With the **Map View Controller** selected in the **Storyboard**, click the **Show Assistant Editor** button. The custom `MapViewController.swift` file you created earlier is now opened.

To create an outlet for the **Map** control, **Ctrl-drag** from the **Map** control to the `MapViewController.swift` file, below the class definition. Name the new outlet `mapView`:

![Display your current location on the map](fiori-ios-scpms-geolocation-47.png)

Click **Connect** once done. Your code should now give an couple of errors. This is because it cannot resolve the `MKMapKit` class for the `mapView` outlet:

![Display your current location on the map](fiori-ios-scpms-geolocation-48.png)

Add the following import statements:

```swift
import MapKit
import CoreLocation
import SAPCommon
```

The `MapKit` import solves the error message, and `CoreLocation` is needed to determine your location, as well as handling the geofences later on in the tutorial. The `SAPCommon` import is used to implement the SDK's `Logger` functionality.

Add the following private stored properties just above the `viewDidLoad()` method:

```swift
private var locationManager = CLLocationManager()
private let logger = Logger.shared(named: "MapViewControllerLogger")

```

Inside the `viewDidLoad()` method, replace the comment with the following code:

```swift
mapView.delegate = self

locationManager.delegate = self
locationManager.requestAlwaysAuthorization()
```

Here you set the view controller as the delegate for both the `mapView` and `locationManager` instances. You also set the required location permissions to **Always**. This is needed because you want the app to monitor geofences also when the app is not running. To allow the user to grant this authorization, open `Info.plist` and add the following two entries:

| Field | Value |
|----|----|
| Key | `Privacy - Location When In Use Usage Description` |
| Value | `SAPGeo requires your location in order to notify you when you enter a geofence` |

| Field | Value |
|----|----|
| Key | `Privacy - Location Always And When In Use Usage Description` |
| Value | `SAPGeo requires your location also when you are not using the app` |

![Display your current location on the map](fiori-ios-scpms-geolocation-49.png)

Now you only need to display your current location on the map, and correct the two errors that are shown in the editor. These errors are because you have set the view controller as a delegate, but you haven't yet implemented the required delegate methods.

At the bottom of the `MapViewController.swift` file, add the following extensions:

```swift
// MARK: - Map View Delegate
extension MapViewController: MKMapViewDelegate {

    func mapView(_ mapView: MKMapView, viewFor annotation: MKAnnotation) -> MKAnnotationView? {
        // TODO: Implement later!
        return nil
    }

    func mapView(_ mapView: MKMapView, rendererFor overlay: MKOverlay) -> MKOverlayRenderer {
        // TODO: Implement later!
        return MKOverlayRenderer(overlay: overlay)
    }

}

// MARK: - Location Manager Delegate
extension MapViewController: CLLocationManagerDelegate {

    // Allow access to your current location if the "always" authorization is set (see r:25)
    func locationManager(_ manager: CLLocationManager, didChangeAuthorization status: CLAuthorizationStatus) {
        mapView.showsUserLocation = status == .authorizedAlways
    }

    func locationManager(_ manager: CLLocationManager, monitoringDidFailFor region: CLRegion?, withError error: Error) {
        logger.error("Monitoring did fail for region: \(region!.identifier)")
    }

    func locationManager(_ manager: CLLocationManager, didFailWithError error: Error) {
        logger.error("Location Manager did fail with error: \(error)")
    }

}
```

The delegate methods for the `mapView` instance will be implemented later, and will eventually display a pin and overlay for the geofences onto the map. The delegate method for the `locationManager` instance enables the `mapView` instance to display your current location. If you now build and run the app and navigate to the map, you should first grant access for the app to always use your location:

![Display your current location on the map](fiori-ios-scpms-geolocation-50.png)

If you run the app from the simulator, click the **Simulate Location** button and select a location nearest to you:

![Display your current location on the map](fiori-ios-scpms-geolocation-52.png)

On the map, scroll to the selected location, and you should now see a blue dot with your simulated location:

![Display your current location on the map](fiori-ios-scpms-geolocation-51.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Display the stored geofences on the map)]

In this step, you will display the geofences stored in the SAP HANA MDC onto the map.

The OData service returns instances of `GeoLocation`. While this is perfectly fine, it is convenient to translate these into objects which are easier to handle for both the map as well as the location manager. For both offline storage as well as using the object as an annotation on the map, the class should implement both `NSCoding` as well as `MKAnnotation`.

**Right-click** the `Model` group in the **Project navigator**, and select **New File...**. Add a new **Swift File** and name it `SAPGeoLocation`. An empty file is created:

![Display the stored geofences on the map](fiori-ios-scpms-geolocation-53.png)

Replace the content of the file with the following code:

```swift

import MapKit
import CoreLocation

class SAPGeoLocation: NSObject, NSCoding, MKAnnotation {
    let identifier: String?
    let title: String?
    let subtitle: String?
    let coordinate: CLLocationCoordinate2D
    let radius: Double

    init(geoLocationType: GeoLocationType) {
        self.identifier = geoLocationType.id
        self.title      = geoLocationType.title
        self.subtitle   = geoLocationType.description
        self.coordinate = CLLocationCoordinate2D(latitude: geoLocationType.latitude!, longitude: geoLocationType.longitude!)
        self.radius     = geoLocationType.radius!
    }

    required init?(coder aDecoder: NSCoder) {
        identifier    = aDecoder.decodeObject(forKey: SAPGeoLocationKey.identifier) as? String
        title         = aDecoder.decodeObject(forKey: SAPGeoLocationKey.title) as? String
        subtitle      = aDecoder.decodeObject(forKey: SAPGeoLocationKey.subtitle) as? String
        let latitude  = aDecoder.decodeDouble(forKey: SAPGeoLocationKey.latitude)
        let longitude = aDecoder.decodeDouble(forKey: SAPGeoLocationKey.longitude)
        coordinate    = CLLocationCoordinate2D(latitude: latitude, longitude: longitude)
        radius        = aDecoder.decodeDouble(forKey: SAPGeoLocationKey.radius)
    }

    func encode(with aCoder: NSCoder) {
        aCoder.encode(identifier, forKey: SAPGeoLocationKey.identifier)
        aCoder.encode(title, forKey: SAPGeoLocationKey.title)
        aCoder.encode(subtitle, forKey: SAPGeoLocationKey.subtitle)
        aCoder.encode(coordinate.latitude, forKey: SAPGeoLocationKey.latitude)
        aCoder.encode(coordinate.longitude, forKey: SAPGeoLocationKey.longitude)
        aCoder.encode(radius, forKey: SAPGeoLocationKey.radius)
    }
}

struct SAPGeoLocationKey {
    static let identifier = "identifier"
    static let title      = "title"
    static let subtitle   = "subtitle"
    static let latitude   = "latitude"
    static let longitude  = "longitude"
    static let radius     = "radius"
}
```

The constructor takes the OData service's `GeoLocationType` instance as input, and creates a `SAPGeoLocation` which implements both `NSCoding` and `MKAnnotation`. The `required init?` and `encode` methods implement the `NSCoding`'s required decode and encode functionality, respectively. The structure is for convenience and contains the property names as strings.

Switch back to the `MapViewController.swift` file, and add the following private method to the `MapViewController` class:

```swift
/**
 Converts array of `GeoLocationType` objects to array of `SAPGeoLocation` objects, for convenience.
 - Parameters:
   - locations: Array of `GeoLocationType` entities
 - Returns: Array of `SAPGeoLocation` objects
 */
private func getArrayOfSAPGeoLocationsFromEntities(locations: [GeoLocationType]) -> [SAPGeoLocation] {
    var sapGeoLocations: [SAPGeoLocation] = []
    for location in locations {
        let sapGeoLocation = SAPGeoLocation(geoLocationType: location)
        sapGeoLocations.append(sapGeoLocation)
    }
    return sapGeoLocations
}

```

This method takes an array of `GeoLocationType` objects returned from the OData service, and returns an array of `SAPGeoLocation` objects.

Add the following private method to the `MapViewController` class:

```swift
/**
 Renders all geolocations on the map
 - Parameters:
   - locations: Array of `SAPGeoLocation` entities
 */
private func renderLocationsOnMap(locations: [SAPGeoLocation]) {
    for location in locations {
        mapView.addAnnotation(location)
        mapView.add(MKCircle(center: location.coordinate, radius: location.radius))

        // Uncomment line below later in the tutorial
        // registerGeofence(location: location)
    }
}
```

This method takes the array of `SAPGeoLocation` objects, adds an annotation to the map, and in addition, adds a circle at the given coordinates and radius. For both these calls, the respective delegate methods of the `mapView` instance are called, but they are not yet implemented. Find the `MKMapViewDelegate` extension and replace both delegate methods with the following two methods:

```swift
func mapView(_ mapView: MKMapView, viewFor annotation: MKAnnotation) -> MKAnnotationView? {
    if let annotation = annotation as? SAPGeoLocation {
        let identifier = "pin"
        var view: MKPinAnnotationView
        if let dequeuedView = mapView.dequeueReusableAnnotationView(withIdentifier: identifier)
            as? MKPinAnnotationView {
            dequeuedView.annotation = annotation
            view = dequeuedView
        } else {
            view = MKPinAnnotationView(annotation: annotation, reuseIdentifier: identifier)
            view.canShowCallout = true
            view.calloutOffset = CGPoint(x: -5, y: 5)
            view.rightCalloutAccessoryView = UIButton(type: .detailDisclosure) as UIView
        }
        view.pinTintColor = UIColor.preferredFioriColor(forStyle: .tintColorDark)

        return view
    }
    return nil
}

func mapView(_ mapView: MKMapView, rendererFor overlay: MKOverlay) -> MKOverlayRenderer {
    if overlay is MKCircle {
        let circleRenderer = MKCircleRenderer(overlay: overlay)
        circleRenderer.lineWidth = 1.0
        circleRenderer.strokeColor = UIColor.preferredFioriColor(forStyle: .tintColorDark)
        circleRenderer.fillColor = UIColor.preferredFioriColor(forStyle: .tintColorLight).withAlphaComponent(0.4)
        return circleRenderer
    }
    return MKOverlayRenderer(overlay: overlay)
}
```

The first delegate method is called when the map instance's `addAnnotation` method is called. It adds an `MKPinAnnotationView` instance with a pin in one of the standard SAP Fiori colors. The second delegate method checks whether the object being added is of type `MKCircle`, and adds it as an overlay on the map, again with one of the standard SAP Fiori colors.

The one thing missing is to actually load the stored `GeoLocationType` objects, and call the methods to plot them on the map.

Just below the stored property `logger`, add the following stored property referencing the applications `AppDelegate`:

```swift
private let appDelegate = UIApplication.shared.delegate as! AppDelegate
```

You will use the `appDelegate` instance to get a reference to the OData service. Add the following method:

```swift
/**
 Loads all geolocations from OData service on SAP Cloud Platform
 */
func loadLocations() {
    appDelegate.sapGeoService.fetchGeoLocation() { (geolocations, error) in
        guard let geolocations = geolocations else {
            return
        }

        let locations = self.getArrayOfSAPGeoLocationsFromEntities(locations: geolocations)
        // Uncomment line below later in the tutorial
        // self.storeLocationsToUserDefaults(locations: locations)
        self.renderLocationsOnMap(locations: locations)
    }
}
```

This method loads the actual `GeoLocationType` entities from the OData service, converts the resulting array to an array of `SAPGeoLocation` objects, which is then provided to the previously created `renderLocationsOnMap(locations:)` method. Call this `loadLocations()` function at the end of the `viewDidLoad()` method so it resembles this:

```swift
override func viewDidLoad() {
    super.viewDidLoad()

    mapView.delegate = self

    locationManager.delegate = self
    locationManager.requestAlwaysAuthorization()

    loadLocations()
}
```

If you now run the app, you should see one or more pins marking your stored geofences. If you click on it, it shows the call-out with the geofence title and subtitle, as well as a detail disclosure indicator as specified in the `mapView(_ mapView: MKMapView, viewFor annotation: MKAnnotation)` delegate method:

![Display the stored geofences on the map](fiori-ios-scpms-geolocation-54.png)

Zoom in (with the simulator, use **Alt-Click** for two-finger pinch) and you should now see the circular representation of the geofences too:

![Display the stored geofences on the map](fiori-ios-scpms-geolocation-55.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Add map zoom button)]

Depending on how far apart you have specified your geofences, you might need to zoom in significantly to distinguish the various geofences you have defined in the database. In this step, you will add a toolbar button which will zoom in to the selected geofence.

Open the **Storyboard** and drag a **Bar Button Item** onto the **Map View Controller**'s toolbar. In the **Attribute inspector**, set the title to `Zoom to geofence`:

![Add map zoom button](fiori-ios-scpms-geolocation-56.png)

Open the **Assistant editor**, and **Ctrl-drag** the newly added toolbar button to the `MapViewController` class, just below the `mapView` outlet.

Specify the following parameters:

| Field | Value |
|----|----|
| Connection | `Action` |
| Name | `zoomToGeofence` |

![Add map zoom button](fiori-ios-scpms-geolocation-57.png)

Implement the newly added action so it resembles the following:

```swift
@IBAction func zoomToLocation(_ sender: Any) {
    if mapView.selectedAnnotations.count > 0 {
        let selected = mapView.selectedAnnotations[0]
        let region = MKCoordinateRegionMakeWithDistance(selected.coordinate, 250, 250)
        mapView.setRegion(region, animated: true)
    }
}
```

This method checks if a geofence is selected on the map, and then sets the map viewport to center at the geofence coordinates, and span the north-to-south distance as well as the east-to-west distance to approximately 250 meters.

Run the app, select a pin on the map, and click the **Zoom to geofence** button. You will now zoom in on the map with the selected geofence in the center:

![Add map zoom button](fiori-ios-scpms-geolocation-58.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Store geofences for offline usage)]

Until now, you can only display the stored geofences. In this step, you will store the geofences and register them for monitoring. After that, you will enhance the app to react on entering a geofence.

So first, you need to store the geofences for offline usage. You have a couple of possibilities here:

* [`Offline OData`](https://help.sap.com/doc/978e4f6c968c4cc5a30f9d324aa4b1d7/Latest/en-US/Documents/Frameworks/SAPODataOffline/index.html)
* [`SecureKeyValueStore`](https://help.sap.com/doc/978e4f6c968c4cc5a30f9d324aa4b1d7/Latest/en-US/Documents/Frameworks/SAPFoundation/Classes/SecureKeyValueStore.html)
* [`NSUserDefaults`](https://developer.apple.com/documentation/foundation/userdefaults)

Offline OData could work just perfectly here, but to convert from Online OData to Offline OData takes a couple of extra steps which would increase the complexity of this tutorial.

Storing it in the SDK's `SecureKeyValueStore` is also a possibility, but since it only accepts single `NSCoding` objects, adding an array of objects for a single key is not possible without extra coding.

So for least complexity, in this tutorial the geofences are stored in the `UserDefaults` database, which makes them persistent even when the app is not running.

Add the following method to the `MapViewController` class:

```swift
/**
 Add locations to `UserDefaults` for offline access
 - Parameters:
   - locations: Array of `SAPGeoLocation` entities
 */
private func storeLocationsToUserDefaults(locations: [SAPGeoLocation]) {
    var listSAPGeoLocations: [Data] = []
    for item in locations {
        let sapGeoLocation = NSKeyedArchiver.archivedData(withRootObject: item)
        listSAPGeoLocations.append(sapGeoLocation)
    }

    UserDefaults.standard.set(listSAPGeoLocations, forKey: "geofences")
}
```

This method stores the whole array of `SAPGeoLocation` objects into a single `UserDefaults` key.

Locate the `loadLocations()` function, and uncomment the commented out line, so it now calls the newly added `storeLocationsToUserDefaults()` method.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 18: ](Register geofences for monitoring)]

Add the following two methods to the `MapViewController` class:

```swift
/**
 Registers a region to location manager and start monitoring for crossing the geofence
 - Parameters:
   - location: The `SAPGeoLocation` object which will be registered as a geofence
 */
private func registerGeofence(location: SAPGeoLocation) {
    let region = getRegionForLocation(location: location)

    locationManager.startMonitoring(for: region)
}

/**
 Returns a circular geofence region
 - Parameters:
   - location: The `SAPGeoLocation` object which will be used to define the geofence
 - Returns: Instance of `CLCircularRegion`
 */
private func getRegionForLocation(location: SAPGeoLocation) -> CLCircularRegion {
    let region = CLCircularRegion(center: location.coordinate, radius: location.radius, identifier: location.identifier!)
    region.notifyOnEntry = true
    region.notifyOnExit = false
    return region
}
```

Method `getRegionForLocation(location:)` takes an `SAPGeoLocation` instance as input, and creates a `CLCircularRegion` instance off it. A `CLCircularRegion` is a circular region defining the actual geofence at the specified location. The geofence is set up so the location manager gets notified only when you enter the geofence.

> You could extend the `GeoLocation` database table to have extra columns for notifications upon entry and exit, making this a dynamic instead of a fixed setting.

.

Method `registerGeofence(location:)` then instructs the location manager to start monitoring the supplied `SAPGeoLocation` geofence.

Finally, locate the `renderLocationsOnMap(locations:)` function, and uncomment the commented out line, so it now calls the newly added `registerGeofence(location:)` method.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 19: ](Detect geofence events)]

As stated before, you want to detect geofence events even when the app is inactive, not running or offline. The way the location manager works is, if your device detects a geofence event, it will launch the app in the background. Acting on geofence events is then best done in the app's `AppDelegate` class.

Open `AppDelegate.swift` and import `CoreLocation`:

```swift
import CoreLocation
```

Then, add the following stored property:

```swift
let locationManager = CLLocationManager()
```

Locate method `applicationDidFinishLaunching(_:)` and below the line `UINavigationBar.applyFioriStyle()` add the following:

```swift
locationManager.delegate = self
locationManager.requestAlwaysAuthorization()
```

The editor should now indicate an error, since you haven't yet implemented the required `CLLocationManager` delegate methods.

At the bottom of the `AppDelegate.swift` file, add the following extension:

```swift
extension AppDelegate: CLLocationManagerDelegate {

    func locationManager(_ manager: CLLocationManager, didEnterRegion region: CLRegion) {
        if region is CLCircularRegion {
            handleEvent(forRegion: region, didEnter: true)
        }
    }

    func locationManager(_ manager: CLLocationManager, didExitRegion region: CLRegion) {
        if region is CLCircularRegion {
            handleEvent(forRegion: region, didEnter: false)
        }
    }
}
```

The editor should now complain about the missing `handleEvent(forRegion:didEnter:)` method.

Add the following two methods:

```swift
/**
 Processes the geofence event received from one of the `CLLocationManagerDelegate` delegate methods `locationManager(_:didEnterRegion:)` or `locationManager(_:didExitRegion:)`
 If the app is running in the foreground, it will show an alert.
 If the app is running in the background, it will show a local notification
 - Parameters:
   - region: The `CLRegion` instance which has been detected
   - didEnter: `true` if the geofence has been entered, `false` if the geofence has been exited
 */
func handleEvent(forRegion region: CLRegion!, didEnter: Bool) {

    let geoLocation = self.getGeoLocation(fromRegionIdentifier: region.identifier)

    if geoLocation != nil {
        let message = geoLocation?.title ?? "Unknown title"

        logger.debug("\(didEnter ? "Entered" : "Exited") geofence: \(message)")

        if UIApplication.shared.applicationState == .active {
            let view = window?.rootViewController
            let alert = UIAlertController(title: "Geofence crossed", message: message, preferredStyle: .alert)
            let action = UIAlertAction(title: "OK", style: .cancel, handler: nil)
            alert.addAction(action)
            view?.present(alert, animated: true, completion: nil)
        } else {
            let content = UNMutableNotificationContent()
            content.title = "Geofence crossed"
            content.body = message
            content.sound = UNNotificationSound.default()

            let notificationTrigger = UNTimeIntervalNotificationTrigger(timeInterval: 1, repeats: false)
            let request = UNNotificationRequest(identifier: "notification1", content: content, trigger: notificationTrigger)

            UNUserNotificationCenter.current().add(request, withCompletionHandler: nil)
        }
    }
}

/**
 Retrieves an instance of `SAPGeoLocation` from the array stored in `UserDefaults` based on the `identifier` provided
 - Parameters:
   - identifier: The id of the geofence
 - Returns: Instance of `SAPGeoLocation` or `nil` if the geofence could not be found
 */
func getGeoLocation(fromRegionIdentifier identifier: String) -> SAPGeoLocation? {
    let storedLocations = UserDefaults.standard.array(forKey: "geofences") as? [NSData]
    let sapGeoLocations = storedLocations?.map { NSKeyedUnarchiver.unarchiveObject(with: $0 as Data) as? SAPGeoLocation }
    let index = sapGeoLocations?.index { $0?.identifier == identifier }
    return index != nil ? sapGeoLocations?[index!] : nil
}
```

Method `getGeoLocation(fromRegionIdentifier:)` retrieves an instance of `SAPGeoLocation` which has been stored in `UserDetails`. Method `handleEvent(forRegion:didEnter:)` takes the `CLRegion` geofence received from the `CLLocationManagerDelegate` delegate, and displays a notification.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 20: ](Testing your geofences)]

Your app is now ready to test the stored geofences. You could now deploy the app on a physical device and drive around town, but that would be both quite cumbersome as well as impossible to detect any failures or analyze logged messages. You could, however, test geofences using a GPX file.

At the root of your project, add a new **Group** and name it `Test`. **Right-click** the `Test` group and from the context menu, select **New File...**. From the dialog, choose `GPX File`:

![Testing your geofences](fiori-ios-scpms-geolocation-59.png)

Click **Next**. In the next page, name the file `TestLocations` and make sure it sits in the `Test` group:

![Testing your geofences](fiori-ios-scpms-geolocation-60.png)

Click **Create** when done. A new `TestLocations.gpx` file is added to your project:

![Testing your geofences](fiori-ios-scpms-geolocation-61.png)

Add at least two waypoints which will cross one or more geofences:

![Testing your geofences](fiori-ios-scpms-geolocation-62.png)

> If you do an online search for "GPX generator", you will find some tools which allow you to simply click on a map and generate a GPX file with a series of waypoints

.

Now, build and run the app in the simulator. If the app runs, click the **Locations** button in the **Debug** pane and select `TestLocations`:

![Testing your geofences](fiori-ios-scpms-geolocation-63.png)

Navigate to the map. You should now see your simulated location move over the map, based on the waypoints you have defined in the `TestLocations.gpx` file. Even more, if you cross a geofence, it will fire a geofence event, and displays an alert:

![Testing your geofences](fiori-ios-scpms-geolocation-64.png)

Also, if you dismiss the app to the background, you will receive a notification:

![Testing your geofences](fiori-ios-scpms-geolocation-65.png)

You may find the simulator acts quite inaccurate at times when testing geofence events. Build and deploy the app on a physical device and enjoy a greater accuracy!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 21: ](Where to go from here)]

The tutorial ends here. However, you could enhance the app even further.

For instance, you now only receive a notification when crossing a geofence. You could simply create a second database table which stores records for the geofence events with timestamps and user details, and instead of displaying an alert or notification, add a record in that table. Imagine being a truck driver crossing multiple geofences around warehouses. The logistics department would then be notified which driver is in the vicinity of which warehouse.

You could also use Offline OData for storing geofence data, which may give you different kinds of possibilities.

Or use the event to trigger a separate REST service on SAP Cloud Platform which sends a signal to an IoT device, for instance a connected gate or garage door... The geofencing possibilities are endless!

[DONE]
[ACCORDION-END]


## Next Steps
 - [View all How-Tos](http://www.sap.com/developer/tutorial-navigator.how-to.html)
