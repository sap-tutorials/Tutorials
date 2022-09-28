---
title: Extend Your MDK App With a Map Custom Control (Using Metadata Approach)
description: Build and run the Mobile Development Kit client with Map custom control functionality for Android and iOS platforms.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>advanced, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services, software-product>sap-business-application-studio ]
time: 35
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial**: [Set Up for the Mobile Development Kit (MDK)](group.mobile-dev-kit-setup)
- **Download and install:** **SAP Mobile Services Client** on your [iOS](https://apps.apple.com/us/app/sap-mobile-services-client/id1413653544) device (If you are connecting to `AliCloud` accounts then you will need to brand your [custom MDK client](cp-mobile-dev-kit-build-client) by allowing custom domains.)
- **Download the latest version of Mobile Development Kit SDK** either from community [download page](https://developers.sap.com/trials-downloads.html?search=Mobile%20development%20kit) or [SAP Software Center](https://launchpad.support.sap.com/#/softwarecenter/search/Mobile%2520development%2520kit) if you are a SAP Mobile Services customer

## Details
### You will learn
  - How to register and consume an Extension control in MDK Metadata
  - How to build a Mobile development kit client for iOS and Android
  - How to connect to SAP Mobile application

You may clone an existing metadata project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/6-Create-Extension-Controls-in-Mobile-Development-Kit-Apps/3-Extend-Your-MDK-App-With-Map-Custom-Control-using-Metadata-approach) and start directly with step 5 in this tutorial.

---

To extend the functionality, or customise the look and feel, and behaviour of your client app, you can create extension controls other than the already existing MDK built-in controls using `NativeScript` (TypeScript/JavaScript applicable for both Android and iOS)

In this tutorial, you will create a Map extension via `NativeScript` (in TypeScript language), you will view the Map in Apple Maps on iOS devices and in Google Maps on Android devices.

![MDK](img-1.gif)

[ACCORDION-BEGIN [Step 1: ](Create a new MDK project in SAP Business Application Studio)]

This step includes creating the mobile development kit project in the editor.

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Click **Start from template** on Welcome page.

    !![MDK](img-1.2.png)

    >If you do not see the Welcome page, you can access it via **Help** menu or via **View** menu > Find Command > Welcome.

3. Select **MDK Project** and click **Next**.

    !![MDK](img-1.3.png)

    >If you do not see the **MDK Project** option check if your Dev Space has finished loading or reload the page in your browser and try again.

4. In *Basic Information* step, provide the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `MDK Template Type`| Select `List Detail` from the dropdown |
    | `Your Project Name` | Provide a name of your choice. `MDK_Maps` is used for this tutorial |
    | `Your Application Name` | <default name is same as project name, you can provide any name of your choice> |    
    | `Target MDK Client Version` | Leave the default selection as `MDK 6.0+ (For use with MDK 6.0 or later clients)` |  
    | `Choose a target folder` | By default, the target folder uses project root path. However, you can choose a different folder path |  

    !![MDK](img-1.4.png)

    >This screen will only show up when your CF login session has expired. Enter your login credentials, click Login icon and select the org & space where you have set up the initial configuration for your MDK app.

    >!![MDK](img-1.4.1.png)


5. In *Service configuration* step, provide the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `Data Source` | Select `Mobile Services` from the dropdown |
    | `Mobile Services Landscape` | Select `standard` from the dropdown |
    | `Application Id` | Select `com.sap.mdk.demo` from the dropdown |
    | `Destination` | Select `SampleServiceV2` from the dropdown |
    | `Enter a path to the OData service` | Leave it as it is |
    | `Enable Offline` | Choose `No` |  

    !![MDK](img-1.5.png)

    >Regardless of whether you are creating an online or offline application, this step is needed app to connect to an OData service. When building an Mobile Development Kit application, it assumes the OData service created and the destination that points to this service is set up in Mobile Services and SAP Business Technology Platform.

    Since you will create an online based app, hence _Enable Offline Store_ option is unchecked.

6. In *Data Collections* step, select `Customers` (if not selected by default). Click **Finish** to finish the project creation.

    !![MDK](img-1.6.png)    

7. After clicking **Finish**, the wizard will generate your MDK Application based on your selections. You should now see the `MDK_Maps` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Register an Extension Control)]

The extension control that you will be creating to extend the functionality of your app can be used as base controls by registering it using the MDK editor.

1. Download [this](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Images/map.png) image and save it locally. This image will be used as a display image on the page editor to represent the extension control.

2. Drag & drop `map.png` file on **Images** folders.

    !![MDK](img_2.2.png)

3. Right-click **Extensions** | select **MDK: Register Extension Control**.

    !![MDK](img-2.3.png)

4. In `Template Selection` step, select **New and register Metadata Extension Control**. Click **Next**.

    !![MDK](img_2.4.png)

5. Provide the below information:

    | Field | Value |
    |----|----|
    | `Control Name`| `mdk_maps` |
    | `Module` | `MyMapModule` |
    | `Control` | `MyMapExtension` |
    | `Class` | `MyMapClass` |
    | `Display` | bind it to `map.png` file  |

    Here is the basic definition for properties you defined above:

    **Module**: It is used to identify the extension control.
      The path to the extension module under `<MetadataProject>/Extensions/`.

    **Control**: The name of the file under the `<MetadataProject>/Extensions/<Module>/controls` that contains the extension class. If not specified, module name would be used as the value for this property.

    **Class**: The class name of your custom extension class. The client will check for this class at runtime and if it's found, your extension will be instantiated. Otherwise, a stub with an error message will be shown.

    **Display**: This property is used for the image to be displayed on the page editor to represent the extension control.

    !![MDK](img_2.5.png)

6. In **Schema Information** step, fill schema details in **Schema** column and click **Next**.

    ```JSON
    {
    	"type": "object",
    	"BindType": "",
    	"properties": {
    		"Prop": {
    			"type": "object",
    			"BindType": "",
    			"properties": {
    				"City": {
    					"type": "string",
    					"BindType": ""
    				},
    				"Country": {
    					"type": "string",
    					"BindType": ""
    				},
    				"HouseNumber": {
    					"type": "string",
    					"BindType": ""
    				},
    				"LastName": {
    					"type": "string",
    					"BindType": ""
    				},
    				"PostalCode": {
    					"type": "string",
    					"BindType": ""
    				},
    				"Street": {
    					"type": "string",
    					"BindType": ""
    				}
    			}
    		}
    	}
    }
    ```

    !![MDK](img-2.6.png)

    >Above schema will add these predefined properties (`City`, `Country`, `HouseNumber`, `LastName`, `PostalCode`, `Street`) in the map extension control which you will bind to **Customer** entity properties in next step.

7. Click **Finish** to confirm.

    Some additional files and folders are added to the **Extensions** folder. You will learn more about it in following steps.

    !![MDK](img_2.7.png)

    >You can find more details about registering extension control in [this](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/advanced/extensions/registering-extension-in-bas.html) guide.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Consume Extension Control in MDK Metadata)]

You will add this registered control in the generated `Customers_Detail.page`.

  1. Navigate to **Pages** folder | **Customers** | `Customers_Detail.page`.

  2. Remove the body section of the page.

    !![MDK](img-3.2.gif)

  3. Expand **Section Registered Extension Control**, drag & drop the registered `mdk_maps` control on the page area.

    !![MDK](img_3.3.png)

    >You can find more details about the **Section Extension** in [this](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/SectionedTable/Container/Extension.schema.html) guide.

  4. In the **Properties** section, set the **Height** to 600.

    !![MDK](img-3.4.png)  

  5. Bind the registered Extension control properties to **Customers** properties.

    Under **Extension Properties** section, expand `Prop`, click the **link** icon to open the Object Browser for the **City** property. Double click the **City** property of the **Customer** entity to set it as the binding expression and click **OK**.

    !![MDK](img-3.5.gif)

    Repeat the above step and bind other properties.

    !![MDK](img-3.5.png)

    >Be careful not to bind properties from Address (ESPM.Address).


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Implement Extension using metadata approach)]

1. Navigate to **Extensions** | `MyMapModule` | `controls` | `MyMapExtension.ts`, replace the code with the following.

    ```JavaScript / TypeScript
    import * as app from '@nativescript/core/application';
    import { IControl } from 'mdk-core/controls/IControl';
    import { BaseObservable } from 'mdk-core/observables/BaseObservable';
    import { EventHandler } from 'mdk-core/EventHandler'

    export class MyMapClass extends IControl {
        private _observable: BaseObservable;
        private _mapView: any;
        private _geo: any;
        private _gMap: any;
        private _marker: any;
        private _customerInfo = {
            lastName: "",
            houseNumber: "",
            street: "",
            city: "",
            country: "",
            postalCode: "",
            latitiude: "",
            longitude: ""
        }

        public initialize(props: any): any {
            super.initialize(props);

            //Access the properties passed from Customers_Detail.page to the extension control.
            //in this tutorial, you will be accessing the customer's last name and address
            if (this.definition().data.ExtensionProperties.Prop) {
                var property = this.definition().data.ExtensionProperties.Prop;
                this._customerInfo.lastName = property.LastName;
                this._customerInfo.houseNumber = property.HouseNumber;
                this._customerInfo.street = property.Street;
                this._customerInfo.city = property.City;
                this._customerInfo.country = property.Country;
                this._customerInfo.postalCode = property.PostalCode;
            }

            if (app.android) {
                //You will display the Google Maps in a MapView.For more details on Google Maps API for android, visit
                //https://developers.google.com/android/reference/com/google/android/gms/maps/package-summary

                this._mapView = new com.google.android.gms.maps.MapView(this.androidContext());
                var localeLanguage = java.util.Locale;

                //GeoCoder is required to convert a location to get latitude and longitude
                this._geo = new android.location.Geocoder(this.androidContext(), localeLanguage.ENGLISH);
                this._mapView.onCreate(null);
                this._mapView.onResume();

                //when mapview control is used, all the lifecycle activities has to be frowaded to below methods.
                app.android.on(app.AndroidApplication.activityPausedEvent, this.onActivityPaused, this);
                app.android.on(app.AndroidApplication.activityResumedEvent, this.onActivityResumed, this);
                app.android.on(app.AndroidApplication.saveActivityStateEvent, this.onActivitySaveInstanceState, this);
                app.android.on(app.AndroidApplication.activityDestroyedEvent, this.onActivityDestroyed, this);
                var that = this;

                //A GoogleMap must be acquired using getMapAsync(OnMapReadyCallback).
                //The MapView automatically initializes the maps system and the view

                var mapReadyCallBack = new com.google.android.gms.maps.OnMapReadyCallback({
                    onMapReady: (gMap) => {
                        console.log("inside onMapReady function");
                        that._gMap = gMap;
                        var zoomValue = 6.0;
                        that._gMap.setMinZoomPreference = zoomValue;
                        var customerAddress = that._customerInfo.houseNumber + ' ' + that._customerInfo.street + ' ' + that._customerInfo.city + ' ' +
                            that._customerInfo.country + ' ' + that._customerInfo.postalCode;
                        var data = that._geo.getFromLocationName(customerAddress, 1);
                        var latLng = new com.google.android.gms.maps.model.LatLng(data.get(0).getLatitude(), data.get(0).getLongitude());
                        that._gMap.addMarker(new com.google.android.gms.maps.model.MarkerOptions().position(latLng).title(this._customerInfo.lastName +
                            "'s " + "location"));
                        that._gMap.moveCamera(new com.google.android.gms.maps.CameraUpdateFactory.newLatLng(latLng));
                    }
                });
                this._mapView.getMapAsync(mapReadyCallBack);
            }

            if (app.ios) {

                /*initiating Apple Maps
                For more details on the Apple Maps visit
                https://developer.apple.com/documentation/mapkit */
                this._mapView = MKMapView.alloc().initWithFrame(CGRectMake(0, 0, 1000, 1000));
            }
        }

        private onActivityPaused(args) {
            console.log("onActivityPaused()");
            if (!this._mapView || this != args.activity) return;
            this._mapView.onPause();
        }

        private onActivityResumed(args) {
            console.log("onActivityResumed()");
            if (!this._mapView || this != args.activity) return;
            this._mapView.onResume();
        }

        private onActivitySaveInstanceState(args) {
            console.log("onActivitySaveInstanceState()");
            if (!this._mapView || this != args.activity) return;
            this._mapView.onSaveInstanceState(args.bundle);
        }

        private onActivityDestroyed(args) {
            console.log("onActivityDestroyed()");
            if (!this._mapView || this != args.activity) return;
            this._mapView.onDestroy();
        }

        //In case of iOS you'll use CLGeocoder API to convert an address to get latitude and longitude.
        //NOTE - API getlatlang is called only on ios devices

        private getlatlang(customerAddress) {
            const that = this;
            return new Promise((resolve, reject) => {
                var latLng = new CLGeocoder();
                latLng.geocodeAddressStringCompletionHandler(customerAddress, function (placemarks, error) {
                    if (error === null && placemarks && placemarks.count > 0) {
                        var pm = placemarks[0];
                        var cordinates = {
                            latitiude: "",
                            longitude: ""
                        }
                        cordinates.latitiude = pm.location.coordinate.latitude;
                        cordinates.longitude = pm.location.coordinate.longitude;
                        resolve(cordinates);
                    } else {
                        reject();
                    }
                });
            });
        }

        public view() {
            this.valueResolver().resolveValue([this._customerInfo.houseNumber, this._customerInfo.street, this._customerInfo.city, this._customerInfo
                .country, this._customerInfo.postalCode, this._customerInfo.lastName
            ], this.context)
                .then((address) => {

                    this._customerInfo.houseNumber = address[0];
                    this._customerInfo.street = address[1];
                    this._customerInfo.city = address[2];
                    this._customerInfo.country = address[3];
                    this._customerInfo.postalCode = address[4];
                    this._customerInfo.lastName = address[5];

                    var customerAddress = address[0] + ' ' + address[1] + ' ' + address[2] + ' ' + address[3] + ' ' + address[4];
                    console.log("customer's address = " + customerAddress);

                    if (app.ios) {
                        return this.getlatlang(customerAddress)
                            .then((cordinates) => {
                                /* below code is for the apple maps */
                                var latlong = CLLocationCoordinate2DMake(cordinates.latitiude, cordinates.longitude);
                                var annotation = MKPointAnnotation.alloc().init();
                                annotation.coordinate = latlong;
                                annotation.title = this._customerInfo.lastName + "'s" + " location";
                                this._mapView.centerCoordinate = latlong;
                                this._mapView.addAnnotation(annotation);
                            });
                    }
                });

            if (app.android) {
                return this._mapView;
            }
            if (app.ios) {
                return this._mapView;
            }
        }

        public viewIsNative() {
            return true;
        }

        public observable() {
            if (!this._observable) {
                this._observable = new BaseObservable(this, this.definition(), this.page());
            }
            return this._observable;
        }

        public setContainer(container: IControl) {
            // do nothing
        }

        public setValue(value: any, notify: boolean, isTextValue?: boolean): Promise<any> {
            // do nothing
            return Promise.resolve();
        }
    }
    ```
    >In your import function, if you see errors related to `@nativescript/core` or `mdk-core`, you can ignore them. There is currently no reference of such libraries in the MDK editor.

2. Save the `MyMapExtension.ts` file.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy the application)]

So far, you have learned how to build an MDK application in the SAP Business Application Studio editor. Now, you will deploy the application definitions to Mobile Services to use in the Mobile client.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-4.1.png)

2. Select deploy target as **Mobile Services**.

    !![MDK](img-4.2.png)

    If you want to enable source for debugging the deployed bundle, then choose **Yes**.

    !![MDK](img-4.4.png)

    You should see **Deploy to Mobile Services successfully!** message.

    !![MDK](img-4.3.png)

    >When deploying from VS Code to App Update and using an MDK 6.0+ client, you need to set the TS Target to use es6 instead of the default es5 version. See below for the setting in VS Code where you change it for es6.
    !![MDK](img-4.3.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Get the API Key to use the Maps SDK for Android (Required only for Android client))]

Since you will display the customer's address in Google Maps on Android device, you will need to provide an API key in generated MDK project (step 8).

  1. Visit the [Google Cloud Platform Console](https://cloud.google.com/console/google/maps-apis/overview).

  2. Click the project drop-down and select or create a new project for which you want to add an API key.

    !![MDK](img-6.1.png)

3. Click **Maps SDK for Android** API.

    !![MDK](img-6.4.png)

5. Click **ENABLE**.

    !![MDK](img_6.4.1.png)

6. Open [Credentials console](https://console.cloud.google.com/apis/credentials), click **CREATE CREDENTIALS** and click **API Key**.

    !![MDK](img_6.5.png)

7. Copy this generated key and save it locally. This will be required in step 7.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create Your Branded MDK Client (Required only for Android))]

For iOS, you can just use the App store client. Continue with next step.

For Android, you will pass the API key to the MDK client, there is no way public store client can access it, hence you will create a branded client using MDK SDK or SAP Cloud Build Service.

1.  Follow steps 1 to 3 from [this tutorial](cp-mobile-dev-kit-build-client).

2. Create below file structure under `DemoSampleApp.mdkproject`.

            DemoSampleApp.mdkproject
              ├── App_Resources_Merge
                  └── Android
                      ├── app.gradle
                      └── src
                          └── main
                              └── AndroidManifest.xml


      !![MDK](img-7.1.png)

    >Files specified in the `.mdkproject/App_Resources_Merge` folder override a part of the files in `<generated-project>/app/App_Resources`. You can find more details about it in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/app-resources-merge.html).

3. Provide below information in the `app.gradle` file. Save the changes.

    ```Java
    dependencies { implementation 'com.google.android.gms:play-services-maps:17.0.0' }
    ```

4. Provide below information in the `AndroidManifest.xml` file. Save the changes.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <manifest xmlns:android="http://schemas.android.com/apk/res/android" package="__PACKAGE__" xmlns:tools="http://schemas.android.com/tools" android:versionCode="1" android:versionName="1.0">
    	<!-- Always include this permission -->
    	<!-- This permission is for "approximate" location data -->
    	<uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION" />
    	<!-- Include only if your app benefits from precise location access. -->
    	<!-- This permission is for "precise" location data -->
    	<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />
    	<!--
    	Required only when requesting background location access on
    	Android 10 (API level 29) and higher.
    	-->
    	<uses-permission android:name="android.permission.ACCESS_BACKGROUND_LOCATION" />
    	<application>
    		<meta-data android:name="com.google.android.geo.API_KEY" android:value="Enter your API Key generated in step 6" />
    	</application>
    </manifest>
    ```

5. Create your MDK client either using MDK SDK by following the step 4 from [this](cp-mobile-dev-kit-build-client) tutorial OR using SAP Cloud Build Service by following [this](cp-mobile-dev-kit-cbs-client) tutorial.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Run the MDK Client)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

In this step, you will run the app on an Android device.

1. Attach the device to your Mac or Windows machine and run `tns device android` command to print a list of attached devices.

    !![MDK](img_8.5.png)

    >Make sure **Developer option** and **USB debugging** option is enabled in android device.

2. Copy the **Device Identifier** value for your device.

3. In terminal or command line window, navigate to the app name folder **`DemoSampleApp`** (in `MDClient_SDK` path) and use `tns run android --device <device identifier>` command to run the MDK client on android device.

    !![MDK](img_8.6.png)

4. Once, above command gets successfully executed, you will see new MDK client up and running in Android device.

    In Welcome screen, Tap **Agree** on End User License Agreement.

    ![MDK](img-8.7.png)

5. Tap **Start** to connect MDK client to SAP Business Technology Platform (BTP).

    ![MDK](img_8.8.png)

6. Enter your credentials to login to SAP Business Technology Platform (BTP).

    ![MDK](img-8.9.png)
    ![MDK](img-8.9.1.png)    


7. Choose a passcode with at least 8 characters for unlocking the app and tap **Next**.

    ![MDK](img-8.11.png)

8. Confirm the passcode and tap **Done**.

    ![MDK](img-8.12.png)

    Optionally, you can enable fingerprint to get faster access to the app data.

    ![MDK](img-5.9.1.png)

9. Tap **OK** to update the client with new MDK metadata.

    ![MDK](img-8.13.png)    

10. Tap `Customers` to navigate to customers list.

    ![MDK](img-8.14.png)  

11. Tap any of customer record to navigate to details page.

    ![MDK](img-8.15.png)    

11. In Customer Details page, you will see the Customer's address loading in Google Maps.

    ![MDK](img-8.16.png)  

[OPTION END]

[OPTION BEGIN [iOS]]

SAP Business Application Studio has a feature to display the QR code for onboarding in the Mobile client.

1. Click the **Application.app** to open it in MDK Application Editor and then click the **Application QR Code** icon.

    !![MDK](img-8.17.png)

    The On-boarding QR code is now displayed.

    !![MDK](img-8.18.png)

    >Leave the Onboarding dialog box open for the next step.

2. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-iOS-client/Onboarding-iOS-client.md) to on-board the MDK client.

3. After you have accepted the app update, tap `Customers` to navigate to customers list and then tap any of customer record to navigate to details page.

    ![MDK](img-8.20.png)     

5. In Customer Details page, you will see the Customer's address loading in Apple Maps.

    ![MDK](img-8.21.png)  

[OPTION END]

[VALIDATE_4]
[ACCORDION-END]

---
