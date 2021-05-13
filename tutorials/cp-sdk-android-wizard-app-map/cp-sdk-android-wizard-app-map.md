---
author_name: Bruce Meng
author_profile: https://github.com/flyingfish162
title: Display Customer Locations Using a Fiori Map Control
description: Further customize the generated app to display customer locations on a map and try out the features of the Fiori Map control, including the toolbar, map panel, clustering, and map annotation.
auto_validation: true
time: 90
tags: [ tutorial>beginner, operating-system>android, topic>mobile, products>sap-btp-sdk-for-android, products>sap-business-technology-platform ]
primary_tag: products>sap-btp-sdk-for-android
---

## Prerequisites
- You completed [Try Out SAP BTP SDK Wizard for Android](cp-sdk-android-wizard-app).


## Details
### You will learn
  - How to add a Google Map to the wizard-generated app and display customer locations
  - How to add a Fiori Map control and try out its features

A Fiori Map control extends the Google [Maps SDK for Android](https://developers.google.com/maps/documentation/android-sdk/intro) or `Esri` [`ArcGIS Runtime SDK for Android`](https://developers.arcgis.com/android/). It provides additional APIs that handle clustering, as well as a toolbar, panel, and an editor to annotate map. For additional details, see [Fiori Design Guidelines](https://experience.sap.com/fiori-design-android/).

---

[ACCORDION-BEGIN [Step 1: ](Create a new screen to display a map)]
In this section you will create a new activity to display a map.

[OPTION BEGIN [Java]]

1.  In Android Studio, in the project explorer, navigate to **`app > java > com.sap.wizapp > mdui > customers`**.

2.  Right-click and choose **`New > Activity > Gallery... > Google Maps Activity`**.

    !![Adding Google Map from gallery](add-maps-activity.png)

3.  Set **Activity Name** to be **`CustomersMapActivity`**.

    !![Creating map activity](map-create-dialog-java.png)

4.  Click **Finish**.

5.  In the **`google_maps_api.xml`**, on line 7 correct the package name at the end of the line to be **`com.sap.wizapp`** only.

6.  Paste the URL (line 7) into a browser to register the application with the Maps SDK for Android. Follow the instructions to get an **API Key** and paste it into line 23 of `google_maps_api.xml` in place of the text **`YOUR_KEY_HERE`**.

7.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`EntitySetListActivity`** to open `EntitySetListActivity.java`.

8.  On Windows, press **`Ctrl+F`**, or, on a Mac press **`command+F`**, and search for **`CustomersActivity.class`**.

9.  Replace `CustomersActivity.class` with **`CustomersMapActivity.class`** so that when the user taps on **Customers**, the app will navigate to the newly added activity with a map on it.

10.  On Windows press **`Ctrl+N`** or on a Mac press **`command+O`**, and type **`CustomersMapActivity`** to open `CustomersMapActivity.java`.

11.  Add the following import if it doesn't automatically add itself:

    ```Java
    import com.sap.wizapp.R;
    ```

12.  Run the app. Select **Customers**.

    !![Entities screen](tap-on-customers.png)

    Instead of a customer list, a map is now displayed.

    !![Map screen](empty-map-screen.png)

    >If a message appears that says Wiz App is having trouble with Google Play services, try running the app on an Android emulator that includes the Google Play Store app.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  In Android Studio, in the project explorer, navigate to **`app > java > com.sap.wizapp > mdui > customers`**.

2.  Right-click and choose **`New > Activity > Gallery... > Google Maps Activity`**.

    !![Adding Google Map from gallery](add-maps-activity.png)

3.  Set **Activity Name** to be **`CustomersMapActivity`**.

    !![Creating map activity](map-create-dialog-kotlin.png)

4.  Click **Finish**.

5.  In the **`google_maps_api.xml`**, on line 7 correct the package name at the end of the line to be **`com.sap.wizapp`** only.

6.  Paste the URL (line 7) into a browser to register the application with the Maps SDK for Android.  Follow the instructions to get an API key and paste it into line 23 of `google_maps_api.xml` in place of the text **`YOUR_KEY_HERE`**.

7.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`EntitySetListActivity`** to open `EntitySetListActivity.kt`.

8.  On Windows, press **`Ctrl+F`**, or, on a Mac press **`command+F`**, and search for **`CustomersActivity::class`**.

9.  Replace `CustomersActivity::class` with **`CustomersMapActivity::class`** so that when the user taps on **Customers**, the app will navigate to the newly added activity with a map on it.

10.  On Windows press **`Ctrl+N`** or on a Mac press **`command+O`**, and type **`CustomersMapActivity`** to open `CustomersMapActivity.kt`.

11.  Add the following import if it doesn't automatically add itself:

    ```Kotlin
    import com.sap.wizapp.R
    ```

12.  Run the app. Select **Customers**.

    !![Entities screen](tap-on-customers.png)

    Instead of a customer list, a map is now displayed.

    !![Map screen](empty-map-screen.png)

    >If a message appears that says Wiz App is having trouble with Google Play services, try running the app on an Android emulator that includes the Google Play Store app.

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Populate the map with customer locations )]
In this section, you will add code to place a marker on the map for each customer.

[OPTION BEGIN [Java]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersMapActivity`** to open `CustomersMapActivity.java`.

2.  Add the following class variable:
    ```Java
    private HashMap<String, LatLng> locations = new HashMap<String, LatLng>();
    ```

3.  Add the following methods:

    ```Java
    private void addCustomersToMap() {
        DataQuery query = new DataQuery()
                .from(ESPMContainerMetadata.EntitySets.customers)
                .where(Customer.country.equal("US")
                        .or(Customer.country.equal("CA"))
                        .or(Customer.country.equal("MX")));
        SAPServiceManager sapServiceManager = ((SAPWizardApplication) getApplication()).getSAPServiceManager();
        ESPMContainer espmContainer = sapServiceManager.getESPMContainer();
        espmContainer.getCustomersAsync(query, (List<Customer> customers) -> {
            for (Customer customer : customers) {
                Log.d("", "Adding a marker for " + customer.getCity());
                addCustomerMarkerToMap(customer);
            }
        }, (RuntimeException re) -> Log.d("", "An error occurred during async query:  " + re.getMessage()));
    }

    private LatLng getCustomerLatLongFromAddress(String address) {
        //import android.location.Address;
        List<Address> addresses;
        LatLng latLng = locations.get(address);
        if (latLng != null) {
            return latLng;
        }

        //String strAddress = "Wilmington, Delaware, US";
        Geocoder coder = new Geocoder(this);

        try {
            // May throw an IOException
            addresses = coder.getFromLocationName(address, 5);
            if (addresses == null || addresses.size() == 0) {
                return null;
            }

            Address location = addresses.get(0);
            latLng = new LatLng(location.getLatitude(), location.getLongitude());
            return latLng;
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    private void addCustomerMarkerToMap(Customer customer) {
        LatLng latLng = getCustomerLatLongFromAddress(customer.getCity() + ", " + customer.getCountry());
        if (latLng != null) {
            Marker customerMarker = mMap.addMarker(new MarkerOptions()
                    //.snippet("")
                    .position(latLng)
                    .title(customer.getFirstName() + " " + customer.getLastName())
            );
            customerMarker.setTag(customer);
        }
    }
    ```

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onMapReady`** to move to the `onMapReady` method.

5.  Replace it with the following code:

    ```Java
    @Override
    public void onMapReady(GoogleMap googleMap) {
        mMap = googleMap;
        LatLng centre = new LatLng(39.8283, -98.5795);
        mMap.moveCamera(CameraUpdateFactory.newLatLng(centre));

        // For demo purposes, speed up the lookup of address details.
        // Will use Geocoder to translate an address to a LatLng if address is not in this list
        locations.put("Wilmington, Delaware, US", new LatLng(39.744655, -75.5483909));
        locations.put("Antioch, Illinois, US", new LatLng(42.4772418, -88.0956396));
        locations.put("Santa Clara, California, US", new LatLng(37.354107899999995, -121.9552356));
        locations.put("Hermosillo, MX", new LatLng(29.0729673, -110.9559192));
        locations.put("Bismarck, North Dakota, US", new LatLng(46.808326799999996, -100.7837392));
        locations.put("Ottawa, CA", new LatLng(45.4215296, -75.69719309999999));
        locations.put("México, MX", new LatLng(23.634501, -102.55278399999999));
        locations.put("Boca Raton, Florida, US", new LatLng(26.368306399999998, -80.1289321));
        locations.put("Carrollton, Texas, US", new LatLng(32.9756415, -96.8899636));
        locations.put("Lombard, Illinois, US", new LatLng(41.8800296, -88.00784349999999));
        locations.put("Moorestown, US", new LatLng(39.9688817, -74.948886));
        addCustomersToMap();
    }
    ```

6.  Run the app.

7.  Select **Customers** and notice that a map is displayed that contains a marker for every customer.

    !![Map screen with markers](map-screen-with-markers.png)

    If a marker is tapped, an info marker is displayed with additional customer details.

    !![Map screen with info markers](map-screen-with-info-markers.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersMapActivity`** to open `CustomersMapActivity.kt`.

2.  Add the following class variable:
    ```Kotlin
    private val locations = HashMap<String, LatLng>()
    ```

3.  Add the following methods:

    ```Kotlin
    private fun addCustomersToMap() {
        val query = DataQuery()
                .from(ESPMContainerMetadata.EntitySets.customers)
                .where(Customer.country.equal("US")
                        .or(Customer.country.equal("CA"))
                        .or(Customer.country.equal("MX")))
        val sapServiceManager = (application as SAPWizardApplication).sapServiceManager
        val eSPMContainer = sapServiceManager?.eSPMContainer
        eSPMContainer?.let {
            it.getCustomersAsync(query, { customers: List<Customer> ->
                for (customer in customers) {
                    Log.d("", "Adding a marker for " + customer.city)
                    addCustomerMarkerToMap(customer)
                }
            }, { re: RuntimeException -> Log.d("", "An error occurred during async query: " + re.message) })
        }
    }

    private fun getCustomerLatLongFromAddress(address: String): LatLng? {
        locations[address]?.let {
            return it
        }

        //String strAddress = "Wilmington, Delaware, US";
        val coder = Geocoder(this)
        try
        {   // May throw an IOException
            val addresses = coder.getFromLocationName(address, 5)
            if (addresses.isNullOrEmpty())
            {
                return null
            }
            val location = addresses[0]
            return LatLng(location.latitude, location.longitude)
        }
        catch (ex: IOException) {
            ex.printStackTrace()
            return null
        }
    }

    private fun addCustomerMarkerToMap(customer: Customer) {
        val latLng = getCustomerLatLongFromAddress(customer.city + ", " + customer.country)
        latLng?.let {
            val customerMarker = mMap.addMarker(MarkerOptions()
                    //.snippet("")
                    .position(it)
                    .title(customer.firstName + " " + customer.lastName)
            )
            customerMarker.tag = customer
        }
    }
    ```

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onMapReady`** to move to the `onMapReady` method.

5.  Replace it with the following code:

    ```Kotlin
    override fun onMapReady(googleMap: GoogleMap) {
        mMap = googleMap
        val centre = LatLng(39.8283, -98.5795)
        mMap.moveCamera(CameraUpdateFactory.newLatLng(centre))

        // For demo purposes, speed up the lookup of address details.
        // Will use Geocoder to translate an address to a LatLng if address is not in this list
        locations.put("Wilmington, Delaware, US", LatLng(39.744655, -75.5483909))
        locations.put("Antioch, Illinois, US", LatLng(42.4772418, -88.0956396))
        locations.put("Santa Clara, California, US", LatLng(37.354107899999995, -121.9552356))
        locations.put("Hermosillo, MX", LatLng(29.0729673, -110.9559192))
        locations.put("Bismarck, North Dakota, US", LatLng(46.808326799999996, -100.7837392))
        locations.put("Ottawa, CA", LatLng(45.4215296, -75.69719309999999))
        locations.put("México, MX", LatLng(23.634501, -102.55278399999999))
        locations.put("Boca Raton, Florida, US", LatLng(26.368306399999998, -80.1289321))
        locations.put("Carrollton, Texas, US", LatLng(32.9756415, -96.8899636))
        locations.put("Lombard, Illinois, US", LatLng(41.8800296, -88.00784349999999))
        locations.put("Moorestown, US", LatLng(39.9688817, -74.948886))
        addCustomersToMap()
    }
    ```

6.  Run the app.

7.  Select **Customers** and notice that a map is displayed that contains a marker for every customer.

    !![Map screen with markers](map-screen-with-markers.png)

    If a marker is tapped, an info marker is displayed with additional customer details.

    !![Map screen with info markers](map-screen-with-info-markers.png)

[OPTION END]

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Implement navigation to the customer details screen)]
In this section, you will add code to display the customer detail screen when the info marker is tapped.

[OPTION BEGIN [Java]]

1.  In the class definition for `CustomersMapActivity`, after `implements OnMapReadyCallback`, add:
    ```Java
    , GoogleMap.OnInfoWindowClickListener
    ```

2.  Add the below method to the class:

    ```Java
    @Override
    public void onInfoWindowClick(Marker marker) {
        Customer customer = (Customer) marker.getTag();
        Intent intent = new Intent(this, CustomersActivity.class);
        intent.putExtra(BundleKeys.ENTITY_INSTANCE, customer);
        startActivity(intent);
    }
    ```

3.  In the `onMapReady` method in `CustomersMapActivity`, add the following line to the end of the method:

    ```Java
    mMap.setOnInfoWindowClickListener(this);
    ```

4.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersActivity`** to open `CustomersActivity.java`.

5.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`** to navigate to the `onCreate` method.

6.  Replace the content of the `else` block in the `onCreate` method with the following code:

    ```Java
    CustomersListFragment listFragment = new CustomersListFragment();
    Bundle extra = getIntent().getExtras();
    if (extra != null && extra.containsKey(BundleKeys.ENTITY_INSTANCE)) {
        listFragment.setArguments(extra);
    }
    getSupportFragmentManager().beginTransaction()
        .replace(R.id.masterFrame, listFragment, UIConstants.LIST_FRAGMENT_TAG)
        .commit();
    ```

7.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersListFragment`** to open `CustomersListFragment.java`.

8.  Add the following variable to the top of the class:

    ```Java
    private Customer entityFromMap = null;
    ```

9.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`** to navigate to the `onCreate` method.

10.  Add the following code to the end of the method:

    ```Java
    if (getArguments() != null) {
        if (getArguments().containsKey(BundleKeys.ENTITY_INSTANCE)) {
            entityFromMap = (Customer) getArguments().get(BundleKeys.ENTITY_INSTANCE);
        }
    }
    ```

11.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onViewStateRestored`** to navigate to the `onViewStateRestored` method.

12.  Add the following code to the end of the method:

    ```Java
    if (entityFromMap != null) {
        viewModel.setSelectedEntity(entityFromMap);
        listener.onFragmentStateChange(UIConstants.EVENT_ITEM_CLICKED, entityFromMap);
        entityFromMap = null;
    }
    ```

13.  Run the app. Select **Customers**, and tap on a marker. Then tap on the info marker.

    !![Map screen with info markers](map-screen-with-info-markers2.png)

    This sequence displays the customer details page.

    !![Customer details screen](customer-details-screen.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  In the class definition for `CustomersMapActivity`, after `OnMapReadyCallback`, add:
    ```Kotlin
    , GoogleMap.OnInfoWindowClickListener
    ```

2.  Add the following method to the class:

    ```Kotlin
    override fun onInfoWindowClick(marker: Marker) {//import com.google.android.gms.maps.model.Marker
        val customer = marker.tag as Customer
        val intent = Intent(this, CustomersActivity::class.java)
        intent.putExtra(BundleKeys.ENTITY_INSTANCE, customer)
        startActivity(intent)
    }
    ```

3.  In the `onMapReady` method in `CustomersMapActivity`, add the following line to the end of the method:

    ```Kotlin
    mMap.setOnInfoWindowClickListener(this)
    ```

4.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersActivity`** to open `CustomersActivity.kt`.

5.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`** to navigate to the `onCreate` method.

6.  Replace the `if (savedInstanceState == null)` block (keep `else-block` unchanged) in the `onCreate` method with the following code:

    ```Kotlin
    if (savedInstanceState == null) {
        val listFragment = CustomersListFragment()
        intent.extras?.let {
            if (it.containsKey(BundleKeys.ENTITY_INSTANCE)) {
                listFragment.arguments = it
            }
        }
        supportFragmentManager.beginTransaction()
                .replace(R.id.masterFrame, listFragment, UIConstants.LIST_FRAGMENT_TAG)
                .commit()
    }
    ```

7.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersListFragment`** to open `CustomersListFragment.kt`.

8.  Add the following variable to the top of the class:

    ```Kotlin
    private var entityFromMap: Customer? = null
    ```

9.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`** to navigate to the `onCreate` method.

10.  Add the following code to the end of the method:

    ```Kotlin
    arguments?.let {
        if (it.containsKey(BundleKeys.ENTITY_INSTANCE)) {
            entityFromMap = it.get(BundleKeys.ENTITY_INSTANCE) as Customer
        }
    }
    ```

11.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onViewStateRestored`** to navigate to the `onViewStateRestored` method.

12.  Add the following code to the end of the method:

    ```Kotlin
    entityFromMap?.let {
        viewModel.setSelectedEntity(it)
        listener?.onFragmentStateChange(UIConstants.EVENT_ITEM_CLICKED, it)
        entityFromMap = null
    }
    ```

13.  Run the app. Select **Customers**, and tap on a marker. Then tap on the info marker.

    !![Map screen with info markers](map-screen-with-info-markers2.png)

    This sequence displays the customer details page.

    !![Customer details screen](customer-details-screen.png)

[OPTION END]

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Enhance the app to use the Fiori Map control)]

In this section, you will create a new activity that uses the Fiori Map control.

[OPTION BEGIN [Java]]

1.  Press **`Shift`** twice, and type **`styles.xml`** to open `styles.xml`.

2.  Declare the style of `AppTheme` as:

    ```XML
    <style name="AppTheme" parent="FioriTheme">
    ```

3.  Add the following dependency in the app's `build.gradle` file in the dependencies object.

    ```Gradle
    implementation group: 'com.sap.cloud.android', name: 'google-maps', version: sdkVersion
    ```

    !![Add Map Dependency](app-build-gradle.png)

4.  Create a new **Layout Resource File** in `res/layout` called **`detail_panel.xml`** and replace its contents with the following code.

    !![Create new Layout Resource File](create-new-layout-resource-file.png)

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <androidx.constraintlayout.widget.ConstraintLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:layout_constraintVertical_weight="100">

        <TextView
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:paddingTop="4dp"
            android:paddingLeft="4dp"
            android:text="Default panel content goes here" />

    </androidx.constraintlayout.widget.ConstraintLayout>
    ```

5.  Create a new **Layout resource file** in `res/layout` called **`search_auto_complete.xml`** and replace its contents with the following code:

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="match_parent">
        <androidx.appcompat.widget.AppCompatTextView
            android:id="@+id/search_auto_complete_text"
            android:layout_width="match_parent"
            android:layout_height="50dp" />

    </LinearLayout>
    ```


6.  In Android Studio, using the project explorer, navigate to **`app > java > com.sap.wizapp > mdui > customers`**.

7.  Right-click and choose **New** > **Activity** > **Empty Activity**.

8.  Set **Activity Name** to be **`CustomersFioriMapActivity`**.

9.  Click **Finish**.

    !![Dialog to create Fiori map activity](create-fiori-map-activity.png)

10.  Replace the file contents in the newly created `CustomersFioriMapActivity.java` with the following code:

    ```Java
    package com.sap.wizapp.mdui.customers;

    import android.app.SearchManager;
    import android.content.Context;
    import android.location.Address;
    import android.location.Geocoder;
    import android.os.Bundle;
    import android.util.Log;
    import android.view.LayoutInflater;
    import android.view.View;
    import android.view.inputmethod.InputMethodManager;
    import android.widget.ArrayAdapter;
    import android.widget.ImageButton;

    import com.google.android.gms.maps.CameraUpdateFactory;
    import com.google.android.gms.maps.GoogleMap;
    import com.google.android.gms.maps.model.LatLng;
    import com.sap.cloud.android.odata.espmcontainer.Customer;
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainer;
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainerMetadata;
    import com.sap.cloud.mobile.fiori.maps.FioriMapSearchView;
    import com.sap.cloud.mobile.fiori.maps.FioriMarkerOptions;
    import com.sap.cloud.mobile.fiori.maps.FioriPoint;
    import com.sap.cloud.mobile.fiori.maps.LegendButton;
    import com.sap.cloud.mobile.fiori.maps.LocationButton;
    import com.sap.cloud.mobile.fiori.maps.SettingsButton;
    import com.sap.cloud.mobile.fiori.maps.ZoomExtentButton;
    import com.sap.cloud.mobile.fiori.maps.google.GoogleFioriMapView;
    import com.sap.cloud.mobile.fiori.maps.google.GoogleMapActionProvider;
    import com.sap.cloud.mobile.fiori.maps.google.GoogleMapViewModel;
    import com.sap.cloud.mobile.odata.DataQuery;
    import com.sap.wizapp.R;
    import com.sap.wizapp.app.SAPWizardApplication;
    import com.sap.wizapp.service.SAPServiceManager;

    import org.jetbrains.annotations.NotNull;

    import java.io.IOException;
    import java.util.ArrayList;
    import java.util.Arrays;
    import java.util.HashMap;
    import java.util.List;

    import androidx.appcompat.app.AppCompatActivity;

    public class CustomersFioriMapActivity extends AppCompatActivity implements GoogleFioriMapView.OnMapCreatedListener {
        private GoogleFioriMapView mGoogleFioriMapView;
        private boolean mUseClustering = false;
        private int mMapType;
        private HashMap<String, LatLng> locations = new HashMap<String, LatLng>();  // Used for demo purposes to speed up the process of converting an address to lat, long
        private HashMap<String, FioriMarkerOptions> markers = new HashMap<String, FioriMarkerOptions>();  // Used to associate an address with a marker for search
        private ArrayList<String> addresses = new ArrayList<String>();  // Used to populate the list of addresses that are searchable

        GoogleMapActionProvider mActionProvider;

        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_customers_fiori_map);

            mGoogleFioriMapView = findViewById(R.id.googleFioriMap);
            mGoogleFioriMapView.onCreate(savedInstanceState);

            if (savedInstanceState != null) {
                mUseClustering = savedInstanceState.getBoolean("UseClustering", false);
                mMapType = savedInstanceState.getInt("MapType", GoogleMap.MAP_TYPE_NORMAL);
            }
            mGoogleFioriMapView.setOnMapCreatedListener(this);
        }

        /**
         * Manipulates the map once available.
         * This callback is triggered when the map is ready to be used.
         * This is where we can add markers or lines, add listeners or move the camera. In this case,
         * we just add a marker near Toronto, Canada.
         */
        @Override
        public void onMapCreated() {
            mActionProvider = new GoogleMapActionProvider(mGoogleFioriMapView, this);
            // For demo purposes, speed up the lookup of address details.
            // Will use Geocoder to translate an address to a LatLng if address is not in this list
            locations.put("Wilmington, Delaware, US", new LatLng(39.744655, -75.5483909));
            locations.put("Antioch, Illinois, US", new LatLng(42.4772418, -88.0956396));
            locations.put("Santa Clara, California, US", new LatLng(37.354107899999995, -121.9552356));
            locations.put("Hermosillo, MX", new LatLng(29.0729673, -110.9559192));
            locations.put("Bismarck, North Dakota, US", new LatLng(46.808326799999996, -100.7837392));
            locations.put("Ottawa, CA", new LatLng(45.4215296, -75.69719309999999));
            locations.put("México, MX", new LatLng(23.634501, -102.55278399999999));
            locations.put("Boca Raton, Florida, US", new LatLng(26.368306399999998, -80.1289321));
            locations.put("Carrollton, Texas, US", new LatLng(32.9756415, -96.8899636));
            locations.put("Lombard, Illinois, US", new LatLng(41.8800296, -88.00784349999999));
            locations.put("Moorestown, US", new LatLng(39.9688817, -74.948886));
            addCustomersToMap();

            // Setup toolbar buttons and add to the view.
            SettingsButton settingsButton = new SettingsButton(mGoogleFioriMapView.getToolbar().getContext());
            LegendButton legendButton = new LegendButton(mGoogleFioriMapView.getToolbar().getContext());
            LocationButton locationButton = new LocationButton(mGoogleFioriMapView.getToolbar().getContext());
            ZoomExtentButton extentButton = new ZoomExtentButton(mGoogleFioriMapView.getToolbar().getContext());
            ImageButton[] buttons = {settingsButton, legendButton, locationButton, extentButton};
            mGoogleFioriMapView.getToolbar().addButtons(Arrays.asList(buttons));

            // Setup draggable bottom panel
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            View detailView = inflater.inflate(R.layout.detail_panel, null);
            mGoogleFioriMapView.setDefaultPanelContent(detailView);

            mActionProvider.setClustering(false);

            LatLng currentPosition = ((GoogleMapViewModel)mActionProvider.getMapViewModel()).getLatLng();
            float currentZoom = ((GoogleMapViewModel)mActionProvider.getMapViewModel()).getZoom();
            if (currentPosition != null && currentZoom != 0) {
                // Position the camera after a lifecycle event.
                mGoogleFioriMapView.getMap().moveCamera(CameraUpdateFactory.newLatLngZoom(currentPosition, currentZoom));
            } else {
                // Move the camera to the centre of North America
                LatLng centre = new LatLng(39.8283, -98.5795);
                mGoogleFioriMapView.getMap().animateCamera(CameraUpdateFactory.newLatLng(centre));
            }

            FioriMapSearchView mFioriMapSearchView = findViewById(R.id.fiori_map_search_view);
            if (mFioriMapSearchView != null) {
                SearchManager searchManager = (SearchManager) getSystemService(Context.SEARCH_SERVICE);
                mFioriMapSearchView.setSearchableInfo(searchManager.getSearchableInfo(getComponentName()));
                mFioriMapSearchView.setAdapter(new ArrayAdapter<String>(CustomersFioriMapActivity.this, R.layout.search_auto_complete, R.id.search_auto_complete_text, addresses));
                mFioriMapSearchView.setThreshold(2);
                mFioriMapSearchView.setOnItemClickListener((parent, view, position, id) -> {
                    mFioriMapSearchView.setQuery(parent.getItemAtPosition(position).toString(), false);
                    searchResultSelected((String) parent.getItemAtPosition(position));
                    InputMethodManager inputMethodManager = (InputMethodManager) getSystemService(INPUT_METHOD_SERVICE);
                    inputMethodManager.hideSoftInputFromWindow(mFioriMapSearchView.getWindowToken(), 0);
                });
            }
        }

        private void searchResultSelected(String selectedSearchResult) {
            LatLng latLng = locations.get(selectedSearchResult);
            if (latLng != null) {
                // Center the marker.
                mGoogleFioriMapView.getMap().moveCamera(CameraUpdateFactory.newLatLng(latLng));
                // Select the marker (or cluster the marker is in).
                mActionProvider.selectMarker(markers.get(selectedSearchResult));
            }
        }

        // Methods overriding the lifecycle events are required for FioriMapView to run properly
        @Override
        public void onStart() {
            super.onStart();
            mGoogleFioriMapView.onStart();
        }

        @Override
        public void onResume() {
            super.onResume();
            mGoogleFioriMapView.onResume();
        }

        @Override
        public void onPause() {
            super.onPause();
            mGoogleFioriMapView.onPause();
        }

        @Override
        public void onStop() {
            super.onStop();
            mGoogleFioriMapView.onStop();
        }

        @Override
        public void onDestroy() {
            super.onDestroy();
            mGoogleFioriMapView.onDestroy();
        }

        @Override
        protected void onSaveInstanceState(@NotNull Bundle bundle) {
            super.onSaveInstanceState(bundle);
            mGoogleFioriMapView.onSaveInstanceState(bundle);

            bundle.putBoolean("UseClustering", mUseClustering);
            bundle.putInt("MapType", mMapType);
        }

        @Override
        public void onLowMemory() {
            super.onLowMemory();
            mGoogleFioriMapView.onLowMemory();
        }

        private LatLng getCustomerLatLongFromAddress(String address) {
            // import android.location.Address;
            List<Address> addresses;
            LatLng latLng = locations.get(address);
            if (latLng != null) {
                return latLng;
            }

            // String strAddress = "Wilmington, Delaware";
            Geocoder coder = new Geocoder(this);

            try {
                // May throw an IOException
                addresses = coder.getFromLocationName(address, 5);
                if (addresses == null || addresses.size() == 0) {
                    return null;
                }

                Address location = addresses.get(0);
                latLng = new LatLng(location.getLatitude(), location.getLongitude());
                return latLng;
            } catch (IOException ex) {
                ex.printStackTrace();
                return null;
            }
        }

        private void addCustomerMarkerToMap(Customer customer) {
            LatLng latLng = getCustomerLatLongFromAddress(customer.getCity() + ", " + customer.getCountry());
            if (latLng != null) {
                FioriMarkerOptions customerMarker = new FioriMarkerOptions.Builder()
                        .tag(customer)
                        .point(new FioriPoint(latLng.latitude, latLng.longitude))
                        .title(customer.getFirstName() + " " + customer.getLastName())
                        .legendTitle("Customer")
                        .build();
                mActionProvider.addMarker(customerMarker);
                markers.put(customer.getCity() + ", " + customer.getCountry(), customerMarker);
                mGoogleFioriMapView.getMap().moveCamera(CameraUpdateFactory.newLatLng(latLng));
            }
        }

        private void addCustomersToMap() {
            DataQuery query = new DataQuery()
                    .from(ESPMContainerMetadata.EntitySets.customers)
                    .where(Customer.country.equal("US")
                            .or(Customer.country.equal("CA"))
                            .or(Customer.country.equal("MX")));
            SAPServiceManager sapServiceManager = ((SAPWizardApplication) getApplication()).getSAPServiceManager();
            ESPMContainer espmContainer = sapServiceManager.getESPMContainer();
            espmContainer.getCustomersAsync(query, (List<Customer> customers) -> {
                for (Customer customer : customers) {
                    addCustomerMarkerToMap(customer);
                    addresses.add(customer.getCity() + ", " + customer.getCountry());
                }
                mActionProvider.doExtentsAction();
            }, (RuntimeException re) -> Log.d("", "An error occurred during async query:  " + re.getMessage()));
        }
    }
    ```

11.  Press **Shift** twice and type **`activity_customers_fiori_map.xml`** to open `activity_customers_fiori_map.xml`.

12.  Replace its contents with the following code:

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <androidx.constraintlayout.widget.ConstraintLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:context=".mdui.customers.CustomersFioriMapActivity">

        <com.sap.cloud.mobile.fiori.maps.google.GoogleFioriMapView
            android:id="@+id/googleFioriMap"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            />

    </androidx.constraintlayout.widget.ConstraintLayout>
    ```

13.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`EntitySetListActivity`** to open `EntitySetListActivity.java`.

14.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersMapActivity.class`**.

15.  Replace `CustomersMapActivity.class` with **`CustomersFioriMapActivity.class`** so that when the user taps on **Customers**, the app will navigate to the newly added activity with the Fiori map on it.

16.  On Windows, press **`Ctrl+Shift+N`**, or, on a Mac, press **`command+Shift+O`**, and type **`AndroidManifest`** to open `AndroidManifest.xml`.

17.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersFioriMapActivity`**.

18.  Modify the activity so it specifies the `NoActionBar` theme, which will cause the activity to not display an action bar.

    ```XML
    <activity android:name=".mdui.customers.CustomersFioriMapActivity"
        android:theme="@style/AppTheme.NoActionBar">
    </activity>
    ```

19.  Run the app.

    You should be able to see markers on the screen representing customers.

    !![Fiori Map View](non-clustered-markers.png)

    Users can use the search bar at the top of the screen to find markers. For example, enter **`Illinois`** or **`MX`**.

    !![Search for MX markers](search-bar.png)

    The toolbar on the side provides icons for a settings dialog, marker legend, current location, and zoom to the extent of the markers on the map.

    !![Map toolbar](map-toolbar.png)

    The floating action button in the bottom right corner opens the edit annotations panel, which provides the capability to draw points, lines, and polygons on the map.

    The bottom panel is where additional details for a selected marker can be displayed.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Press **`Shift`** twice, and type **`styles.xml`** to open `styles.xml`.

2.  Declare the style of `AppTheme` as:

    ```XML
    <style name="AppTheme" parent="FioriTheme">
    ```

3.  Add the following dependency in the app's `build.gradle` file in the dependencies object.

    ```Gradle
    implementation group: 'com.sap.cloud.android', name: 'google-maps', version: sdkVersion
    ```

    !![Add Map Dependency](app-build-gradle.png)

4.  Create a new **Layout Resource File** in `res/layout` called **`detail_panel.xml`** and replace its contents with the following code.

    !![Create new Layout Resource File](create-new-layout-resource-file.png)

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <androidx.constraintlayout.widget.ConstraintLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:layout_constraintVertical_weight="100">

        <TextView
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:paddingTop="4dp"
            android:paddingLeft="4dp"
            android:text="Default panel content goes here" />

    </androidx.constraintlayout.widget.ConstraintLayout>
    ```

5.  Create a new **Layout Resource File** in `res/layout` called **`search_auto_complete.xml`** and replace its contents with the following code.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="match_parent">
        <androidx.appcompat.widget.AppCompatTextView
            android:id="@+id/search_auto_complete_text"
            android:layout_width="match_parent"
            android:layout_height="50dp" />

    </LinearLayout>
    ```

6.  In Android Studio, using the project explorer, navigate to **`app > java > com.sap.wizapp > mdui > customers`**.

7.  Right-click and choose **New** > **Activity** > **Empty Activity**.

8.  Set **Activity Name** to be **`CustomersFioriMapActivity`**.

9.  Click **Finish**.

    !![Dialog to create Fiori map activity](create-fiori-map-activity-kotlin.png)

10.  Replace the file contents in the newly created `CustomersFioriMapActivity.kt` with the following code:

    ```Kotlin
    package com.sap.wizapp.mdui.customers

    import android.app.SearchManager
    import android.content.Context
    import android.content.Intent
    import android.location.Geocoder
    import android.os.Bundle
    import android.util.Log
    import android.view.LayoutInflater
    import android.view.View
    import android.view.inputmethod.InputMethodManager
    import android.widget.ArrayAdapter
    import android.widget.ImageButton

    import com.google.android.gms.maps.CameraUpdateFactory
    import com.google.android.gms.maps.GoogleMap
    import com.google.android.gms.maps.model.LatLng
    import com.sap.cloud.android.odata.espmcontainer.Customer
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainerMetadata
    import com.sap.cloud.mobile.fiori.maps.google.GoogleFioriMapView
    import com.sap.cloud.mobile.fiori.maps.google.GoogleMapActionProvider
    import com.sap.cloud.mobile.fiori.maps.google.GoogleMapViewModel
    import com.sap.cloud.mobile.odata.DataQuery
    import com.sap.wizapp.R
    import com.sap.wizapp.app.SAPWizardApplication
    import com.sap.wizapp.mdui.BundleKeys

    import androidx.appcompat.app.AppCompatActivity
    import com.sap.cloud.mobile.fiori.maps.*
    import java.io.IOException

    class CustomersFioriMapActivity : AppCompatActivity(), GoogleFioriMapView.OnMapCreatedListener {
        private lateinit var mGoogleFioriMapView: GoogleFioriMapView
        private var mUseClustering = false
        private var mMapType: Int = 0
        private val locations = HashMap<String, LatLng>() // Used for demo purposes to speed up the process of converting an address to lat, long
        private val markers = HashMap<String, FioriMarkerOptions>() // Used to associate an address with a marker for search
        private val addresses = arrayListOf<String>() // Used to populate the list of addresses that are searchable
        private lateinit var mActionProvider: GoogleMapActionProvider

        override fun onCreate(savedInstanceState: Bundle?) {
            super.onCreate(savedInstanceState)

            val intent = intent
            setContentView(R.layout.activity_customers_fiori_map)

            mGoogleFioriMapView = findViewById(R.id.googleFioriMap)
            mGoogleFioriMapView.onCreate(savedInstanceState)

            savedInstanceState?.let {
                mUseClustering = it.getBoolean("UseClustering", false)
                mMapType = it.getInt("MapType", GoogleMap.MAP_TYPE_NORMAL)
            }
            mGoogleFioriMapView.setOnMapCreatedListener(this)
        }

        /**
         * Manipulates the map once available.
         * This callback is triggered when the map is ready to be used.
         * This is where we can add markers or lines, add listeners or move the camera. In this case,
         * we just add a marker near Toronto, Canada.
         */
        override fun onMapCreated() {
            mActionProvider = GoogleMapActionProvider(mGoogleFioriMapView, this)
            // For demo purposes, speed up the lookup of address details.
            // Will use Geocoder to translate an address to a LatLng if address is not in this list
            locations.put("Wilmington, Delaware, US", LatLng(39.744655, -75.5483909))
            locations.put("Antioch, Illinois, US", LatLng(42.4772418, -88.0956396))
            locations.put("Santa Clara, California, US", LatLng(37.354107899999995, -121.9552356))
            locations.put("Hermosillo, MX", LatLng(29.0729673, -110.9559192))
            locations.put("Bismarck, North Dakota, US", LatLng(46.808326799999996, -100.7837392))
            locations.put("Ottawa, CA", LatLng(45.4215296, -75.69719309999999))
            locations.put("México, MX", LatLng(23.634501, -102.55278399999999))
            locations.put("Boca Raton, Florida, US", LatLng(26.368306399999998, -80.1289321))
            locations.put("Carrollton, Texas, US", LatLng(32.9756415, -96.8899636))
            locations.put("Lombard, Illinois, US", LatLng(41.8800296, -88.00784349999999))
            locations.put("Moorestown, US", LatLng(39.9688817, -74.948886))
            addCustomersToMap()

            // Setup toolbar buttons and add to the view.
            val settingsButton = SettingsButton(mGoogleFioriMapView.toolbar.context)
            val legendButton = LegendButton(mGoogleFioriMapView.toolbar.context)
            val locationButton = LocationButton(mGoogleFioriMapView.toolbar.context)
            val extentButton = ZoomExtentButton(mGoogleFioriMapView.toolbar.context)
            val buttons = arrayOf<ImageButton>(settingsButton, legendButton, locationButton, extentButton)
            mGoogleFioriMapView.toolbar.addButtons(buttons.asList())

            // Setup draggable bottom panel
            val inflater = getSystemService(Context.LAYOUT_INFLATER_SERVICE) as LayoutInflater
            val detailView = inflater.inflate(R.layout.detail_panel, null)
            mGoogleFioriMapView.setDefaultPanelContent(detailView)

            mActionProvider.setClustering(false)

            val currentPosition = (mActionProvider.mapViewModel as GoogleMapViewModel).latLng
            val currentZoom = (mActionProvider.mapViewModel as GoogleMapViewModel).zoom
            if (currentPosition != null && currentZoom != 0f)
            {
                // Position the camera after a lifecycle event.
                mGoogleFioriMapView.map?.moveCamera(CameraUpdateFactory.newLatLngZoom(currentPosition, currentZoom))
            }
            else
            {
                // Move the camera to the centre of North America
                val centre = LatLng(39.8283, -98.5795)
                mGoogleFioriMapView.map?.animateCamera(CameraUpdateFactory.newLatLng(centre))
            }

            findViewById<FioriMapSearchView>(R.id.fiori_map_search_view)?.let {
                val searchManager = getSystemService(Context.SEARCH_SERVICE) as SearchManager
                it.setSearchableInfo(searchManager.getSearchableInfo(componentName))
                it.setAdapter(ArrayAdapter<String>(this@CustomersFioriMapActivity, R.layout.search_auto_complete, R.id.search_auto_complete_text, addresses))
                it.setThreshold(2)
                it.setOnItemClickListener{ parent, view, position, id ->
                    it.setQuery(parent.getItemAtPosition(position).toString(), false)
                    searchResultSelected(parent.getItemAtPosition(position) as String)
                    val inputMethodManager = getSystemService(INPUT_METHOD_SERVICE) as InputMethodManager
                    inputMethodManager.hideSoftInputFromWindow(it.windowToken, 0) }
            }
        }

        private fun searchResultSelected(selectedSearchResult: String) {
            locations[selectedSearchResult]?.let { latLng ->
                // Center the marker.
                mGoogleFioriMapView.map?.moveCamera(CameraUpdateFactory.newLatLng(latLng))
                // Select the marker (or cluster the marker is in).
                mActionProvider.selectMarker(markers[selectedSearchResult])
            }
        }

        // Methods overriding the lifecycle events are required for FioriMapView to run properly
        override fun onStart() {
            super.onStart()
            mGoogleFioriMapView.onStart()
        }

        override fun onResume() {
            super.onResume()
            mGoogleFioriMapView.onResume()
        }

        override fun onPause() {
            super.onPause()
            mGoogleFioriMapView.onPause()
        }

        override fun onStop() {
            super.onStop()
            mGoogleFioriMapView.onStop()
        }

        override fun onDestroy() {
            super.onDestroy()
            mGoogleFioriMapView.onDestroy()
        }

        override fun onSaveInstanceState(bundle: Bundle) {
            super.onSaveInstanceState(bundle)
            mGoogleFioriMapView.onSaveInstanceState(bundle)

            bundle.putBoolean("UseClustering", mUseClustering)
            bundle.putInt("MapType", mMapType)
        }

        override fun onLowMemory() {
            super.onLowMemory()
            mGoogleFioriMapView.onLowMemory()
        }

        private fun getCustomerLatLongFromAddress(address: String) : LatLng? {
            locations[address]?.let {
                return it
            }

            // String strAddress = "Wilmington, Delaware";
            val coder = Geocoder(this)
            try
            {   // May throw an IOException
                val addresses = coder.getFromLocationName(address, 5)
                if (addresses.isNullOrEmpty())
                {
                    return null
                }

                val location = addresses[0]
                return LatLng(location.latitude, location.longitude)
            }
            catch (ex: IOException) {
                ex.printStackTrace()
                return null
            }
        }

        private fun addCustomerMarkerToMap(customer: Customer) {
            getCustomerLatLongFromAddress(customer.city + ", " + customer.country)?.let {latLng ->
                val customerMarker = FioriMarkerOptions.Builder()
                        .tag(customer)
                        .point(FioriPoint(latLng.latitude, latLng.longitude))
                        .title(customer.firstName + " " + customer.lastName)
                        .legendTitle("Customer")
                        .build()
                mActionProvider.addMarker(customerMarker)
                markers.put(customer.city + ", " + customer.country, customerMarker)
                mGoogleFioriMapView.map?.moveCamera(CameraUpdateFactory.newLatLng(latLng))
            }
        }

        private fun addCustomersToMap() {
            val query = DataQuery()
                    .from(ESPMContainerMetadata.EntitySets.customers)
                    .where(Customer.country.equal("US")
                            .or(Customer.country.equal("CA"))
                            .or(Customer.country.equal("MX")))
            val sapServiceManager = (application as SAPWizardApplication).sapServiceManager
            val eSPMContainer = sapServiceManager?.eSPMContainer
            eSPMContainer?.let {
                it.getCustomersAsync(query, { customers: List<Customer> ->
                    for (customer in customers)
                    {
                        addCustomerMarkerToMap(customer)
                        addresses.add(customer.city + ", " + customer.country)
                    }
                    mActionProvider.doExtentsAction() }, { re: RuntimeException -> Log.d("", "An error occurred during async query: " + re.message) })
            }
        }
    }

    ```

11.  Press **Shift** twice and type **`activity_customers_fiori_map.xml`** to open `activity_customers_fiori_map.xml`.

12.  Replace its contents with the following code.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <androidx.constraintlayout.widget.ConstraintLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:context=".mdui.customers.CustomersFioriMapActivity">

        <com.sap.cloud.mobile.fiori.maps.google.GoogleFioriMapView
            android:id="@+id/googleFioriMap"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            />

    </androidx.constraintlayout.widget.ConstraintLayout>
    ```

13.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`EntitySetListActivity`** to open `EntitySetListActivity.kt`.

14.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersMapActivity::class`**.

15.  Replace `CustomersMapActivity::class` with **`CustomersFioriMapActivity::class`** so that when the user taps on **Customers**, the app will navigate to the newly added activity with the Fiori map on it.

16.  On Windows, press **`Ctrl+Shift+N`**, or, on a Mac, press **`command+Shift+O`**, and type **`AndroidManifest`** to open `AndroidManifest.xml`.

17.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersFioriMapActivity`**.

18.  Modify the activity so it specifies the `NoActionBar` theme, which will cause the activity to not display an action bar.

    ```XML
    <activity android:name=".mdui.customers.CustomersFioriMapActivity"
        android:theme="@style/AppTheme.NoActionBar">
    </activity>
    ```

19.  Run the app.

    You should be able to see markers on the screen that represent customers.

    !![Fiori Map View](non-clustered-markers.png)

    Users can use the search bar at the top of the screen to find markers. For example, enter **`Illinois`** or **`MX`**.

    !![Search for MX markers](search-bar.png)

    The toolbar on the side provides icons for a settings dialog, marker legend, current location, and zoom to the extent of the markers on the map.

    !![Map toolbar](map-toolbar.png)

    The floating action button in the bottom right corner opens the edit annotations panel, which provides the capability to draw points, lines, and polygons on the map.

    The bottom panel is where additional details for a selected marker can be displayed.

[OPTION END]

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Implement the map panel)]
In this section, the bottom panel will be populated with details of the selected marker and an action will be implemented to enable navigation to the selected customer's detail page.

[OPTION BEGIN [Java]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.java`.

2.  At the top of the class, add the following variables:

    ```Java
    private MapListPanel mMapListPanel;
    private MapResultsAdapter mMapResultsAdapter;
    ```

3.  At the bottom of the class, add the following methods and the `MapResultsAdapter` class.

    ```Java
    private void setupInfoProvider() {
        AnnotationInfoAdapter infoAdapter = new AnnotationInfoAdapter() {
            @Override
            public Object getInfo(Object tag) {
                return tag;
            }

            @Override
            public void onBindView(MapPreviewPanel mapPreviewPanel, Object info) {
                Customer customer = (Customer) info;
                mapPreviewPanel.setTitle(customer.getFirstName() + " " + customer.getLastName());
                ObjectHeader objectHeader = mapPreviewPanel.getObjectHeader();
                objectHeader.setHeadline(customer.getCity() + ", " + customer.getCountry());
                LatLng customerLatLng = getCustomerLatLongFromAddress(customer.getCity() + ", " + customer.getCountry());
                objectHeader.setBody("Latitude: " + customerLatLng.latitude);
                objectHeader.setFootnote("Longitude " + customerLatLng.longitude);
                ActionCell cell = new ActionCell(CustomersFioriMapActivity.this);
                cell.setText(customer.getPhoneNumber());
                cell.setIcon(R.drawable.ic_phone_black_24dp);
                ActionCell cell2 = new ActionCell(CustomersFioriMapActivity.this);
                cell2.setText(customer.getEmailAddress());
                cell2.setIcon(R.drawable.ic_email_black_24dp);
                ActionCell cell3 = new ActionCell(CustomersFioriMapActivity.this);
                cell3.setText(customer.getHouseNumber() + " " + customer.getStreet());
                cell3.setIcon(R.drawable.ic_map_marker_unselected);
                ActionCell cell4 = new ActionCell(CustomersFioriMapActivity.this);
                cell4.setText("Additional Details");
                cell4.setIcon(R.drawable.ic_list_24dp);
                cell4.setOnClickListener(v -> {
                            Intent intent = new Intent(CustomersFioriMapActivity.this, CustomersActivity.class);
                            intent.putExtra(BundleKeys.ENTITY_INSTANCE, customer);
                            startActivity(intent);
                        }
                );
                mapPreviewPanel.setActionCells(cell, cell2, cell3, cell4);
            }
        };
        mActionProvider.setAnnotationInfoAdapter(infoAdapter);
    }

    private void setListAdapter() {
        if (mMapListPanel == null) {
            mMapListPanel = mGoogleFioriMapView.getMapListPanel();
            mMapResultsAdapter = new MapResultsAdapter();
            mMapListPanel.setAdapter(mMapResultsAdapter);
        }
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ObjectCell objectCell;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            if (itemView instanceof ObjectCell) {
                objectCell = (ObjectCell) itemView;
            }
        }
    }

    class MapResultsAdapter extends RecyclerView.Adapter<ViewHolder> implements MapListPanel.MapListAdapter {
        List<Customer> customers = new ArrayList<>();

        @NonNull
        @Override
        public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            ObjectCell cell = new ObjectCell(parent.getContext());
            cell.setPreserveIconStackSpacing(true);
            cell.setPreserveDetailImageSpacing(false);
            ViewHolder viewHolder = new ViewHolder(cell);
            return viewHolder;
        }

        @Override
        public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
            Customer customer = customers.get(position);
            ObjectCell resultCell = holder.objectCell;
            resultCell.setHeadline(customer.getFirstName() + " " + customer.getLastName());
            resultCell.setSubheadline(customer.getHouseNumber() + " " + customer.getStreet() + ", " + customer.getCity());
            resultCell.setStatus(customer.getCountry(), 1);
            resultCell.setOnClickListener(v -> {
                Intent intent = new Intent(CustomersFioriMapActivity.this, CustomersActivity.class);
                intent.putExtra(BundleKeys.ENTITY_INSTANCE, customer);
                startActivity(intent);
            });
        }

        @Override
        public int getItemCount() {
            return customers.size();
        }

        @Override
        public void clusterSelected(@Nullable List<FioriMarkerOptions> list) {
            customers.clear();
            for (FioriMarkerOptions fmo : list) {
                Object tag = fmo.tag;
                if (tag != null) {
                    customers.add((Customer) tag);
                }
            }
        }
    }

    ```

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and search for **`onMapCreated`**.

5.  At the top of the method, below the line that sets `mActionProvider`, add the following code:

    ```Java
    setupInfoProvider();
    setListAdapter();
    ```

6.  In the same method, comment out the following code. This will disable the default content panel so that the panel can be populated by the new methods.

    ```Java
    View detailView = inflater.inflate(R.layout.detail_panel, null);
    mGoogleFioriMapView.setDefaultPanelContent(detailView);
    ```

7.  Run the app.

    Now when you tap on a marker, the bottom panel should be populated with customer data.

    !![Tap on marker to open details](new-panel.png)

8.  Tap on **Additional Details**.

    !![Customer details in a fully opened panel](new-panel-full-screen.png)

    Notice that the customer details screen is now displayed.

    !![Customer details](customer-details.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.kt`.

2.  At the top of the class, add the following variables:

    ```Kotlin
    private lateinit var mMapResultsAdapter: MapResultsAdapter
    ```

3.  At the bottom of the class, add the following methods and the `MapResultsAdapter` class.

    ```Kotlin
    private fun setupInfoProvider() {
        val infoAdapter = object:AnnotationInfoAdapter {
            override fun getInfo(tag: Any) : Any {
                return tag
            }

            override fun onBindView(mapPreviewPanel: MapPreviewPanel, info: Any) {
                val customer = info as Customer
                mapPreviewPanel.setTitle(customer.firstName + " " + customer.lastName)

                val objectHeader = mapPreviewPanel.objectHeader
                objectHeader.apply {
                    headline = customer.city + ", " + customer.country
                    val customerLatLng = getCustomerLatLongFromAddress(customer.city + ", " + customer.country)
                    customerLatLng?.let {
                        body = "Latitude: " + it.latitude
                        footnote = "Longitude " + it.longitude
                    }
                }

                val cell = ActionCell(this@CustomersFioriMapActivity)
                cell.apply {
                    setText(customer.phoneNumber)
                    setIcon(R.drawable.ic_phone_black_24dp)
                }

                val cell2 = ActionCell(this@CustomersFioriMapActivity)
                cell2.apply {
                    setText(customer.emailAddress)
                    setIcon(R.drawable.ic_email_black_24dp)
                }

                val cell3 = ActionCell(this@CustomersFioriMapActivity)
                cell3.apply {
                    setText(customer.houseNumber + " " + customer.street)
                    setIcon(R.drawable.ic_map_marker_unselected)
                }

                val cell4 = ActionCell(this@CustomersFioriMapActivity)
                cell4.apply {
                    setText("Additional Details")
                    setIcon(R.drawable.ic_list_24dp)
                    setOnClickListener{ v ->
                        val intent = Intent(this@CustomersFioriMapActivity, CustomersActivity::class.java)
                        intent.putExtra(BundleKeys.ENTITY_INSTANCE, customer)
                        startActivity(intent) }
                }

                mapPreviewPanel.setActionCells(cell, cell2, cell3, cell4)
            }
        }
        mActionProvider.annotationInfoAdapter = infoAdapter
    }

    private fun setListAdapter() {
            val mMapListPanel = mGoogleFioriMapView.mapListPanel
            mMapResultsAdapter = MapResultsAdapter()
            mMapListPanel.setAdapter(mMapResultsAdapter)
    }

    class ViewHolder(itemView: View): RecyclerView.ViewHolder(itemView) {
        lateinit var objectCell: ObjectCell
        init{
            if (itemView is ObjectCell) {
                objectCell = itemView
            }
        }
    }

    inner class MapResultsAdapter: RecyclerView.Adapter<ViewHolder>(), MapListPanel.MapListAdapter {
        val customers = arrayListOf<Customer>()

        override fun onCreateViewHolder(parent: ViewGroup, viewType: Int) : ViewHolder {
            val cell = ObjectCell(parent.context)
            cell.preserveIconStackSpacing = true
            cell.preserveDetailImageSpacing = false
            return ViewHolder(cell)
        }

        override fun onBindViewHolder(holder: ViewHolder, position: Int) {
            val customer = customers[position]
            val resultCell = holder.objectCell
            resultCell.apply {
                headline = customer.firstName + " " + customer.lastName
                subheadline = customer.houseNumber + " " + customer.street + ", " + customer.city
                setStatus(customer.country, 1)
                setOnClickListener{ v->
                    val intent = Intent(this@CustomersFioriMapActivity, CustomersActivity::class.java)
                    intent.putExtra(BundleKeys.ENTITY_INSTANCE, customer)
                    startActivity(intent) }
            }
        }

        override fun getItemCount(): Int {
            return customers.size
        }

        override fun clusterSelected(list: List<FioriMarkerOptions>?) {
            customers.clear()
            list?.let { l ->
                for (fmo in l) {
                    fmo.tag?.let {
                        customers.add(it as Customer)
                    }
                }
            }
        }
    }

    ```

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and search for **`onMapCreated`**.

5.  At the top of the method, below the line that sets `mActionProvider`, add the following code:

    ```Kotlin
    setupInfoProvider()
    setListAdapter()
    ```

6.  In the same method, comment out the following code. This will disable the default content panel so that the panel can be populated by the new methods.

    ```Kotlin
    val detailView = inflater.inflate(R.layout.detail_panel, null)
    mGoogleFioriMapView.setDefaultPanelContent(detailView)
    ```

7.  Run the app.

    Now when you tap on a marker, the bottom panel should be populated with customer data.

    !![Tap on marker to open details](new-panel.png)

8.  Tap on **Additional Details**.

    !![Customer details in a fully opened panel](new-panel-full-screen.png)

    Notice that the customer details screen is now displayed.

    !![Customer details](customer-details.png)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Implement settings)]
In this section you will implement the settings dialog to include a map type setting and a clustering toggle.

[OPTION BEGIN [Java]]

1.  Create a new **Layout resource file** in `res/layout` called **`settings_panel.xml`** and replace its contents with the following code. This creates the layout for the settings page.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <ScrollView xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <LinearLayout xmlns:app="http://schemas.android.com/apk/res-auto"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:orientation="vertical">

            <LinearLayout
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:layout_gravity="fill_horizontal"
                android:orientation="horizontal">

                <com.sap.cloud.mobile.fiori.formcell.ChoiceFormCell
                    android:id="@+id/map_type"
                    android:layout_width="match_parent"
                    android:layout_height="wrap_content"
                    app:key="Map Type" />

            </LinearLayout>

            <com.sap.cloud.mobile.fiori.formcell.SwitchFormCell
                android:id="@+id/use_clustering"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:showText="true"
                app:key="Clustering"
                app:value="true" />

        </LinearLayout>
    </ScrollView>
    ```

2.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.java`.

3.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onMapCreated`** to navigate to the `onMapCreated` method.

4.  Find the following line of code:

    ```Java
    mActionProvider.setClustering(false);
    ```

5.  Replace the line above with the following code.

    ```Java
    View settingsView = inflater.inflate(R.layout.settings_panel, null);

    // Setup selection of a different map type
    ChoiceFormCell mapTypeChoice = settingsView.findViewById(R.id.map_type);
    mapTypeChoice.setValueOptions(new String[]{"Normal", "Satellite", "Terrain", "Hybrid"});
    mapTypeChoice.setCellValueChangeListener(new FormCell.CellValueChangeListener<Integer>() {
        @Override
        public void cellChangeHandler(Integer value) {
            switch(value) {
                case 0:
                    mMapType = GoogleMap.MAP_TYPE_NORMAL;
                    break;
                case 1:
                    mMapType = GoogleMap.MAP_TYPE_SATELLITE;
                    break;
                case 2:
                    mMapType = GoogleMap.MAP_TYPE_TERRAIN;
                    break;
                case 3:
                    mMapType = GoogleMap.MAP_TYPE_HYBRID;
                    break;
                }
                mGoogleFioriMapView.getMap().setMapType(mMapType);
          }
    });

    if (mMapType == 0) {
        mapTypeChoice.setValue(mMapType);
    } else {
        mapTypeChoice.setValue(mMapType - 1);
    }

    mGoogleFioriMapView.setSettingsView(settingsView);

    if (mMapType != GoogleMap.MAP_TYPE_NONE) {
        mGoogleFioriMapView.getMap().setMapType(mMapType);
    }

    // Setup clustering selection.
    SwitchFormCell useClusteringSwitch = settingsView.findViewById(R.id.use_clustering);
    useClusteringSwitch.setValue(mUseClustering);
    mActionProvider.setClustering(mUseClustering);
    useClusteringSwitch.setCellValueChangeListener(new FormCell.CellValueChangeListener<Boolean>() {
        @Override
        protected void cellChangeHandler(@NonNull Boolean value) {
            mUseClustering = value;
            mActionProvider.setClustering(mUseClustering);
        }
    });
    ```

6.  Run the app.

7.  Tap on the settings icon in the toolbar.

    !![Setting icon in toolbal](setting-icon-in-toolbar.png)

8.  Change the map type to **Hybrid** and turn **Clustering** on.

    !![Settings](settings.png)

    !![Hybrid map example](hybrid-map.png)

    Notice that the markers in close proximity are now grouped together and a number indicates how many markers are in the cluster.

    !![Map panel for Cluster](cluster-panel.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Create a new **Layout resource file** in `res/layout` called **`settings_panel.xml`** and replace its contents with the following code. This creates the layout for the settings page.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <ScrollView xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <LinearLayout xmlns:app="http://schemas.android.com/apk/res-auto"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:orientation="vertical">

            <LinearLayout
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:layout_gravity="fill_horizontal"
                android:orientation="horizontal">

                <com.sap.cloud.mobile.fiori.formcell.ChoiceFormCell
                    android:id="@+id/map_type"
                    android:layout_width="match_parent"
                    android:layout_height="wrap_content"
                    app:key="Map Type" />

            </LinearLayout>

            <com.sap.cloud.mobile.fiori.formcell.SwitchFormCell
                android:id="@+id/use_clustering"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:showText="true"
                app:key="Clustering"
                app:value="true" />

        </LinearLayout>
    </ScrollView>
    ```

2.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.kt`.

3.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onMapCreated`** to navigate to the `onMapCreated` method.

4.  Find the following line of code:

    ```Kotlin
    mActionProvider.setClustering(false)
    ```

5.  Replace the line above with the following code:

    ```Kotlin
    val settingsView = inflater.inflate(R.layout.settings_panel, null)

    // Setup selection of a different map type
    val mapTypeChoice = settingsView.findViewById<ChoiceFormCell>(R.id.map_type)
    mapTypeChoice.valueOptions = arrayOf<String>("Normal", "Satellite", "Terrain", "Hybrid")
    mapTypeChoice.cellValueChangeListener = object : FormCell.CellValueChangeListener<Int>() {
        override fun cellChangeHandler(value: Int?) {
            value?.let {
                when (it) {
                    0 -> mMapType = GoogleMap.MAP_TYPE_NORMAL
                    1 -> mMapType = GoogleMap.MAP_TYPE_SATELLITE
                    2 -> mMapType = GoogleMap.MAP_TYPE_TERRAIN
                    3 -> mMapType = GoogleMap.MAP_TYPE_HYBRID
                }
                mGoogleFioriMapView.map?.mapType = mMapType
            }
        }
    }

    if (mMapType == 0) {
        mapTypeChoice.value = mMapType
    } else {
        mapTypeChoice.value = mMapType - 1
    }

    mGoogleFioriMapView.setSettingsView(settingsView)

    if (mMapType != GoogleMap.MAP_TYPE_NONE) {
        mGoogleFioriMapView.map?.mapType = mMapType
    }

    // Setup clustering selection.
    val useClusteringSwitch = settingsView.findViewById<SwitchFormCell>(R.id.use_clustering)
    useClusteringSwitch.value = mUseClustering
    mActionProvider.setClustering(mUseClustering)
    useClusteringSwitch.cellValueChangeListener = object: FormCell.CellValueChangeListener<Boolean>() {
        override fun cellChangeHandler(value: Boolean?) {
            value?.let {
                mUseClustering = it
                mActionProvider.setClustering(mUseClustering)
            }
        }
    }

    ```

6.  Run the app.

7.  Tap on the settings icon in the toolbar.

    !![Setting icon in toolbal](setting-icon-in-toolbar.png)

8.  Change the map type to **Hybrid** and turn **Clustering** on.

    !![Settings](settings.png)

    !![Hybrid map example](hybrid-map.png)

    Notice that the markers in close proximity are now grouped together and a number indicates how many markers are in the cluster.

    !![Map panel for Cluster](cluster-panel.png)

[OPTION END]

Congratulations! You have now successfully added a Fiori Map to an application and tried out some of the features it provides.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Enable annotating)]
In this section, you will test the three different types of annotations.

[OPTION BEGIN [Java]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.java`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`** to navigate to the `onCreate` method.

3.  Add the following code after the `mGoogleFioriMapView.onCreate` is called. This will save any points, `polylines`, or polygons drawn on the map.

    ```Java
    // Handle the editor's save event.
    // This is where you can implement functions to save the new annotated points, polylines and polygons
    mGoogleFioriMapView.getEditorView().setOnSaveListener(annotation -> {
        String message = null;
        if (annotation instanceof PointAnnotation) {
            message = "This is a point";
        } else if (annotation instanceof PolylineAnnotation) {
            message = "This is a polyline with " + annotation.getPoints().size() + " points";
        } else if (annotation instanceof PolygonAnnotation) {
            message = "This is a polygon with " + annotation.getPoints().size() + " points";
        }

        // import android.app AlertDialog
        AlertDialog.Builder builder = new AlertDialog.Builder(mGoogleFioriMapView.getContext(), com.sap.cloud.mobile.fiori.R.style.FioriAlertDialogStyle);
        builder.setMessage(message);
        builder.setPositiveButton("Save", (dialogInterface, i) -> {
            // Close the editor.
            mGoogleFioriMapView.setEditable(false);
            if (annotation instanceof PointAnnotation) {
                mActionProvider.addCircle(
                        new FioriCircleOptions.Builder().center((FioriPoint) annotation.getPoints().get(0)).
                                radius(40).
                                strokeColor(getResources().getColor(R.color.maps_marker_color_5, null)).
                                fillColor(getResources().getColor(R.color.maps_marker_color_6, null)).
                                title("Editor Circle").
                                build());
            } else if (annotation instanceof PolylineAnnotation) {
                mActionProvider.addPolyline(
                        new FioriPolylineOptions.Builder().addAll(annotation.getPoints()).
                                color(getResources().getColor(R.color.maps_marker_color_3, null)).
                                strokeWidth(4).
                                title("Editor Polyline").
                                build());
            } else if (annotation instanceof PolygonAnnotation) {
                mActionProvider.addPolygon(
                        new FioriPolygonOptions.Builder().addAll(annotation.getPoints()).
                                strokeColor(getResources().getColor(R.color.maps_marker_color_3, null)).
                                fillColor(getResources().getColor(R.color.maps_marker_color_4, null)).
                                strokeWidth(4).
                                title("Editor Polygon").
                                build());
            }
        });
        builder.setNegativeButton("Cancel", (dialogInterface, i) -> dialogInterface.cancel());
        AlertDialog dialog = builder.create();
        dialog.show();
    });
    ```

4.  Run the app.

5.  Change the emulator's location information so that Waterloo, Ontario, is the new default location.

    Click on the three dots on the emulator's toolbar to navigate to the emulator's settings.

    !![Emulator toolbar settings button](emulator-settings.png)

6.  Under **Location** > **Single points**, search for **`University of Waterloo`** and select the first instance.

    !![Up to date emulator location settings screen 1](up-to-date-emulator-location-settings-1.png)

7.  Tap **SAVE POINT**.

    !![Up to date emulator location settings screen 2](up-to-date-emulator-location-settings-2.png)

8.  Set the name you want to save as.

    !![Up to date emulator location settings screen 3](up-to-date-emulator-location-settings-3.png)

9.  Select the saved point and tap **SET LOCATION** to set the default location.

    !![Up to date emulator location settings screen 4](up-to-date-emulator-location-settings-4.png)

10.  Tap the current location button to zoom into the University of Waterloo and you should see the screen below.

    !![Zoom into current location](current-location-effect.png)

11.  To annotate the map, tap on the floating action button in the corner.

    !![Floating action button to edit annotations](fab.png)

    The bottom panel displays options to annotate the map.

    !![Edit annotations panel](edit-annotations.png)

12.  To add the current location as a point:
    -	Tap on the **Add Point** option in the panel. Note that it may take a few moments for the emulator to process the new coordinates from before.

        !![Add point button in panel](add-point.png)

    -	Tap on **Current Location** under the search bar.

        !![Current location button](current-location-button.png)

        A list of location options at the University of Waterloo is displayed.

        !![List of University of Waterloo buildings](list-of-current-location.png)

    > If an API error occurs, such as `Failed to get location addresscom.google.android.gms.common.api.ApiException`, or nothing happens after tapping **Current Location**, ensure that the **Places API** is enabled on the [Google Cloud Platform](https://console.developers.google.com/). Type **`Places API`** in the search bar and you'll be redirected to the following page.
    !![Google Cloud Platform Places API page](places-api.png)

    > If you enabled **Places API** but still nothing happens after tapping, use
    ```URL
    https://maps.googleapis.com/maps/api/place/details/json?place_id=ChIJN1t_tDeuEmsRUsoyG83frY4&fields=name,rating,formatted_phone_number&key=YOUR_API_KEY
    ```
    to see if your application could get certain place details successfully. Note that you'll need to replace the key in this example with your own API key. See [Google Maps Platform](https://developers.google.com/places/web-service/details#PlaceDetailsStatusCodes) for additional information.

    You can also add a point by tapping directly on the map. This will create a white and blue dot indicating where the new point is located. The added point will appear in the panel under the Address portion. You can only add one point to the map. You can move it by long pressing on it and then dragging it to a new location or you can delete it (select the point and then click the **X** mark in the **ADDRESS** list) and then you can add a new point.

    !![Point added onto map](added-point.png)

13.  To add a `polyline` to the map, select the **`Polyline`** option and tap different places on the map to add multiple points. The added points will be connected with a line.

    !![Add polyline to map](add-polyline.png)

14.  To add a polygon, select the **Polygon** option and tap different places on the map to add multiple points. The points are connected in the order that they appear in the list within the panel and take up the least amount of area.

    !![Add polygon to map](add-polygon.png)

    You can move the existing points on the map by holding onto a point and then dragging it to the desired location.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.kt`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`** to navigate to the `onCreate` method.

3.  Add the following code after the `mGoogleFioriMapView.onCreate` is called. This will save any points, `polylines`, or polygons drawn on the map.

    ```Kotlin
    // Handle the editor's save event.
    // This is where you can implement functions to save the new annotated points, polylines and polygons
    mGoogleFioriMapView.editorView.setOnSaveListener{annotation ->
        var message: String? = null
        when (annotation) {
            is PointAnnotation -> {
                message = "This is a point"
            }
            is PolylineAnnotation -> {
                message = "This is a polyline with " + annotation.points.size + " points"
            }
            is PolygonAnnotation -> {
                message = "This is a polygon with " + annotation.points.size + " points"
            }
        }

        // import android.app AlertDialog
        val builder = AlertDialog.Builder(mGoogleFioriMapView.context, com.sap.cloud.mobile.fiori.R.style.FioriAlertDialogStyle)
        builder.setMessage(message)
        builder.setPositiveButton("Save") { _, _ ->
            // Close the editor.
            mGoogleFioriMapView.isEditable = false
            when (annotation) {
                is PointAnnotation -> {
                    mActionProvider.addCircle(
                            FioriCircleOptions.Builder().center(annotation.points[0] as FioriPoint).radius(40.0).strokeColor(resources.getColor(R.color.maps_marker_color_5, null)).fillColor(resources.getColor(R.color.maps_marker_color_6, null)).title("Editor Circle").build())
                }
                is PolylineAnnotation -> {
                    mActionProvider.addPolyline(
                            FioriPolylineOptions.Builder().addAll(annotation.points as Iterable<FioriPoint>).color(resources.getColor(R.color.maps_marker_color_3, null)).strokeWidth(4f).title("Editor Polyline").build())
                }
                is PolygonAnnotation -> {
                    mActionProvider.addPolygon(
                            FioriPolygonOptions.Builder().addAll(annotation.points as Iterable<FioriPoint>).strokeColor(resources.getColor(R.color.maps_marker_color_3, null)).fillColor(resources.getColor(R.color.maps_marker_color_4, null)).strokeWidth(4f).title("Editor Polygon").build())
                }
            }
        }
        builder.setNegativeButton("Cancel") { dialogInterface, _ -> dialogInterface.cancel() }
        val dialog = builder.create()
        dialog.show()
    }
    ```

4.  Run the app.

5.  Change the emulator's location information so that Waterloo, Ontario, is the new default location.

    Click on the three dots on the emulator's toolbar to navigate to the emulator's settings.

    !![Emulator toolbar settings button](emulator-settings.png)

6.  Under **Location** > **Single points**, search for **`University of Waterloo`** and select the first instance.

    !![Up to date emulator location settings screen 1](up-to-date-emulator-location-settings-1.png)

7.  Tap **SAVE POINT**.
    !![Up to date emulator location settings screen 2](up-to-date-emulator-location-settings-2.png)

8.  Set the name you want to save as.
    !![Up to date emulator location settings screen 3](up-to-date-emulator-location-settings-3.png)

9.  Select the saved point and tap **SET LOCATION** to set the default location.
    !![Up to date emulator location settings screen 4](up-to-date-emulator-location-settings-4.png)

10.  Tap the current location button to zoom into the University of Waterloo and you should see the screen below.

    !![Zoom into current location](current-location-effect.png)

11.  To annotate the map, tap on the floating action button in the corner.

    !![Floating action button to edit annotations](fab.png)

    The bottom panel displays options to annotate the map.

    !![Edit annotations panel](edit-annotations.png)

12.  To add the current location as a point:
    -	Tap on the **Add Point** option in the panel. Note that it may take a few moments for the emulator to process the new coordinates from before.

        !![Add point button in panel](add-point.png)

    -	Tap on Current Location under the search bar.

        !![Current location button](current-location-button.png)

        A list of location options at the University of Waterloo is displayed.

        !![List of University of Waterloo buildings](list-of-current-location.png)

    > If an API error occurs, such as `Failed to get location addresscom.google.android.gms.common.api.ApiException`, or nothing happens after tapping **Current Location**, ensure that the **Places API** is enabled on the [Google Cloud Platform](https://console.developers.google.com/). Type **`Places API`** in the search bar and you'll be redirected to the following page.
    !![Google Cloud Platform Places API page](places-api.png)

    > If you enabled **Places API** but still nothing happens after tapping, use
    ```URL
    https://maps.googleapis.com/maps/api/place/details/json?place_id=ChIJN1t_tDeuEmsRUsoyG83frY4&fields=name,rating,formatted_phone_number&key=YOUR_API_KEY
    ```
    to see if your application could get certain place details successfully. Note that you'll need to replace the key in this example with your own API key. See [Google Maps Platform](https://developers.google.com/places/web-service/details#PlaceDetailsStatusCodes) for additional information.

    You can also add a point by tapping directly on the map. This will create a white and blue dot indicating where the new point is located. The added point will appear in the panel under the Address portion. You can only add one point to the map. You can move it by long pressing on it and then dragging it to a new location or you can delete it (select the point and then click the **X** mark in the **ADDRESS** list) and then add a new point.

    !![Point added onto map](added-point.png)

13.  To add a `polyline` to the map, select the **`Polyline`** option and tap different places on the map to add multiple points. The added points will be connected with a line.

    !![Add polyline to map](add-polyline.png)

14.  To add a polygon, select the **Polygon** option and tap different places on the map to add multiple points. The points are connected in the order that they appear in the list within the panel and take up the least amount of area.

    !![Add polygon to map](add-polygon.png)

    You can move the existing points on the map by holding onto a point and then dragging it to the desired location.

[OPTION END]

In order to redraw the points the next time the map is opened, you must save and store them in the app using the on save click listener.

[VALIDATE_4]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Customize map markers and legend)]
In this section you will customize the map markers based on the customer's country. The different marker colors will be recorded in the legend as well.

[OPTION BEGIN [Java]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.java`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`addCustomerMarkerToMap`** to navigate to the `addCustomerMarkerToMap` method.

3.  Replace the method with the following code:

    ```Java
    private void addCustomerMarkerToMap(Customer customer) {
        FioriMarkerOptions customerMarker;
        String country = customer.getCountry();
        LatLng latLng = getCustomerLatLongFromAddress(customer.getCity() + ", " + customer.getCountry());
        if (latLng != null) {
            int color = (Color.parseColor("#E9573E")); // US
            if (country.equals("MX")) {
                color = (Color.parseColor("#FFA02B"));
            } else if (country.equals("CA")){
                color =  Color.parseColor("#2E4A62");
            }
            customerMarker = new FioriMarkerOptions.Builder()
                    .tag(customer)
                    .point(new FioriPoint(latLng.latitude, latLng.longitude))
                    .title(customer.getFirstName() + " " + customer.getLastName())
                    .legendTitle(customer.getCountry())
                    .color(color)
                    .build();
            mActionProvider.addMarker(customerMarker);
            markers.put(customer.getCity() + ", " + customer.getCountry(), customerMarker);
            mGoogleFioriMapView.getMap().moveCamera(CameraUpdateFactory.newLatLng(latLng));
        }
    }
    ```

4.  Run the app.

    The markers now have different colors depending on whether they are located in Canada (CA), the United States (US), or Mexico (MX). The meaning of the colors is shown in the legend.

    !![Marker Legend](legend.png)

    With clustering enabled, notice that clustered markers turn white if the markers in the cluster are located in different countries.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`CustomersFioriMapActivity`** to open `CustomersFioriMapActivity.kt`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`addCustomerMarkerToMap`** to navigate to the `addCustomerMarkerToMap` method.

3.  Replace the method with the following code:

    ```Kotlin
    private fun addCustomerMarkerToMap(customer: Customer) {
        val country = customer.country
        val latLng = getCustomerLatLongFromAddress(customer.city + ", " + customer.country)
        latLng?.let {
            var color = (Color.parseColor("#E9573E")) // US
            if (country == "MX") {
                color = (Color.parseColor("#FFA02B"))
            } else if (country == "CA") {
                color = Color.parseColor("#2E4A62")
            }
            val customerMarker = FioriMarkerOptions.Builder()
                    .tag(customer)
                    .point(FioriPoint(it.latitude, it.longitude))
                    .title(customer.firstName + " " + customer.lastName)
                    .legendTitle(customer.country)
                    .color(color)
                    .build()
            mActionProvider.addMarker(customerMarker)
            markers[customer.city + ", " + customer.country] = customerMarker
            mGoogleFioriMapView.map?.moveCamera(CameraUpdateFactory.newLatLng(it))
        }
    }
    ```

4.  Run the app.

    The markers now have different colors depending on whether they are located in Canada (CA), the United States (US), or Mexico (MX). The meaning of the colors is shown in the legend.

    !![Marker Legend](legend.png)

    With clustering enabled, notice that clustered markers turn white if the markers in the cluster are located in different countries.

[OPTION END]

Congratulations. You have created an activity that makes use of the Fiori map control.

[VALIDATE_5]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Esri map)]

In this section, an `EsriFioriMapView` will be used.

[OPTION BEGIN [Java]]

1.  Add the following permissions to the `AndroidManifest.xml` file before the application section.

    ```XML
    <uses-permission android:name="android.permission.INTERNET" />
    <uses-feature android:glEsVersion="0x00020000" android:required="true" />
    ```

2.  Add the following to the `ext.repos` section of the project's `build.gradle` file:

    ```Gradle
    maven {
        url 'https://esri.bintray.com/arcgis'
    }
    ```

3.  Add the following dependency to the app's `build.gradle` file and click **Sync Now**.

    ```Gradle
    implementation group: 'com.sap.cloud.android', name: 'esri-maps', version: sdkVersion
    ```

4.  In Android Studio, using the **Project Explorer**, navigate to **`app > java > com.sap.wizapp > mdui > customers`**.

5.  Right-click and choose **`New > Activity > Empty Activity`**.

6.  Set **Activity Name** to be **`CustomersFioriEsriMapActivity`**.

7.  Click **Finish**.

8.  Replace the file contents in the newly created `CustomersFioriEsriMapActivity.java` with the following code:

    ```Java
    package com.sap.wizapp.mdui.customers;

    import android.app.SearchManager;
    import android.content.Context;
    import android.content.Intent;
    import android.location.Address;
    import android.location.Geocoder;
    import android.os.Bundle;
    import android.util.Log;
    import android.view.LayoutInflater;
    import android.view.View;
    import android.view.inputmethod.InputMethodManager;
    import android.widget.ArrayAdapter;
    import android.widget.ImageButton;

    import com.esri.arcgisruntime.geometry.Point;
    import com.esri.arcgisruntime.geometry.SpatialReferences;
    import com.google.android.gms.maps.model.LatLng;
    import com.sap.cloud.android.odata.espmcontainer.Customer;
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainer;
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainerMetadata;
    import com.sap.cloud.mobile.fiori.maps.FioriMapSearchView;
    import com.sap.cloud.mobile.fiori.maps.FioriMarkerOptions;
    import com.sap.cloud.mobile.fiori.maps.FioriPoint;
    import com.sap.cloud.mobile.fiori.maps.LegendButton;
    import com.sap.cloud.mobile.fiori.maps.LocationButton;
    import com.sap.cloud.mobile.fiori.maps.SettingsButton;
    import com.sap.cloud.mobile.fiori.maps.ZoomExtentButton;
    import com.sap.cloud.mobile.fiori.maps.esri.EsriFioriMapView;
    import com.sap.cloud.mobile.fiori.maps.esri.EsriMapActionProvider;
    import com.sap.cloud.mobile.odata.DataQuery;
    import com.sap.wizapp.R;
    import com.sap.wizapp.app.SAPWizardApplication;
    import com.sap.wizapp.service.SAPServiceManager;

    import java.io.IOException;
    import java.util.ArrayList;
    import java.util.Arrays;
    import java.util.HashMap;
    import java.util.List;

    import androidx.appcompat.app.AppCompatActivity;

    public class CustomersFioriEsriMapActivity extends AppCompatActivity  {
        private EsriFioriMapView mEsriFioriMapView;
        private HashMap<String, LatLng> locations = new HashMap<String, LatLng>();  // Used for demo purposes to speed up the process of converting an address to lat, long
        private HashMap<String, FioriMarkerOptions> markers = new HashMap<String, FioriMarkerOptions>();  // Used to associate an address with a marker for search
        private ArrayList<String> addresses = new ArrayList<String>();  // Used to populate the list of addresses that are searchable

        EsriMapActionProvider mActionProvider;

        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            Intent intent = getIntent();
            setContentView(R.layout.activity_customers_fiori_esri_map);
            mEsriFioriMapView = findViewById(R.id.esriFioriView);

            mActionProvider = new EsriMapActionProvider(mEsriFioriMapView, this);
            // For demo purposes, speed up the lookup of address details.
            // Will use Geocoder to translate an address to a LatLng if address is not in this list
            locations.put("Wilmington, Delaware, US", new LatLng(39.744655, -75.5483909));
            locations.put("Antioch, Illinois, US", new LatLng(42.4772418, -88.0956396));
            locations.put("Santa Clara, California, US", new LatLng(37.354107899999995, -121.9552356));
            locations.put("Hermosillo, MX", new LatLng(29.0729673, -110.9559192));
            locations.put("Bismarck, North Dakota, US", new LatLng(46.808326799999996, -100.7837392));
            locations.put("Ottawa, CA", new LatLng(45.4215296, -75.69719309999999));
            locations.put("México, MX", new LatLng(23.634501, -102.55278399999999));
            locations.put("Boca Raton, Florida, US", new LatLng(26.368306399999998, -80.1289321));
            locations.put("Carrollton, Texas, US", new LatLng(32.9756415, -96.8899636));
            locations.put("Lombard, Illinois, US", new LatLng(41.8800296, -88.00784349999999));
            locations.put("Moorestown, US", new LatLng(39.9688817, -74.948886));
            addCustomersToMap();

            // Setup toolbar buttons and add to the view.
            SettingsButton settingsButton = new SettingsButton(mEsriFioriMapView.getToolbar().getContext());
            LegendButton legendButton = new LegendButton(mEsriFioriMapView.getToolbar().getContext());
            LocationButton locationButton = new LocationButton(mEsriFioriMapView.getToolbar().getContext());
            ZoomExtentButton extentButton = new ZoomExtentButton(mEsriFioriMapView.getToolbar().getContext());
            ImageButton[] buttons = {settingsButton, legendButton, locationButton, extentButton};
            mEsriFioriMapView.getToolbar().addButtons(Arrays.asList(buttons));

            // Setup draggable bottom panel
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            View detailView = inflater.inflate(R.layout.detail_panel, null);
            mEsriFioriMapView.setDefaultPanelContent(detailView);

            FioriMapSearchView mFioriMapSearchView = findViewById(R.id.fiori_map_search_view);
            if (mFioriMapSearchView != null) {
                SearchManager searchManager = (SearchManager) getSystemService(Context.SEARCH_SERVICE);
                mFioriMapSearchView.setSearchableInfo(searchManager.getSearchableInfo(getComponentName()));
                mFioriMapSearchView.setAdapter(new ArrayAdapter<String>(CustomersFioriEsriMapActivity.this, R.layout.search_auto_complete, R.id.search_auto_complete_text, addresses));
                mFioriMapSearchView.setThreshold(2);
                mFioriMapSearchView.setOnItemClickListener((parent, view, position, id) -> {
                    mFioriMapSearchView.setQuery(parent.getItemAtPosition(position).toString(), false);
                    searchResultSelected((String) parent.getItemAtPosition(position));
                    InputMethodManager inputMethodManager = (InputMethodManager) getSystemService(INPUT_METHOD_SERVICE);
                    inputMethodManager.hideSoftInputFromWindow(mFioriMapSearchView.getWindowToken(), 0);
                });
            }
        }

        /**
         * Manipulates the map once available.
         * This callback is triggered when the map is ready to be used.
         * This is where we can add markers or lines, add listeners, or move the camera. In this case,
         * we just add a marker near Toronto, Canada.
         */

        private void searchResultSelected(String selectedSearchResult) {
            LatLng latLng = locations.get(selectedSearchResult);
            if (latLng != null) {
                Point stationLocation = new Point(latLng.longitude, latLng.latitude, SpatialReferences.getWgs84());
                mEsriFioriMapView.getMapView().setViewpointCenterAsync(stationLocation);
                // Select the marker (or cluster the marker is in).
                mActionProvider.selectMarker(markers.get(selectedSearchResult));
            }
        }

        @Override
        public void onDestroy() {
            super.onDestroy();
            //mActionProvider.cleanup();
        }

        private LatLng getCustomerLatLongFromAddress(String address) {
            // import android.location.Address;
            List<Address> addresses;
            LatLng latLng = locations.get(address);
            if (latLng != null) {
                return latLng;
            }

            // String strAddress = "Wilmington, Delaware";
            Geocoder coder = new Geocoder(this);

            try {
                // May throw an IOException
                addresses = coder.getFromLocationName(address, 5);
                if (addresses == null || addresses.size() == 0) {
                    return null;
                }

                Address location = addresses.get(0);
                latLng = new LatLng(location.getLatitude(), location.getLongitude());
                return latLng;
            } catch (IOException ex) {
                ex.printStackTrace();
                return null;
            }
        }

        private void addCustomerMarkerToMap(Customer customer) {
            LatLng latLng = getCustomerLatLongFromAddress(customer.getCity() + ", " + customer.getCountry());
            if (latLng != null) {
                FioriMarkerOptions customerMarker = new FioriMarkerOptions.Builder()
                        .tag(customer)
                        .point(new FioriPoint(latLng.latitude, latLng.longitude))
                        .title(customer.getFirstName() + " " + customer.getLastName())
                        .legendTitle("Customer")
                        .build();
                mActionProvider.addMarker(customerMarker);
                markers.put(customer.getCity() + ", " + customer.getCountry(), customerMarker);

                Point stationLocation = new Point(latLng.longitude, latLng.latitude, SpatialReferences.getWgs84());
                mEsriFioriMapView.getMapView().setViewpointCenterAsync(stationLocation);
            }
        }

        private void addCustomersToMap() {
            DataQuery query = new DataQuery()
                    .from(ESPMContainerMetadata.EntitySets.customers)
                    .where(Customer.country.equal("US")
                            .or(Customer.country.equal("CA"))
                            .or(Customer.country.equal("MX")));
            SAPServiceManager sapServiceManager = ((SAPWizardApplication) getApplication()).getSAPServiceManager();
            ESPMContainer espmContainer = sapServiceManager.getESPMContainer();
            espmContainer.getCustomersAsync(query, (List<Customer> customers) -> {
                for (Customer customer : customers) {
                    addCustomerMarkerToMap(customer);
                    addresses.add(customer.getCity() + ", " + customer.getCountry());
                }
                mActionProvider.doExtentsAction();
            }, (RuntimeException re) -> {
                Log.d("", "An error occurred during async query:  " + re.getMessage());
            });
        }
    }
    ```

9.  Press **Shift** twice and type **`activity_customers_fiori_esri_map.xml`** to open `activity_customers_fiori_esri_map.xml`.

10.  Replace its contents with the following code:

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:context=".mdui.customers.CustomersFioriEsriMapActivity">

        <com.sap.cloud.mobile.fiori.maps.esri.EsriFioriMapView
            android:id="@+id/esriFioriView"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            app:initialLatitude="45.5017"
            app:initialLongitude="-73.5673"
            >
        </com.sap.cloud.mobile.fiori.maps.esri.EsriFioriMapView>

    </FrameLayout>
    ```

11.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`EntitySetListActivity`** to open `EntitySetListActivity.java`.

12.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersFioriMapActivity.class`**.

13.  Replace `CustomersFioriMapActivity.class` with **`CustomersFioriEsriMapActivity.class`** so that when the user taps on **Customers**, the app will navigate to the newly added activity with the Fiori-Esri map on it.

14.  On Windows, press **`Ctrl+Shift+N`**, or, on a Mac, press **`command+Shift+O`**, and type **`AndroidManifest`** to open `AndroidManifest.xml`.

15.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersFioriEsriMapActivity`**.

16.  Modify the activity so that it specifies the `NoActionBar` theme, which will cause the activity to not display an action bar.

    ```XML
    <activity android:name=".mdui.customers.CustomersFioriEsriMapActivity"
        android:theme="@style/AppTheme.NoActionBar">
    </activity>
    ```

17.  Run the app.

    You should be able to see markers on the screen that represent customers displayed on an `Esri` map.

    !![Esri Fiori Map](fiori-esri-map.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Add the following permissions to the `AndroidManifest.xml` file before the application section.

    ```XML
    <uses-permission android:name="android.permission.INTERNET" />
    <uses-feature android:glEsVersion="0x00020000" android:required="true" />
    ```

2.  Add the following to the `ext.repos` section of the project's `build.gradle` file:

    ```Gradle
    maven {
        url 'https://esri.bintray.com/arcgis'
    }
    ```

3.  Add the following dependency to the app's `build.gradle` file and click **Sync Now**.

    ```Gradle
    implementation group: 'com.sap.cloud.android', name: 'esri-maps', version: sdkVersion
    ```

4.  In Android Studio, using the **Project Explorer**, navigate to **`app > java > com.sap.wizapp > mdui > customers`**.

5.  Right-click and choose **`New > Activity > Empty Activity`**.

6.  Set **Activity Name** to be **`CustomersFioriEsriMapActivity`**.

7.  Click **Finish**.

8.  Replace the file contents in the newly created `CustomersFioriEsriMapActivity.kt` with the following code:

    ```Kotlin
    package com.sap.wizapp.mdui.customers

    import android.app.SearchManager
    import android.content.Context
    import android.location.Geocoder
    import android.os.Bundle
    import android.util.Log
    import android.view.LayoutInflater
    import android.view.inputmethod.InputMethodManager
    import android.widget.ArrayAdapter
    import android.widget.ImageButton

    import com.esri.arcgisruntime.geometry.Point
    import com.esri.arcgisruntime.geometry.SpatialReferences
    import com.google.android.gms.maps.model.LatLng
    import com.sap.cloud.android.odata.espmcontainer.Customer
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainerMetadata
    import com.sap.cloud.mobile.fiori.maps.FioriMapSearchView
    import com.sap.cloud.mobile.fiori.maps.FioriMarkerOptions
    import com.sap.cloud.mobile.fiori.maps.FioriPoint
    import com.sap.cloud.mobile.fiori.maps.LegendButton
    import com.sap.cloud.mobile.fiori.maps.LocationButton
    import com.sap.cloud.mobile.fiori.maps.SettingsButton
    import com.sap.cloud.mobile.fiori.maps.ZoomExtentButton
    import com.sap.cloud.mobile.fiori.maps.esri.EsriFioriMapView
    import com.sap.cloud.mobile.fiori.maps.esri.EsriMapActionProvider
    import com.sap.cloud.mobile.odata.DataQuery
    import com.sap.wizapp.R
    import com.sap.wizapp.app.SAPWizardApplication

    import java.io.IOException
    import kotlin.collections.HashMap

    import androidx.appcompat.app.AppCompatActivity

    class CustomersFioriEsriMapActivity : AppCompatActivity() {
        private lateinit var mEsriFioriMapView: EsriFioriMapView
        private val locations = HashMap<String, LatLng>() // Used for demo purposes to speed up the process of converting an address to lat, long
        private val markers = HashMap<String, FioriMarkerOptions>() // Used to associate an address with a marker for search
        private val addresses = arrayListOf<String>() // Used to populate the list of addresses that are searchable

        private lateinit var mActionProvider: EsriMapActionProvider

        override fun onCreate(savedInstanceState: Bundle?) {
            super.onCreate(savedInstanceState)
            setContentView(R.layout.activity_customers_fiori_esri_map)
            mEsriFioriMapView = findViewById(R.id.esriFioriView)

            mActionProvider = EsriMapActionProvider(mEsriFioriMapView, this)
            // For demo purposes, speed up the lookup of address details.
            // Will use Geocoder to translate an address to a LatLng if address is not in this list
            locations["Wilmington, Delaware, US"] = LatLng(39.744655, -75.5483909)
            locations["Antioch, Illinois, US"] = LatLng(42.4772418, -88.0956396)
            locations["Santa Clara, California, US"] = LatLng(37.354107899999995, -121.9552356)
            locations["Hermosillo, MX"] = LatLng(29.0729673, -110.9559192)
            locations["Bismarck, North Dakota, US"] = LatLng(46.808326799999996, -100.7837392)
            locations["Ottawa, CA"] = LatLng(45.4215296, -75.69719309999999)
            locations["México, MX"] = LatLng(23.634501, -102.55278399999999)
            locations["Boca Raton, Florida, US"] = LatLng(26.368306399999998, -80.1289321)
            locations["Carrollton, Texas, US"] = LatLng(32.9756415, -96.8899636)
            locations["Lombard, Illinois, US"] = LatLng(41.8800296, -88.00784349999999)
            locations["Moorestown, US"] = LatLng(39.9688817, -74.948886)
            addCustomersToMap()

            // Setup toolbar buttons and add to the view.
            val settingsButton = SettingsButton(mEsriFioriMapView.toolbar.context)
            val legendButton = LegendButton(mEsriFioriMapView.toolbar.context)
            val locationButton = LocationButton(mEsriFioriMapView.toolbar.context)
            val extentButton = ZoomExtentButton(mEsriFioriMapView.toolbar.context)
            val buttons = arrayOf<ImageButton>(settingsButton, legendButton, locationButton, extentButton)
            mEsriFioriMapView.toolbar.addButtons(buttons.asList())

            // Setup draggable bottom panel
            val inflater = getSystemService(Context.LAYOUT_INFLATER_SERVICE) as LayoutInflater
            val detailView = inflater.inflate(R.layout.detail_panel, null)
            mEsriFioriMapView.setDefaultPanelContent(detailView)

            val mFioriMapSearchView = findViewById<FioriMapSearchView>(R.id.fiori_map_search_view)
            mFioriMapSearchView?.let {
                val searchManager = getSystemService(Context.SEARCH_SERVICE) as SearchManager
                it.setSearchableInfo(searchManager.getSearchableInfo(componentName))
                it.setAdapter(ArrayAdapter<String>(this@CustomersFioriEsriMapActivity, R.layout.search_auto_complete, R.id.search_auto_complete_text, addresses))
                it.setThreshold(2)
                it.setOnItemClickListener{ parent, view, position, id ->
                    it.setQuery(parent.getItemAtPosition(position).toString(), false)
                    searchResultSelected(parent.getItemAtPosition(position) as String)
                    val inputMethodManager = getSystemService(INPUT_METHOD_SERVICE) as InputMethodManager
                    inputMethodManager.hideSoftInputFromWindow(it.windowToken, 0) }
            }
        }

        /**
         * Manipulates the map once available.
         * This callback is triggered when the map is ready to be used.
         * This is where we can add markers or lines, add listeners, or move the camera. In this case,
         * we just add a marker near Toronto, Canada.
         */

        private fun searchResultSelected(selectedSearchResult: String) {
            locations[selectedSearchResult]?.let { latLng ->
                val stationLocation = Point(latLng.longitude, latLng.latitude, SpatialReferences.getWgs84())
                mEsriFioriMapView.mapView.setViewpointCenterAsync(stationLocation)
                // Select the marker (or cluster the marker is in).
                mActionProvider.selectMarker(markers[selectedSearchResult])
            }
        }

        private fun getCustomerLatLongFromAddress(address: String): LatLng? {
            locations[address]?.let {
                return it
            }

            // String strAddress = "Wilmington, Delaware";
            val coder = Geocoder(this)

            try {
                // May throw an IOException
                val addresses = coder.getFromLocationName(address, 5)
                if (addresses.isNullOrEmpty()) {
                    return null
                }

                val location = addresses[0]
                return LatLng(location.latitude, location.longitude)
            }
            catch (ex: IOException) {
                ex.printStackTrace()
                return null
            }
        }

        private fun addCustomerMarkerToMap(customer: Customer) {
            val latLng = getCustomerLatLongFromAddress(customer.city + ", " + customer.country)
            latLng?.let {
                val customerMarker = FioriMarkerOptions.Builder()
                        .tag(customer)
                        .point(FioriPoint(latLng.latitude, latLng.longitude))
                        .title(customer.firstName + " " + customer.lastName)
                        .legendTitle("Customer")
                        .build()
                mActionProvider.addMarker(customerMarker)
                markers[customer.city + ", " + customer.country] = customerMarker
                val stationLocation = Point(latLng.longitude, latLng.latitude, SpatialReferences.getWgs84())
                mEsriFioriMapView.mapView.setViewpointCenterAsync(stationLocation)
            }
        }

        private fun addCustomersToMap() {
            val query = DataQuery()
                    .from(ESPMContainerMetadata.EntitySets.customers)
                    .where(Customer.country.equal("US")
                            .or(Customer.country.equal("CA"))
                            .or(Customer.country.equal("MX")))
            val sapServiceManager = (application as SAPWizardApplication).sapServiceManager
            val espmContainer = sapServiceManager?.eSPMContainer
            espmContainer?.getCustomersAsync(query, { customers: List<Customer> ->
                for (customer in customers) {
                    addCustomerMarkerToMap(customer)
                    addresses.add(customer.city + ", " + customer.country)
                }
                mActionProvider.doExtentsAction() }, { re:RuntimeException-> Log.d("", "An error occurred during async query: " + re.message) })
        }
    }
    ```

9.  Press **Shift** twice and type **`activity_customers_fiori_esri_map.xml`** to open `activity_customers_fiori_esri_map.xml`.

10.  Replace its contents with the following code:

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:context=".mdui.customers.CustomersFioriEsriMapActivity">

        <com.sap.cloud.mobile.fiori.maps.esri.EsriFioriMapView
            android:id="@+id/esriFioriView"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            app:initialLatitude="45.5017"
            app:initialLongitude="-73.5673"
            >
        </com.sap.cloud.mobile.fiori.maps.esri.EsriFioriMapView>

    </FrameLayout>
    ```

11.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`EntitySetListActivity`** to open `EntitySetListActivity.kt`.

12.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersFioriMapActivity::class`**.

13.  Replace `CustomersFioriMapActivity::class` with **`CustomersFioriEsriMapActivity::class`** so that when the user taps on **Customers**, the app will navigate to the newly added activity with the Fiori-Esri map on it.

14.  On Windows, press **`Ctrl+Shift+N`**, or, on a Mac, press **`command+Shift+O`**, and type **`AndroidManifest`** to open `AndroidManifest.xml`.

15.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for **`CustomersFioriEsriMapActivity`**.

16.  Modify the activity so that it specifies the `NoActionBar` theme, which will cause the activity to not display an action bar.

    ```XML
    <activity android:name=".mdui.customers.CustomersFioriEsriMapActivity"
        android:theme="@style/AppTheme.NoActionBar">
    </activity>
    ```

17.  Run the app.

    You should be able to see markers on the screen that represent customers displayed on an `Esri` map.

    !![Esri Fiori Map](fiori-esri-map.png)

[OPTION END]

>Because `Esri` maps use a native library, and some Android devices have 64-bit processors, these devices generate and check the `arm64` folder to load a native library. So, if you cannot run the app properly in this step, try the following solution:

>1.  Add the following filters to the `defaultConfig` section in your app module's `build.gradle` file. When your device tries to run your app, it will not generate any folders and will use the existing native library.
    ```Gradle
    ndk {
        abiFilters "armeabi-v7a", "x86", "armeabi", "mips"
    }
    ```

>2.  Add the following code to your `gradle.properties` (Project Properties) to use the deprecated `ndk`.
    ```Gradle
    android.useDeprecatedNdk=true
    ```

Congratulations. You have created an activity that makes use of the Fiori map control.

[DONE]
[ACCORDION-END]

---
