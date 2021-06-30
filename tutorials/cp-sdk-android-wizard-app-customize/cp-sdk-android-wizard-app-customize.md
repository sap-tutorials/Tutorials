---
author_name: Bruce Meng
author_profile: https://github.com/flyingfish162
title: Customize the Wizard Generated Application
description: Customize a wizard-generated application and learn how to use Fiori object cells, search UI, and the collection view.
primary_tag: products>sap-btp-sdk-for-android
auto_validation: true
tags: [  tutorial>beginner, operating-system>android, topic>mobile, products>sap-btp-sdk-for-android, products>sap-business-technology-platform ]
time: 60
---

## Prerequisites
- You completed [Try Out the SAP BTP SDK Wizard for Android](cp-sdk-android-wizard-app).


## Details
### You will learn
- How to customize the values displayed in an object cell
- How to modify the navigation between screens
- How to change menu options
- How to add a Fiori search UI enabling the filtering of object cells on a list screen
- How to add a collection view showing the top products

---

[ACCORDION-BEGIN [Step 1: ](Examine the products list screen)]

1.  Run the previously created project.

2.  Tap the **Products** entity.

    !![Entities screen](entities-screen.png)

    Notice that it displays the category name rather than the product name.

    !![Original Products Screen](original-products.png)

    The category name is displayed (rather than the product name) because the app was generated from the OData service's metadata, which does not indicate which of the many fields from the product entity to display. When creating the sample user interface, the SDK wizard uses the first property found as the value to display. To view the complete metadata document, open the `res/raw/com_sap_edm_sampleservice_v2.xml` file.

    !![Product metadata](product-metadata.png)

    Each product is displayed in an [object cell](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/android/object-cell.html), which is one of the Fiori UI for Android controls.

    !![object cell](object-cell.png)

    As seen above, an object cell is used to display information about an entity.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Update the products list screen)]

In this section, you will configure the object cell to display a product's name, category, description, and price.

[OPTION BEGIN [Java]]

1.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductsListFragment`**, to open `ProductsListFragment.java`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`populateObjectCell`**, to move to the `populateObjectCell` method. Change the parameter in the first line of the method in **`getDataValue`** from **`Product.category`** to **`Product.name`**. This will cause the product name to be shown as the headline value of the object cell:

    ```Java
    DataValue dataValue = productEntity.getDataValue(Product.name);
    ```

3.  At the end of the `populateObjectCell` method, find the lines that set the `subheadline`, `footnote`, and `icon`, as shown below.

    ```Java
    viewHolder.objectCell.setSubheadline("Subheadline goes here");
    viewHolder.objectCell.setFootnote("Footnote goes here");
    if (masterPropertyValue == null || masterPropertyValue.isEmpty()) {
        viewHolder.objectCell.setIcon("?", 0);
    } else {
        viewHolder.objectCell.setIcon(masterPropertyValue.substring(0,1), 0);
    }
    viewHolder.objectCell.setIcon(R.drawable.default_dot, 1, R.string.attachment_item_content_desc);
    viewHolder.objectCell.setIcon("!", 2);
    ```

4.  Replace these lines with the following code, which will display category, description, and price.

    ```Java
    dataValue = productEntity.getDataValue(Product.category);

    if (dataValue != null) {
        viewHolder.objectCell.setSubheadline(dataValue.toString());
    }
    dataValue = productEntity.getDataValue(Product.shortDescription);
    if (dataValue != null) {
        viewHolder.objectCell.setFootnote(dataValue.toString());
    }

    dataValue = productEntity.getDataValue(Product.price);
    if (dataValue != null) {
        viewHolder.objectCell.setStatusWidth(200);
        viewHolder.objectCell.setStatus("$ " + dataValue.toString(), 1);
    }
    ```

5.  On Windows press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`** and type **`onViewStateRestored`**, to move to the `onViewStateRestored` method.

6.  After the `if (recyclerView == null)` line, paste the following code, which adds a divider between product items.

    ```Java
    LinearLayoutManager linearLayoutManager = new LinearLayoutManager(currentActivity);
    DividerItemDecoration dividerItemDecoration = new DividerItemDecoration(recyclerView.getContext(), linearLayoutManager.getOrientation());
    recyclerView.addItemDecoration(dividerItemDecoration);
    recyclerView.setLayoutManager(linearLayoutManager);
    ```

    If classes `LinearLayoutManager` and `DividerItemDecoration` appear red, this indicates that Android Studio could not locate the classes. Select each class and on Windows press **`Alt+Enter`**, or, on a Mac, press **`option+return`** to make use of Android Studio quick fix to add the missing imports.

    An alternate option is to enable the below setting. (Windows: **Settings**, Mac: **Android Studio > Preferences...**)

    !![Add unambiguous imports on the fly](auto-import-java.png)

7.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`** and type **`Repository`**, to open `Repository.java`.

8.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`** and type **`initialRead`**, to move to the `initialRead` method.

9.  Below the line that calls the **`orderBy`** method of **`dataQuery`**, add the following to specify that the sort order be by category and then by name for products.

    ```Java
    if (entitySet.getEntityType() == ESPMContainerMetadata.EntityTypes.product) {
        dataQuery.thenBy(Product.name, SortOrder.ASCENDING);
    }
    ```

    After the change the code should appear as shown below.

    ```Java
    if (orderByProperty != null) {
        dataQuery = dataQuery.orderBy(orderByProperty, SortOrder.ASCENDING);
        if (entitySet.getEntityType() == ESPMContainerMetadata.EntityTypes.product) {
            dataQuery.thenBy(Product.name, SortOrder.ASCENDING);
        }
    }
    ```

10.  Re-run (quit first) the app and notice that the **Products** screen has been formatted to show the product's name, category, description, and price and the entries are now sorted by category and then name.

    !![Nicely formatted product list](reformatted-product-list.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  In Android Studio, on Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductsListFragment`**, to open `ProductsListFragment.kt`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`populateObjectCell`**, to move to the `populateObjectCell` method. Change the parameter in the first line of the method in **`getDataValue`** from **`Product.category`** to **`Product.name`**. This will cause the product name to be shown as the headline value of the object cell:

    ```Kotlin
    val dataValue = productEntity.getDataValue(Product.name)
    ```

3.  Replace the `viewHolder.objectCell.apply` block with the following code, which will display category, description, and price.

    ```Kotlin
    viewHolder.objectCell.apply {
      headline = masterPropertyValue
      detailImage = null

      (productEntity.getDataValue(Product.category))?.let {
        subheadline = it.toString()
      }

      (productEntity.getDataValue(Product.shortDescription))?.let {
        footnote = it.toString()
      }

      (productEntity.getDataValue(Product.price))?.let {
        statusWidth = 200
        setStatus("$ $it", 1)
      }
    }
    ```

4.  On Windows press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`** and type **`onViewStateRestored`**, to move to the `onViewStateRestored` method.

5.  Replace `(currentActivity.findViewById<RecyclerView>(R.id.item_list))?.let` block with the following code, which adds a divider between product items.

    ```Kotlin
    (currentActivity.findViewById<RecyclerView>(R.id.item_list))?.let {
      val linearLayoutManager = LinearLayoutManager(currentActivity)
      val dividerItemDecoration = DividerItemDecoration(it.context, linearLayoutManager.orientation)
      it.addItemDecoration(dividerItemDecoration)
      it.layoutManager = linearLayoutManager
      this.adapter = ProductListAdapter(currentActivity, it)
      it.adapter = this.adapter
    } ?: throw AssertionError()
    ```

    If classes `LinearLayoutManager` and `DividerItemDecoration` appear red, this indicates that Android Studio could not locate the classes. Select each class and on Windows press **`Alt+Enter`**, or, on a Mac, press **`option+return`** to make use of Android Studio quick fix to add the missing imports.

    An alternate option is to enable the below setting. (Windows: **Settings**, Mac: **Android Studio > Preferences...**)

    !![Add unambiguous imports on the fly](auto-import-kotlin.png)

6.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`Repository`**, to open `Repository.kt`.

7.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`initialRead`**, to move to the `initialRead` method.

8.  Replace `if (orderByProperty != null)` block with the following code to specify that the sort order be by category and then by name for products.

    ```Kotlin
    orderByProperty?.let {
        dataQuery = dataQuery.orderBy(it, SortOrder.ASCENDING)
        if (entitySet.entityType == ESPMContainerMetadata.EntityTypes.product) {
            dataQuery.thenBy(Product.name, SortOrder.ASCENDING)
        }
    }
    ```

9.  Re-run (quit first) the app and notice that the **Products** screen has been formatted to show the product's name, category, description, and price and the entries are now sorted by category and then name.

    !![Nicely formatted product list](reformatted-product-list.png)

[OPTION END]

[VALIDATE_2]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Customize the ProductCategories screen)]

Examine the **`ProductCategories`** screen.

!![Original product categories screen](original-product-categories.png)

In this section, you will update the screen's title, configure the object cell to show the category name, main category name, add the number of products in a category, and add a separator decoration between cells.

[OPTION BEGIN [Java]]

1.  Press **`Shift`** twice and type **`strings.xml`** to open `res/values/strings.xml`.

2.  Add the following entry:

    ```XML
    <string name="product_categories_title">Product Categories</string>
    ```

3.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductCategoriesListFragment`**, to open `ProductCategoriesListFragment.java`.

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onViewStateRestored`**, to move to the `onViewStateRestored` method, find the `currentActivity.setTitle(activityTitle)` line.

5.  On Windows, press **`Ctrl+/`**, or, on a Mac, press **`command+/`**, to comment out the line.

6.  Add the following line right after the line to set the screen's title:

    ```Java
    currentActivity.setTitle(getResources().getString(R.string.product_categories_title));
    ```

7.  Still in this method, after the `if (recyclerView == null)` line, paste the following code that adds a divider between categories:

    ```Java
    LinearLayoutManager linearLayoutManager = new LinearLayoutManager(currentActivity);
    DividerItemDecoration dividerItemDecoration = new DividerItemDecoration(recyclerView.getContext(), linearLayoutManager.getOrientation());
    recyclerView.addItemDecoration(dividerItemDecoration);
    recyclerView.setLayoutManager(linearLayoutManager);
    ```

8.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`populateObjectCell`** to move to the `populateObjectCell` method.

9.  At the end of the method, find the following lines that set the **subheadline**, **footnote**, and **icon**.

    ```Java
    viewHolder.objectCell.setSubheadline("Subheadline goes here");
    viewHolder.objectCell.setFootnote("Footnote goes here");
    if (masterPropertyValue == null || masterPropertyValue.isEmpty()) {
        viewHolder.objectCell.setIcon("?", 0);
    } else {
        viewHolder.objectCell.setIcon(masterPropertyValue.substring(0, 1), 0);
    }
    viewHolder.objectCell.setIcon(R.drawable.default_dot, 1, R.string.attachment_item_content_desc);
    viewHolder.objectCell.setIcon("!", 2);
    ```

10.  Replace these lines with the following, to display the main category instead, hide the footnote, and show the number of products per category.

    ```Java
    dataValue = productCategoryEntity.getDataValue(ProductCategory.mainCategoryName);
    if (dataValue != null) {
        viewHolder.objectCell.setSubheadline(dataValue.toString());
    }

    viewHolder.objectCell.setLines(2);  //Not using footnote

    dataValue = productCategoryEntity.getDataValue(ProductCategory.numberOfProducts);
    if (dataValue != null) {
        viewHolder.objectCell.setStatusWidth(220);
        viewHolder.objectCell.setStatus(dataValue.toString() + " Products", 1);
    }
    ```

11.  Run the app again and notice that the **title**, **subheadline**, and **status** are now displayed and the **icon** and **footnote** are no longer shown.

    !![Modified ProductCategories Screen](modified-product-categories.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Press **`Shift`** twice and type **`strings.xml`** to open `res/values/strings.xml`.

2.  Add the following entry:

    ```XML
    <string name="product_categories_title">Product Categories</string>
    ```

3.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductCategoriesListFragment`**, to open `ProductCategoriesListFragment.kt`.

4.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onViewStateRestored`**, to move to the `onViewStateRestored` method, find the `currentActivity.setTitle(activityTitle)` line.

5.  On Windows, press **`Ctrl+/`**, or, on a Mac, press **`command+/`**, to comment out the line.

6.  Add the following line right after the line to set the screen's title:

    ```Kotlin
    currentActivity.title = resources.getString(R.string.product_categories_title)
    ```

7.  Still in this method, replace the `(currentActivity.findViewById<RecyclerView>(R.id.item_list))?.let` block with the following code, which adds a divider between categories:

    ```Kotlin
    (currentActivity.findViewById<RecyclerView>(R.id.item_list))?.let {
      val linearLayoutManager = LinearLayoutManager(currentActivity)
      val dividerItemDecoration = DividerItemDecoration(it.context, linearLayoutManager.orientation)
      it.addItemDecoration(dividerItemDecoration)
      it.layoutManager = linearLayoutManager
      this.adapter = ProductCategoryListAdapter(currentActivity, it)
      it.adapter = this.adapter
    } ?: throw AssertionError()
    ```

8.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`populateObjectCell`**, to move to the `populateObjectCell` method.

9.  Replace the `viewHolder.objectCell.apply` block with the following to display the main category instead, hide the footnote, and show the number of products per category.

    ```Kotlin
    viewHolder.objectCell.apply {
      headline = masterPropertyValue
      detailImage = null

      (productCategoryEntity.getDataValue(ProductCategory.mainCategoryName))?.let {
        subheadline = it.toString()
      }

      lines = 2  //Not using footnote

      (productCategoryEntity.getDataValue(ProductCategory.numberOfProducts))?.let {
        statusWidth = 220
        setStatus("$it Products", 1)
      }
    }
    ```

10.  Run the app again and notice that the **title**, **subheadline**, and **status** are now displayed and the **icon** and **footnote** are no longer shown.

    !![Modified ProductCategories Screen](modified-product-categories.png)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Customize the navigation)]

In this section, you will modify the app to initially show the **Product Categories** screen when opened. Selecting a category will navigate to a **Products** screen for the selected category. The floating action button on the **Categories** screen will be removed.

[OPTION BEGIN [Java]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`MainBusinessActivity`**, to open `MainBusinessActivity.java`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`startEntitySetListActivity`**, to move to the `startEntitySetListActivity` method.

3.  Add the following line below the other Intent declaration:

    ```Java
    Intent pcIntent = new Intent(this, ProductCategoriesActivity.class);
    ```

4.  After the call to `startActivity(intent)`, add the following line:

    ```Java
    startActivity(pcIntent);
    ```

    This will cause the **Product Categories** screen to be the first screen seen when opening the app, but because the **EntityList** screen is opened first, it can be navigated to by pressing the **Back** button. The **EntityList** screen contains the **Settings** menu so, to simplify things, this screen is still displayed.

5.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductCategoriesListFragment`**, to open `ProductCategoriesListFragment.java`.

6.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onViewStateRestored`**, to move to the `onViewStateRestored` method.

7.  Find the following code:

    ```Java
    if (navigationPropertyName != null && parentEntityData != null) {
        floatButton.hide();
    } else {
        floatButton.setOnClickListener((v) -> {
            listener.onFragmentStateChange(UIConstants.EVENT_CREATE_NEW_ITEM, null);
        });
    }
    ```

8.  Replace it with the following code:

    ```Java
    floatButton.hide();
    ```

9.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreateOptionsMenu`**, to move to the `onCreateOptionsMenu` method.

10.  Add the following line below the `inflater.inflate` call, which will remove the **Home** menu from the **Product Categories** screen, which is now the home screen of the app.

    ```Java
    menu.removeItem(R.id.menu_home);
    ```

11.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`setOnClickListener`**, to move to the `setOnClickListener` method.

12.  Replace the code with the following, which will enable the navigation from the Category list screen to the Product list screen.

    ```Java
    holder.view.setOnClickListener(view -> {
        Intent productsIntent = new Intent(currentActivity, ProductsActivity.class);
        productsIntent.putExtra("category", productCategoryEntity.getCategoryName());
        view.getContext().startActivity(productsIntent);
    });
    ```

13.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductsListFragment`**, to open `ProductsListFragment.java`.

14.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for `adapter.setItems(products)`. Replace that line with the following code, which will filter the products list to only show products for a selected category.

    ```Java
    String category = currentActivity.getIntent().getStringExtra("category");
    if (category != null) {
        List<Product> matchingProducts = new ArrayList<>();
        for (Product product : products) {
            if (product.getCategory() != null && product.getCategory().equals(category)) {
                matchingProducts.add(product);
            }
        }
        adapter.setItems(matchingProducts);
    } else {
        adapter.setItems(products);
    }
    ```

15.  Run the app again and notice that the **Product Categories** screen is now the first screen shown, that the **Home** menu is no longer shown, and that selecting a category shows the products list screen, which now displays only products for the selected category.

    !![Product category list screen](reformatted-product-category-list2.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`MainBusinessActivity`**, to open `MainBusinessActivity.kt`.

2.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`startEntitySetListActivity`**, to move to the `startEntitySetListActivity` method.

3.  Add the following line below the other Intent declaration:

    ```Kotlin
    val pcIntent = Intent(this, ProductCategoriesActivity::class.java)
    ```

4.  After the call to `startActivity(intent)`, add the following line:

    ```Kotlin
    startActivity(pcIntent)
    ```

    This will cause the **Product Categories** screen to be the first screen seen when opening the app, but because the **EntityList** screen is opened first, it can be navigated to by pressing the **Back** button. The **EntityList** screen contains the **Settings** menu so, to simplify things, this screen is still displayed.

5.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductCategoriesListFragment`**, to open `ProductCategoriesListFragment.kt`.

6.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onViewStateRestored`**, to move to the `onViewStateRestored` method.

7.  Replace the `(currentActivity.findViewById<FloatingActionButton>(R.id.fab))?.let` block with the following code:

    ```Kotlin
    (currentActivity.findViewById<FloatingActionButton>(R.id.fab))?.hide()
    ```

8.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreateOptionsMenu`**, to move to the `onCreateOptionsMenu` method.

9.  Add the following line below the `inflater.inflate` call, which will remove the **Home** menu from the **Product Categories** screen, which is now the home screen of the app.

    ```Kotlin
    menu.removeItem(R.id.menu_home)
    ```

10.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`setOnClickListener`**, to move to the `setOnClickListener` method.

11.  Replace the code with the following, which will enable the navigation from the Category list screen to the Product list screen.

    ```Kotlin
    holder.view.setOnClickListener { view ->
        val productsIntent = Intent(currentActivity, ProductsActivity::class.java)
        productsIntent.putExtra("category", productCategoryEntity.categoryName)
        view.context.startActivity(productsIntent)
    }
    ```

12.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductsListFragment`**, to open `ProductsListFragment.kt`.

13.  On Windows, press **`Ctrl+F`**, or, on a Mac, press **`command+F`**, and search for `listAdapter.setItems(entityList)`. Replace that line with the following code, which will filter the products list to only show products for a selected category.

    ```Kotlin
    currentActivity.intent.getStringExtra("category")?.let { category ->
        val matchingProducts = arrayListOf<Product>()
        for (product in entityList) {
            product.category?.let {
                if (it == category) {
                  matchingProducts.add(product)
                }
            }
        }
        listAdapter.setItems(matchingProducts)
    } ?: listAdapter.setItems(entityList)
    ```

14.  Run the app again and notice that the **Product Categories** screen is now the first screen shown, that the **Home** menu is no longer shown, and that selecting a category shows the products list screen, which now displays only products for the selected category.

    !![Product category list screen](reformatted-product-category-list2.png)

[OPTION END]

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Add category filtering with a FioriSearchView)]

In this section you will add a search field to `ProductCategoriesListActivity`, enabling a user to filter the results displayed on the product category screen.

[OPTION BEGIN [Java]]

1.  First, right-click the `res/drawable` folder to create a new **Drawable Resource File** **`ic_search_icon.xml`**, and use the following XML content.

    !![Create a new Drawable Resource File](create-new-drawable-resource-file.png)

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <vector xmlns:android="http://schemas.android.com/apk/res/android"
        android:width="24dp"
        android:height="24dp"
        android:viewportWidth="24"
        android:viewportHeight="24">
        <path
            android:fillColor="#FFF"
            android:pathData="M15.5,14h-0.79l-0.28,-0.27C15.41,12.59 16,11.11 16,9.5 16,5.91 13.09,3 9.5,3S3,5.91 3,9.5 5.91,16 9.5,16c1.61,0 3.09,-0.59 4.23,-1.57l0.27,0.28v0.79l5,4.99L20.49,19l-4.99,-5zM9.5,14C7.01,14 5,11.99 5,9.5S7.01,5 9.5,5 14,7.01 14,9.5 11.99,14 9.5,14z"/>
    </vector>
    ```

    The current menu `res/menu/itemlist_menu.xml` is shared among all list screens. We will now use a new XML file for the **Product Categories** screen.

2.  Right-click the `res/menu` folder to add a new **Menu Resource File** named **`product_categories_menu.xml`**, and use the following XML for its contents.

    !![Add a new Menu Resource File](add-new-menu-resource-file.png)

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <menu xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto">

        <item
            android:id="@+id/action_search"
            android:icon="@drawable/ic_search_icon"
            android:title="Search"
            app:actionViewClass="com.sap.cloud.mobile.fiori.search.FioriSearchView"
            app:showAsAction="always|collapseActionView"
            style="@style/FioriSearchView" />

        <item
            android:id="@+id/menu_refresh"
            android:icon="@drawable/ic_menu_refresh"
            app:showAsAction="always"
            android:title="@string/menu_refresh"/>
    </menu>
    ```

3.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductCategoryListAdapter`**, to open the `ProductCategoryListAdapter` class, which is in the `ProductCategoriesListFragment.java` file.

4.  Add the following member to the top of this class.

    ```Java
    private List<ProductCategory> allProductCategories;
    ```

5.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`setItems`**, to move to the `setItems` method.

6.  Add the following to the top of the function:

    ```Java
    if (allProductCategories == null) {
        allProductCategories = new ArrayList<>(currentProductCategories);
    }
    ```

7.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreateOptionsMenu`**, to move to the `onCreateOptionsMenu` method.

8.  Replace the contents of the method with the following code, which uses the new `product_categories_menu` and sets a listener that will filter the list of categories in the list when text is entered in the search view. (Make sure to import all the un-imported classes with **`alt+Enter`** on Windows or **`option+Enter`** on Macs.)

    ```Java
    inflater.inflate(R.menu.product_categories_menu, menu);
    FioriSearchView searchView = (FioriSearchView) menu.findItem(R.id.action_search).getActionView();
    searchView.setBackgroundResource(R.color.transparent);
    // make sure to import androidx.appcompat.widget.SearchView
    searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
      @Override
      public boolean onQueryTextSubmit(String s) {
          return false;
      }

      @Override
      public boolean onQueryTextChange(String newText) {
          if (adapter == null) {
              return false;
          }

          List filteredCategoriesList = new ArrayList();
          if (newText != null && newText.trim().length() > 0) {
              for (int i = 0; i < adapter.allProductCategories.size(); i++) {
                  ProductCategory pc = adapter.allProductCategories.get(i);
                  if (pc.getCategoryName().toLowerCase().contains(newText.toLowerCase())) {
                      filteredCategoriesList.add(pc);
                  }
              }
          }
          else {
              filteredCategoriesList = adapter.allProductCategories;
          }
          adapter.productCategories = filteredCategoriesList;
          return false;
      }
    });
    ```

9.  Run the app again and notice that there is now a search toolbar item.

    !![Filter Categories in action 1](search-view.png)

10.  Try it out: click the **search** item, enter some text, press **`Enter`**, and notice that the product categories that are displayed in the list are now filtered.

    !![Filter Categories in action 2](filter-in-action.png)

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  First, right-click the `res/drawable` folder to create a new **Drawable Resource File** **`ic_search_icon.xml`**, and use the following XML content.

    !![Create a new Drawable Resource File](create-new-drawable-resource-file.png)

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <vector xmlns:android="http://schemas.android.com/apk/res/android"
        android:width="24dp"
        android:height="24dp"
        android:viewportWidth="24"
        android:viewportHeight="24">
        <path
            android:fillColor="#FFF"
            android:pathData="M15.5,14h-0.79l-0.28,-0.27C15.41,12.59 16,11.11 16,9.5 16,5.91 13.09,3 9.5,3S3,5.91 3,9.5 5.91,16 9.5,16c1.61,0 3.09,-0.59 4.23,-1.57l0.27,0.28v0.79l5,4.99L20.49,19l-4.99,-5zM9.5,14C7.01,14 5,11.99 5,9.5S7.01,5 9.5,5 14,7.01 14,9.5 11.99,14 9.5,14z"/>
    </vector>
    ```

    The current menu `res/menu/itemlist_menu.xml` is shared among all list screens. We will now use a new XML file for the **Product Categories** screen.

2.  Right-click the `res/menu` folder to add a new **Menu Resource File** named **`product_categories_menu.xml`**, and use the following XML for its contents.

    !![Add a new Menu Resource File](add-new-menu-resource-file.png)

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <menu xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto">

        <item
            android:id="@+id/action_search"
            android:icon="@drawable/ic_search_icon"
            android:title="Search"
            app:actionViewClass="com.sap.cloud.mobile.fiori.search.FioriSearchView"
            app:showAsAction="always|collapseActionView"
            style="@style/FioriSearchView" />

        <item
            android:id="@+id/menu_refresh"
            android:icon="@drawable/ic_menu_refresh"
            app:showAsAction="always"
            android:title="@string/menu_refresh"/>
    </menu>
    ```

3.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductCategoryListAdapter`**, to open the `ProductCategoryListAdapter` class, which is in the `ProductCategoriesListFragment.kt` file.

4.  Add the following member and methods to the top of this class.

    ```Kotlin
    var allProductCategories = listOf<ProductCategory>()

    fun setProductCategories(productcategories: MutableList<ProductCategory>) {
        productCategories = productcategories
    }
    ```

5.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`setItems`**, to move to the `setItems` method.

6.  Add the following to the top of the function:

    ```Kotlin
    if (allProductCategories.isEmpty()) {
        allProductCategories = currentProductCategories
    }
    ```

7.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreateOptionsMenu`**, to move to the `onCreateOptionsMenu` method.

8.  Replace the contents of the method with the following code, which uses the new `product_categories_menu` and sets a listener that will filter the list of categories in the list when text is entered in the search view. (Make sure to import all the un-imported classes with **`alt+Enter`** on Windows or **`option+Enter`** on Macs.)

    ```Kotlin
    inflater.inflate(R.menu.product_categories_menu, menu)
    val searchView = menu.findItem(R.id.action_search).actionView as FioriSearchView
    searchView.setBackgroundResource(R.color.transparent)
    // make sure to import androidx.appcompat.widget.SearchView
    searchView.setOnQueryTextListener(object: SearchView.OnQueryTextListener {
      override fun onQueryTextSubmit(s: String): Boolean {
          return false
      }

      override fun onQueryTextChange(newText: String): Boolean {
          adapter?.let { adapter ->
            val filteredCategoriesList = mutableListOf<ProductCategory>()
            if (newText.trim().isNotEmpty()) {
                for (i in adapter.allProductCategories.indices) {
                    val pc = adapter.allProductCategories[i]
                    pc.categoryName?.let {
                      if (it.toLowerCase().contains(newText.toLowerCase())) {
                          filteredCategoriesList.add(pc)
                      }
                    }
                }
            }
            else {
                for (i in adapter.allProductCategories.indices) {
                    filteredCategoriesList.add(adapter.allProductCategories[i])
                }
            }
            adapter.setProductCategories(filteredCategoriesList)
            return false
          } ?: return false
      }
    })
    ```

9.  Run the app again and notice that there is now a search toolbar item.

    !![Filter Categories in action 1](search-view.png)

10.  Try it out: click the **search** item, enter some text, press **`Enter`**, and notice that the product categories that are displayed in the list are now filtered.

    !![Filter Categories in action 2](filter-in-action.png)

[OPTION END]

>Further information on the Fiori search UI can be found at [SAP Fiori for Android Design Guidelines](https://experience.sap.com/fiori-design-android/search-2/) and [Fiori Search User Interface](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/android/fiori-search-ui.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add a Top Products section with a CollectionView)]

In this section, you will add a Top Products section to the **Products** screen, which displays the products that have the most sales, as shown below.

!![Collection View on Products Screen](collection-view.png)

First, we'll generate additional sales data in the sample OData service.

[OPTION BEGIN [Java]]

1.  In [SAP Mobile Services cockpit](https://mobile-service-cockpit-web.cfapps.eu10.hana.ondemand.com/), navigate to **Mobile Applications** > **Native/Hybrid** > **com.sap.wizapp** and go to **Mobile Sample OData ESPM**.

    !![Sample OData feature on Mobile Services](sample-odata-feature.png)

2.  Change the **Entity Sets** dropdown to **`SalesOrderItems`** and then click the **generate sample sales orders** icon five times. This will create additional sales order items, which we can use to base our top products on, based on the quantity sold.

    !![Generating Sample Sales Orders on Mobile Services](generate-sample-sales.png)

3.  In Android Studio, on Windows, press **`Ctrl+Shift+N`**, or, on a Mac, press **`command+Shift+O`**, and type **`fragment_entityitem_list`**, to open `fragment_entityitem_list.xml`.

4.  Replace the `fragment_entityitem_list.xml` content with the following code. This adds the `CollectionView` to the **Products** pane when created.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <com.google.android.material.floatingactionbutton.FloatingActionButton
            android:id="@+id/fab"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_gravity="bottom|end"
            android:layout_margin="@dimen/fab_margin"
            android:src="@drawable/ic_add_circle_outline_black_24dp"
            app:tint="@color/colorWhite"
            app:backgroundTint="@color/onboarding_blue_200"
            app:fabSize="normal" />

        <LinearLayout
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:orientation="vertical"
            android:id="@+id/wrapperLayout" >

            <com.sap.cloud.mobile.fiori.object.CollectionView
                app:layout_scrollFlags="scroll|enterAlways"
                xmlns:android="http://schemas.android.com/apk/res/android"
                xmlns:app="http://schemas.android.com/apk/res-auto"
                android:id="@+id/collectionView"
                android:layout_height="wrap_content"
                android:layout_width="match_parent"
                android:layout_below="@id/toolbar"
                android:background="@color/transparent"
                tools:minHeight="200dp">
            </com.sap.cloud.mobile.fiori.object.CollectionView>

            <androidx.swiperefreshlayout.widget.SwipeRefreshLayout
                android:id="@+id/swiperefresh"
                android:layout_width="match_parent"
                android:layout_height="match_parent">

                <androidx.recyclerview.widget.RecyclerView
                    xmlns:android="http://schemas.android.com/apk/res/android"
                    xmlns:tools="http://schemas.android.com/tools"
                    android:id="@+id/item_list"
                    android:name="com.company.collectionview.ItemListFragment"
                    android:layout_width="@dimen/item_width"
                    android:layout_height="match_parent"
                    app:layoutManager="androidx.recyclerview.widget.LinearLayoutManager"
                    tools:context="com.company.collectionview.ItemListActivity" />
            </androidx.swiperefreshlayout.widget.SwipeRefreshLayout>

        </LinearLayout>

    </FrameLayout>
    ```

5.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductsListFragment`**, to open `ProductsListFragment.java`.

6.  Add the following import libraries to the top of the document:

    ```Java
    import android.widget.LinearLayout;
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainer;
    import com.sap.cloud.android.odata.espmcontainer.SalesOrderItem;
    import com.sap.cloud.mobile.fiori.common.FioriItemClickListener;
    import com.sap.cloud.mobile.fiori.object.AbstractEntityCell;
    import com.sap.cloud.mobile.fiori.object.CollectionView;
    import com.sap.cloud.mobile.fiori.object.CollectionViewItem;
    import com.sap.cloud.mobile.odata.DataQuery;

    import java.util.Collections;
    import java.util.Comparator;
    import java.util.HashMap;
    import java.util.Iterator;
    import java.util.LinkedHashMap;
    import java.util.LinkedList;
    import java.util.Map;
    ```

7.  Add the following variables to the top of the `ProductsListFragment` class:

    ```Java
    private List<Product> productList = new ArrayList<>();
    private HashMap<String, Integer> salesList = new HashMap<>();
    private HashMap<String, Product> productTracker = new HashMap<>();
    ```

8.  Add the following methods to the class:

    ```Java
    // Function to sort hashmap by values
    public static HashMap<String, Integer> sortByValue(HashMap<String, Integer> hm) {
        // Create a list from elements of HashMap
        List<Map.Entry<String, Integer>> list = new LinkedList<>(hm.entrySet());

        // Sort the list
        Collections.sort(list, new Comparator<Map.Entry<String, Integer>>() {
            public int compare(Map.Entry<String, Integer> o1, Map.Entry<String, Integer> o2) {
                return (o2.getValue()).compareTo(o1.getValue());
            }
        });

        // Put data from sorted list to linked hashmap
        HashMap<String, Integer> temp = new LinkedHashMap<String, Integer>();
        for (Map.Entry<String, Integer> aa : list) {
            temp.put(aa.getKey(), aa.getValue());
            LOGGER.debug("CollectionView: id = " + aa.getKey() + ", count = " + aa.getValue());
        }
        return temp;
    }

    // Function to query the products
    private void queryProducts() {
        SAPServiceManager sapServiceManager = ((SAPWizardApplication) currentActivity.getApplication()).getSAPServiceManager();
        ESPMContainer espmContainer = sapServiceManager.getESPMContainer();
        DataQuery query = new DataQuery().orderBy(Product.productID);
        LOGGER.debug("CollectionView" + query.toString());
        espmContainer.getProductsAsync(query, (List<Product> queryProducts) -> {
            LOGGER.debug("CollectionView: executed query in onCreate");
            for (Product product : queryProducts) {
                LOGGER.debug("CollectionView" + product.getName() + " : " + product.getProductID() + " : " + product.getPrice());
                productTracker.put(product.getProductID(), product);
            }

            LOGGER.debug("CollectionView: size of topProducts = " + queryProducts.size());
            createTopProductsList();
            CollectionView cv = currentActivity.findViewById(R.id.collectionView);
            createCollectionView(cv);
        }, (RuntimeException re) -> {
            LOGGER.debug("CollectionView: An error occurred during products async query: " + re.getMessage());
        });
    }

    // Function to order product list by the sorted sales list
    private void createTopProductsList() {
        Iterator it = salesList.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry pair = (Map.Entry) it.next();
            productList.add(productTracker.get(pair.getKey()));
            it.remove();
        }
    }

    // Function to set features of the CollectionView
    private void createCollectionView(CollectionView cv) {
        LOGGER.debug("CollectionView: in createCollectionView method");
        cv.setHeader(" Top Products");
        cv.setFooter(" SEE ALL (" + productTracker.size() + ")");

        // If the footer "SEE ALL" is clicked then the Products page will open
        cv.setFooterClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                cv.setVisibility(View.GONE);
            }
        });

        // If any object is clicked in CollectionView then the Product's detail page for that object will open
        cv.setItemClickListener(new FioriItemClickListener() {
            @Override
            public void onClick(@NonNull View view, int position) {
                LOGGER.debug("You clicked on: " + productList.get(position).getName() + "(" + productList.get(position).getProductID() + ")");
                showProductDetailActivity(view.getContext(), UIConstants.OP_READ, productList.get(position));
            }

            @Override
            public void onLongClick(@NonNull View view, int position) {
                Toast.makeText(currentActivity.getApplicationContext(), "You long clicked on: " + position, Toast.LENGTH_SHORT).show();
            }
        });

        CollectionViewAdapter collectionViewAdapter = new CollectionViewAdapter();
        cv.setCollectionViewAdapter(collectionViewAdapter);

        if (getResources().getBoolean(R.bool.two_pane)) {
            refreshLayout = currentActivity.findViewById(R.id.swiperefresh);
            LinearLayout linearLayout = currentActivity.findViewById(R.id.wrapperLayout);
            int height = linearLayout.getHeight() - cv.getHeight();
            refreshLayout.setMinimumHeight(height);
        }
    }

    // Opens the product's detail page activity
    private void showProductDetailActivity(@NonNull Context context, @NonNull String operation, @Nullable Product productEntity) {
        LOGGER.debug("within showProductDetailActivity for " + productEntity.getName());
        boolean isNavigationDisabled = ((ProductsActivity) currentActivity).isNavigationDisabled;
        if (isNavigationDisabled) {
            Toast.makeText(currentActivity, "Please save your changes first...", Toast.LENGTH_LONG).show();
        } else {
            adapter.resetSelected();
            adapter.resetPreviouslyClicked();
            viewModel.setSelectedEntity(productEntity);
            listener.onFragmentStateChange(UIConstants.EVENT_ITEM_CLICKED, productEntity);
        }
    }

    public class CollectionViewAdapter extends CollectionView.CollectionViewAdapter {
        private List<Product> products;

        public CollectionViewAdapter() {
            products = productList;
        }

        @Override
        public void onBindViewHolder(@NonNull CollectionViewItemHolder collectionViewItemHolder, int i) {
            CollectionViewItem cvi = collectionViewItemHolder.collectionViewItem;
            Product prod = products.get(i);
            String productName = prod.getName();

            cvi.setDetailImage(null);
            cvi.setHeadline(productName);
            cvi.setSubheadline(prod.getCategoryName() + "");
            cvi.setImageOutlineShape(AbstractEntityCell.IMAGE_SHAPE_OVAL);

            if (prod.getPictureUrl() == null) {
                // No picture is available, so use a character from the product string as the image thumbnail
                cvi.setDetailImageCharacter(productName.substring(0, 1));
                cvi.setDetailCharacterBackgroundTintList(com.sap.cloud.mobile.fiori.R.color.sap_ui_contact_placeholder_color_1);
            } else {
                SAPServiceManager sapServiceManager = ((SAPWizardApplication)currentActivity.getApplication()).getSAPServiceManager();
                cvi.prepareDetailImageView().setScaleType(ImageView.ScaleType.FIT_CENTER);
                Glide.with(currentActivity.getApplicationContext())
                    .load(EntityMediaResource.getMediaResourceUrl(prod, sapServiceManager.getServiceRoot()))
                    // Import com.bumptech.glide.Glide for RequestOptions()
                    .apply(new RequestOptions().fitCenter())
                    .transition(DrawableTransitionOptions.withCrossFade())
                    .into(cvi.prepareDetailImageView());
            }
        }

        @Override
        public int getItemCount() {
            return products.size();
        }
    }
    ```

9.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`prepareViewModel`**, to move to the `prepareViewModel` method.

10.  Add the following code to the first "else"-block of the method:

    ```Java
    CollectionView cv = currentActivity.findViewById(R.id.collectionView);
    createCollectionView(cv);
    ```

11.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`**, to move to the `onCreate` method.

12.  Add the following lines of code at the end of the method:

    ```Java
    // Get the DataService class, which we will use to query the back-end OData service
    ESPMContainer espmContainer = sapServiceManager.getESPMContainer();

    // Query the SalesOrderItems and order by gross amount received from sales
    // Change the orderBy arguments to SalesOrderItem.property_name to rearrange the CollectionView order of products
    DataQuery dq = new DataQuery().orderBy(SalesOrderItem.productID);
    espmContainer.getSalesOrderItemsAsync(dq, (List<SalesOrderItem> querySales) -> {
        LOGGER.debug("CollectionView: executed sales order query in onCreate");
        if (querySales != null) {
            for (SalesOrderItem sale : querySales) {
                if (salesList.containsKey(sale.getProductID())) {
                    salesList.put(sale.getProductID(), salesList.get(sale.getProductID()).intValue() + sale.getQuantity().intValue());
                } else {
                    salesList.put(sale.getProductID(), sale.getQuantity().intValue());
                }
                LOGGER.debug("CollectionView" + sale.getProductID() + ": " + sale.getQuantity() + ": " + sale.getGrossAmount());
            }

            salesList = sortByValue(salesList);

            LOGGER.debug("CollectionView: salesList size = " + salesList.size());
            queryProducts();
        } else {
            LOGGER.debug("CollectionView: sales query list is null");
        }
    }, (RuntimeException re) -> {
        LOGGER.debug("CollectionView: An error occurred during async sales query: " + re.getMessage());
    });
    ```

13.  Run the app and notice that the **Products** screen now has a component at the top of the screen that allows horizontal scrolling to view the top products. Tap a product to see more details. Alternatively, tap **SEE ALL** to see all the products.

    !![Collection View on Products Screen](collection-view.png)

    >For more details, see [Collection View in SAP Fiori for Android Design Guidelines](https://experience.sap.com/fiori-design-android/collection-view/) and [Collection View](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/android/collection-view.html)

    >For more information on SAP Fiori for Android and the generated app, see [Fiori UI Overview](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/android/overview.html), [SAP Fiori for Android Design Guidelines](https://experience.sap.com/fiori-design-android/explore/), [Fiori UI Demo Application](https://github.com/SAP/cloud-sdk-android-fiori-ui-components) and the `WizardAppReadme.md` file located in the generated app.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  In [SAP Mobile Services cockpit](https://mobile-service-cockpit-web.cfapps.eu10.hana.ondemand.com/), navigate to **Mobile Applications** > **Native/Hybrid** > **com.sap.wizapp** and go to **Mobile Sample OData ESPM**.

    !![Sample OData feature on Mobile Services](sample-odata-feature.png)

2.  Change the **Entity Sets** dropdown to **`SalesOrderItems`** and then click the **generate sample sales orders** icon five times. This will create additional sales order items, which we can use to base our top products on, based on the quantity sold.

    !![Generating Sample Sales Orders on Mobile Services](generate-sample-sales.png)

3.  In Android Studio, on Windows, press **`Ctrl+Shift+N`**, or, on a Mac, press **`command+Shift+O`**, and type **`fragment_entityitem_list`**, to open `fragment_entityitem_list.xml`.

4.  Replace the `fragment_entityitem_list.xml` content with the following code. This adds the `CollectionView` to the **Products** pane when created.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <com.google.android.material.floatingactionbutton.FloatingActionButton
            android:id="@+id/fab"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_gravity="bottom|end"
            android:layout_margin="@dimen/fab_margin"
            android:src="@drawable/ic_add_circle_outline_black_24dp"
            app:tint="@color/colorWhite"
            app:backgroundTint="@color/onboarding_blue_200"
            app:fabSize="normal" />

        <LinearLayout
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:orientation="vertical"
            android:id="@+id/wrapperLayout" >

            <com.sap.cloud.mobile.fiori.object.CollectionView
                app:layout_scrollFlags="scroll|enterAlways"
                xmlns:android="http://schemas.android.com/apk/res/android"
                xmlns:app="http://schemas.android.com/apk/res-auto"
                android:id="@+id/collectionView"
                android:layout_height="wrap_content"
                android:layout_width="match_parent"
                android:layout_below="@id/toolbar"
                android:background="@color/transparent"
                tools:minHeight="200dp">
            </com.sap.cloud.mobile.fiori.object.CollectionView>

            <androidx.swiperefreshlayout.widget.SwipeRefreshLayout
                android:id="@+id/swiperefresh"
                android:layout_width="match_parent"
                android:layout_height="match_parent">

                <androidx.recyclerview.widget.RecyclerView
                    xmlns:android="http://schemas.android.com/apk/res/android"
                    xmlns:tools="http://schemas.android.com/tools"
                    android:id="@+id/item_list"
                    android:name="com.company.collectionview.ItemListFragment"
                    android:layout_width="@dimen/item_width"
                    android:layout_height="match_parent"
                    app:layoutManager="androidx.recyclerview.widget.LinearLayoutManager"
                    tools:context="com.company.collectionview.ItemListActivity" />
            </androidx.swiperefreshlayout.widget.SwipeRefreshLayout>

        </LinearLayout>

    </FrameLayout>
    ```

5.  On Windows, press **`Ctrl+N`**, or, on a Mac, press **`command+O`**, and type **`ProductsListFragment`**, to open `ProductsListFragment.kt`.

6.  Add the following import libraries to the top of the document:

    ```Kotlin
    import android.widget.ListView
    import android.content.Intent
    import android.widget.LinearLayout
    import com.sap.cloud.android.odata.espmcontainer.ESPMContainer
    import com.sap.cloud.android.odata.espmcontainer.SalesOrderItem
    import com.sap.cloud.mobile.fiori.common.FioriItemClickListener
    import com.sap.cloud.mobile.fiori.`object`.AbstractEntityCell
    import com.sap.cloud.mobile.fiori.`object`.CollectionView
    import com.sap.cloud.mobile.fiori.`object`.CollectionViewItem
    import com.sap.cloud.mobile.odata.DataQuery

    import java.util.*
    import kotlin.collections.ArrayList
    import kotlin.collections.HashMap
    import kotlin.collections.LinkedHashMap
    ```

7.  Add the following variables to the top of the `ProductsListFragment` class:

    ```Kotlin
    private val productList = arrayListOf<Product>()
    private var salesList = HashMap<String, Int>()
    private val productTracker = HashMap<String, Product>()
    ```

8.  On Windows, press **`ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`resetSelected`**, to move to the `resetSelected` method.

9. Change the modifier from `private` to `internal`

10. Do the same to `resetPreviouslyClicked` method.

11.  Add the following method to the `companion object` section:

    ```Kotlin
    // Function to sort hashmap by values
    fun sortByValue(hashmap: HashMap<String, Int>): HashMap<String, Int> {
        // Create a list from elements of HashMap
        val list: List<Map.Entry<String, Int>> = LinkedList<Map.Entry<String, Int>>(hashmap.entries)

        // Sort the list
        Collections.sort(list) {
            o1, o2 -> o2.value.compareTo(o1.value)
        }

        // Put data from sorted list into the linked hashmap
        val temp: HashMap<String, Int> = LinkedHashMap<String, Int>()
        for ((key, value) in list) {
            temp[key] = value
            LOGGER.debug("CollectionView: id = $key, count = $value")
        }
        return temp
    }

    ```

12.  Add the following methods to the class:

    ```Kotlin
    // Function to query the products
    private fun queryProducts() {
        val sapServiceManager = (currentActivity.application as SAPWizardApplication).sapServiceManager
        val query = DataQuery().orderBy(Product.productID)
        LOGGER.debug("CollectionView $query")
        val espmContainer = sapServiceManager?.eSPMContainer
        espmContainer?.let {
            it.getProductsAsync(query, {queryProducts: List<Product> ->
                LOGGER.debug("CollectionView: executed query in onCreate")
                for (product in queryProducts) {
                    LOGGER.debug("CollectionView ${product.name} : ${product.productID} : ${product.price}")
                    productTracker[product.productID] = product
                }

                LOGGER.debug("CollectionView: size of topProducts = ${queryProducts.size}")
                createTopProductsList()
                val cv: CollectionView = currentActivity.findViewById(R.id.collectionView)
                createCollectionView(cv)
            }, {re: RuntimeException -> LOGGER.debug("CollectionView: An error occurred during products async query: ${re.message}")
            })
        }
    }

    // Function to order product list by the sorted sales list
    private fun createTopProductsList() {
      for ((key, value) in salesList) {
        productList.add(productTracker[key]!!)
      }
    }

    // Function to set features of the CollectionView
    private fun createCollectionView(cv: CollectionView) {
        LOGGER.debug("CollectionView: in createCollectionView method")
        cv.apply {
          setHeader(" Top Products")
          setFooter(" SEE ALL (${productTracker.size})")

          // If the footer "SEE ALL" is clicked then the Products page will open
          setFooterClickListener {
              visibility = View.GONE
          }

          // If any object is clicked in CollectionView then the Product's detail page for that object will open
          setItemClickListener(object: FioriItemClickListener {
              override fun onClick(view: View, position: Int) {
                  LOGGER.debug("You clicked on: ${productList[position].name}(${productList[position].productID})")
                  showProductDetailActivity(view.context, UIConstants.OP_READ, productList[position])
              }

              override fun onLongClick(view: View, position: Int) {
                  Toast.makeText(currentActivity.applicationContext, "You long clicked on: $position", Toast.LENGTH_SHORT).show()
              }
          })

          val collectionViewAdapter = CollectionViewAdapter(currentActivity, productList.toList())
          setCollectionViewAdapter(collectionViewAdapter)
        }

        if (resources.getBoolean(R.bool.two_pane)) {
            refreshLayout = currentActivity.findViewById(R.id.swiperefresh)
            val linearLayout = currentActivity.findViewById<LinearLayout>(R.id.wrapperLayout)
            val height = linearLayout.height - cv.height
            refreshLayout.minimumHeight = height
        }
    }

    // Opens the product's detail page activity
    private fun showProductDetailActivity(context: Context, operation: String, productEntity: Product?) {
        productEntity?.let {
          LOGGER.debug("within showProductDetailActivity for ${it.name}")
          val isNavigationDisabled = (currentActivity as ProductsActivity).isNavigationDisabled
          if (isNavigationDisabled) {
              Toast.makeText(currentActivity, "Please save your changes first...", Toast.LENGTH_LONG).show()
          } else {
              adapter?.resetSelected()
              adapter?.resetPreviouslyClicked()
              viewModel.setSelectedEntity(it)
              listener?.onFragmentStateChange(UIConstants.EVENT_ITEM_CLICKED, it)
          }
        }
    }

    private class CollectionViewAdapter(activity: FragmentActivity, productList: List<Product>) : CollectionView.CollectionViewAdapter() {
        private val products: List<Product>
        private val currentActivity: FragmentActivity

        override fun onBindViewHolder(collectionViewItemHolder: CollectionViewItemHolder, i: Int) {
            val cvi: CollectionViewItem = collectionViewItemHolder.collectionViewItem
            val prod = products[i]
            val productName = prod.name
            cvi.apply {
                detailImage = null
                headline = productName
                subheadline = prod.categoryName + ""
                imageOutlineShape = AbstractEntityCell.IMAGE_SHAPE_OVAL
                prod.pictureUrl?.let {
                    val sapServiceManager = (currentActivity.application as SAPWizardApplication).sapServiceManager
                    prepareDetailImageView().scaleType = ImageView.ScaleType.FIT_CENTER
                    sapServiceManager?.let {sapServiceManager ->
                        Glide.with(currentActivity.applicationContext)
                                .load(EntityMediaResource.getMediaResourceUrl(prod, sapServiceManager.serviceRoot)) // Import com.bumptech.glide.Glide for RequestOptions()
                                .apply(RequestOptions().fitCenter())
                                .transition(DrawableTransitionOptions.withCrossFade())
                                .into(prepareDetailImageView())
                    }
                } ?: run { // No picture is available, so use a character from the product string as the image thumbnail
                    detailImageCharacter = productName?.substring(0, 1)
                    setDetailCharacterBackgroundTintList(com.sap.cloud.mobile.fiori.R.color.sap_ui_contact_placeholder_color_1)
                }
            }
        }

        override fun getItemCount(): Int {
            return products.size
        }

        init {
            products = productList
            currentActivity = activity
        }
    }
    ```

13.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`prepareViewModel`**, to move to the `prepareViewModel` method.

14.  Replace the `ViewModelProvider(currentActivity).get(ProductViewModel::class.java).also {` block of the first "else"-block of the method with the following code:

    ```Kotlin
    ViewModelProvider(currentActivity).get(ProductViewModel::class.java).also {
        it.initialRead{errorMessage ->
            showError(errorMessage)
        }
        val cv: CollectionView = currentActivity.findViewById(R.id.collectionView)
        createCollectionView(cv)
    }
    ```

15.  On Windows, press **`Ctrl+F12`**, or, on a Mac, press **`command+F12`**, and type **`onCreate`**, to move to the `onCreate` method.

16.  Add the following lines of code at the end of the method:

    ```Kotlin
    // Query the SalesOrderItems and order by gross amount received from sales
    // Change the orderBy arguments to SalesOrderItem.property_name to rearrange the CollectionView order of products
    val dq = DataQuery().orderBy(SalesOrderItem.productID)
    // Get the DataService class, which we will use to query the back-end OData service
    val espmContainer = sapServiceManager?.eSPMContainer
    espmContainer?.let {
      it.getSalesOrderItemsAsync(dq, { querySales: List<SalesOrderItem>? ->
          LOGGER.debug("CollectionView: executed sales order query in onCreate")
          querySales?.let { querysales ->
              for (sale in querysales) {
                  if (salesList.containsKey(sale.productID)) {
                      salesList[sale.productID] = salesList[sale.productID]!!.toInt() + sale.quantity!!.intValueExact()
                  } else {
                      salesList[sale.productID] = sale.quantity!!.intValueExact()
                  }
                  LOGGER.debug("CollectionView ${sale.productID} : ${sale.quantity} : ${sale.grossAmount}")
              }
              salesList = sortByValue(salesList)
              LOGGER.debug("CollectionView: salesList size = ${salesList.size}")
              queryProducts()
          } ?: LOGGER.debug("CollectionView: sales query list is null")
      }, { re: RuntimeException -> LOGGER.debug("CollectionView: An error occurred during async sales query: ${re.message}")})
    }

    ```

17.  Run the app and notice that the **Products** screen now has a component at the top of the screen that allows horizontal scrolling to view the top products. Tap a product to see more details. Alternatively, tap **SEE ALL** to see all the products.

    !![Collection View on Products Screen](collection-view.png)

    >For more details, see [Collection View in SAP Fiori for Android Design Guidelines](https://experience.sap.com/fiori-design-android/collection-view/) and [Collection View](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/android/collection-view.html)

    >For more information on SAP Fiori for Android and the generated app, see [Fiori UI Overview](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/android/overview.html), [SAP Fiori for Android Design Guidelines](https://experience.sap.com/fiori-design-android/explore/), [Fiori UI Demo Application](https://github.com/SAP/cloud-sdk-android-fiori-ui-components) and the `WizardAppReadme.md` file located in the generated app.

[OPTION END]

Congratulations! You have now made use of SAP Fiori for Android and have an understanding of some of the ways that the wizard-generated application can be customized to show different fields on the list screens, add or remove menu items, perform a search, and use a collection view.

[DONE]
[ACCORDION-END]
---
