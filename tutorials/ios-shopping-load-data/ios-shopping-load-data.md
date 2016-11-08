---
title: Load product data from SAP HANA Cloud Platform
description: Load product data via SAP HANA Cloud Platform
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, products>sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Create an Xcode project for the shopping app](http://go.sap.com/developer/tutorials/ios-shopping-create-project.html)

## Next Steps
 - [Displaying the product details](http://go.sap.com/developer/tutorials/ios-shopping-display-details.html)

## Details
### You will learn  
After your Xcode project setup has been completed, you will modify the project to load the product list data from SAP HANA Cloud Platform and present it in a table view:

### Time to Complete
**10 Min**.

---

[ACCORDION-BEGIN [Step 1: ]( )]

Select `ViewController.swift` in the **Project Navigator** and rename the file to `ProductListViewController.swift`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ]( )]

Open the **`SDK Scout App`** on your mobile device and select `Object Cell` from the **components** list. This will give you an overview of the `Object Cell` and how it looks. Select the first style to get a preview of `Object Cell`.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ]( )]

Show the code snippet for the cell by tapping the button in the toolbar.

![code snippet tool bar icon](2-3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ]( )]

Copy the code snippet from your mobile device to your Mac.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ]( )]

Once you received the code snippet on your Mac, replace the content in `ProductListViewController.swift` completely with the code snippet from the `SDK Scout`. Rename the class name to `ProductListViewController`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ]( )]

Select `Main.storyboard` in the **Project Navigator** and select the `Shop` view. Now set the custom class in the **Identity Inspector** to `ProductListViewController`. Press **Return** and the **Module** should be set automatically to **`Current - Shop`**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ]( )]

Select `ProductListViewController.swift` in the **Project Navigator**. Add the code snippet below to add:

 - Required import statements for the next steps
 - A logger for the controller 
 - A local variable which will be used to save the loaded product list


```
 import UIKit

 import HCPFoundation
 import HCPOData
 import FioriUIKit
 import FioriBetaToolKit

 class ProductListViewController: UITableViewController {
    let logger = Logger.shared(withName: "ProductListViewController")
    var products: [Product] = []
    // ...
 }
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ]( )]

Change the protocol function `numberOfRowsInSection` which you received with the code snippet to return the correct number of products.

 ```
 override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return products.count
 }
 ```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ]( )]

In the function `tableView(_:cellForRowAt:)`, add a line to retrieve a product from the `products` array:

 ```
 let productItem = self.products[indexPath.row]
 ```
 
Change the values of the cell to match the following:

Cell Property     | Product Property   |  
:---------------- | :----------------  | 
`headlineText`    | `name`             | 
`subheadlineText` | `id`               | 
`footnoteText`    | `mainCategoryName` | 
`descriptionText` | `description`      | 
`detailImage.accessibilityIdentifier`  | `name`      | 
`statusText`      | `formattedPrice()` | 
`substatusText` 	 | `stockAvailability()` | 
`detailImage`	    | `FioriAssets.placeholder` | 
`accessoryType`  | `disclosureIndicator` |

For example:

 ```
 cell.headlineText = productItem.name
 ```
 
The `detailImage` is initially set to a placeholder, because the actual image will be loaded asynchonously.
 
[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 10: ]( )]

Additionally, you need to load the product image from SAP HANA Cloud Platform. The image will be loaded asynchronously so that the table cell is returned immmediately, ensuring smooth scrolling. 

Add the following code snippet after the assignment of the cell values and before the function returns the cell.

```
productItem.loadImage { image, error in
    if let error = error {
        self.logger.warn("Error while loading image.",  error: error)
    }
            
    if let image = image {
        if let delayedUpdatedCell = tableView.cellForRow(at: indexPath) as? ObjectCell {
            delayedUpdatedCell.detailImage = image
        }
    }
 }
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 11: ]( )]

To make the stock availability of the product visually more appealing, you can change the `textColor` on the `substatusLabel` of the cell based on the products `stockQuantity` property.

```
 cell.substatusLabel.textColor = UIColor.preferredFioriColor(forStyle: productItem.stockQuantity == 0 ? .negative : .positive)
```
 
[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 12: ]( )]

With the table to display the list of products for each cell you can now add the code to load the data from SAP HANA Cloud Platform. 

You have already prepared the first function `loadProducts()`in `Shop.swift`. Add a call to `loadProducts()` in `viewDidLoad()` of `ProductListViewController.swift`.


```swift
    override func viewDidLoad() {
        super.viewDidLoad()
        
        Shop.shared.loadProducts { loadedProducts, error in
            
            if let loadedProducts = loadedProducts {
                self.products = loadedProducts
            } else {
                self.products = []
                self.logger.error("Error loading products.", error: error)
            }
            
            self.tableView.reloadData()
        }
    }
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 13: ]( )]

Run the application.

![app display](2-13.png)

[DONE]
[ACCORDION-END]


## Next Steps
 - [Displaying the product details](http://go.sap.com/developer/tutorials/ios-shopping-display-details.html)
