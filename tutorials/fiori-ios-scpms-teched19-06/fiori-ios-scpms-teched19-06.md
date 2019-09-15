---
title: Implement Action Sheet and Image Picker for the Core ML Image Classification
description: Implement an Action Sheet and Popover (for regular size class on iPad) to pick an image from Photo Library or Camera; the Image will later be fed to the Core ML Image Classification Model.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, topic>odata, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
time: 15
---

## Prerequisites  
- **Development environment:** Apple Mac running macOS High Sierra or higher with Xcode 10 or higher
- **SAP Cloud Platform SDK for iOS:** Version 3.0 SP02

## Details
### You will learn  
  - How to implement and Action Sheet and make sure it will automatically be replaced with a Popover when running on regular size class on iPad
  - How to implement an Image Picker using Camera or Photo Library

---

[ACCORDION-BEGIN [Step 1: ](Add a Bar Button Item to Overview View Controller)]

In order for the user to later pick an image for the classification you will implement a Bar Button Item bringing up an Action Sheet or a Popover when running on Regular mode on iPad.

To do so please open the `Main.storyboard` and locate the Overview View Controller. There please use the **Object Library** to drag a **Bar Button Item** onto the Navigation Item of the Overview View Controller.

![Bar Button Item](fiori-ios-scpms-teched19-01.png)

Please open the **Assistant Editor** in Xcode and create an `IBOutlet` for the Bar Button Item in the `OverviewViewController` class by **control + drag** into the View Controller class, name it `actionListButton` and place it directly below the `tableView` outlet.

![Bar Button Item](fiori-ios-scpms-teched19-02.png)

Next you will need an `IBAction` to react to the user's interaction with that button. Please create an `IBAction` in the `OverviewViewController` class by **control + drag** into the View Controller class, choose **Action** this time instead of **Outlet**, name the action `didPressActionListButton`.

![Bar Button Item](fiori-ios-scpms-teched19-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Implement a UIImagePickerController)]

Please close the **Assistant Editor** if not done already and open the class `OverviewViewController`. In there please add two properties for holding the `UIImagePickerController` and the picked image. Add the following lines of code below the segue properties:

```Swift

private let pickerController = UIImagePickerController()
private var pickedImage: UIImage!

```

Next implement a method with the name `setupImagePicker` below the just added `IBAction`:

```Swift

private func setupImagePicker() {
    pickerController.delegate = self
    pickerController.allowsEditing = false

    // Only allow images here
    pickerController.mediaTypes = ["public.image"]
}

```

Next call that just added method in the `viewDidLoad(:)` right below the `loadInitialData()` method call:

```Swift

setupImagePicker()

```

The compiler will currently complain because the `OverviewViewController` is not conforming to the `UIImagePickerControllerDelegate` or the `UINavigationControllerDelegate`. Please add an extension below the Table View extension `extension OverviewViewController: UITableViewDelegate, UITableViewDataSource`, please read the inline comments for more information:

```Swift

extension OverviewViewController: UIImagePickerControllerDelegate, UINavigationControllerDelegate {

  // If the Image Picker Controller did get cancelled, just dismiss the Image Picker Controller
  public func imagePickerControllerDidCancel(_ picker: UIImagePickerController) {
          picker.dismiss(animated: true, completion: nil)
      }

  // If the user picked an image check if the image can be unwrapped to an UIImage, if not log an error and dismiss the Image Picker
  public func imagePickerController(_ picker: UIImagePickerController,
                                    didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey: Any]) {
      guard let image = info[.originalImage] as? UIImage else {
          logger.error("Image is nil! Please check UIImagePicker implementation.")
          return picker.dismiss(animated: true, completion: nil)
      }

      // If all went good, please assign the picked image to the pickedImage property, dismiss the the Image Picker and perform the segue to the classification View Controller
      pickedImage = image
      picker.dismiss(animated: true) {
          self.performSegue(withIdentifier: self.showProductClassificationSegue, sender: self)
      }
  }

}

```

Please continue to the next step to implement a segue to the `ProductClassificationTableViewController` you will implement. The compile errors will then go away.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create segue and Product Classification Table View Controller)]

Please select the `SalesAssistant` group in the **Project Navigator** and create a new `UITableViewController` with the name `ProductClassificationTableViewController`.

![Product Classification VC](fiori-ios-scpms-teched19-05.png)

Next open the `Main.storyboard` and use the **Object Library** to create a **Table View Controller** right above the Overview View Controller.

![Product Classification VC](fiori-ios-scpms-teched19-04.png)

Please select the newly added Table View Controller and set it's custom class to `ProductClassificationTableViewController` by clicking the **Identity Inspector**.

![Product Classification VC](fiori-ios-scpms-teched19-06.png)

Next we want the Table View Controller to have a Navigation Bar so let's embed that one in a **Navigation Controller** by selecting the `ProductClassificationTableViewController` and then click on `Editor -> Embed In -> Navigation Controller`.

![Product Classification VC](fiori-ios-scpms-teched19-07.png)

Last step would be to create the segue from the Bar Button Item inside the Overview View Controller to the Product Classification Table View Controller. Please select the added Bar Button Item and **control + drag** to the Product Classification Table View Controller, as action please choose **Present Modally** as we want to display this Table View Controller in a modal fashion.

![Product Classification VC](fiori-ios-scpms-teched19-08.png)

Now select the created segue and set the identifier to `showProductClassification` in the **Attributes Inspector**. The final step would be to create a constant in the `OverviewViewController` class holding that identifier. Please close the `Main.storyboard` and open the `OverviewViewController` class, add the following line of code directly below the `private let showCustomerDetailSegue = "showCustomerDetails"` line:

```Swift

private let showProductClassificationSegue = "showProductClassification"

```

You have to make sure that the selected image later on get's passed on to the `ProductClassificationTableViewController` for the classification. For that reason please locate the `prepare(for:sender:)` method and add a new Switch case to it:

```Swift

case showProductClassificationSegue:
    let navController = segue.destination as! UINavigationController
    let productPredictionVC = navController.children.first! as! ProductClassificationTableViewController
    productPredictionVC.image = pickedImage

```

Your `prepare(for:sender:)` should look like this now:


```Swift

override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
    switch segue.identifier {
    case showCustomerDetailSegue:
        // Show the selected Customer on the Detail view
        guard let indexPath = self.tableView.indexPathForSelectedRow else {
            return
        }

        let selectedEntity = self.customers[indexPath.row]
        let detailViewController = segue.destination as! CustomerDetailTableViewController
        guard let customerID = selectedEntity.customerID else {
            AlertHelper.displayAlert(with: "We're having issues displaying the details for the customer with name \(selectedEntity.lastName ?? "")", error: nil, viewController: self)
            self.logger.error("Unexpectly customerID is nil! Can't pass customerID into CustomerDetailViewController.")
            return
        }
        detailViewController.customerId = customerID
        detailViewController.navigationItem.title = "\(self.customers[indexPath.row].firstName ?? ""), \(self.customers[indexPath.row].lastName ?? "")"
    case showProductClassificationSegue:
        let navController = segue.destination as! UINavigationController
        let productPredictionVC = navController.children.first! as! ProductClassificationTableViewController
        productPredictionVC.image = pickedImage
    default:
        return
    }
}

```

Remember that we want to pass on the selected image to the `ProductClassificationTableViewController`? You have to implement that property to the `ProductClassificationTableViewController` class first to make the compiler happy. Please open the `ProductClassificationTableViewController` class and add the following line of code right above the `viewDidLoad(_:)` method:

```Swift

var image: UIImage!

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Implement action for Bar Button Item)]

When the user taps on the Bar Button Item it should show an Action Sheet or a Popover when running on Regular mode on the iPad. For that you will implement that code in the `didPressActionListButton(_:)` method. Before doing that you will add one more line of code to the `viewDidLoad(_:)` to make sure the Bar Button Item shows an Icon instead of just the word **Item**. Please add the following line of code to the `viewDidLoad(_:)`:


```Swift

// Using the FUI Icon Library
actionListButton.image = FUIIconLibrary.system.more

```

Now add the following lines of code to the `didPressActionListButton(_:)` method, please read the inline comments carefully:

```Swift

// You will use an Action Sheet and Pop-Over on regular mode on iPad
// Create an UIAlertController with the preferred style actionSheet
let alertController = UIAlertController(title: nil, message: nil, preferredStyle: .actionSheet)

// Define the image sources as a tuple having a description and the actual source type
let imageSources = [
    ("Using Camera", UIImagePickerController.SourceType.camera),
    ("Based on Photo", UIImagePickerController.SourceType.photoLibrary)
]

// Iterate over the tuple and create an UIAlertAction accordingly. Add those actions to the alertController
for (sourceName, sourceType) in imageSources where UIImagePickerController.isSourceTypeAvailable(sourceType) {
    alertController.addAction(UIAlertAction(title: "Find Product \(sourceName)", style: .default) { _ in
        self.pickerController.sourceType = sourceType
    })
}

// Add a cancel action as well for the user to cancel the alertController
alertController.addAction(UIAlertAction(title: "Cancel", style: .cancel))

// If in a regular layout on iPad, show as a popover
if let popoverController = alertController.popoverPresentationController {
    popoverController.barButtonItem = sender
}

// Present the alertController
self.present(alertController, animated: true)

```

All the needed code is now implemented for the user to choose a picture from the Photo Library or if running on an actual device take a picture with the device's camera.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add permissions to Info.plist file)]

Because iOS is a privacy focused operation system, you have to ask the user for permission to access both camera and photo library within your app. Those permissions are maintained in the Info.plist file. Please open the Info.plist and add the following two information Properties to it by clicking on the small **+** icon.


| Property | Value |
|----|----|
| Privacy - Photo Library Usage Description | Please permit using Photo Library |
| Privacy - Camera Usage Description | Please permit using Camera |

![Product Classification VC](fiori-ios-scpms-teched19-09.png)

That's it, the first time your app will try to access the Camera or Photo Library, the user will get asked for permission for the app to perform this action.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add back navigation)]

Right now the user has no possibility to navigate back from the Product Classification Table View Controller. For that please open the `Main.storyboard` and add a Bar Button Item to the left side of the Navigation Item of the Product Classification Table View Controller.

![Product Classification VC](fiori-ios-scpms-teched19-10.png)

Now in the **Attributes Inspector** for the Bar Button Item, select the **System Item** to be **Done**.

![Product Classification VC](fiori-ios-scpms-teched19-11.png)

Again you will need an `IBAction` for that Bar Button Item, please open the **Assistant Editor** and **control + drag** from the Bar Button Item to the Table View Controller class, select **Action** as type and `doneButtonTapped` as **Name** and click **Create**.

Close the **Assistant Editor** and open the `ProductClassificationTableViewController` class. Locate the `doneButtonTapped(_:)` method and add the following line of code, responsible for dismissing this modally presented Table View Controller:

```Swift

self.dismiss(animated: true)

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run app to see results of your work)]

You have implemented all necessary steps to choose or take a picture using an Action Sheet or Popover, make the needed navigations and passing on the selected image. Now it is time to test if all that works. Please run the app and click on the Bar Button Item in the Overview View Controller.

![Product Classification VC](fiori-ios-scpms-teched19-12.png)

[VALIDATE_7]
[ACCORDION-END]
