---
title: Implement the Image classification using a trained Core ML model and the Vision framework.
description: You will learn how to import a trained Core ML model and use the Vision framework to prepare a certain image for the classification. You will then use the classification result to fetch products of a certain product category from the sample data service.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, topic>odata, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
time: 25
---

## Prerequisites  
- **Development environment:** Apple Mac running macOS High Sierra or higher with Xcode 10 or higher
- **SAP Cloud Platform SDK for iOS:** Version 3.0 SP02

## Details
### You will learn  
  - How to import a trained Core ML model into Xcode
  - How to use the Vision framework to process an Image and feed it to the classification model
  - How to take the classification result and fetch products for the classified product category

---

[ACCORDION-BEGIN [Step 1: ](Understand the Core ML model)]

For this tutorial you will import the `TechEd2019.mlmodel` Core ML model into your app Xcode project. This model has been trained on product data like notebooks and office chairs. The data service your app is talking against has exactly those products in the catalogue. The goal for this tutorial is to later feed an image of a MacBook as well as an image of an office chair to the model. The Core ML model should classify those two images the correct way and your app will load similar products from the data service and display them in a Table View.


[DONE]
[ACOORDION-END]

[ACCORDION-BEGIN [Step 2: ](Import the Core ML model to your Xcode app project)]

In order to use the `TechEd2019.mlmodel` Core ML model you have to add it to your Xcode project. The model is located on the desktop of the `TechEd` machine in the **Tutorial Content** folder. Please go into that folder and **drag + drop** that model into the **Project Navigator** of Xcode. Xcode will bring up a dialogue where you please make sure **Copy Items if needed** and **Create folder references** is selected and click on **Finish**.

![Import Model](fiori-ios-scpms-teched19-01.png)

The model will now be referenced in your Xcode app project and can be initialized within the app code.

[DONE]
[ACOORDION-END]

[ACCORDION-BEGIN [Step 3: ](Setup the Product Classification Table View Controller for data loading)]

Because you want to fetch all products of the classified product category, you have to setup the Table View Controller class like the View Controllers before. Please add the following properties right above the `viewDidLoad(_:)` method:

```swift

private var dataService: ESPMContainer<OnlineODataProvider>?
private let appDelegate = UIApplication.shared.delegate as! AppDelegate

private let logger = Logger.shared(named: "ProductClassificationTableViewController")

private var products = [Product]()

```

Next add the needed import statements below the `import UIKit` statement above the class declaration:

```swift

import SAPFiori
import SAPOData
import SAPCommon

```

Because the classification as well as the data loading will be performed on background threads you should display a loading indicator to let the user know that your app is currently processing. Please let the `ProductClassificationTableViewController` conform to the `SAPFioriLoadingIndicator` protocol:

```swift

class ProductClassificationTableViewController: UITableViewController, SAPFioriLoadingIndicator

```

The protocol wants you to add the `FUILoadingIndicatorView` as a property to your class. Please add the following line of code right above the `dataService` property:

```swift

var loadingIndicator: FUILoadingIndicatorView?

```

You will use an `FUIObjectTableViewCell` to display the products in the Table View, also you will need a data service instance. Please add the following lines of code to the `viewDidLoad(_:)` method:

```swift

tableView.estimatedRowHeight = 80
tableView.rowHeight = UITableView.automaticDimension

tableView.register(FUIObjectTableViewCell.self, forCellReuseIdentifier: FUIObjectTableViewCell.reuseIdentifier)

guard let dataService = appDelegate.sessionManager.onboardingSession?.odataController.espmContainer else {
    AlertHelper.displayAlert(with: "OData service is not reachable, please onboard again.", error: nil, viewController: self)
    logger.error("OData service is nil. Please check onboarding.")
    return
}

self.dataService = dataService

```

This code should already look familiar to you.

[DONE]
[ACOORDION-END]

[ACCORDION-BEGIN [Step 3: ](Classify the input image)]

To classify the image the user chose, you can use the **Vision** framework to prepare the image for classification.

Please import the Vision framework by adding the import statement right below the `UIKit` import above the class declaration:

```swift

import Vision

```

You will use a so called `VNCoreMLRequest` which will contain an instance of the Core ML model for image classification. Please implement the following code, read the inline comments carefully:

```swift

lazy var classificationRequest: VNCoreMLRequest = {
    do {
        // Instantiate the Core ML model
        let model = try VNCoreMLModel(for: TechEd2019().model)

        // Create a VNCoreMLRequest passing in the model and starting the classification process in the completionHandler.
        let request = VNCoreMLRequest(model: model, completionHandler: { [weak self] request, error in
            self?.processClassifications(for: request, error: error)
        })

        // Crop and scale the image
        request.imageCropAndScaleOption = .centerCrop
        return request
    } catch {
        fatalError("Failed to load Vision ML model: \(error)")
    }
}()

```

Next you want to implement a method performing the requests. Please implement a method called `updateClassifications(for:)`:

```swift

/// - Tag: PerformRequests
func updateClassifications(for image: UIImage) {

    // show the loading indicator
    self.showFioriLoadingIndicator("Finding similiar products...")

    // make sure the orientation of the image is passed in the CGImagePropertyOrientation to set the orientation of the image
    let orientation = CGImagePropertyOrientation(image.imageOrientation)

    // Create a CIImage as needed by the model for classification. If that fails throw a fatalError.
    guard let ciImage = CIImage(image: image) else { fatalError("Unable to create \(CIImage.self) from \(image).") }

    // Dispatch to the Global queue to asynchronously perform the classification request.
    DispatchQueue.global(qos: .userInitiated).async {
        let handler = VNImageRequestHandler(ciImage: ciImage, orientation: orientation)
        do {
            try handler.perform([self.classificationRequest])
        } catch {
            /*
             This handler catches general image processing errors. The `classificationRequest`'s
             completion handler `processClassifications(_:error:)` catches errors specific
             to processing that request.
             */
            print("Failed to perform classification.\n\(error.localizedDescription)")
        }
    }
}

```

Right now the code above will throw compile time errors because you need an extension on `CGImagePropertyOrientation` to manually match the orientations of `UIImage` to the `CGImagePropertyOrientation`. Please create create a new Swift class with the name `CGImagePropertyOrientation+UIImageOrientation` in the **Project Navigator**.

![Create Swift Class](fiori-ios-scpms-teched19-02.png)

![Create Swift Class](fiori-ios-scpms-teched19-03.png)

In that extension class please implement the following code:

```swift

import UIKit
import ImageIO

extension CGImagePropertyOrientation {
    init(_ orientation: UIImage.Orientation) {
        switch orientation {
        case .up: self = .up
        case .upMirrored: self = .upMirrored
        case .down: self = .down
        case .downMirrored: self = .downMirrored
        case .left: self = .left
        case .leftMirrored: self = .leftMirrored
        case .right: self = .right
        case .rightMirrored: self = .rightMirrored
        }
    }
}

```

Please go back to the `ProductClassificationTableViewController` swift class and implement a method processing the image classification. This method will also fetch the products according to the classification result. Please implement the following method directly below `updateClassifications(for:)` and read the inline comments carefully:

```swift

/// - Tag: ProcessClassifications
func processClassifications(for request: VNRequest, error: Error?) {

    // Use the main dispatch queue
    DispatchQueue.main.async {

        // Check if the results are nil and display the error in an Alert Dialogue
        guard let results = request.results else {
            self.logger.error("Unable to classify image.", error: error)
            AlertHelper.displayAlert(with: "Unable to classify image.", error: error, viewController: self)
            return
        }
        // The `results` will always be `VNClassificationObservation`s, as specified by the Core ML model in this project.
        let classifications = results as! [VNClassificationObservation]

        if classifications.isEmpty {
            AlertHelper.displayAlert(with: "Couldn't recognize the image", error: nil, viewController: self)
        } else {
            // Retrieve top classifications ranked by confidence.
            let topClassifications = classifications.prefix(2)
            let categoryNames = topClassifications.map { classification in
                return String(classification.identifier)
            }

            // Safe unwrap the first classification, because that will be the category with the highest confidence.
            guard let category = categoryNames.first else {
                AlertHelper.displayAlert(with: "Unable to identify product category", error: nil, viewController: self)
                self.logger.error("Something went wrong. Please check the classification code.")
                return
            }

            // Set the Navigation Bar's title to the classified category
            self.navigationItem.title = category

            // Define a DataQuery to only fetch the products matching the classified product category
            let query = DataQuery().filter(Product.category == category)

            // Fetch the products matching the defined query
            self.dataService?.fetchProducts(matching: query) { [weak self] result, error in
                if let error = error {
                    AlertHelper.displayAlert(with: "Failed to load list of products!", error: error, viewController: self!)
                    self?.logger.error("Failed to load list of products!", error: error)
                    return
                }

                // Hide the loading indicator
                self?.hideFioriLoadingIndicator()
                self?.products = result!

                // You will display the product images as well, for that reason create a new array containing the picture urls.
                self?.productImageURLs = result!.map { $0.pictureUrl ?? "" }
                self?.tableView.reloadData()
            }
        }
    }
}

```

Please also add the `productImageURLs` property right below the product array property above in the class:

```swift

private var productImageURLs = [String]()

```

The last step would be to call the `updateClassifications(for:)` method inside the `viewDidLoad(_:)` method as the last line of code:

```swift

updateClassifications(for: image)

```

Your `viewDidLoad(_:)` should now look like this:

```swift

override func viewDidLoad() {
    super.viewDidLoad()

    tableView.estimatedRowHeight = 80
    tableView.rowHeight = UITableView.automaticDimension

    tableView.register(FUIObjectTableViewCell.self, forCellReuseIdentifier: FUIObjectTableViewCell.reuseIdentifier)

    guard let dataService = appDelegate.sessionManager.onboardingSession?.odataController.espmContainer else {
        AlertHelper.displayAlert(with: "OData service is not reachable, please onboard again.", error: nil, viewController: self)
        logger.error("OData service is nil. Please check onboarding.")
        return
    }

    self.dataService = dataService

    updateClassifications(for: image)
}

```

That's all you need to do to classify an image with Vision and a pre-trained Core ML model. Please continue with the tutorial to implement the displaying of products in the Table View.

[DONE]
[ACOORDION-END]

[ACCORDION-BEGIN [Step 4: ](Implement the Table Views Data Source methods)]

To display the products you have just fetched in the Table View, you will implement the Data Source methods directly in the class like you have done before. Please add the `numberOfSections(in:)` and the `tableView(_:numberOfRowsInSection)` methods right below the `doneButtoneTapped(_:)` method:

```swift

// Return one section.
override func numberOfSections(in tableView: UITableView) -> Int {
    return 1
}

// The number of rows is dependant on the available products.
override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return products.count
}

```

Next implement the `tableView(_:cellForRowAt:)` method and read the inline comments carefully:

```swift

override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {

    // Get the correct product to display
    let product = products[indexPath.row]

    // Dequeue the FUIObjectTableViewCell
    let cell = tableView.dequeueReusableCell(withIdentifier: FUIObjectTableViewCell.reuseIdentifier) as! FUIObjectTableViewCell

    // Set the properties of the Object Cell to the name and category name
    cell.headlineText = product.name ?? ""
    cell.subheadlineText = product.categoryName ?? ""

    // If there is a price available, format it using the NumberFormatter and set it to the footnoteText property of the Object Cell.
    if let price = product.price {
        let formatter = NumberFormatter()
        formatter.numberStyle = .currency
        let formattedPrice = formatter.string(for: price.intValue())

        cell.footnoteText = formattedPrice ?? ""
    }

    // Because you're using a lazy loading mechanism for displaying the product images to avoid lagging of the Table View, you have to set a placeholder image on the detailImageView property of the Object Cell.
    cell.detailImageView.image = FUIIconLibrary.system.imageLibrary

    // The data service will return the image url in the following format: /imgs/HT-2000.jpg
    // In order to build together the URL you have to define the base URL.
    // The Base URL is found in the Mobile Services API tab.
    let baseURL = "https://hcpms-d061070trial.hanatrial.ondemand.com/com.sap.edm.sampleservice.v2"
    let url = URL(string: baseURL.appending(productImageURLs[indexPath.row]))

    // Safe unwrap the URL, the code above could fail when the URL is not in the correct format, so you have to make sure it is safely unwrapped so you can react accordingly. You won't show the product image if the URL is nil.
    guard let unwrapped = url else {
        logger.info("URL for product image is nil. Returning cell without image.")
        return cell
    }
    // You will use an image cache to cache all already loaded images. If the image is already in the cache display it right out of the cache.
    if let img = imageCache[unwrapped.absoluteString] {
        cell.detailImageView.image = img
    }

    // If the image is not loaded yet, use the loadProductImageFrom(_:) method to load the image from the data service.
    else {
        // The image is not cached yet, so download it.
        loadProductImageFrom(unwrapped) { image in
            cell.detailImageView.image = image
        }
    }

    return cell
}

```

Before you will implement the `loadProductImageFrom(_:)` method, you have to define the image cache. As a cache you will simply use a dictionary. Please add the following property definition to below the `private var productImageURLs = [String]()` property:

```swift

private var imageCache = [String:UIImage]()

```

Next you will implement the `loadProductImageFrom(_:)` method to actually fetch the product images. Please implement the method right below the `viewDidLoad(_:)` method and read the inline comments carefully:

```swift

private func loadProductImageFrom(_ url: URL, completionHandler: @escaping (_ image: UIImage) -> Void) {

    // Retrieve an instance of the SAPURLSession
    if let sapURLSession = appDelegate.sessionManager.onboardingSession?.sapURLSession {

        // Use a data task on the SAPURLSession to load the product images
        sapURLSession.dataTask(with: url, completionHandler: { (data, response, error) in

            if let error = error {
                self.logger.error("Failed to load image!", error: error)
                return
            }

            if let image = UIImage(data: data!) {
                // safe image in image cache
                self.imageCache[url.absoluteString] = image
                DispatchQueue.main.async { completionHandler(image) }
            }
        }).resume()
    }
}

```

[DONE]
[ACOORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the app and classify some products)]

Let's classify some images! Please run your app on simulator, because the simulator doesn't have a camera you have to import the product images to classify into the simulator's Photo Library. The images you will classify are on **Desktop** on the TechEd machine inside the **Tutorial Content** folder.

Please select the **MacBook.png** and the **Office Chair.png** image and drag them onto the simulator. The simulator will open up the Photo Library and add the images to it.

![Create Swift Class](fiori-ios-scpms-teched19-04.png)

Please open your app by tapping on the `SalesAssistant` navigation on the top-left corner.

![Create Swift Class](fiori-ios-scpms-teched19-05.png)

In your app enter the App passcode if necessary.

![Create Swift Class](fiori-ios-scpms-teched19-06.png)

In the Overview View Controller of your app tap on the implemented Bar Button Item in the Navigation Bar and select **Find Product Based on Photo**.

![Create Swift Class](fiori-ios-scpms-teched19-07.png)

![Create Swift Class](fiori-ios-scpms-teched19-08.png)

Next the Photo Library of the simulator will open up, please tap on **Recently Added**.

![Create Swift Class](fiori-ios-scpms-teched19-09.png)

Select the **MacBook** in there.

![Create Swift Class](fiori-ios-scpms-teched19-10.png)

The Photo Library will close and the Product Classification Table View Controller will open up and start the classification and the fetching of the products now.

The Table View should actually display all available notebooks available through the data service now and the product images should load and appear as well.

![Create Swift Class](fiori-ios-scpms-teched19-11.png)

Tap on **Done** to go back to the Overview View Controller

To try out the classification with the **Office Chair** image.

![Create Swift Class](fiori-ios-scpms-teched19-12.png)

The Product Classification Table View Controller will show all available Chairs in the data service.

![Create Swift Class](fiori-ios-scpms-teched19-13.png)

[VALIDATE_5]
[ACCORDION-END]
