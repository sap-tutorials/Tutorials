---
title: Change the Generated UI to Make the App Your Own
description: Use Xcode to change the generated UI and add your own views to the app.
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
  - How to change the Assistant generated UI with your own using Xcode and storyboards

---

[ACCORDION-BEGIN [Step 1: ](Replace generated UI with your own)]

The SAP Cloud Platform SDK for iOS Assistant does a great job generating the initial UI for a given data service. While this feature is awesome for reviewing data service entities and impressing your boss or customer with just how fast you can get something built, when it really comes to delivering an app with your unique business functionality, you need to understand how to incorporate your own UI.

Let's start by opening up the generated project in Xcode and clicking on the file name `Main.storyboard` in the project navigator. Theoretically you could build all your UI in Swift but storyboards provides you an easy way to visually create each view, define the UI flow, and configure the views to support all size classes.

![Replace UI](fiori-ios-scpms-teched19-01.png)

Select all the View Controllers in the Main.storyboard file by clicking inside the file content then typing **Command + a**.

![Replace UI](fiori-ios-scpms-teched19-02.png)

Now delete them by hitting the **delete** key on your keyboard.

Click on the **Object Library** button in the upper-right of the Xcode toolbar to present a list of objects you can place in your storyboard.

Filter the list by typing `UIViewController`, then drag and drop the View Controller object onto your storyboard.

![Replace UI](fiori-ios-scpms-teched19-03.png)

We will add additional view controllers in later steps so we'll embed the added View Controller in a Navigation Controller to allow it to handle the the navigation between them.

Select the View Controller then select **Embed In** > **Navigation Controller** from the **Editor** menu.

![Replace UI](fiori-ios-scpms-teched19-04.png)

The last step here is to set the newly created Navigation Controller as the view controller that should be loaded when this storyboard is initialized.

Select the Navigation Controller and in the Attributes Inspector on the right, check the box next to **Is Initial View Controller**.

An arrow should appear left to the Navigation Controller indicating it is defined to be loaded first.

![Replace UI](fiori-ios-scpms-teched19-05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Swift class backing up the storyboard View Controller)]

Your storyboard configuration is almost done now. Any View Controller or Table View Controller added to a storyboard that you'd like to contain custom behavior needs to be backed by a Swift class in which you'll implement your customizations.

To create a Swift class for your custom View Controller, select the `SalesAssistant` folder in Xcode's project navigator on the left side and right-click or hold the control key and click the folder to present the context menu, then select **New File...**.

![Create Class](fiori-ios-scpms-teched19-06.png)

When the file template chooser dialog is presented, select **Cocoa Touch Class** and click **Next**.

![Create Class](fiori-ios-scpms-teched19-07.png)

In the file options dialog, enter these values for the following fields:

| Field | Value |
|----|----|
| Class | `OverviewViewController` |
| Subclass of | `UIViewController` |
| Language | Swift |

Click **Next**.

Accept the defaults in the next dialog and click **Create**.

![Create Class](fiori-ios-scpms-teched19-08.png)

Your newly created Swift class will be added to the project and should automatically be displayed.

However, before we get into coding, you'll want to link this new Swift class to your view controller in the storyboard file named `Main.storyboard`.

Select `Main.storyboard` in the project navigator and select the View Controller you created earlier.

Select the **Identity Inspector** in the the panel on the right-hand side and enter `OverviewViewController` as the value for the **class** field under **Custom Class** and hit return.

Now this View Controller in the storyboard file is backed by your custom Swift class.

![Create Class](fiori-ios-scpms-teched19-09.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Change code in application UI manager class)]

When the Assistant generated our app, it configured the `ApplicationUIManager` class to present the Master / Detail view controller. Since we now want the Navigation View Controller that manages our new `OverviewViewController` to be the initial view controller, we'll need to make some changes to the `ApplicationUIManager` Swift class.

Open the Onboarding folder in the project navigator on the left-hand side and select the file titled `ApplicationUIManager.swift`.

Locate the `showApplicationScreen(completionHandler:)` method, which method contains the code initializing the initial View Controller. In order to quickly find that method you can select the `showApplicationScreen` using the quick navigation feature in the bread-crumb path at the top of the file editor.

![Change Code](fiori-ios-scpms-teched19-10.png)

The Xcode editor should then jump to the selected method.

![Change Code](fiori-ios-scpms-teched19-11.png)

In the `showApplicationScreen` method, replace the following code in the else block:

```Swift

let appDelegate = (UIApplication.shared.delegate as! AppDelegate)
let splitViewController = UIStoryboard(name: "Main", bundle: Bundle.main).instantiateViewController(withIdentifier: "MainSplitViewController") as! UISplitViewController
splitViewController.delegate = appDelegate
splitViewController.modalPresentationStyle = .currentContext
splitViewController.preferredDisplayMode = .allVisible
appViewController = splitViewController

```

with

```Swift

let mainNavigationController = UIStoryboard(name: "Main", bundle: Bundle.main).instantiateInitialViewController() as! UINavigationController
appViewController = mainNavigationController

```

This code will load the `Main.storyboard` and initializes the initial View Controller which in our case is a `UINavigationController`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run your app in simulator)]

Now that have done all that's needed to replace the Assistant generated UI with your own, let's see how the app looks by running it in the iOS Simulator.

Click on the run button in Xcode to compile and run your app.

Because you have run through the on-boarding flow already and enrolled in `FaceID` or `TouchID` the new View Controller should show up.

![Run App](fiori-ios-scpms-teched19-12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add Table View to View Controller)]

The Overview View Controller is going to be used in this tutorial series to display a list of customers and products in a Table View.

Select the file `Main.storyboard` in the project navigator on the left.

Click on the Object Library button and filter the list by typing **Table View** then select,
drag, and drop the Table View into the center of your `OverviewViewController`.

![Add Table View](fiori-ios-scpms-teched19-13.png)

Next, we need to set layout constraints on the Table View in order to ensure that it will layout properly when presented at any size.

Select the Table View and click the **Add New Constraints** icon on the lower-right.

In the fields for **Spacing to nearest neighbor**, change the top value to **25** and set the rest to **0**.

Uncheck the **Constrain to margins** checkbox and click **Add 4 Constraints**.

![Add Table View](fiori-ios-scpms-teched19-14.png)

This will add auto layout constraints to the Table View. You might ask yourself why you have to set the top constraint to 25; the reason is that we want a 25 points wide divider between the Navigation Bar and the Table View.

![Add Table View](fiori-ios-scpms-teched19-15.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create outlet from storyboard to class)]

We'll want to programmatically access this Table View in subsequent steps of this tutorial. To access the Table View from our Swift class, we'll use an `IBOutlet` to create a connection between our code connection and the visual Table View in the storyboard.

Select the Overview View Controller in the `Main.storyboard` file.

Click the Assistant editor button in top-right of the Xcode toolbar to open the `OverviewViewController.swift` file alongside the storyboard.

![Add Table View](fiori-ios-scpms-teched19-16.png)

Click on the Table View in the View Hierarchy or directly in the storyboard and **Control + Drag** from the Table View to just below the class definition in code window.

Make sure the **Connection** option is set to **Outlet** at the top of the dialog.

Enter `tableView` as the value for the outlet **Name** and click **Connect**.

![Add Table View](fiori-ios-scpms-teched19-17.png)

The result is a code connection from the Table View to your Swift class.

![Add Table View](fiori-ios-scpms-teched19-18.png)

Before going to the next step, close the `OverviewViewController.swift` code view by clicking on the **X** in the upper-right of the code view on the right.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Make Overview View Controller delegate and data source for Table View)]

Since our `OverviewViewController` class is a subclass of a `UIViewController`, we'll need to manually set our class as the value for `UITableViewDelegate` and `UITableViewDataSource`.

To conform to those two protocols you can use Swift extensions to extend your Swift class and implement the needed override methods in the class extension. In that case the extension will be used to make your code more readable.

Open up the `OverviewViewController.swift` class and add the following lines of code outside the closing bracket of the class:

```Swift

extension OverviewViewController: UITableViewDelegate, UITableViewDataSource {
  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return 0
  }

  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    return UITableViewCell()
  }
}

```

The `tableView(_:numberOfRowsInSection:)` method is responsible for telling the Table View how many rows should be displayed in a section. Right now, return **0** to stop the compiler from complaining. For the `tableView(_:cellForRowAt:)` return a new Table View Cell -- this cell is not going to be the cell we're going to use but we want to stop the compile time errors for now.

Both of these methods are required by the `UITableViewDataSource`.

You have to set the data source and delegate of the table view. Add the following lines of code to the `viewDidLoad(:)` method of the `OverviewViewController.swift` class:

```Swift

tableView.delegate = self
tableView.dataSource = self

```

The `viewDidLoad(:)` method should look like this:

```Swift

override func viewDidLoad() {
  super.viewDidLoad()

  tableView.delegate = self
  tableView.dataSource = self
}

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run app to see result of your work)]

Seeing the results of your work is the most satisfying part, so you should run your app in the iOS Simulator to ensure that everything is configured properly and have some satisfaction.

Your UI should look like this now:

![Add Table View](fiori-ios-scpms-teched19-19.png)

[VALIDATE_8]
[ACCORDION-END]
