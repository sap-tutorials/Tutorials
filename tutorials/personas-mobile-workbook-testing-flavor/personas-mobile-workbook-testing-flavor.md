---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-screen-personas, topic>mobile, software-product>sap-fiori]
primary_tag: software-product>sap-screen-personas
---

# Test Your Mobile SAP Screen Personas Flavor
<!-- description --> Run your Service Notification app on a mobile device

## You will learn
  - How to access your SAP Screen Personas flavor on a mobile device

## Intro
The focus of this tutorial is on testing your adaptive flavor(s) on a mobile device. The screenshots used will feature a mobile phone, but the same process applies to a tablet.

---

### Access your flavor on a mobile device

On the desktop version of the training system, navigate to transaction **`IW51`** and open the **flavor manager**. Click on your flavor tile, select **Direct Access Link** and then click **Generate QR Code**.

![Generate QR Code](Generate-QR-Code.png)

This will open a QR code that you can scan with your mobile device.

![Personas QR](Personas-QR.png)

Next, scan the QR code with your device and launch the resulting URL. You will be presented with the login screen for the training system.

![Scan Personas QR](Scan-Personas-QR.jpg)

### Test your flavor

Enter your credentials and tap **Log On** to begin testing your flavor.

![Log On](Log-On.jpg)

The adaptive flavor for your device will then open. Click **Create Notification** to navigate to the main screen of your flavor.

![Create Notification](Create-Notification.jpg)

You should then seen the simplified screen for your mobile device. If you have built different flavors to account for device orientation, you can test them by rotating your device. The flavor will automatically switch to account for the different screen size.

![Simplified Screen](Simple-Screen.jpg)

### Optional: Test your barcode scanner

If desired, you can also test the barcode scanner within you flavor. You will need to generate a QR code that represents a valid equipment number in your system. If you are using the training system provided by the SAP Screen Personas team, you can use the following **`210100019`**, **`210100024`**, or **`210100013`**.

You can use <https://www.the-qrcode-generator.com> to generate a QR code. Select **Free Text** and enter your equipment number.

![Generate QR Code](Generate-QR-Code.png)

Tap **Scan Equipment** on your flavor and then scan your QR code.

![Scan QR](Scan-QR.jpg)

Your **Functional Location** and **Equipment** fields should then automatically be filled in.

![Working Flavor](Working-Flavor.jpg)

Congratulations, you have a built a working mobile flavor with SAP Screen Personas!

