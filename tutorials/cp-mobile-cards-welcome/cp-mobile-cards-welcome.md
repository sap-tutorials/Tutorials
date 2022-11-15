---
parser: v2
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>sap-mobile-cards, products>sap-mobile-services ]
time: 10
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---
# Create A New Welcome Card
<!-- description --> Create your first SAP Mobile Card using SAP Business Technology Platform Boosters.

## Prerequisites
- [Get a Free Trial Account on SAP BTP](hcp-create-trial-account)

## You will learn
- How to create an SAP Mobile Card using SAP BTP Boosters
- How to connect the SAP Mobile Cards application to your SAP Mobile Services

---

### The real-world use case


An organisation wants to share a welcome message to a new employee. Upon successful authentication in the SAP Mobile Cards app, the new employee sees a welcome card. In this card, the organisation shows basic details to help the employee have a great first day. This card is available to the user without the user having to subscribe to the card.

Welcome Cards are [Single Instance Cards](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-development-features.html#single-instance-cards-options) that are automatically subscribed. When such a card is created, users automatically get instances of the card because they are automatically subscribed to the card.


### Create welcome card using Boosters


SAP Mobile Cards can be created in multiple ways. In this step, you will create a card using [SAP BTP Boosters](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/fb1b56148f834749a2bf51127421610b.html?q=Boosters), which are sets of guided interactive steps which helps you to achieve specific goals.

1. In your web browser, open the [SAP BTP trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Provide the login details and click **Log On**.

    <!-- border -->![SAP BTP Log On Screen](img_2_2.png)

3. Navigate to the trial global account by clicking **Go To Your Trial Account**.

    <!-- border -->![Trial global account](img_2_3.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region (select the region closest to you). Your user profile will be set up for you automatically.  

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    ><!-- border -->![Enable SAP BTP CF Trial](img_2_3_note.png)

4. In the side navigation bar, click **Boosters**.

    <!-- border -->![CP CF Trial Account](img_2_4.png)

5. Click the **Build a Micro App for SAP Mobile Cards** tile.

    <!-- border -->![Booster View](img_2_5.png)

6. Go through the details mentioned and click **Start**.

    <!-- border -->![Booster View](img_2_6.png)

7. In the *Check Prerequisites* tab of the wizard, click **Next**.

    <!-- border -->![Booster Wizard Tab 1](img_2_7.png)

8. In the *Select Subaccount* tab of the wizard, select the following values.

    |Field|Value|Result|
    |----|---|---|
    | Subaccount | trial | All the spaces under this subaccount will be listed in the *Space dropdown*. |
    | Space | dev | The mobile card kit status is determined. |

    <!-- border -->![Booster Wizard Tab 2](img_2_8.png)

9. If your Mobile Card Kit status is *Not Available*, then Click **Initialize mobile card**.

    <!-- border -->![Booster Wizard Tab 2](img_2_9.png)

10. Click **Initialize**.

    <!-- border -->![Booster Wizard Tab 2](img_2_10.png)

11. When your Mobile Card Kit status is *Available*, then click **Next**.

    <!-- border -->![Booster Wizard Tab 2](img_2_11.png)

12. In the *Select Cards* tab of the wizard, select the **Welcome Card Template - Single Instance** and click **Next**.

    <!-- border -->![Booster Wizard Tab 3](img_2_12.png)

13. In the *Configure Cards* tab of the wizard, enter the name as following and click **Next**.

    |Field|Value|
    |----|---|
    |Name| Booster welcome card |

    <!-- border -->![Booster Wizard Tab 4](img_2_13.png)

14. In the *Review* tab of the wizard, review the details you've entered and click **Finish**.

    <!-- border -->![CPMS cockpit](img_2_14.png)

Upon successful creation you will see a success message.
<!-- border -->![CPMS cockpit](img_2_15.png)



### View card on your mobile device


>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

[OPTION BEGIN [Android]]

In this step, you will configure the SAP Mobile Cards client with your SAP BTP account.

1. Click the **Android** button and **scan the QR Code** on the left to install the SAP Mobile Cards app on your Android device.

    <!-- border -->![CPMS cockpit](img_3_1.png)

2.  Launch Mobile Cards Application on your device.

    <!-- border -->![App Launch](img_3_2.png)

3. Tap **Proceed**.

    <!-- border -->![Mobile Cards](img_3_3.png)

4. Upon reading the End User License Agreement, tap **I AGREE**.

    <!-- border -->![Mobile Cards](img_3_4.png)

5. Tap **Scan QR Code** button in your mobile device.

    <!-- border -->![Mobile Cards](img_3_5.png)

6.  Scan the QR code on the right to onboard your SAP Mobile Cards client.    

    ![CPMS cockpit](img_3_6.png)

7. Enter your SAP BTP credentials and tap **Log On**.

    <!-- border -->![Mobile Cards](img_3_7.png)

8. Choose a passcode with at least 8 characters to unlock the app, and tap **NEXT**.

    <!-- border -->![Mobile Cards](img_3_8.png)

9. Confirm the passcode and tap **DONE**.

    <!-- border -->![Mobile Cards](img_3_9.png)

10. Place your finger on the fingerprint scanner to enable biometric authentication.

    <!-- border -->![Mobile Cards](img_3_10.png)

    > This option is available only on devices that support biometric authentication. It allows you to use your biometric information for app authentication.

11. Tap **Allow only while using the app** option for the location services request.

    <!-- border -->![Location Services Screen](img_3_11.png)

    > SAP Mobile Cards is capable of showing some cards based on your location. [Click here](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-android.html#location-specific-notifications) to read more about it.

    > This option is shown when you launch the application and log in to a server for the first time.

12. You should see the Welcome Card created using SAP BTP Booster for SAP Mobile Cards.

    <!-- border -->![Mobile Cards](img_3_12.png)

    > Welcome cards are auto subscribed by default and downloaded automatically after registration. If you cannot see the card, then perform a pull refresh.

13. Tap the card to see it in full view.

    <!-- border -->![Mobile Cards](img_3_13.png)

14. Tap ![Android flip icon](ico_and_flip.png) button to see the backside of the card.

    <!-- border -->![Mobile Cards](img_3_14.png)

[OPTION END]

[OPTION BEGIN [iOS]]

In this step, you will configure the SAP Mobile Cards client with your SAP BTP account.

1. Launch the **QR Code Scanner** from the Control Center on your iOS device.

    <!-- border -->![Control Center](img_3_15.png)

2. **Scan the QR Code** on the left to install the SAP Mobile Cards app on your iOS device.

    <!-- border -->![CPMS cockpit](img_3_16.png)

2. Open the scanner of your device and scan the QR Code on the right to onboard your SAP Mobile Cards client.

    <!-- border -->![Mobile Cards](img_3_17.png)

4. Tap **Connect**.

    <!-- border -->![Mobile Cards](img_3_18.png)

5. Enter your SAP BTP credentials and tap **Log On**.

    <!-- border -->![Mobile Cards](img_3_19.png)

6. Choose a passcode with at least 8 characters to unlock the app, confirm the passcode and choose **Done**.

    <!-- border -->![Mobile Cards](img_3_20.png)

7. Tap **Enable** to enable biometric authentication.

    <!-- border -->![Mobile Cards](img_3_21.png)

    > This option is available only on devices that support biometric authentication. This biometric option may be Face ID depending on the capabilities of your device. This feature allows you to use your biometric information for app authentication.

    > <!-- border -->![Mobile Cards](img_3_21_note.png)

8. You should see the Welcome Card created using SAP BTP Booster for SAP Mobile Cards.

    <!-- border -->![Mobile Cards](img_3_22.png)

    > Welcome cards are auto subscribed by default and downloaded automatically after registration. If you cannot see the card, then perform a pull refresh.

9. Tap the card to see it in full view.

    <!-- border -->![Mobile Cards](img_3_23.png)

10. Tap ![iOS flip icon](ico_ios_flip_to_back.png) button to see the backside of the card.

    <!-- border -->![Mobile Cards](img_3_24.png)

[OPTION END]

**Congratulations!** You have created your first SAP Mobile Card using a Booster.


