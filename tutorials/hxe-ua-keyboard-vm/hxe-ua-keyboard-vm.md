---
title: Set Keyboard and Time Zone
description: The VM defaults to an English (US) QWERTY keyboard, and the UTC time zone. When prompted, change the keyboard layout and time zone to match your location, or accept the defaults.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loiod0775daa77ca4aaea29ea74b3e2e2ac1 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  You have completed [Import the OVA](https://www.sap.com/developer/tutorials/hxe-ua-ova-vm.html)  

## Details
### You will learn
You'll learn how to start the VM, change the VM default keyboard layout, and change the default time zone.

### Time to Complete
5 min

---

If you don't change the keyboard layout to match the physical keyboard of your host machine (referred to as your **laptop** in this documentation), you may encounter problems later when logging in.

[ACCORDION-BEGIN [Step 1: ](Start your VM)]

Open your hypervisor application.

Power on (or click *Play* on) your SAP HANA 2.0, express edition VM.

![loio6ea1fbd9f13e4f02a2452aea309623fa_HiRes](loio6ea1fbd9f13e4f02a2452aea309623fa_HiRes.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Change the keyboard layout if your laptop doesn't use an English (US) keyboard)]

The system prompts you to either change the VM keyboard, or accept the default English (US) QWERTY keyboard. Enter `Y` to change the keyboard or `N` to use the default.

![loio1d4688914c3f455fb5adf8ee85917238_LowRes](loio1d4688914c3f455fb5adf8ee85917238_LowRes.png)

If you opt to change the keyboard, the System Keyboard Configuration page displays.

![loio5f2d94465a1048e6ac1eecaf05b52fe2_LowRes](loio5f2d94465a1048e6ac1eecaf05b52fe2_LowRes.png)

Use the arrow keys to scroll to the desired keyboard layout. `Tab` to the *OK* button, or press `F10`, to save your changes. A message displays while the system processes the keyboard layout change.

![loio77861b7b1738475a8a680ce2090812f1_LowRes](loio77861b7b1738475a8a680ce2090812f1_LowRes.png) 

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Change time zone)]

Change the time zone if your laptop is not in the default UTC (GMT) time zone.

Enter `Y` to change the time zone, or `N` to accept the default.

![loio93a3c8468a1f460fb3a85835b5f5f853_LowRes](loio93a3c8468a1f460fb3a85835b5f5f853_LowRes.png)

If you opt to change the timezone, the Clock and Time Zone page displays.

![loio04eae8f757f8428d9f6cc768c9e83d0e_LowRes](loio04eae8f757f8428d9f6cc768c9e83d0e_LowRes.png)

In the Region pane, use the arrow keys to scroll down to the correct region. `Tab` to the Time Zone pane and select the correct time zone. `Tab` to the *OK* button, or press `F10`, to save your changes.

[ACCORDION-END]


