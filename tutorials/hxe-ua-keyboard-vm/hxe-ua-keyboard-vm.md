---
title: Set Keyboard and Time Zone
description: The VM defaults to an English (US) QWERTY keyboard, and the UTC time zone. When prompted, change the keyboard layout and time zone to match your location, or accept the defaults.
author_name: John Currie
author_profile: https://github.com/JCurrie27
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 5
---

<!-- loiod0775daa77ca4aaea29ea74b3e2e2ac1 -->

## Prerequisites
 - **Tutorials:**  You have completed [Import the OVA](hxe-ua-ova-vm) 

## Details
### You will learn
You'll learn how to start the VM, change the VM default keyboard layout, and change the default time zone.

---

If you don't change the keyboard layout to match the physical keyboard of your host machine (referred to as your **laptop** in this documentation), you may encounter problems later when logging in.

[ACCORDION-BEGIN [Step 1: ](Start your VM)]

Open your hypervisor application.

Power on (or click *Play* on) your SAP HANA 2.0, express edition VM.

![hxe2_vm_start_0](hxe2_vm_start_0.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Change the keyboard layout if your laptop doesn't use an English (US) keyboard)]

The system prompts you to either change the VM keyboard, or accept the default English (US) QWERTY keyboard. Enter `Y` to change the keyboard or `N` to use the default.

> Note:
> Having difficulty entering text in your virtual machine in `VMWare`? Press ` CTRL G ` to switch the focus from your host machine to the VM.
> 
> 

![HXE_change_keyboard_prompt_5](HXE_change_keyboard_prompt_5.png)

If you opt to change the keyboard, the System Keyboard Configuration page displays.

![HXE_change_keyboard_GUI_PNG_1](HXE_change_keyboard_GUI_PNG_1.png)

Use the arrow keys to scroll to the desired keyboard layout. `Tab` to the *OK* button, or press `F10`, to save your changes. A message displays while the system processes the keyboard layout change.

![HXE_change_keyboard_process_PNG_2](HXE_change_keyboard_process_PNG_2.png) 

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Change time zone)]

Change the time zone if your laptop is not in the default UTC (GMT) time zone.

Enter `Y` to change the time zone, or `N` to accept the default.

![HXE_change_timezone_PNG_4](HXE_change_timezone_PNG_4.png)

If you opt to change the timezone, the Clock and Time Zone page displays.

![HXE_change_timezone_GUI_3](HXE_change_timezone_GUI_3.png)

In the Region pane, use the arrow keys to scroll down to the correct region. `Tab` to the Time Zone pane and select the correct time zone. `Tab` to the *OK* button, or press `F10`, to save your changes.

[DONE]

[ACCORDION-END]


