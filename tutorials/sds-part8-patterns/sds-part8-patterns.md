---
title: Watch for Patterns of Events and Use the CCL Editor in SAP HANA Smart Data Streaming
description: Part 8 of 9. Define and watch for patterns in events. Use CCL Editor.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [ tutorial>beginner, products>sap-hana-streaming-analytics, products>sap-hana-studio ]
author_name: Bill Jiang
author_profile: https://github.com/BillJiangSAP
time: 15
---

## Details
### You will learn  
 - Defining a pattern to watch for.
 - Defining contents of the event to produce when a pattern has been detected.
 - Using the CCL Editor to modify the project.


---

[ACCORDION-BEGIN [Step 1: ](Add a pattern element)]    

1. Click **SAP HANA Streaming Development** to switch to the perspective.

    ![SDS dev](event-patterns/1-sdsdev.png)

2. Drag and drop the **Pattern** item from the palette into the canvas.

    ![add pattern](event-patterns/2-addpattern.png)

3. Connect **`DEVICE_EVENTS`** to **`Pattern1`** using the connector item.

    ![connect](event-patterns/3-connect.png)

4. Rename the **`Pattern1`** to `ALARM_POWER`.

    ![rename](event-patterns/4-rename.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Edit column expressions)]

1. Click on Switch to Text button to switch to CCL Editor. You can also press **F6**.

    ![switch to text](event-patterns/6-switchtotext.png)

2. Click **Yes** if there is a prompt to save the file. You can also press **Alt+y**.

    ![save](event-patterns/7-save.png)

3. Click **Outline** to open the view. If you cannot find the button, you can also go to **Window** > **Show View** > **Other...** and type in `outline` in the search box to find the desired view.

    ![outline](event-patterns/8-outline.png)

4. Select **`ALARM_TEMP`**. The CCL Editor will jump to the corresponding code section.

    ![alarm temp](event-patterns/9-alarmtemp.png)

5. Click Minimize icon on the top right of the **Outline** view to hide it.

    ![close outline](event-patterns/10-closeoutline.png)

6. Select the column expressions from the **`ALARM_TEMP`** window and copy them. You will use these code to speed up the writing of `"ALARM_POWER"` functionality.

    ![copy alarm temp](event-patterns/11-copyalarmtemp.png)

7. Now paste the column expressions into the **`ALARM_POWER`** stream, replacing the **`*`**.

    ![paste alarm power](event-patterns/12-pastealarmpow.png)

8. Edit the **FROM** clause to read `DEVICE_EVENTS A, DEVICE_EVENTS B`. Since you want to watch for a pattern of 2 events from the same stream, you need define two different aliases for the stream.

    ![from clause](event-patterns/13-fromclause.png)

9. Now edit the column expressions to the following shown in the picture.
    - Change the source of each column value coming from the input stream to `A`.
    - Delete the **`AVG_TEMP`** and **`MAX_TEMP`** columns.
    - Edit the text for the **`ALARM_TYPE`** and **`ALARM_DESC`** columns.

    ![change fields](event-patterns/14-changefields.png)

    The final code should be:
    ```sql
    CREATE OUTPUT STREAM ALARM_POWER
    AS SELECT
      A.MACHINEID MACHINEID ,
      A.EVENT_TIME EVENT_TIME ,
      A.LOCATION LOCATION ,
      'POWER' ALARM_TYPE ,
      'Power out for more than allowed duration' ALARM_DESC
    FROM DEVICE_EVENTS A, DEVICE_EVENTS B;
    ```

10. When finished editing, click Switch to Visual button to switch back to visual editor mode. You can also press **F6**.

    ![change to visual](event-patterns/15-changetovisual.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add a pattern to match)]

1. Click Add Pattern icon shown below to define the pattern to watch for.

    ![add pattern](event-patterns/16-addpattern.png)

2. Enter `20 sec` in the Interval box.

    ![interval](event-patterns/17-interval.png)

3. Enter the pattern we want to watch for: `A, !B`. The "," means "followed by" and "!" means not. The expression means to watch for event A that is NOT followed by event B within 20 seconds. ("AND" and "OR" operators are also available in the pattern)

    ![pattern](event-patterns/18-pattern.png)

4. Now we need to define our ON clause. This filters the incoming events to determine which events qualify as an "A" event and a "B" event. Enter the following expression and click **OK** when done:
  `A.MACHINEID = B.MACHINEID AND A.EVENT_VALUE = 'Power off' AND B.EVENT_VALUE = 'Power on'`

    ![on clause](event-patterns/19-onclause.png)

    Tip: you can use **Ctrl+Space** for completion assist.

5. Click Compile Project button to check for errors. Please refer to part 7 of this tutorial group on how to run and re-test the updated project.

    ![compile](event-patterns/20-compile.png)

[VALIDATE_1]

[ACCORDION-END]
