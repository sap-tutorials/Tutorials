---
title: Managing message processing in case of errors
description: How to handle the message execution in case of errors.
auto_validation: true
time: 15
tags: [ tutorial>intermediate, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform-integration-for-process-services
---


## Details
### You will learn
  - How to induce an artificial error or exception in your message pipeline
  - How to handle exceptions through _Exception Sub-process_
  - How to use different end events to manage errors
  - What is a send step

In this exercise, we will induce an artificial exception situation via a script.
An _Exception Sub-process_ will be added to handle the artificial exception.
We will also play with the different End Events to see how they differ.

---

[ACCORDION-BEGIN [Step 1: ](Induce artificial exception)]

Let us induce an artificial error in the integration flow. We shall use the script step for the same.

1. Add a script step:

    * Click on the design Palette
    * Choose _Message Transformers_
    * Choose _Script_
    * Choose _Groovy Script_.
    * Drag it on to the execution pipeline before the _Message Mapping_ step.
    * Click on the _Script_ step and choose _Create (+)_ from the list of speed buttons.


        ![Add script](Add Script.png)

    * Add the following code to the script's _processData_ method:


      ```
      def Message processData(Message message)
      {
          throw new Exception();
      }
    ```     

      ![Exception Script](Exception Script.png)

    * Click _Ok_.

2. Save, deploy and execute the integration flow.

     * Go to the _Monitoring view_ and look for an entry for the message just processed. The status of the message should be __Failed__ - this indicates that an exception was thrown.     

          ![Error View](Error View.png)

     The _Error Details_ section gives more information about the exception.

     * To get additional information about the exception, scroll down to the _logs_ section (or click on the _Logs_ tab) and click on _Info_.

     ![Error Details](Error Details.png)

     A graphical viewer opens up and provides a visual representation of the exception location on the message execution pipeline.

     As the graphical viewer shows, the exception is caused by the _Script_ step.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add an Exception Sub-process to handle the exception)]

Let us now add an Exception Sub-process to catch the exception and handle it.

1. Add a Exception Sub-process to the Integration Flow:

    * Click on the design Palette
    * Choose _Process_
    * Choose _Exception Subprocess_

        ![Choose Exception SubProcess](Choose Exception SubProcess.png)

    * Drag it within the _Integration Flow_.

        ![Add Exception SubProcess](Add Exception SubProcess.png)


        ![Exception SubProcess](Exception SubProcess.png)

2. Save, deploy and execute the integration flow.

       * Go to the _Monitoring view_ and look for an entry for the message just processed.  The status of the message should be __Completed__ - this indicates that the exception thrown by the script step is caught by the _Exception Subprocess_.     

            ![Error View With Exception SubProcess](Error View With Exception SubProcess.png)    


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Send a mail to intimate about the exception)]

You can configure additional actions in the Exception Sub-process that you would like to perform in case of an exception. In the exercise below, we intimate a systems manager about the exception via an email and ask him to resend the request if he likes us to reprocess.

1. Add a mail receiver

    * Add a receiver to the canvas and connect it to the _End Message_ in the _Exception Sub-process_.
    * Choose a mail adapter:

    ![Add Receiver](Add Receiver.png)

    * Copy the mail channel configuration:

        * Click on any of the configured mail channel and choose the speed button for _Copy Configuration_.
        * Now select the newly created mail channel and choose the speed button for _Paste Configuration_. Click _yes_ on the confirmation pop-up.

          ![Copy Mail Configuration](Copy Mail Configuration.png)    

    * Click on the new mail channel, go to the _Properties Sheet_, go to the _Connection_ tab and set the _Subject_ field to _An error occurred - Please resend_:

        ![Exception Mail](Exception Mail.png)    

2. Save, deploy and execute the integration flow.

3. Check your configured inbox. You should get the following email:

    ![Error email](Error email.png)   

[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 4: ](Switch to Error End Event)]

In the following exercise, we will change the _End Message_ in the Exception Sub-process to an _Error End Event_.
The _Error End Event_ will change the Message Execution status to _Failed_ - similar to how it would have been if the exception was not handled in the Exception Sub-process. Additionally you shall be able to perform additional tasks in case of an exception.

Hence to summarize, with _End Message_, the Message Execution Status is set to _Completed_, where as with _Error End Event_, the status is set to _Failed_. In both cases, you can add additional processing in case of an error.

1. Delete the _End Message_ step in the _Exception Sub-process_ and replace it with an _Error End Event_ step:

    * Hover over the _End Message_ step and click on the _Delete_ speed button:

      ![Delete End](Delete End.png)

    * Click on the design Palette
    * Choose _Events_
    * Choose _Error End Event_

        ![Choose Error End Event](Choose Error End Event.png)

    * Drag it within the _Exception Sub-process_ and connect it to the _Error Start Event_.

        ![Add Error End Event](Add Error End Event.png)

2. Save, deploy and execute the integration flow.

    * Go to the _Monitoring view_ and look for an entry for the message just processed.  The status of the message should be __Failed__ - this message status indicates that the exception was raised, caught by the Exception Subprocess and finally set by the _Error End Event_.   

        ![Error View With Error End Event](Error View With Error End Event.png)    


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Use a send step to intimate an exception)]

In the following exercise, we shall be sending an email to the systems manager to intimate about the error in processing. You can model anything in the exception sub-process - similar to how you model the processing logic in the message execution pipeline. Here in the following steps, we are only sending an email.
Please note that you cannot connect the _Error End Event_ to a receiver - Hence we will use an alternate way of sending an email.

1. Add a send step to the message execution pipeline:

    * Click on the design Palette
    * Choose _Call_
    * Choose _External Call_
    * Click on _Send_

        ![Choose Send Step](Choose Send Step.png)

    * Drag it within the _Exception Sub-process_ between the _Error Start Event_ and the _Error End Event_.

        ![Add Send Step](Add Send Step.png)  

  2. Add a mail receiver

      * Add a receiver to the canvas and connect it to the _Send_ step in the _Exception Sub-process_.
      * Choose a mail adapter:

      ![Add Receiver](Add Receiver 2.png)

      * Copy the mail channel configuration:

          * Click on any of the configured mail channel and choose the speed button for _Copy Configuration_.
          * Now select the newly created mail channel and choose the speed button for _Paste Configuration_. Click _yes_ on the confirmation pop-up.

            ![Copy Mail Configuration](Copy Mail Configuration Error End Event.png)    

      * Click on the new mail channel, go to the _Properties Sheet_, go to the _Connection_ tab and set the _Subject_ field to _An error occurred - Please resend_:

          ![Exception Mail](Exception Mail.png)    

  2. Save, deploy and execute the integration flow.

  3. Check your configured inbox. You should get the following email:

      ![Error email](Error email.png)   


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Switch to Escalation End Event)]

In this exercise, we shall change the _Error End Event_ to _Escalation End Event_ and understand how the processing differs. As we shall see, the processing is unchanged - the _Escalation End Event_ gives us a way to set the message execution status to a different value: _Escalated_.

1. Delete the _Error End Event_ step in the _Exception Sub-process_ and replace it with an _Escalation End Event_ step:

    * Hover over the _Error End Event_ step and click on the _Delete_ speed button:

      ![Delete End](Delete End 2.png)

    * Click on the design Palette
    * Choose _Events_
    * Choose _Escalation End Event_

        ![Choose Error End Event](Choose Escalation End Event.png)

    * Drag it within the _Exception Sub-process_ and connect it to the _Send_ step.

        ![Add Error End Event](Add Escalation End Event.png)

        ![Add Error End Event](Add Escalation End Event 2.png)

2. Save, deploy and execute the integration flow.

     * Go to the _Monitoring view_ and look for an entry for the message just processed.  The status of the message should be __Escalated__ - this message status indicates that the exception was raised, caught by the Exception Sub-process and finally set by the _Escalation End Event_.   

        ![Error View With Error End Event](Escalation message status.png)    

        The _Error Details_ section gives more information about the exception.

    * To get additional information about the exception, scroll down to the _logs_ section (or click on the _Logs_ tab) and click on _Info_.

    ![Error Details](Escalation details.png)

    A graphical viewer opens up and provides a visual representation of the exception location on the message execution pipeline. The graphical viewer shows that the exception thrown by the _Script_ step was caught in the _Exception Sub-process_. A mail was send and the message execution status was set to _Escalated_.

3. The mail received is also exactly the same as with the _Error End Event_:

    Check your configured inbox. You should get the following email:

    ![Error email](Error email.png)   


[VALIDATE_1]

[ACCORDION-END]

---
