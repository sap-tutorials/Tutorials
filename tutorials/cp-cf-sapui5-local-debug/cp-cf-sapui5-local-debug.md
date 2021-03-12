---
title: Debug Your SAPUI5 App
description: Debugging and logging are the nut and bolts to inspect your application. Learn how to debug and test your web apps with the Google Chrome Dev Tools.
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>html5, topic>cloud, topic>javascript, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: topic>sapui5
---

## Details
### You will learn
  - How to do add breakpoints to your JavaScript code
  - How to log data to the console
  - How to use the Google Chrome Dev Tools.

---

[ACCORDION-BEGIN [Step : ](Add a message page)]

Add a simple message page to let the users know you are still working on this app.

Replace the existing page in the file `webapp/view/MainView.view.xml` with
```JavaScript
<MessagePage showHeader="false"
  description="More content to come"
  text="Stay tuned!"
  icon="sap-icon://home" />
```


![message](./messagepage.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Write a log message to the console)]

Add this `onBeforeRendering` hook to the file `webapp/controller/MainView.controller.js`. This error function allows you to write error messages to the console. Error messages also write the stack trace to the console, which can be used to trace the message back to the line of invocation.
```JavaScript
onBeforeRendering: function() {
  jQuery.sap.log.error("A problem occurred!");
}
```

![logger](./logger.png)


> This `onBeforeRendering` method is called every time the View is rendered, before the Renderer is called and the HTML is placed in the DOM-Tree. It can be used to perform clean-up-tasks before re-rendering.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add a breakpoint)]

Add this `onAfterRendering` hook to the same file to place a breakpoint in your code. A breakpoint will cause your app to stop when the execution thread reaches it. This gives you the chance to inspect the state of you app.
```JavaScript
,

onAfterRendering: function() {
	debugger
}
```

![debugger](./debugger.png)


> This `onAfterRendering` method is called every time the View is rendered, after the HTML is placed in the DOM-Tree. It can be used to apply additional changes to the DOM after the Renderer has finished.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step: ](Stop at the breakpoint)]

Test the changes  on your local machine.
```
npm start
```
This command should start the app and open your browser automatically. Open the Chrome Dev Tools (or the [dev tools of your favorite browser](https://www.lifewire.com/web-browser-developer-tools-3988965)) by **clicking F12**. **Refresh the page** to run the script one more time.

You should now see that the app reached the breakpoint (the dev tools automatically switched to the `Sources` tab).

![stopped](./stopped.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](See the error log message)]

Click **F8** to jump over the breakpoint and **switch** to the `Console` tab.
Now you should see your error message printed in red. Click on the small triangle on the left side to expand the error message.


Can you spot the line in which you invoked the error message?

![testlog](./testlog.png)





[VALIDATE_1]
[ACCORDION-END]

---
