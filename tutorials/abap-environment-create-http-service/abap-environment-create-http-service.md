---

title: Create an HTTP Service  
description: Create an HTTP service in the ABAP environment that can be called from the browser.
auto_validation: true
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, topic>cloud, products>sap-cloud-platform, tutorial>license ]
time: 15
author_name: Julie Plummer
author_profile: https://github.com/julieplummer20
---

##Prerequisites
- **IMPORTANT**: This tutorial cannot be completed on a trial account

## Details
### You will learn  
  - How to create an HTTP service that can be accessed from a browser
  -	How to return system data using a (whitelisted) ABAP utility class
  - How to expose the service for external consumption, by defining the necessary inbound communication artefacts

**Important**: If you are working in SAP S/4HANA:
ICF services are direct entry points into the ABAP system via the HTTP protocol. Implementing ICF services is a security-critical task with implications on the system and landscape configuration.

Therefore, to follow the best practices adopted by SAP internally, consider implementing your service as an OData service.

---

[ACCORDION-BEGIN [Step 1: ](Create an HTTP service)]
1. Select a package and choose **New > Other Repository Object** from the context menu:

    ![Image depicting step-1a-new-repo-object](step-1a-new-repo-object.png)

2. Enter the filter text **HTTP** and choose **Next**:

    ![Image depicting step-1b-choose-HTTP-service](step-1b-choose-HTTP-service.png)

3. Enter a **Name:`Z_GET_DATE_HTTP_XXX`** and **Description:Get system date** for your service and choose **Next**:

    !![Image depicting step-1c-name-service](step-1c-name-service.png)

4. Choose or create a **transport request**:

    !![Image depicting step-1d-transport-request](step-1d-transport-request.png)

The new HTTP service is displayed on a new tab. The handler class and URL are generated automatically, in the form:
**`https://<server:port>/sap/bc/http/sap/<service_name>?sap-client=100`**

!![Image depicting step-1e-new-service-created](step-1e-new-service-created.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Implement the handler class)]
Now, you will implement the handler class, starting with a simple text output.

1. Open the handler class by clicking on the hyperlink:

    ![Image depicting step-2a-open-handler-class](step-2a-open-handler-class.png)

2. The structure of the class and the interfaces statement for `IF_HTTP_SERVICE_EXTENSION` are generated automatically.
3. Go to the class implementation section and insert the following statement in the method:

    **`response->set_text('Hello!').`**

    ![Image depicting step-2b-insert-method](step-2b-insert-method.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Save and activate)]
**Save (`Ctrl+S`)** and **Activate (`Ctrl+F3`)** your class.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test the service)]
1. Go back to your HTTP Service. Test your service in the browser by clicking the URL link:

    ![Image depicting step-4-test-http-service](step-4-test-http-service.png)

2. If necessary, log in again. The preview open automatically in a new tab and display something like this:

    ![Image depicting step-4b-hello](step-4b-hello.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add system date to the method)]
Now you will add to the method by fetching the date from the back end first.

In the ABAP environment, you can only use whitelisted APIs. Therefore, you cannot use `SY-DATUM`. Instead, you call the appropriate method of the class `CL_ABAP_CONTEXT_INFO`.

You then cast this date variable to a string variable and output that as before.

Delete the statement `response->set_text('Hello again!').` and add the following to your code:

```ABAP
DATA(system_date) = CL_ABAP_CONTEXT_INFO=>get_system_date( ).
DATA: text type string.

text = system_date.
response->set_text( text ).
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test the service again)]
1. Save (`Ctrl+S`) and activate (`Ctrl+F3`) your class.
2. Test your service by clicking the URL link again. This time, the preview should display something like this:

.
    ![Image depicting step-6-system-date](step-6-system-date.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Check code)]
Your code should look like this:

```ABAP
class Z_GET_DATE_HTTP_XXX definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.

CLASS Z_GET_DATE_HTTP_XXX IMPLEMENTATION.

  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
      DATA(system_date) = CL_ABAP_CONTEXT_INFO=>get_system_date( ).
      DATA: text type string.

      text = system_date.
      response->set_text( text ).
  endmethod.
ENDCLASS.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create an inbound Communication Scenario)]
You will now create the artefacts you need to allow other systems to call your service compliantly. This involves some overhead for one consumer; however, the advantage is that you can add several consumer systems, or users (for example, with different authentication) pointing to the same HTTP service, wrapped in the same Communication Scenario.

![step9-create-comm-artefacts-overview](step9-create-comm-artefacts-overview.png)

 First, create the **Communication Scenario**.

1. Select your package, then choose **New > Other Repository Object...** from the context menu.

    ![step9a-new-other](step9a-new-other.png)

2. Add the filter **`scen`**, then choose **Communication Scenario**, then choose **Next**.

    ![step9c-create-comm-scenario](step9c-create-comm-scenario.png)

3. Add a **Name: `Z_WRAP_HTTP_INBOUND_XXX`** and **Description**, choose a transport request, then choose **Finish**.

Your Communication Scenario appears.

![step9-new-comm-scen](step9-new-comm-scen.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Add the HTTP service)]
1. On the **Inbound** tab, choose **Add...**.

    ![step10-add-http-service](step10-add-http-service.png)

2. **IMPORTANT**: Choose **Browse**. You cannot simply enter the name. Then add a filter, such as **`Z_HTTP`**, select your service, then choose **Finish**.

    ![step10a-browse-for-service](step10a-browse-for-service.png)

3. Your service appears. Choose **Publish Locally**.

    ![step10-b-publish-locally](step10-b-publish-locally.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 11: ](Create Communication Arrangement)]
1. Open the dashboard for your system. You can find the URL for the dashboard by selecting your system (that is, ABAP Project in Project Explorer), then choosing **Properties > ABAP Development** from the context menu.

    ![step11a-open-flp](step11a-open-flp.png)

2. From **Communication Management**, choose **Communication Arrangement**. Then choose **New**.

    ![step11b-new](step11b-new.png)

3. Choose your scenario, **`Z_WRAP_HTTP_INBOUND_XXX`** from the drop-down list. Accept the default (identical) Arrangement name.

    ![step11c-name-comm-arr](step11c-name-comm-arr.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 12: ](Create Communication System)]
1. From the Dashboard Home screen, choose **Communication Systems**.

2. Enter the name of your **Communication Arrangement**, then for **Communication System**, choose **New**.

3. Enter a **System ID** and Accept the default (identical) System name, then choose **Create**.

4. In **Technical Data > General > Host Name**, enter **Dummy**. Leave the other defaults and choose **Save**.

    ![step12a-new-system](step12a-new-system.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Create Communication User)]
1. Scroll down to **Users for Inbound Communication**, then create a new user by choosing the **+** icon.

    ![step13a-create-comm-user](step13a-create-comm-user.png)

2. Choose **New User** and the **Authentication Method: User name and password**.

    ![step13b-choose-new-user](step13b-choose-new-user.png)

3. Enter a name and description, then choose **Propose password**, then choose **Create > OK > Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Save Communication Arrangement)]

![step14-save-comm-arr](step14-save-comm-arr.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 15: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

## More Information

- [SAP Help Portal: HTTP Communication](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/dee3a93a2b8d4018b3c4910f745b744f.html)

- [SAP Help Portal: Components of SAP Communication Technology - HTTP Service](https://help.sap.com/doc/saphelp_nwpi71/7.1/en-US/1f/93163f9959a808e10000000a114084/frameset.htm)

---
