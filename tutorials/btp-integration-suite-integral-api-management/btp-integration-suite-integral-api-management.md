---
auto_validation: true
time: 20
tags: [ tutorial>advanced, tutorial>free-tier, software-product>sap-integration-suite, software-product>cloud-integration, software-product>sap-api-management,  software-product>sap-business-technology-platform  ]
primary_tag: software-product>sap-api-management
parser: v2
author_name: Mariajose Martinez 
author_profile: https://github.com/mariajosesap
---
# Configure SAP API Management policies to avoid CORS issues and generate an API Key
<!-- description --> Learn how to set up API Management policies to avoid CORS issues when calling the API endpoint from SAP Build Apps (formerly SAP AppGyver), also you will learn how to apply security policies to enable the API consumption via an API Key.

## Prerequisites

 - You have a SAP BTP account or trial account with access to the SAP Integration Suite.
 - You have completed the previous tutorial [Send SMS using SAP Cloud Integration while consuming a Twilio API](btp-integration-suite-integral-cpi-urlencoded).


## You will learn

  - How to set up API Management policies to avoid CORS issues when calling the API endpoint.
  - How to apply security policies to enable the API consumption via an API Key.

### Set up the policy to avoid CORS issues

1. Go to your SAP API Portal (in the main SAP Integration Suite portal).

2. Create an API and select URL. Here you're going to paste your IFlow endpoint from past tutorials. Give it a name: `APIBestRunDemo`. Give it a path: `/https/salesOrder`, and service type `REST`.

    ![Create the API](create_api.png)

3. After creating it, go to the Proxy Endpoint tab and add the following in this order, and save it:

    ![Add the route rules](route_rules.png)

4. Now, go to Policies (if it doesn't show up in the top-right of your screen, click on the 3 dots).

    Here you'll add the required policies to avoid CORS issues while calling this API from SAP Build Apps and configure the API Key policy later on.

5. On the Edit Mode, start adding the Policies as followed:

    In the PostFlow inside the Proxy Endpoint configuration, add an `Assign Message` policy as an `OutgoingResponse` and name it `setCORS`. Like this:

    ![Add the route rules](create_set_cors_policy.png)

6. Copy and paste this script:

    <!-- cpes-file db/schema.cds -->
    ```XML
    <!-- This policy can be used to create or modify the standard HTTP request and response messages -->
    <AssignMessage async="false" continueOnError="false" enabled="true" xmlns='http://www.sap.com/apimgmt'>
            <Set>
                <Headers>
                    <Header name="Access-Control-Allow-Origin">*</Header>
                    <Header name="Access-Control-Allow-Headers">set-cookie, origin, accept, maxdataserviceversion, x-csrf-token, authorization, dataserviceversion, accept-language, x-http-method, content-type, X-Requested-With, apikey</Header>
                    <Header name="Access-Control-Max-Age">3628800</Header>
                    <Header name="Access-Control-Allow-Methods">GET, PUT, POST, DELETE</Header>
                    <Header name="Access-Control-Expose-Headers">set-cookie, x-csrf-token, x-http-method</Header>
                </Headers>
            </Set>
            <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
            <AssignTo createNew="false" type="response">response</AssignTo>
    </AssignMessage>
    ```
    >Notice that you're already adding `apikey` as a header in the policy.

    You should look it like this:

    ![CORS Policy](set_cors_policy.png)

7. In the ProxyEndpoint click on `+` to add one. Name it `preflight` and in the condition string paste: `request.verb == "OPTIONS"`. Like this:

    ![Preflight Condition String](preflight.png)

    This is all you need to avoid CORS issues when calling our API from SAP Build Apps.

    >On write "preflight" step, you may find a bug on the API Management UI, where the Update button stay on grey/disable. To avoid it and save your changes, you must create a dummy assign message anywhere, save it and then remove it.

### Create a Key Value Map for your SAP Cloud Integration Credentials

1. Before adding the needed policies, create first a Key Value Map with your CPI credentials (to access the Cloud Integration Platform API endpoint). You will need this credential to reference it in the API Key Policy.

    Go to Configure (click on the tool icon) -> go to the `Key Value Maps` tab and create one.


    ![Create a Key Value Map](key_value_map.png)

2. Put `CPICredentials` as the name and declare your CPI credentials (username and password). Check the `Encrypt Key Value Map` box.



    ![Create a Key Value Map](create_cpi_credentials.png)

### Add the Verify API Key Policy

Go back to your API policies.

In the TargetEndpoint, add 3 policies in the PreFlow.

1. Add a Key Value Map Operations policy, name it and leave it as a Incoming Request. This is needed to get the CPI Credentials created before, as a Key Value Map. Like this:

    ![Create a Key Value Map](key_value_policy.png)

    Copy and Paste this script (you will be referencing 'CPICredentials' with the mapIdentifier parameter):

    <!-- cpes-file db/schema.cds -->
    ```XML
    <KeyValueMapOperations mapIdentifier="CPICredentials" continueOnError="false" enabled="true" xmlns="http://www.sap.com/apimgmt">
    <!-- Read parameter with key "username" and assign its value to private variable BasicAuthUsername-->
    <Get assignTo="private.BasicAuthUsername" index='1'>
    <Key><Parameter>username</Parameter></Key>
    </Get>
    <!-- Read parameter with key "password" and assign its value to private variable BasicAuthPassword-->
    <Get assignTo="private.BasicAuthPassword" index='1'>
    <Key><Parameter>password</Parameter></Key>
    </Get>
    <Scope>environment</Scope>
    </KeyValueMapOperations>
    ```       
    Like this:

    ![Set up the Key Value Map Policy](setup_key_value_policy.png)

2. Add a Basic Authentication policy, name it and leave it with the `IncomeRequest` stream. Like this:

    ![Add a Basich Authentication Policy](basic_authentication_policy.png)

    Copy and paste this script:

    <!-- cpes-file db/schema.cds -->
    ```XML
    <BasicAuthentication async='true' continueOnError='false' enabled='true' xmlns='http://www.sap.com/apimgmt'>
        <!-- Operation can be Encode or Decode -->
        <Operation>Encode</Operation>
        <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
        <!-- for Encode, User element can be used to dynamically populate the user value -->
        <User ref='private.BasicAuthUsername'></User>
        <!-- for Encode, Password element can be used to dynamically populate the password value -->
        <Password ref='private.BasicAuthPassword'></Password>

        <!-- Assign to is used to assign the encoded value of username and password to a variable. This should not be used if the operation is Decode -->
        <AssignTo createNew="true">request.header.Authorization</AssignTo>
    </BasicAuthentication>
    ```  

    You should look it like this:

    ![Set up a Basich Authentication Policy](setup_basic_authentication_policy.png)

3. Add a API Key verification policy to request the API Key every time the API is called (you'll later need to create the API Product and subscribe to the application to get the API Key). Add the `Verify API Key` policy, same in the PreFlow. Like this:

    ![Set up a Basich Authentication Policy](setup_basic_authentication_policy.png)

    <!-- cpes-file db/schema.cds -->
    ```XML
    <!--Specify in the APIKey element where to look for the variable containing the api key-->
        <VerifyAPIKey async='true' continueOnError='false' enabled='true'
            xmlns='http://www.sap.com/apimgmt'>
        <APIKey ref='request.header.ApiKey'/>
    </VerifyAPIKey>
    ```

    This should be the first policy to be triggered. Make sure it is set it up as the first one (you can change the order with the arrows in the top-right). Like this:

    ![Set up Verify API Key Policy](setup_verify_api_key.png)

4. Click on Update, Save and Deploy your API project.

    ![Deploy your API](deploy_api.png)

### Create an API Product and Subscribe to the Application

1. Now you need to create the API Product. Go to the pencil icon (develop), click on the `Products` tab and create one. Put this name to your Product `APIBestRunDemoProduct`.

    ![Create an API Product](create_product.png)

3. Reference your API to your Product.

    ![Reference your API](reference_your_api.png)

4. Publish your Product, click on `Publish`.

5. Then, go to the API Business Hub Enterprise portal.

    ![Publish you Product](publish_api.png)

    You should be able to see it in your SAP API Business Hub:

    ![API Product](API_product.png)

6. Enter the API product and subscribe it by creating a new Application:

    ![Create API Application](create_api_application.png)

    Name it `APIBestRunDemoApplication`, like this:

    ![Create API Application](create_api_application_name.png)


7. Here's where you can get your API Key, crucial to authenticate to this API endpoint when consuming its service.

    ![Create API Application](api_key.png)

Now you've successfully created policies to avoid CORS issues and enable an API Key to authenticate and consume your API endpoint from SAP Integration Suite.

### Add additional tasks into your Integration Flow

Right now, as following the whole exercises, the IFlow is returning us the SMS message sent by consuming the Twilio API. But for the purpose of the exercise with SAP Build Apps, you're going to use as the return message, mainly the receipt URL when the payment transaction is successfully done with Stripe. For this, you need to save this message in the IFlow and retrieve it at the end of the IFlow.

Remember the previous tutorial about "Set up Write, Filter and Get Tasks in the Integration Flow…" for filtering the payment data and later calling the SAP C4C OData service? well, you are going to do the same, but in this case to retrieve the receipt URL from the Stripe request response.

1. Go back to SAP Cloud Integration and to your Integration Flow. Add a Write task after the Stripe Connector request reply, and configure the task as following:

    ![Add a Write Task](write_task.png)

2. And now add a Get task at the end of the IFlow to retrieve this returned message:

    ![Add a Get Task](get_task.png)

    Save and deploy your IFlow.

Now, it's time to set up the integration with SAP Build Apps. Check out the next and final tutorial of this series: [Integrate SAP Build Apps with SAP Integration Suite](btp-integration-suite-integral-appgyver).

### Check your knowledge

### One more time, check your knowledge