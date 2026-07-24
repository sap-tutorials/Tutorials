---
author_name: Dominik Schreiber
author_profile: https://github.com/dominikschreiber
auto_validation: true
time: 45
tags: [tutorial>advanced, tutorial>how-to, software-product>sap-business-technology-platform, software-product>ui-theme-designer]
primary_tag: software-product>ui-theme-designer
parser: v2
---

# Integrate SAP BTP, Cloud Foundry Themes with SAP S/4HANA

<!-- description -->Learn how to use UI theme designer service on SAP BTP Cloud Foundry with your SAP S/4HANA on-prem system: Create a service key in CF, a destination with an OAuth client in S/4HANA, and a customizing for remote theming. End-to-end test the connection.

## You will learn

- How to create a service key for UI theme designer on SAP BTP Cloud Foundry ("CF theming service").
- How to create an OAuth client in `SOAUTH2_CLIENT` of your S/4HANA on-prem system ("ABAP system") with this service key, that can be used for theming.
- How to create a destination to the _CF theming service_ in `SM59` of your _ABAP system_.
- How to create a customizing in `SPRO` to use this destination for remote theming of your _ABAP system_.
- How to end-to-end-test the connection: create a theme in the _CF theming service_, apply it in your _ABAP system_.

## Prerequisites

- In your _ABAP system_: User with permission to create an **OAuth client** in `SOAUTH2_CLIENT`.
- In your _ABAP system_: User with permission to create a **destination** in `SM59`.
- In your _ABAP system_: User with permission to do **customizing** in `SPRO`.
- In SAP BTP Cloud Foundry: Subaccount with [UI Theme Designer: Initial Setup of UI Theme Designer](https://help.sap.com/docs/btp/ui-theme-designer/initial-setup-of-ui-theme-designer) successfully carried out.
- In SAP BTP Cloud Foundry: User with permission to create a **Service Key**.
- In SAP BTP Cloud Foundry: User with the `Workzone_Admin`/`Workzone_Advanced_Theming` or `Launchpad_Admin`/`Launchpad_Advanced_Theming` role collection (or role collection of your PaaS setup that includes the theming roles `Viewer`, `Editor`, `CustomCssEditor` and `Publisher`, as described in [UI Theme Designer: Permissions](https://help.sap.com/docs/btp/ui-theme-designer/permissions)).

Throughout this tutorial, we'll refer with _CF theming service_ to the UI theme designer service on SAP BTP Cloud Foundry, and with _ABAP system_ to your SAP S/4HANA on-prem system.

---

### Create the service key in SAP BTP Cloud Foundry

In this tutorial, you'll configure your _ABAP system_ to use the _CF theming service_ as a destination for "remote theming". The _CF theming service_ requires authenticated requests. Your _ABAP system_ will use OAuth2 client credentials to authenticate. In this step, you'll create a service key for the _CF theming service_ in the SAP BTP Cloud Foundry cockpit. Optionally, you can use the `cf` CLI as an alternative route to get this service key.

1. In the BTP Cockpit of your subaccount, from the Cloud Foundry space, choose **Services > Instances** from the left-hand navigation:

    ![1.1: BTP Cockpit - CF space](./01-btp-cockpit-space.png)

2. Select the service with **UI Theme Designer** in plan **standard** to view its details:

    ![1.2: BTP Cockpit - service instances](./02-btp-cockpit-service-instances.png)

3. Under **Service Keys** choose **Create**:

    ![1.3: BTP Cockpit - CF theming service instance](./03-btp-cockpit-service-instance.png)

4. In the **New Service Key** dialog, provide a meaningful machine-friendly **Service Key Name** like "abap-remote-theming", leave binding parameters as the default `{}` and choose **Create**:

    ![1.4: BTP Cockpit - create service key](./04-btp-cockpit-create-service-key.png)

5. Under **Service Keys** select the **newly created service key**:

    ![1.5: BTP Cockpit - CF theming service instance with service key](./05-btp-cockpit-instance-with-service-key.png)

6. From the **Credentials** dialog, copy `uaa.clientid`, `uaa.clientsecret`, `uaa.url` and `uri` (you will need `uaa.clientid` in _step 2.3_, `uaa.url` in _step 2.4_, `uaa.clientsecret` in _step 2.6_ and `uri` in _step 3.4_):

    ![1.6: BTP Cockpit - view service key](./06-btp-cockpit-view-service-key.png)

7. *Alternative:* Use the `cf` CLI to create the service key. See [Cloud Foundry Documentation: Using the Cloud Foundry Command Line Interface (cf CLI)](https://docs.cloudfoundry.org/cf-cli/) for details on how to interact with the `cf` CLI.

    ```sh
    # 1. login to your CF space, given $CF_API_URL, $CF_ORG and $CF_SPACE
    cf login -a $CF_API_URL -o $CF_ORG -s $CF_SPACE
    # 2. find the name of the service with offering=theming and plan=standard
    THEMING_SERVICE=$(cf services | awk '$2 == "theming" && $3 == "standard" {print $1}')
    # 3. create a service key named "abap-remote-theming" for that service
    cf create-service-key $THEMING_SERVICE abap-remote-theming --wait
    # 4. view uaa.clientid, uaa.clientsecret, uaa.url and uri of that service key
    cf service-key $THEMING_SERVICE abap-remote-theming | tail -n +3 | jq '{clientid: .credentials.uaa.clientid, clientsecret: .credentials.uaa.clientsecret, url: .credentials.uaa.url, uri: .credentials.uri}'
    ```

### Create the OAuth2 client with SOAUTH2_CLIENT in SAP S/4HANA

You have just created a service key for your _CF theming service_. Now you'll create an OAuth2 client in your _ABAP system_ so that destinations can authenticate against your _CF theming service_ with that service key.

1. In your _ABAP system_, start the transaction `SOAUTH2_CLIENT`:

    ![2.1: SAPGUI - easy access with SOAUTH2_CLIENT](./07-easy-access-SOAUTH2_CLIENT.png)

2. In transaction `SOAUTH2_CLIENT`, select **Create**:

    ![2.2: SOAUTH2_CLIENT entry screen](./08-SOAUTH2_CLIENT.png)

3. In the **Select Profiles** dialog, select `/UI5/THEMING_REMOTE` as **OAuth 2.0 Client Profile**, use `uaa.clientid` (from _step 1.6_) as **OAuth 2.0 Client ID** and confirm with **OK** (the checkmark button in the dialog footer toolbar):

    ![2.3: SOAUTH2_CLIENT - select OAuth 2.0 profiles dialog](./09-oauth2-profile.png)

4. Back in **General Settings**, use `uaa.url` (from _step 1.6_), without the leading `https://` and with an additional trailing `/oauth/token` as **Token Endpoint**. Example: if your `uaa.url` is "https://example.com", your Token Endpoint is "example.com/oauth/token":

    ![2.4: SOAUTH2_CLIENT with Token Endpoint](./10-SOAUTH2_CLIENT-with-profile.png)

5. Select the tab **Client Authentication**, under **Authentication Methods** choose **Form Fields** and select **Edit** (the pen button next to **Client Secret**):

    ![2.5: SOAUTH2_CLIENT - Client Authentication tab, Authentication Methods section](./11-SOAUTH2_CLIENT-authentication.png)

6. In the **Client Secret** dialog, use `uaa.clientsecret` (from _step 1.6_) as **New Client Secret** and confirm with **OK** (the checkmark button in the dialog footer toolbar):

    ![2.6: SOAUTH2_CLIENT - new client secret dialog](./12-client-secret.png)

7. Back in **Client Authentication**, under **PSE for Https/TLS Communication**, set "DFAULT" as **SSL Client PSE**:

    ![2.7: SOAUTH2_CLIENT Client Authentication tab, PSE for Https/TLS Communication section](./13-pse.png)

8. Select the tab **Grant Types**, choose **Client Credentials**, confirm everything with **Save** (the disk button next to the OKCode field), and switch back to **Display** mode:

    ![2.8: SOAUTH2_CLIENT Grant Types tab](./14-grant-types.png)

9. In **Display** mode, in the header toolbar, select **Token**:

    ![2.9: SOAUTH2_CLIENT in Display mode with Token button](./15-token.png)

10. In the **Token Status** dialog, select **Request Token** in the footer toolbar:

    ![2.10: SOAUTH2_CLIENT - token status dialog](./16-token-status.png)

11. If the **Token Status** dialog has a section **Access Token** with an entry **Valid, expires in 29 minutes**, you've successfully created an OAuth2 client with the _CF theming service_ client credentials. You can now close the `SOAUTH2_CLIENT` transaction:

    ![2.11: SOAUTH2_CLIENT - token status dialog with valid access token](./17-token-status-success.png)

### Create the destination with SM59 in SAP S/4HANA

You have successfully created a service key for your _CF theming service_ and created an OAuth2 client in your _ABAP system_ that uses the service key to authenticate. Now, you'll create a destination in the _ABAP system_ that uses the just created OAuth2 client.

1. In your _ABAP system_, start the transaction `SM59`:

    ![3.1: SAPGUI - easy access with SM59](./18-easy-access-SM59.png)

2. In `SM59`, select **Create** (the document button in the table header toolbar):

    ![3.2: SM59 - initial view](./19-SM59-initial-view.png)

3. In the **Create Destination** dialog, pick a machine-friendly name as **Destination**, for example "abap-remote-theming" (you will need this name in _step 4.4_), select **Connection Type** "G HTTP connection to external server" and confirm with **OK** (the checkmark button in the dialog footer toolbar):

    ![3.3: SM59 - create destination dialog](./20-SM59-create-dialog.png)

4. Back in `SM59`, select the tab **Technical Settings**. Under **Target System Settings**, use the _hostname_ of the `uri` of your service key (from _step 1.6_), without `https://` and any path, as **Host** and "/themeroot/v1" as **Path Prefix**. For example, if your `uri` is "https://example.com/any/path", your **Host** is "example.com":

    ![3.4: SM59 - technical settings](./21-SM59-technical-settings.png)

5. Select the tab **Logon & Security**. Under **Logon Procedure**, section **Logon with User**, choose **OAuth Settings**:

    ![3.5: SM59 - logon & security](./22-SM59-logon-and-security.png)

6. In the **OAUTH Settings** dialog, select the **Value Help** of the **Profl.** input:

    ![3.6: SM59 - oauth settings](./23-SM59-oauth-settings.png)

7. In the value help dialog, select the value where **OAuth 2.0 Client Profile** and **OAuth 2.0 Client Configuration** are "/UI5/THEMING_REMOTE". Confirm with **OK** (the checkmark button in the dialog footer toolbar):

    ![3.7: SM59 - profl value help](./24-SM59-oauth-settings-f4.png)

8. Back in the **OAUTH Settings** dialog, confirm the changes with **Save** (the disk button in the dialog footer toolbar):

    ![3.8: SM59 - oauth settings filled](./25-SM59-oauth-settings-filled.png)

9. Back in **Logon & Security**, under **Security Options**, subsection **Status of Secure Protocol**, choose **Active** for **SSL**. Confirm the profile creation with **Save** (the disk button next to the OKCode field). You can now close the `SM59` transaction:

    ![3.9: SM59 - SSL active](./26-SM59-ssl-active.png)

### Create the customizing with SPRO in SAP S/4HANA

You already have a service key for your _CF theming service_, and in your _ABAP system_ an OAuth2 client that uses the service key, and a destination that uses the OAuth2 client. The last setup step is to create a customizing in the `SPRO` transaction that tells UI theme designer in the _ABAP system_ to use the remote theming scenario.

1. In your _ABAP system_, start the transaction `SPRO`:

    ![4.1: SAPGUI - easy access with SPRO](./27-easy-access-SPRO.png)

2. In `SPRO`, select **SAP Reference IMG** from the header toolbar:

    ![4.2: SPRO - initial view](./28-SPRO-initial-view.png)

3. Under **SAP Customizing Implementation Guide** expand the following hierarchy: **ABAP Platform**, **UI Technologies**, **UI Theme Designer**. Choose **Activate** (the clock/checkmark button) for **Select the Theming Scenario**:

    ![4.3: SPRO - sap reference img](./29-SPRO-sap-reference-img.png)

4. In the `/UI5/THEM_CUSTOMIZE` customization, under **Select the theming scenario**, select **Remote theming scenario**. Use the name of the destination you created (in _step 2.3_) as **RFC Destination (Type G)**. Confirm with **Save**. You can now close the `SPRO` transaction:

    ![4.4: SPRO - /UI5/THEM_CUSTOMIZE](./30-SPRO-ui5-them_customize.png)

### Verify that your SAP BTP, Cloud Foundry Themes are integrated with your SAP S/4HANA system

Congratulations, you have gone through all configuration steps: you have a service key for your _CF theming service_, and in your _ABAP system_ an OAuth2 client that uses the service key, a destination that uses the OAuth2 client, and a customization that tells UI theme designer to use the remote theming scenario. Now you can verify that this setup works as intended.

1. In your _ABAP system_, start the transaction `/UI5/THEME_DESIGNER`:

    ![5.1: SAPGUI - easy access with /UI5/THEME_DESIGNER](./31-easy-access-THEME_DESIGNER.png)

2. Instead of opening the theme designer web app in the browser, the transaction shows an error: **The system is connected to a remote theming infrastructure**. You can now close the `/UI5/THEME_DESIGNER` transaction:

    ![5.2: THEME_DESIGNER - error](./32-THEME_DESIGNER-error.png)

3. Make sure you have a published theme in your _CF theming service_ instance. Follow the [Create a Theme with the UI Theme Designer](https://developers.sap.com/tutorials/create-custom-theme.html) tutorial or [UI Theme Designer: Create and Edit Themes and Theme Sets](https://help.sap.com/docs/btp/ui-theme-designer/create-and-edit-themes-and-theme-sets) to create one:

    ![5.3: theming-ui - published theme](./33-theming-ui-published.png)

4. In your _ABAP system_, start the transaction `/UI2/FLP`:

    ![5.4: SAPGUI - easy access with /UI2/FLP](./34-easy-access-FLP.png)

5. In the **FLP**, select your **Avatar** (the circle in the top-right corner) to open the **User Menu**:

    ![5.5: FLP - initial view](./35-FLP-initial-view.png)

6. From the **User Menu**, select **Settings**:

    ![5.6: FLP - user menu](./36-FLP-user-menu.png)

7. From the **Settings** dialog, select **Appearance**:

    ![5.7: FLP - settings dialog](./37-FLP-settings-dialog.png)

8. In addition to the SAP-provided themes, the list of available themes should contain exactly the published themes from your _CF theming service_. Select your published theme and confirm with **Save**.

    ![5.8: FLP - appearance dialog](./38-FLP-appearance-dialog.png)

9. Reload the page. It should now appear in the theme you published in your _CF theming service_ instance:

    ![5.9: FLP - with custom theme](./39-FLP-custom-theme.png)

---
