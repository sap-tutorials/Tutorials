---
parser: v2
author_name: Marc Bell
author_profile: https://github.com/MarcBell-SAP
auto_validation: true
time: 20
tags: [ tutorial>advanced, software-product>sap-concur]
primary_tag: software-product>sap-concur
---

# Refresh token management automation
<!-- description --> Uses timestamps and a json file to automatically manage refresh token expiration.

## Prerequisites
 - You have created a Refresh Token from an API call using the previous [concur-get-refresh-token tutorial](https://developers.sap.com/tutorials/concur-get-refresh-token.html).
 - SAP Concur instance credentials
 - [Node.js](https://nodejs.org/)
 - [GitHub account](https://github.com/)
 - IDE or text editor

## You will learn
  - How to manage refresh token expiration using timestamps and a json file

---

### Regenerate your company request token
1. Go to the [**Company Request Token**](https://www.concursolutions.com/nui/authadmin/companytokens) screen.
2. Enter your App ID in the required field and click Submit to regenerate your company request token.
3. Copy/paste the Company UUID and company request token fields to a text file.
        <!-- border -->![image](img/company-request-token-created.png)
4. Click **OK** after you have saved this information to a text file.
        <!-- border -->![image](img/company-request-token-created-ok.png)

### Setup the NodeJs app
1. Get the [`NodeJs sample code`](https://github.com/SAP-samples/concur-api-samplecode).
2. Open ``SampleCode/Tutorial_2/RefreshTokenManagement.js`` in your IDE or text editor.

### Regenerate your credentials file
1. Type ``node RefreshTokenManagement.js`` in terminal to run the app to generate your ``credentials.json`` file. This generates messages in the terminal that show the credentials object.
        <!-- border -->![image](img/run-refresh-token-management-app.png)
        <!-- border -->![image](img/run-refresh-token-management-app-generate-file.png)
2. Fill in the empty fields in this file with the information in the credential object. You can use all of the information from your ``credentials.json`` file in the previous [concur-get-refresh-token tutorial](https://developers.sap.com/tutorials/concur-get-refresh-token.html) except for the company request token.
        <!-- border -->![image](img/credentials-json.png)

### Regenerate and store your refresh token in a json file
1. Run the app again. Since the app does not have a stored valid refresh token, the app will not find a valid refresh token in ``refreshTokenObjectFile.json``.
        <!-- border -->![image](img/run-refresh-token-management-app.png)
        <!-- border -->![image](img/no-stored-refresh-token-found.png)
2. The app will call the Oauth2 /token endpoint using your credentials and regenerate your refresh token.
        <!-- border -->![image](img/regenerate-refresh-token.png)
3. If the API call is successful, the refresh token data is converted from JSON format into a refresh token object and stored in the file ``refreshTokenObjectFile.json``.
        <!-- border -->![image](img/stored-refresh-token.png)
        <!-- border -->![image](img/refresh-token-object-file.png)

### Load your stored refresh token from from a json file
1. Run the app again. Since the app now has a valid refresh token, the app will successfully load and check the validity of the file ``refreshTokenObjectFile.json``.
        <!-- border -->![image](img/run-refresh-token-management-app.png)
2. Check for a successful validity check on the refresh token object loaded from ``refreshTokenObjectFile.json``. If the refresh token object validity check is successful, the refresh token object will be stored in the app for use with future API calls.
        <!-- border -->![image](img/loaded-refresh-token-valid.png)
        <!-- border -->![image](img/refresh-token-object-file.png)


---
