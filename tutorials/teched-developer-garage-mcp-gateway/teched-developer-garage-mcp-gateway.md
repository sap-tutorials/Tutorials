---
parser: v2
auto_validation: true
primary_tag: software-product>sap-integration-suite
tags: [tutorial>intermediate, programming-tool>sap-integration-suite, software-product>sap-business-technology-platform]
time: 60
author_name: Manuel Namyslo
author_profile: https://github.com/manouxnam
variables:
  - name: userNumber
    label: Your user number
    type: string
    maxLength: 3
    required: true
    placeholder: XX
---
# SAP TechEd Developer Garage: Build and Govern MCP Servers with SAP Integration Suite
<!-- description -->Use the MCP Gateway in SAP Integration Suite to expose the Shopify API as a governed MCP server for AI agents to consume

## You will learn
- How to work with the new API artifact in SAP Integration Suite
- How to host custom MCP servers with the MCP Gateway
- How to govern your MCP resources by enforcing policies
- How to make your MCP servers discoverable and consumable in the Developer Hub
- How to test your results with the MCP Inspector

## Prerequisites
- You have access to an SAP Integration Suite tenant provided by your SAP instructors
- A Shopify destination has been set up in advance by your SAP instructors

## Intro
Hello, dear TechEd attendees! I hope you're enjoying the event so far. I'm glad you made it to the Developer Garage to get your hands dirty with SAP's latest technology. You're in exactly the right place, because in this tutorial you'll learn how to use the MCP Gateway in SAP Integration Suite to expose third-party APIs as MCP servers for AI agents to consume. So roll up your sleeves, and let's get to work!

This tutorial gives you an introductory, hands-on experience with the new MCP Gateway capability of SAP Integration Suite. Today you'll work in an environment provided by SAP. To use this feature in a productive environment, you need either the Premium or [Enhanced Edition](https://community.sap.com/t5/technology-blog-posts-by-sap/sap-integration-suite-enhanced-edition-the-trust-layer-for-agentic-ai/ba-p/14376343) of SAP Integration Suite.

If you're new to this topic, here's a brief introduction: The MCP Gateway is the enterprise control plane for MCP. Where an MCP server hosts tools, the gateway governs access to them — enforcing authentication, authorization, rate limiting, and payload protection, while providing full monitoring and traceability. The MCP Gateway in SAP Integration Suite aggregates tools from SAP APIs, non-SAP APIs, integration flows, data sources, and external MCP servers into a single governed entry point.

In this hands-on tutorial, you'll create a new API artifact for a third-party API — in this case, Shopify. You'll then use this API artifact to host an entirely new MCP server and enforce policies and guardrails that apply at runtime. Finally, you'll publish the new MCP resource as a product in the SAP Developer Hub, making it discoverable and consumable from your preferred AI agent development environment.

---

### System access

As part of this tutorial, you will need to access the provisioned systems using the credentials shared with you. In the table below, you will find the necessary information to access each system.

> There are two SAP Integration Suite environments available for use in this tutorial. Use the one corresponding to your user range.

| System | URL |
| --- | --- |
| SAP Integration Suite - Dev Garage 1 (User01 - User99) | <https://url.sap/054nme> |
| SAP Integration Suite - Dev Garage 2 (User100 - User180) | <https://url.sap/i18ebv> |

### Create the API artifact

1. Start by creating a new integration package. In SAP Integration Suite, go to the **Design** tab and click **Create**.

![picture](screen1.png)

2. Enter the following values for your new integration package:

- **Name:** Shopify Integration Scenario User{{var:userNumber}} (replace `XX` with your user number)
- **Short Description:** This integration package establishes API and MCP connectivity to Shopify
- **Version:** 1.0.0

Then click **Save**.

![picture](screen2.png)

3. Make sure you're in edit mode. In your new integration package, go to the **Artifacts** tab, click **Add**, and select **API**.

![picture](screen3.png)

4. Select **Integration Cell** as your runtime profile and click **Next**.

![picture](screen4.png)

5. Your instructors have already added a Shopify destination to the BTP subaccount, so you can use it to create your API artifact. Select **API Provider** and click **Next**.

![picture](screen5.png)

6. On the **Destination** tab, select the **Shopify** destination and click **Next**.

![picture](screen6.png)

7. Enter the following values for your API artifact:

- **Name:** ShopifyAPI User{{var:userNumber}} (replace `XX` with your user number)
- **Relative URL:** /admin/api/2026-07
- **API Base Path:** /webshopUser{{var:userNumber}} (replace `XX` with your user number)
- **API Version:** 1.0.0
- **Runtime Profile:** Integration Cell

Click **Next** to proceed.

![picture](screen7.png)

8. To generate the API specification for the Shopify system, use the natural-language API specification feature in SAP Integration Suite. Enter the following prompt:

    ```text
    Create a REST API with three operations for managing Shopify orders (also called sales orders). Server URL: https://sap-teched-2025.myshopify.com/admin/api/2026-07; paths relative to it. In every response schema only `id` and `name` are required; other fields optional and nullable.

    **Op 1 — Query orders.** GET `/orders.json`. Optional query params: status (string: open, closed, cancelled, any; default open, pass any for all), limit (integer, max 250), financial_status (string), ids (string, comma-separated). Returns an `orders` array; each item: id (integer int64), name (string), email (string), financial_status (string), total_price (string), created_at (string).

    **Op 2 — Get order by ID.** GET `/orders/{order_id}.json`. order_id: required path param, integer int64. Returns a single `order` object with the same fields.

    **Op 3 — Create order** (a Shopify order, also called a sales order). POST `/orders.json`. The request body must wrap all details in a top-level `order` object containing: email (string, customer email, e.g. demouser@sap.com), financial_status (string, lowercase, one of: paid, pending, refunded, voided), and line_items (array, at least one item; each item has title (string, product name), price (string, unit price e.g. 29.99), quantity (integer)). Add a clear description to every field. Returns the created record wrapped in an `order` object with id (integer int64), name (string), total_price (string).
    ```

    Click **Generate** and wait a few seconds for the specification to be created. Then click **Add and Open in API Designer**.

![picture](screen8.png)

9. Your new API artifact has been assembled, and you can see the available operations. Click **Switch to API Details** to complete the setup.

![picture](screen9.png)

10. Switch to edit mode and go to the **Policies** tab. In the process flow, add a content modifier right after the **Authorization** step: select the connecting line and click the **+** button.

![picture](screen10.png)

11. Search for **Content Modifier** and select it.

![picture](screen11.png)

12. Open the settings of your content modifier and select the **Message Header** tab. Add a new header to the API call:

- **Action:** Create
- **Name:** X-Shopify-Access-Token
- **Source Type:** Constant
- **Source Value:** `<shopify-access-token>` (provided by your instructors)

![picture](screen12.png)

13. Select the **Authorization** step and open its **Policy Settings**. Add **ESBMessaging.send** to the scope and tick the box for **Trust Upstream MCP Authorization**.

![picture](screen13.png)

14. Save your project and click **Deploy**. When deploying, make sure you select **Integration Cell** as the runtime profile.

![picture](screen14.png)

### Create a new MCP server

1. With your API artifact deployed, you can now add a new MCP server on top of it. Go back to your integration package and open the **Artifacts** tab. In edit mode, click **Add** and select **MCP Server**.

![picture](screen15.png)

2. Select **API** as the source type and click **Next**.

![picture](screen16.png)

3. Select the API you created in the previous step (look for your user number). In the MCP details, enter the following values:

- **Name:** ShopifyAPI_User{{var:userNumber}}_mcpServer (replace `XX` with your user number)
- **MCP Path:** /webshopUser{{var:userNumber}}/mcp (replace `XX` with your user number)
- **Version:** 1.0.0
- **Runtime Profile:** Integration Cell

Click **Next** to proceed.

![picture](screen17.png)

4. In the tools configuration, select all three operations from your API artifact. Click **Add** to proceed.

![picture](screen18.png)

5. Your MCP project is now in place, and you can add the guardrails and governance rules your organization needs. Switch to edit mode and open the **Traffic Management** policies. Drag and drop a **Surge Protection** policy right after the **Authorization** step. Surge protection shields your backend systems from being overwhelmed by too many requests at peak times.

![picture](screen19.png)

6. Open the **Configure** tab of the surge protection and set it to abort the operation at **5 calls per 10 seconds**, as shown in the screenshot. Then click **Save**.

![picture](screen20.png)

7. When you're happy with your configuration, click **Deploy**. Select **Integration Cell** as the runtime profile and click **Deploy** again.

![picture](screen21.png)

### Create a product and AI agent subscription in the Developer Hub

1. To make your new MCP server discoverable and consumable across your organization, you'll publish it as a product in the Developer Hub. At the top right of your screen, select **Developer Hub**.

![picture](screen22.png)

2. In the Developer Hub, you can expose integration artifacts such as APIs, events, and MCP servers to other developers in your organization. In the **Admin Center**, click **Content**.

![picture](screen23.png)

3. On the **Business System** tab, select your Integration Cell instance. This shows all APIs and MCP servers deployed in your environment.

![picture](screen24.png)

4. Select the **MCP Servers** tab and search for the Shopify MCP server you deployed earlier (look for your user number). Select it and click **Create Product**.

![picture](screen25.png)

5. Give your new MCP product a clear name and description so other developers can identify and consume it:

- **Name:** Shopify MCP Product User{{var:userNumber}} (replace `XX` with your user number)
- **Description:** This product exposes Shopify endpoints as an MCP server

![picture](screen26.png)

Click **Publish** to proceed.

6. After a few minutes, your new product is deployed and visible on the Developer Hub landing page. Click your product to proceed.

![picture](screen27.png)

7. To let an AI agent consume this resource, create a new subscription by clicking **Create New Subscription for Agent**.

![picture](screen28.png)

8. Make sure your user number is included in the agent subscription name and click **Create**.

![picture](screen29.png)

9. After a few minutes, your new subscription is available and shows the corresponding OAuth credentials. Your MCP server can now be picked up by both SAP and non-SAP AI agents.

![picture](screen30.png)

### Test your MCP server with the MCP Inspector

1. Now that your MCP server is deployed and has a subscription, it's time to test it. For this, I recommend the **MCP Inspector**, an open-source tool from the Model Context Protocol project (created by Anthropic). Because SAP Integration Suite uses machine-to-machine (client credentials) authentication, you first need to retrieve a **Bearer token**. Run the command for your operating system in a terminal.

    [OPTION BEGIN [macOS]]
    Requires `jq` — install it with `brew install jq` if needed. In Terminal:

    ```bash
    TOKEN=$(curl -s -X POST '<your-token-URL>' \
      --data-urlencode 'grant_type=client_credentials' \
      --data-urlencode 'client_id=<your-key>' \
      --data-urlencode 'client_secret=<your-secret>' \
      | jq -r '.access_token')
    printf 'Bearer %s' "$TOKEN" | pbcopy
    ```
    [OPTION END]

    [OPTION BEGIN [Windows]]
    In **PowerShell**:

    ```powershell
    $resp = Invoke-RestMethod -Method Post `
      -Uri '<your-token-URL>' `
      -Body @{
        grant_type    = 'client_credentials'
        client_id     = '<your-key>'
        client_secret = '<your-secret>'
      }
    "Bearer $($resp.access_token)" | Set-Clipboard
    ```
    [OPTION END]

    Replace the token URL, key, and secret with the values from your Developer Hub subscription, and **keep the single quotes** around them — your key and secret may contain characters like `$` or `|` that would otherwise be misinterpreted. The command prints nothing on success: your Bearer token is now on the clipboard, ready to paste into the Inspector. Save it somewhere in case you copy something else in the meantime. The token is valid for one hour; if you get a `401` later, simply run the command again.

2. To launch the MCP Inspector, run the following command in your terminal (requires Node.js, which provides `npx` — get it from nodejs.org if needed):

    ```bash
    npx @modelcontextprotocol/inspector
    ```

    The Inspector starts a local proxy and opens automatically in your browser. If it doesn't, use the URL printed in the terminal, since it may include a required authentication token.

3. Once the MCP Inspector has opened, click **Add Servers** and select **+ Add manually**.

![picture](screen31.png)

4. Give your server an ID and change the transport mechanism to **streamable-http**. Then paste your MCP server URL, which you can find in your MCP project in SAP Integration Suite. Click **Add**.

![picture](screen32.png)

5. Go to your new server ID and select **Settings**. In the server settings, add a **Custom Header**: click **Add Header** and enter **Authorization** as the name. For the value, paste the Bearer token you copied in sub-step 1. It already includes the `Bearer` prefix, so paste it exactly once — the value should read `Bearer <your-bearer-token>`, not `Bearer Bearer …`.

![picture](screen33.png)

6. Once you have added the Bearer token, switch the toggle of your server to **Connected**. When the connection is verified, go to the **Tools** tab.

![picture](screen34.png)

7. The **Tools** tab gives you access to all MCP operations you selected during deployment. In this case, you can create and retrieve sales orders in Shopify. Try out the **Create order** tool by providing an email, a financial status, and the line items. In the MCP Inspector, line items need to be entered as an array — but don't worry, that's just the Inspector's view. With a real conversational agent, you can describe the line items in natural language. Here's an example line-item input you can use:

    ```json
    [{ "title": "Widget", "price": "29.99", "quantity": 2 }]
    ```

![picture](screen35.png)

8. After a few seconds, the MCP server creates a new sales order in Shopify and returns the result body, including the new sales order ID and a confirmation number.

![picture](screen36.png)

9. Copy the sales order ID and switch to the **Get order by ID** tool. Enter the sales order ID and run the tool to retrieve the order.

![picture](screen37.png)

10. Copy the confirmation number and share it with one of the instructors.

![picture](screen38.png)

They can show you what your sales order looks like in the Shopify instance:

![picture](screen39.png)

Congratulations — you just turned a plain REST API into a governed MCP server and let an AI-ready tool place a real order, all without writing a single line of backend code. Your surge protection is standing guard, your Developer Hub product is open for business, and somewhere in Shopify a very patient widget is waiting to be shipped. Time to grab a coffee before your agents start asking for a raise.
