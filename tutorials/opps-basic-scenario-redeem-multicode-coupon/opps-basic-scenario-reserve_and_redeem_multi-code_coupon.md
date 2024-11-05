---
parser: v2
author_name: Aathira P
author_profile: https://github.com/Aathira-I553036
auto_validation: true
time: 20
tags: [tutorial>advanced, topic>cloud, products>sap-business-technology-platform, products>sap-btp--cloud-foundry-environment]
primary_tag: products>sap-business-technology-platform
---

# Reserve and Redeem a Multi-Code Coupon with SAP Omnichannel Promotion Pricing
<!-- description --> Learn how to reserve and redeem a multi-code coupon in a sales transaction using the Coupon Management service of SAP Omnichannel Promotion Pricing and how to calculate the effective sales price using the Calculation service.

## Prerequisites
- You have registered for a trial account on [SAP BTP](hcp-create-trial-account) in the Singapore - Azure region.
- You should be familiar with the SAP BTP trial landscape and terminology (see [Take a Tour of SAP BTP Trial](cp-trial-quick-onboarding)).
- You have a subaccount in the Cloud Foundry environment, with Microsoft Azure as provider and the Singapore region (AP21).
- You have set up SAP Omnichannel Promotion Pricing as described in [Set Up SAP Omnichannel Promotion Pricing](opps-manual-setup).
- You have completed the tutorial <link to the first tutorial>. With this, you have generated a multi-code coupon and a batch of unique coupon codes for it. You should have noted down a code value from the codes thus generated.


## You will learn
- How to use the **Coupon Management** service of **SAP Omnichannel Promotion Pricing** in your trial environment on SAP BTP. 
- How to **reserve and redeem a coupon code** with the **Coupon Management** service. 
- How to calculate the effective sales price in a sales transaction with the **Calculation** service of **SAP Omnichannel Promotion Pricing** by applying a coupon that triggers a discount on an item in the shopping cart. 


---

### Get to know the business scenario


* Business goal: You are a promotion planner working for a coffee machine vendor. As sales has slowed down, you launched a promotional campaign for your target group **coffee lovers** in the **Heidelberg** region. 
* As part of this promotional campaign, you have created a multi-code coupon with the coupon ID **COFFEELOVER**. The coupon is valid from **October 1, 2026, to December 31, 2026**. For this, you have generated a batch of unique coupon codes that can be redeemed only once by each customer. 
* You have distributed the coupon **COFFEELOVER** with the unique coupon codes to your target customers. 
* You have also created a promotion in the **Promotion Maintenance** app of **SAP Omnichannel Promotion Pricing**: **Buy a coffee machine and show the coupon COFFEELOVER to get EURO 50 off**. 
* The promotion is valid in your **Heidelberg** store (business unit ID: FC01) from **October 1, 2026, to December 31, 2026**.
* On **October 25, 2026**, one of your customers that received the coupon **COFFEELOVER** comes to your **Heidelberg** store to purchase a coffee machine. Your customer selects a coffee machine (material number: `COFFEE_MACHINE`). The **regular sales price** for the **coffee machine** is **Euro 1000**.
* The POS system calls the **Coupon Management** service to **reserve the coupon code**.
* The cashier continues to complete the purchase. The POS system calls the **Calculation** service with the shopping cart information. The Calculation services **calculates the effective sales price for the coffee machine** (regular price minus discount granted by the coupon) and returns it to the POS.
* Your customer completes the purchase. The POS triggers a request to the **Coupon Management** service **to redeem the coupon code**. The redemption information is  returned to the POS, which confirms the completion of the payment to the customer. Your customer is now done and returns home happily with his new coffee machine.


### Create environments in SAP Business Accelerator Hub


In order to continue with the next steps, you need to have already set up your trial environments for the **Coupon Management service, Data Upload service, Data Access service, and Calculation service** in the **SAP Business Accelerator Hub**. If you need guidance, have a look at step 2 of the tutorial [Apply a Simple Promotion with SAP Omnichannel Promotion Pricing](opps-basic-scenario). 


### Reserve a Coupon Code


You can now reserve a coupon code for the multi-code coupon **COFFEELOVER** using the **Coupon Management** service. 

* Scenario: In your **Heidelberg** store (business unit ID: FC01), your customer has selected a coffee machine (material number: `COFFEE_MACHINE`, regular sales price: Euro 1000). At the time of check-out, the customer presents the coupon code for the multi-code coupon **COFFEELOVER** to the cashier. The client system calls the **Coupon Management** service to reserve the coupon code.

* Desired Result: The coupon code is reserved. 


Let's see if this is the case:

Navigate to the [Coupon Management service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/CouponManagementService/overview). Log on with the same login information that you use for your SAP BTP account.

<!-- border -->![Create coupon(create_coupon.png)]

1. Navigate to the **Try Out** section.
2. For **API Environment**, select the trial environment you have created for the **Coupon Management** service.
3. On the left-hand side, choose **`Service Operation`**.
4. Open the **POST** request **`/reserveCouponCode`**.
5. For the request **body**, copy the following data and paste it into the body:
```json
{ 
  "couponCodeValue": "string",
   "reservationDate": "2026-10-25",
   "transactionID": "string"

}
```
6. Choose **Run**.
8. Make sure you get the HTTP response **200**.

The response should look like this:

<!-- border -->![Coupon creation success](create_coupon_success.png)

9. From the response, note down the **redemption information ID** **`redemptionInfoID`** and **reservation token** **`reservationToken`**.

With this, you have reserved the coupon code for 30 minutes. It is also possible to extend the reservation by 30 minutes.


### Calculate the Effective Sales Price


You can now calculate the effective sales price using the **Calculation** service. 

* Scenario: In your **Heidelberg** store (business unit ID: FC01), your customer has selected a coffee machine of **regular price EURO 1000**. At the time of check-out on **October 25, 2026**, the customer presents the coupon code for the multi-code coupon **COFFEELOVER** to the cashier. 


* Desired Result: The effective sales price for the coffee machine should be EURO 950, as the customer is eligible for the promotion **Buy a coffee machine and show the coupon COFFEELOVER to get EURO 50 off**.


Let's see if this is the case:

Navigate to the [Calculation service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/PriceCalculation/overview). Log on with the same login information that you use for your SAP BTP account.

<!-- border -->![Create coupon(create_coupon.png)]

1. For **API Environment**, select the trial environment you have created for the **Calculation** service.
2. Choose **Try Out**.
3. Open the **POST** request **`/restapi/{tenantname}e`**.
4. Under **tenantName**, enter the **`identityzone`** from the service key you have created for the **Calculation** service in your SAP BTP account. If you need assistance, see step 4 of the tutorial [Set Up SAP Omnichannel Promotion Pricing](https://developers.sap.com/tutorials/opps-manual-setup.html) .
5. For the **Content Type**, choose **`XML`**.  
4. Copy the following data and paste it into the **body**:
```XML
{ 
  <PriceCalculate
            xmlns="http://www.sap.com/IXRetail/namespace/" InternalMajorVersion="15" InternalMinorVersion="0">
            <ARTSHeader ActionCode="Calculate" MessageType="Request">
                        <MessageID>9a89f2edfd1e413ea147e334b9c2ed4b</MessageID>
                        <DateTime>2026-10-25T00:00:00.427-05:00</DateTime>
                        <BusinessUnit TypeCode="RetailStore">FC01</BusinessUnit>
                        <MasterDataSourceSystemID>RS9CLNT990</MasterDataSourceSystemID>
            </ARTSHeader>
            <PriceCalculateBody TransactionType="SaleTransaction" NetPriceFlag="true" RecommendationMode="CalcAndRec">
                        <TransactionID>9a89f2edfd1e413ea147e334b9c2ed4b</TransactionID>
                        <DateTime>2026-10-25T16:48:30.427-05:00</DateTime>
                        <RegularSalesUnitPriceRoundingRule Multiple="0.01000" RoundingMethod="Commercial"/>
                        <ShoppingBasket>
                                    <LineItem>
                                                <SequenceNumber>0</SequenceNumber>
                                                <MerchandiseHierarchy ID="R3">COFFEEMACHINES</MerchandiseHierarchy>
                                                <Sale ItemType="Stock" NonDiscountableFlag="false" FixedPriceFlag="true" FrequentShopperPointsEligibilityFlag="true">
                                                            <TaxIncludedInPriceFlag>false</TaxIncludedInPriceFlag>
                                                            <NonPieceGoodFlag>false</NonPieceGoodFlag>
                                                            <NotConsideredByPriceEngineFlag>false</NotConsideredByPriceEngineFlag>
                                                            <ItemID>COFFEEMACHINE</ItemID>
                                                            <Quantity Units="1" UnitOfMeasureCode="EA">1</Quantity>
                                                            <RegularSalesUnitPrice Currency="EUR">1000.0</RegularSalesUnitPrice>
                                                </Sale>
                                    </LineItem>
                                     <LineItem>
                        <SequenceNumber>1</SequenceNumber>
                        <Coupon>
                                    <PrimaryLabel>COFFEELOVER</PrimaryLabel>
                                    <Quantity>1</Quantity>
                        </Coupon>
      </LineItem> 
                        </ShoppingBasket>
            </PriceCalculateBody>
</PriceCalculate>
}
```
7. Choose **Run**.
8. Make sure you get the HTTP response **200**.

The response should look like this:

<!-- border -->![Coupon creation success](create_coupon_success.png)

With this, the **Calculation** service has calculated the effective sales price for the coffee machine as EURO 950 by applying the promotion **Buy a coffee machine and show the coupon COFFEELOVER to get EURO 50 off**. It then returns the effective sales price to the POS.

### Redeem the Coupon Code


You can now redeem the coupon code for the multi-code coupon **COFFEELOVER** using the Coupon Management service. 

* Scenario: The customer has completed the purchase of the coffee machine for which he used the coupon **COFFEELOVER** with the respective coupon code. The POS system calls the **Coupon Management** service to redeem the coupon code. The redemption information is returned to the system which then confirms the completion of the purchase.

* Desired Result: The coupon code is redeemed. 


Let's see if this is the case:

Navigate to the [Coupon Management service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/CouponManagementService/overview). Log on with the same login information that you use for your SAP BTP account.

<!-- border -->![Create coupon(create_coupon.png)]

1. For **API Environment**, select the trial environment you have created for the **Coupon Management** service.
2. Choose **Try Out**.
3. On the left-hand side, choose **Redemption Information**.
4. Open the **POST** request **`/RedemptionInfo({ID})/CouponManagementService.redeemCouponCode`**.
5. Under **Parameters**, enter the **`redemptionInfoID`** which you noted down when you reserved the coupon code.
5. For the **body**, enter the **`reservation token`** which you noted down at the time of reserving the coupon code.
7. Choose **Run**.
8. Make sure you get the HTTP response **200**.

The response should look like this:

<!-- border -->![Coupon creation success](create_coupon_success.png)

With this, you have successfully redeemed the coupon code. The redemption information is returned to the POS which then confirms the completion of payment. Your customer has now successfully purchased the coffee machine at a discounted price.

### Additional Information

* [Official Product Documentation](https://help.sap.com/docs/OPP)
* [Coupon Management](https://help.sap.com/docs/omnichannel-promotion-pricing/service-guide/coupon-management)
* [Trial Documentation](https://help.sap.com/docs/OPP/0c145d124b784b548b618cda8a5b2aba/31b8aedc8ce14fcd9f6021ad4f6323c9.html)
* [SAP Omnichannel Promotion Pricing on the SAP Business Accelerator Hub](https://help.sap.com/docs/OPP/7c87270e23c64c2aa922ce297a6df23d/67a9da084cf84e058cb3a3911971bdd0.html?version=Cloud)
* [Video: SAP Omnichannel Promotion Pricing in a nutshell](https://www.sap.com/assetdetail/2020/07/9060b3a5-a67d-0010-87a3-c30de2ffd8ff.html)

---
