---
parser: v2
author_name: Aathira P
author_profile: https://github.com/Aathira-I553036
auto_validation: true
time: 20
tags: [tutorial>advanced, topic>cloud, products>sap-business-technology-platform, products>sap-btp--cloud-foundry-environment]
primary_tag: products>sap-business-technology-platform
---

# Reserve and Redeem a Multi-Code Coupons with SAP Omnichannel Promotion Pricing
<!-- description --> Learn how to reserve and redeem a multi-code coupon in a sales transaction using the Coupon Management service of SAP Omnichannel Promotion Pricing. Also, calculate the effective sales price using the Calculation service for multi-code coupons in your promotions.

## Prerequisites
- You have registered for a trial account on [SAP BTP](hcp-create-trial-account) in the Singapore - Azure region.
- You should be familiar with the SAP BTP trial landscape and terminology (see [Take a Tour of SAP BTP Trial](cp-trial-quick-onboarding)).
- You have a subaccount in the Cloud Foundry environment, with Microsoft Azure as provider and the Singapore region (AP21).
- You have set up SAP Omnichannel Promotion Pricing as described in [Set Up SAP Omnichannel Promotion Pricing](opps-manual-setup).
- You have completed the tutorial <link to the first tutorial>. With this, you have generated a multi-code coupon and a batch of unique coupon codes for it. You should have noted down a code value from the codes thus generated.


## You will learn
- How to use the **Coupon Management** service of **SAP Omnichannel Promotion Pricing** in your trial environment on SAP BTP. 
- How to **reserve, extend reservation and redeem a coupon code** with the **Coupon Management** service. 
- How to calculate the effective sales price in a sales transaction with the **Calculation** service of **SAP Omnichannel Promotion Pricing** by applying a coupon that triggers a discount on an item in the shopping cart. 


---

### Get to know the business scenario


* Business goal: You are a promotion planner working for a coffee machine vendor. As sales has slowed down, you are planning a promotional campaign to increase revenue and reduce stock. You want to create a promotional campaign for your target group **coffee lovers** in the **Heidelberg** store. 
* As part of this promotional campaign, you have created a multi-code coupon with the coupon ID **COFFEELOVER**. The coupon is valid from **October 1st, 2025, to December 31st, 2025**. For this, you have also generated a batch of unique coupon codes which can be redeemed once by each customer. 
* You have distributed the coupon **COFFEELOVER** with the unique coupon codes to your target customers. 
* You have also created a promotion in the **Promotion Maintenance** app of **SAP Omnichannel Promotion Pricing**: **Buy a coffee machine and show the coupon COFFEELOVER to get EURO 50 off**. 
* The promotion is valid in your **Heidelberg** store (business unit ID: FC01) from **October 1st, 2025, to December 31st, 2025**.
* The **regular sales price** for the **coffee machine** (material number: `COFFEE_MACHINE`) is **Euro 1000**.


### Create environments in SAP Business Accelerator Hub


In order to continue with the next steps, you need to have already set up your trial environments for the **Coupon Management service, Data Upload service, Data Access service, and Calculation service** in the **SAP Business Accelerator Hub**. If you need guidance, have a look at step 2 of the tutorial [Apply a Simple Promotion with SAP Omnichannel Promotion Pricing](opps-basic-scenario). 


### Reserve a Coupon Code


You can now reserve a coupon code for the multi-code coupon **COFFEELOVER** using the Coupon Management service. 

* Business scenario: In your **Heidelberg** store (business unit ID: FC01), your customer has selected a coffee machine of regular price EURO 1000. At the time of check-out, the customer presents the coupon code for the multi-code coupon **COFFEELOVER** to the cashier. The client system calls the **Coupon Management** service to reserve the coupon code.

* Desired result: The coupon code is reserved. 

Navigate to the [Coupon Management service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/CouponManagementService/overview). Log on with the same login information that you use for your SAP BTP account.

1. For **API Environment**, select the trial environment you have created for the **Coupon Management** service.
2. On the left-hand side, choose **Coupon**.
3. Open the **POST** request `/Coupon`.
4. Choose **Try Out**.
5. For the **Request body**, choose `application/json`.
6. Copy the following raw data and paste it into the body:
```json
{ 
  "ID": "01234567-89ab-cdef-0123-456789abcdef",
  "status": "INACTIVE",
  "displayID": "COFFEELOVER",
  "type": "MULTICODE",
  "name": "COFFEELOVER",
  "description": "Discount 50 Euro for coffee machine",
  "validFrom": "2024-10-01",
  "validTo": "2024-12-31",
  "couponCodeValidityPeriodInDays": 90,
  "maxCodeGenerationCount": 100
}
```
7. Choose **Execute**.
8. Make sure you get the HTTP response **201**.

<!-- border -->![Create coupon(create_coupon.png)]

The response should look like this:

<!-- border -->![Coupon creation success](create_coupon_success.png)


9. From the response, note down the technical coupon ID as you will use it later.

With this, you have created a multi-code coupon with the ID **COFFEELOVER** and validity period **October 1, 2024 to December 31, 2024**.


**Optional**: If you want to check whether you have successfully created the multi-code coupon **COFFEELOVER**, you can proceed with the following steps:


1. On the left-hand side, choose **Coupon**.
2. Open the **GET** request `/Coupon`.
3. Choose **Run**.
4. Make sure you get the HTTP response **200**. 
5. In the response body, you should see the coupon that you have created.  

<!-- border -->![Check created coupon](GET_coupon.png)


### Change status of coupon object


When creating the coupon, the **status** of the coupon was set as **inactive** because it cannot be created in the **active** status. To generate the coupon codes, you must first change the **status** of the coupon object from **INACTIVE** to **ACTIVE**.

1. On the left-hand side, choose **Coupon**.
2. Open the **PATCH** request `/Coupon({ID})`.
3. Choose **Parameters**.
4. For ID, enter the **technical coupon ID** from the coupon object you just created.
5. For the **Request body**, choose `application/json`.
6. Copy the following raw data and paste it into the body.
```json
{ 
  "status": "ACTIVE"
}
```
7. Choose **Run**. 
8. Make sure you get the HTTP response **200**.

<!-- border -->![Change coupon status](change_coupon_status.png)
<!-- border -->![Coupon status change success](change_coupon_status_success.png)

You have now successfully changed the status of the coupon code. 


### Generate coupon codes


Now that you have created the multi-code coupon object and set it to active, you can generate a batch of 100 coupon codes to be used in the promotional campaign.

1. On the left-hand side, choose **Coupon**.
2. Open the **POST** request `/Coupon({ID})/CouponManagementService.generateCouponCodes`.
3. Choose **Parameters**.
4. For ID, enter the **technical coupon ID** from the coupon object you just created.
5. Copy the following raw data and paste it into the body:
```json
{ 
"generatedCodeCount": 100, 
"validFrom": "2024-10-01" 
}
```
6. Choose **Run**.

<!-- border -->![Generate coupon codes](generate_coupon_code.png)

7. Make sure you get the HTTP response **200**.

<!-- border -->![Generate coupon codes success](generate_coupon_code_success.png)

With this, you have now created a batch of **100** unique codes for the coupon **COFFEELOVER**. You must note down the batch ID to read the coupon codes.


### Read coupon codes


You can now read the coupon codes created using the Coupon Management service to circulate the coupon information to the target group as part of your promotional campaign. 

1. On the left-hand side, choose **Code Batch**.
2. Open the **GET** request `/CodeBatch({ID})/codes`.
3. Choose **Parameters**.
4. Enter the **Batch ID** noted down at the time of generation of coupon codes. 
5. Choose **Run**.
6. Make sure you get the HTTP response **200**.

<!-- border -->![Read coupon codes](read_coupon_code.png)

With this, you can read the coupon codes generated for the multi-code coupon **COFFEELOVER**. This is the coupon code value to be communicated to the target group members as part of your promotional campaign. 

You must note down a coupon code value as it will be used for redeeming the coupon.


### Create a promotion for multi-code coupon

To finalize your promotional campaign, you want to set up the following promotion for your **Heidelberg** store (business unit ID **FC01**):
* Buy a coffee machine and show the coupon **COFFEELOVER to get 50 Euro off**.  
* The promotion is valid from October 1st, 2024 to December 31st, 2024.
You can do so either using the **Maintain Promotions** app or the **Data Upload** service.
In this tutorial, we will use the **Data Upload** service.


Navigate to the [Data Upload service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/DataUpload/resource). Log on with the same login information that you use for your SAP BTP account.

<!-- border -->![Data upload 1](data_upload_1.png)
<!-- border -->![Data upload 2](data_upload_2.png)


1. For **API Environment**, select the trial environment you have created for the Data Upload service.
2. On the left-hand side, choose **Upload promotions using OAuth2**.
3. Open the **POST** request `/idocinbound`.
4. Choose **Try Out**.
5. For the **Request body**, choose `application/xml`.
6. Copy the following raw data and paste it into the body:
```XML
<?xml version="1.0" encoding="utf-8"?>
<_-ROP_-PROMOTION02>
  <IDOC BEGIN="1">
    <EDI_DC40 SEGMENT="1"> 
      <IDOCTYP>/ROP/PROMOTION02</IDOCTYP> 
    </EDI_DC40>
    <_-ROP_-E1_PROMOTION> 
      <PROMOTION_ID>2401</PROMOTION_ID> 
      <OFFER_ID>27DB2B4C00F61EEF9CB76E606FF07D88</OFFER_ID>
<EXTERN_PROMO_ID>000000000000000000000000000000000821</EXTERN_PROMO_ID> 
      <EFFECTIVE_DATE>20241001000000</EFFECTIVE_DATE> 
      <EXPIRY_DATE>20241231235959</EXPIRY_DATE> 
      <CHANGED_ON>20240913103445</CHANGED_ON> 
      <STATUS_TCD>AC</STATUS_TCD>
      <CHANGE_INDICATOR>I</CHANGE_INDICATOR>
      <LOGSYS>RS9CLNT990</LOGSYS>
      <ORIGIN>01</ORIGIN> 
      <MIN_PPS_RELEASE>0</MIN_PPS_RELEASE>
      <OFFER_ID>42010AEF0FAA1EEBBABE7EEA0C2D0823</OFFER_ID>
      <ORIGIN>01</ORIGIN>
      <PROMOTION_ID>771</PROMOTION_ID>
      <PROMOTION_TYPE>Z001</PROMOTION_TYPE>
      <STATUS_TCD>AC</STATUS_TCD>
      <_-ROP_-E1_PROMOTION_BU SEGMENT="1">
        <BU_ID>FC01</BU_ID>
        <BU_TYPE>1040</BU_TYPE>
      </_-ROP_-E1_PROMOTION_BU>
      <_-ROP_-E1_PROMOTION_RULE SEGMENT="1">
        <ELIGIBILITY_ID>772</ELIGIBILITY_ID>
        <ONLY_BASKET_MODE>X</ONLY_BASKET_MODE>
        <PRICE_RULE_ID>772</PRICE_RULE_ID>
        <PROMO_RULE_ID>772</PROMO_RULE_ID>
        <RESOLUTION>1</RESOLUTION>
        <SALE_RETURN_TCD>00</SALE_RETURN_TCD>
        <SEQUENCE>772</SEQUENCE>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <COMBINATION_CODE>&amp;&amp;</COMBINATION_CODE>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>772</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>772</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>772</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>772</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COMB</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <CONSUMPTION_TCD>00</CONSUMPTION_TCD>
          <COUPON_NUMBER>TIME FOR COFFEE</COUPON_NUMBER>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>773</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>772</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>772</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>772</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COUP</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>774</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>1.000</INTERVAL_QTY>
          <ITEM_ID>COFFEE_MACHINE</ITEM_ID>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>9999999999.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>772</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>772</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>772</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>1.000</THRESHOLD_QTY>
          <THRESHOLD_TCD>QUTI</THRESHOLD_TCD>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>ITEM</TYPE_CODE>
          <UOM_ISO_CODE>PCE</UOM_ISO_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_PRICE_RULE SEGMENT="1">
          <CALCULATION_BASE>00</CALCULATION_BASE>
          <CALC_BASE_SEQUENCE>1-</CALC_BASE_SEQUENCE>
          <CONSIDER_PREVIOUS_RULES>X</CONSIDER_PREVIOUS_RULES>
          <COUPON_VALIDITY>0</COUPON_VALIDITY>
          <CURRENCY_ISO_CODE>EUR</CURRENCY_ISO_CODE>
          <DISC_METHOD_TCD>00</DISC_METHOD_TCD>
          <EX_PRICE_RULE_ID>0000000000000000000</EX_PRICE_RULE_ID>
          <ITEM_METHOD>00</ITEM_METHOD>
          <MAM_LIMIT_CNT>0.000</MAM_LIMIT_CNT>
          <NEW_PRICE_AMT>0.00000</NEW_PRICE_AMT>
          <NEW_SET_PRCE_AMT>0.00000</NEW_SET_PRCE_AMT>
          <PRICE_MODIF_AMT>5.00000</PRICE_MODIF_AMT>
          <PRICE_MODIF_CODE>RT</PRICE_MODIF_CODE>
          <PRICE_MODIF_PERC>0.000</PRICE_MODIF_PERC>
          <PRICE_RULE_ID>772</PRICE_RULE_ID>
          <ROUNDING_METHOD>00</ROUNDING_METHOD>
          <ROUND_DECIMALS>2</ROUND_DECIMALS>
          <ROUND_DESTINATION>1</ROUND_DESTINATION>
          <RULE_CTRL_CODE>PO</RULE_CTRL_CODE>
          <STATUS_TCD>AC</STATUS_TCD>
          <SUBSEQ_NEW_PRICE_AMT>0.00000</SUBSEQ_NEW_PRICE_AMT>
          <SUBSEQ_PRICE_MODIF_AMT>0.00000</SUBSEQ_PRICE_MODIF_AMT>
          <SUBSEQ_PRICE_MODIF_PERC>0.000</SUBSEQ_PRICE_MODIF_PERC>
          <TYPE_CODE>RB</TYPE_CODE>
        </_-ROP_-E1_PRICE_RULE>
      </_-ROP_-E1_PROMOTION_RULE>
      <_-ROP_-E1_PROMOTION_RULE SEGMENT="1">
        <ELIGIBILITY_ID>775</ELIGIBILITY_ID>
        <ONLY_BASKET_MODE>X</ONLY_BASKET_MODE>
        <PRICE_RULE_ID>775</PRICE_RULE_ID>
        <PROMO_RULE_ID>775</PROMO_RULE_ID>
        <RESOLUTION>0</RESOLUTION>
        <SALE_RETURN_TCD>00</SALE_RETURN_TCD>
        <SEQUENCE>775</SEQUENCE>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <COMBINATION_CODE>&amp;&amp;</COMBINATION_CODE>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>775</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>775</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>775</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>775</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COMB</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <CONSUMPTION_TCD>00</CONSUMPTION_TCD>
          <COUPON_NUMBER>TIME FOR COFFEE</COUPON_NUMBER>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>776</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>775</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>775</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>775</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COUP</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>777</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>1.000</INTERVAL_QTY>
          <ITEM_ID>COFFEE_MACHINE</ITEM_ID>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>9999999999.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>775</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>775</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>775</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>1.000</THRESHOLD_QTY>
          <THRESHOLD_TCD>QUTI</THRESHOLD_TCD>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>ITEM</TYPE_CODE>
          <UOM_ISO_CODE>PCE</UOM_ISO_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_PRICE_RULE SEGMENT="1">
          <CALCULATION_BASE>00</CALCULATION_BASE>
          <CALC_BASE_SEQUENCE>1-</CALC_BASE_SEQUENCE>
          <CONSIDER_PREVIOUS_RULES>X</CONSIDER_PREVIOUS_RULES>
          <COUPON_VALIDITY>0</COUPON_VALIDITY>
          <CURRENCY_ISO_CODE>EUR</CURRENCY_ISO_CODE>
          <DISC_METHOD_TCD>00</DISC_METHOD_TCD>
          <EX_PRICE_RULE_ID>0000000000000000000</EX_PRICE_RULE_ID>
          <ITEM_METHOD>00</ITEM_METHOD>
          <MAM_LIMIT_CNT>0.000</MAM_LIMIT_CNT>
          <NEW_PRICE_AMT>0.00000</NEW_PRICE_AMT>
          <NEW_SET_PRCE_AMT>0.00000</NEW_SET_PRCE_AMT>
          <PRICE_MODIF_AMT>100.00000</PRICE_MODIF_AMT>
          <PRICE_MODIF_CODE>RT</PRICE_MODIF_CODE>
          <PRICE_MODIF_PERC>0.000</PRICE_MODIF_PERC>
          <PRICE_RULE_ID>775</PRICE_RULE_ID>
          <REWARD_AS_POINTS>X</REWARD_AS_POINTS>
          <ROUNDING_METHOD>00</ROUNDING_METHOD>
          <ROUND_DECIMALS>0</ROUND_DECIMALS>
          <ROUND_DESTINATION>1</ROUND_DESTINATION>
          <RULE_CTRL_CODE>PO</RULE_CTRL_CODE>
          <STATUS_TCD>AC</STATUS_TCD>
          <SUBSEQ_NEW_PRICE_AMT>0.00000</SUBSEQ_NEW_PRICE_AMT>
          <SUBSEQ_PRICE_MODIF_AMT>0.00000</SUBSEQ_PRICE_MODIF_AMT>
          <SUBSEQ_PRICE_MODIF_PERC>0.000</SUBSEQ_PRICE_MODIF_PERC>
          <TYPE_CODE>RB</TYPE_CODE>
        </_-ROP_-E1_PRICE_RULE>
      </_-ROP_-E1_PROMOTION_RULE>
      <_-ROP_-E1_PROMOTION_TEXT SEGMENT="1">
        <DESCRIPTION>Buy a coffee machine and show coupon TIME FOR COFFEE to get 100 Euro off</DESCRIPTION>
        <NAME>Coffee machine promotion</NAME>
        <SPRAS_ISO>EN</SPRAS_ISO>
      </_-ROP_-E1_PROMOTION_TEXT>
    </_-ROP_-E1_PROMOTION>
  </IDOC>
</_-ROP_-PROMOTION02>
```
7. Choose **Run**.
8. Make sure you get the HTTP response **200**.

<!-- border -->![Upload a promotion](upload_promotion.png)

With this, you uploaded the promotional rule "Buy a coffee machine and show coupon COFFEELOVER to get Euro 50 off" for the **Heidelberg** store (business unit ID: `FC01`) and the validity period **October 1st, 2024 to December 31st, 2024**. 


### Check uploaded master data


You can use the **Data Access** service for checking your uploaded data. If you need guidance, have a look at step 4 of the tutorial [Apply a Simple Promotion with SAP Omnichannel Promotion Pricing](opps-basic-scenario).

With this, you are all set to launch your promotional campaign. 


### What to Do Next

 Navigate to the intermediate group [Set up Complex Promotions with SAP Omnichannel Promotion Pricing](group.opps-complex-promotion) and continue with the next tutorial [Reserve and Redeem a Multi-Code Coupon with SAP Omnichannel Promotion Pricing].


### Additional Information

* [Official Product Documentation](https://help.sap.com/docs/OPP)
* [Coupon Management](https://help.sap.com/docs/omnichannel-promotion-pricing/service-guide/coupon-management)
* [Trial Documentation](https://help.sap.com/docs/OPP/0c145d124b784b548b618cda8a5b2aba/31b8aedc8ce14fcd9f6021ad4f6323c9.html)
* [SAP Omnichannel Promotion Pricing on the SAP Business Accelerator Hub](https://help.sap.com/docs/OPP/7c87270e23c64c2aa922ce297a6df23d/67a9da084cf84e058cb3a3911971bdd0.html?version=Cloud)
* [Video: SAP Omnichannel Promotion Pricing in a nutshell](https://www.sap.com/assetdetail/2020/07/9060b3a5-a67d-0010-87a3-c30de2ffd8ff.html)

---
