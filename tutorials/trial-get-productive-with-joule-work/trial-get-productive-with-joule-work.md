---
parser: v2
author_name: DJ Adams
author_profile: https://github.com/qmacro
auto_validation: false
primary_tag: software-product>joule
tags: [ software-product>sap-business-technology-platform, topic>cloud, tutorial>beginner ]
time: 15
---

# Get productive with Joule Work

<!-- description --> Using a business scenario, learn about the SAP Business AI Platform (BAIP) and Joule Work.

## You will learn

- What the BAIP platform is, and what it offers
- How Joule Work is essential to building with BAIP

## Prerequisites

To complete this tutorial, you will need:

- an SAP BTP global account
- an SAP BTP subaccount set up with an SAP Cloud Identity Services (CIS) tenant
- Access to Joule Work Desktop
- an activated BAIP trial

This tutorial is designed for first-time users, so no prior experience with
BAIP or Joule is required.

## Intro

The SAP Business AI Platform is a unified enterprise AI foundation. It combines
data, process context, and governance into a single stack. It brings together
the SAP Business Technology Platform (BTP) and Business Data Cloud to let
organizations build, run, and scale AI agents and apps.

Joule Work is a central workspace where AI agents handle busywork - so you can
focus on driving outcomes.

In this tutorial you'll understand how these two technologies work together.

---

### Understand the example scenario

Before you start working with Joule, it helps to understand the business
context you'll be using throughout this tutorial.

The scenario follows a Demand and Supply Planner at a discrete manufacturing
company who needs to quickly review production actuals against the IBP forecast
for the upcoming quarter, identify potential supply gaps, and decide where to
act — all without switching between SAP S/4HANA, SAP IBP, and a series of
manual reports.

This is exactly the kind of busywork BAIP and Joule Work are designed to
eliminate. Rather than navigating reports and dashboards yourself, you'll ask
Joule directly in natural language and let it do the heavy lifting.

Throughout the steps that follow, keep this scenario in mind. Each interaction
you have with Joule maps to a real task this person would otherwise spend
significant time on.

> The data used in this tutorial is sample data provided as part of the BAIP
> trial environment. It reflects a realistic but fictional business.

### Have Joule present actuals and forecast data

Now that you understand the scenario, put Joule to work.

👉 Open Joule Work Desktop and start a new conversation.

👉 Ask Joule to surface the actuals and forecast data for the upcoming
production cycle. You can phrase this naturally: "Show me production actuals vs
IBP forecast for Q3".

Joule will immediately retrieve the data (actuals from SAP S/4HANA and the
forecast from SAP IBP) and present it in a structured, readable format directly
in the conversation. The forecast is predicted by leveraging SAP RPT 1.5, SAP's
tabular AI model, which uses your historical production data to produce highly
accurate demand predictions.

👉 Take a moment to explore the result. Notice that Joule organizes the data
into a visual card with the key metrics highlighted.

This step demonstrates a core capability of BAIP: Joule can access live
business data through its connected data foundation and present it in context,
without you navigating to a new system or opening a report.

### Create and learn about spaces

A single conversation is useful, but Joule Work becomes even more powerful when
you use Spaces — dynamically generated dashboards that automatically assemble
relevant insights and data around a specific business goal or task in real
time.

👉 In the conversation from the previous step, locate the actuals vs forecast
card Joule generated.

👉 Save it to a new Space. Give the Space a meaningful name, such as "Q3
Production Review".

👉 Open the Space. You'll see the card pinned there, ready to reference at any
time.

👉 Back in the conversation, ask Joule a follow-up question - for example,
which region or product line is showing the largest variance — and add that
card to the same Space.

Spaces are the connective tissue between conversations and ongoing work. They
let you build up a curated view of a topic over time, across multiple
interactions with Joule and with agents.

> Spaces persist between sessions. When you return to Joule Work tomorrow, your
> Space and its cards will be exactly as you left them.

### Call an agent in the space context

Joule Work goes beyond the traditional conversational interface. It serves as
an orchestration layer for AI agents built on BAIP. In this step, you'll invoke
a specialized agent from within your Space.

👉 Open the “Q3 Production Review” Space you created in the previous step.

👉 With the Space active, start a new conversation and ask Joule to call the
planning agent to analyze the forecast variance: "@planning-agent Analyze the
Q3 variance data and suggest where we should rebalance production."

Joule routes the request to the manufacturing agent, which processes the data
and returns a prioritized set of production adjustment recommendations with
projected impact.

👉 Add the agent's output to your Space as a new card.

This step illustrates how BAIP connects the conversation layer (Joule Work) to
the agentic execution layer underneath. The agent runs in the background; you
stay focused on the outcome.

### Extend the conversation with Joule

Joule allows you to continue, refine, and build on your conversations while
maintaining context throughout.

👉 Return to the conversation from the earlier steps.

👉 Ask a follow-up question that builds on what's already been discussed: "What
are the current live production rates on Line 4?"

This time, Joule surfaces a data gap: the live production rate data isn't
available in the connected sources. Rather than estimating or fabricating an
answer, Joule tells you exactly what's missing and why it can't respond fully.

This shows that Joule is grounded in real, connected data. When something is
missing, it's transparent about it and signals when it needs additional
context.

Joule Work functions as a persistent, context-aware workspace rather than a
stateless chatbot. Every interaction builds on the last, and the Space anchors
everything to the business problem you're solving.

### Wrap-up and further info

Well done, you now have an overview of BAIP and Joule Work. For further info, see:

- Learn more about [Joule Agents and Joule
  Assistants](https://www.sap.com/products/artificial-intelligence/ai-agents.html)
- An overview of [Joule
  Work](https://www.sap.com/products/artificial-intelligence/joule-work.html)
- Information on
  [SAP-RPT](https://www.sap.com/products/artificial-intelligence/sap-rpt.html)
