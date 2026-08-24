---
parser: v2
time: 5
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, products>sap-hana\,-express-edition]
primary_tag: products>sap-hana\,-express-edition
---

# SAP HANA, express edition - Choose a Server-Only Deployment Method

<!-- description --> A decision guide to help you pick the right way to deploy the database-only (server-only) version of SAP HANA, express edition on your local computer, then point you to the hands-on installation tutorial that matches your setup.

## Prerequisites

- **Registration:** You must register for the product before downloading SAP HANA, express edition. Follow the steps in [Register for SAP HANA, express edition](hxe-ua-register).
- **Memory:** 8 GB RAM minimum; 12 GB recommended for the server-only version.
- **Disk space:** Sufficient free disk space for the download and installation — see your chosen installation guide for the exact figure.
- **Operating system:** A supported 64-bit Linux distribution. The server-only version does not install natively on Windows or macOS — on those hosts use Docker, a Linux virtual machine, or SAP HANA Cloud trial.
- **Docker (Docker path only):** A working Docker installation on a supported Linux distribution.

## You will learn

- How to compare the local deployment methods for the server-only (database) version of SAP HANA, express edition
- How to choose the method that fits your operating system and experience level
- Where to go next once your instance is running

## Intro

This is a short **decision guide**, not a hands-on installation walkthrough. Its goal is to help you pick the deployment method that matches your machine and skill level, then send you to the dedicated tutorial that walks through that method step by step.

This tutorial covers the **server-only** package — the SAP HANA database engine on its own, without XS Advanced or other application services. If you need those application services, follow one of the full-installation guides linked below instead.

---

### Confirm your machine has enough memory

Before choosing a method, confirm your laptop or server meets the minimum requirements for a local deployment:

- **8 GB RAM minimum**, **12 GB recommended** for the server-only version.
- Sufficient free disk space for the download and install.

> ### Not enough RAM or disk space?
>
> If your machine does not meet these requirements, use **SAP HANA Cloud trial** instead — a completely free, hosted, cloud-managed solution with nothing to install locally. See [Deploy SAP HANA Cloud trial](hana-cloud-deploying).

### Choose your deployment method

Pick the row that matches your situation and follow the linked tutorial. You only need **one** of these methods.

| If you… | Choose | Then follow |
| --- | --- | --- |
| Want the fastest setup on a supported Linux distribution and are comfortable with Docker | **Docker container** | [Installing SAP HANA, express edition with Docker](hxe-ua-install-using-docker) |
| Want to customize the operating system or platform and are comfortable with Linux administration | **Binary installer** | [Install SAP HANA, express edition on a Native Linux Machine](group.hxe-install-binary) |
| Are on Windows or macOS, or have limited local resources | **SAP HANA Cloud trial** (no local install) | [Deploy SAP HANA Cloud trial](hana-cloud-deploying) |

> The **Docker** and **binary installer** options run on Linux operating systems only. On Windows or macOS, run them inside a Linux virtual machine, or use SAP HANA Cloud trial instead.

### What's next

Once your server-only instance is running, continue with:

- [Get Started with the SAP HANA Database Explorer](hana-dbx-overview) — connect to your new instance, set your passwords, and run your first SQL.
- [Register for SAP HANA, express edition](hxe-ua-register) — if you have not registered yet.
- Explore more SAP HANA learning content on [SAP Learning](https://learning.sap.com) to deepen your database administration and development skills.

---
