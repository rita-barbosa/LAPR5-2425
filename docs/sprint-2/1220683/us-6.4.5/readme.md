# US 6.4.5

<!-- TOC -->
* [US 6.4.5](#us-645)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
<!-- TOC -->


## 1. Context

* This is the first time this user story is worked on.


* MBCO (Minimum Business Continuity Objective) specifies the minimum operability level that must be maintained during a disruption in the infrastructure.
 It guides what should be recovered and how extensive the recovery should be.

## 2. Requirements

**US 6.4.5:** As system administrator, I want to define the MBCO (Minimum Business Continuity Objective) to propose to stakeholders.

**Acceptance Criteria:**

No acceptance criteria yet.

**Dependencies/References:**

This user story is related to all user stories/functionalities of the Business Continuity Module, documenting the impacts
of disruptions in their operations and the recovery strategies taken into account.

**Client Clarifications:**

No client clarifications yet.

## 3. Analysis

An MBCO refers to the minimum level of operation that must be maintained during a disruption within the infrastructure.

The first step to define the MBCO is to identify which services/modules/functions are essential and must continue working
regardless of a disruption.

The proposal must be clear regarding the impact such disruptions bring to the system and associated risks, following the
definition of what each minimum operating level means for each affected component, such as conditioning of user access or
data availability. Ranking such elements based in need for continuity is advised.

Two more criteria must be explicitly defined for each critical component:
* **MTPD** (Maximum Tolerable Period of Disruption) - the maximum amount of performance time below the infrastructure requirements
* **MTD** (Maximum Tolerable Downtime) - the maximum amount of inoperability in the infrastructure

To recapitulate, the proposal must have the following structure:

> ### MBCO Proposal Structure
>
> - Introduction to the system context
>   - Identification of critical services and functions
> - Definition of minimum levels of operation
>   * service/function and minimum operation goal, MTD/MTPD metrics

## 4. Design

In this case, the RFP module must remain active, including any systems that support internal network access, VPN access,
and any user-specific features critical for continuity.

The following features/services are the ones that must remain active in any scenario of disruption.


>* **Login**
>  * **Why is it critical?** Essential to ensure that only authorized users can access the system, maintain security protocols, and support continuity.
>  * **Minimum Operation Goal:** Ensure login functionality remains accessible 24/7 with redundancy to prevent unauthorized access attempts during any downtime.
>  * **MTD:** 1 minute
>  * **MTPD:** 30 minutes (allowing rapid restoration of login functionality to avoid prolonged outages)


>* Operation Requests addition (for emergencies)
>  * **Why is it critical?** Necessary to allow emergency operation requests, especially during unpredictable disruptions.
>  * **Minimum Operation Goal:** Provide a way to log emergency requests promptly, with a goal of ensuring requests are processed within a maximum of 5 minutes from initiation.
>  * **MTD:** Immediate (goal of zero delay for emergency additions)
>  * **MTPD:** 5 minutes (small tolerance to account for technical issues)


>* Operation Scheduling changes (update and deletion of operation requests)
>  * **Why is it critical?** Essential for updating or canceling operations in response to real-time shifts in personnel, availability, or emergency situations.
>  * **Minimum Operation Goal:** Ensure scheduling changes can be processed within an acceptable delay to keep information current.
>  * **MTD:** 2 minutes
>  * **MTPD:** 10 minutes (allows for minor delays but prioritizes timely updates for real-time accuracy)


>* Internal network access
>  * **Why is it critical?** Essential for accessing the resources necessary for operations, especially in local network settings where external connectivity may be limited.
>  * **Minimum Operation Goal:** Maintain internal network connectivity at a minimum bandwidth level sufficient to access core system modules.
>  * **MTD:** 1 minute
>  * **MTPD:** 30 minutes (limited delay acceptable due to fallback methods for access)


>* VPN access
>  * **Why is it critical?** Ensures remote access for key staff or administrators to access the network and maintain operation continuity from remote locations.
>  * **Minimum Operation Goal:** Maintain secure VPN access for a minimum of key staff during disruptions, with fallback systems to ensure continuous, secure connections.
>  * **MTD:** 2 minutes
>  * **MTPD:** 15 minutes (critical but has redundancy in terms of access tiers)

