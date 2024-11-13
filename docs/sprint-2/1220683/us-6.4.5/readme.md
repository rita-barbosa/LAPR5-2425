# US 6.4.5

<!-- TOC -->
* [US 6.4.5](#us-645)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
    * [4.1. Realization](#41-realization)
      * [Logical View](#logical-view)
      * [Process View](#process-view)
        * [Level 1](#level-1)
        * [Level 2](#level-2)
        * [Level 3](#level-3)
      * [Development View](#development-view)
      * [Physical View](#physical-view)
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

### 4.1. Realization

The logical, physical, development and scenario views diagrams are generic for all the use cases of the backoffice component.

#### Logical View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#1-logical-view).

#### Process View

##### Level 1

![Process View - Level 1]()

##### Level 2

_[This diagram is not relevant.]_

##### Level 3

![Process View - Level 3]()


#### Development View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#3-development-view).

#### Physical View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#4-physical-view).
