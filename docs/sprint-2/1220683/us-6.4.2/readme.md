# US 6.4.2

<!-- TOC -->
* [US 6.4.2](#us-642)
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

This is the first time this user story is worked on.

## 2. Requirements

**US 6.4.2:** As a system administrator, I want only customers from DEI's internal network (wired or via VPN) to access the solution.

**Acceptance Criteria:**

- 6.4.2.1: Only DEI internal network addresses are allowed.

**Dependencies/References:**

This user story has no dependencies.

**Client Clarifications:**

> **Question:**
>
> **Answer:**

## 3. Analysis

To allow only the customers from DEI's internal network to access the solution provided some limitations must be implemented
regarding the network packet traffic - a firewall. Taking into account the information retrieved, this filtering mechanism
must impose the following restrictions:

- Allow packets from DEI's internal network IP addresses range
- Allow packets from VPN IP addresses range
- Allow Localhost (Loopback) access
- Drop all other incoming traffic

By applying these limitations we can guarantee that only the chosen users can access the solution.

## 4. Design

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
