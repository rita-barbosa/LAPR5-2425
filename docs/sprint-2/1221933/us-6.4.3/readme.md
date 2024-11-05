# US 6.4.3

<!-- TOC -->
* [US 6.4.3](#us-643)
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

**US 6.4.3:** As system administrator, I want the clients listed in the requirement 6.4.2 to be able to be defined by 
simply changing a text file.

**Acceptance Criteria:**

- **6.4.3.1:** Only the clients on the DEI's internal network should have access to the solution.

- **6.4.3.2:** The list of clients should be defined on a text file, using the list obtained on the user story 6.4.2.

**Dependencies/References:**

**6.4.2:** This user story needs to implemented, so the clients can be listed.

**Client Clarifications:**

> **Question:** US 6.4.3 - "As system administrator, I want the clients listed in the requirement 6.3.2 to be able to be defined by simply changing a text file".
> US 6.3.2 - "As an Admin, I want to know till what dimension in terms of number of surgeries is possible to ask for the better solution".
> Dear Client,
> The relation between US 6.4.3 and US 6.3.2 doesn't make sense. Is it possible to provide additional information?
> Thank you in advance.
>
> **Answer:** Regarding US 6.4.3, please consider the 6.4.2, that is, "As system administrator, I want the clients listed in the requirement 6.4.2 to be able to be defined by simply changing a text file"."


## 3. Analysis

The main goal of this user story is to allow the manage the list of client by simply editing a text file. To implement
this, it will be needed:

  * text file configuration
  * DEI Network

It's important to be sure that only the system administrator has the permission to edit the file.


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
