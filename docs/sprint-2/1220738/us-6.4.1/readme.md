# US 6.4.1

<!-- TOC -->
- [US 6.4.1](#us-641)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
      - [Logical View](#logical-view)
      - [Process View](#process-view)
        - [Level 1](#level-1)
        - [Level 2](#level-2)
        - [Level 3](#level-3)
      - [Development View](#development-view)
      - [Physical View](#physical-view)
<!-- TOC -->

## 1. Context

This is the first time this US is being worked on.
This US pertains to the Admin.

## 2. Requirements

**US 6.4.1:** As system administrator, I want the deployment of one of the RFP modules in a DEI VM to be systematic, validating it on a scheduled bases with the test plan.

**Acceptance Criteria:**

**US 6.4.1.1 -** The deployment of one of the RFP modules in a DEI VM is to be systematic. 

**US 6.4.1.2 -** Validating the module should be doen through regular testing.

**Dependencies/References:**

USs related to the BackOffice are going to depend on this implementation. 

**Client Clarifications:**

> None yet.

## 3. Analysis

After some consideration, it was agreed on the MDBackOffice to be the module that will be implemented in the DEI VM. 

In an effort to make the implementation be systematic, a *script* and *scheduled events* are going to be created to regularly run tests to check on the status of the module.

## 4. Design

### 4.1. Realization

After questioning teachers and the client, this US will be implemented in the following manner:

1. A Linux DEI VM will be created to host the RFP Module.
2. With the help of the deployment scripts already available by BitBucket, we'll adapt them to the MDBackoffice Module.
3. A script, similar to a *pipeline*, will be created using Linux Scripts that will use the already existing tests to check on the good work of the module
4. These tests will be done regurlarly and so the cronstad will be used to check on the module every 2-3 days at 5AM, as to not disrupt the possible operations that could be scheduled.
5. When a commit is done, the script will check the changes, test them and if something does not pass the tests, it will rollback, again, to not affect the system's well being.
