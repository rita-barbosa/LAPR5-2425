# US 5.1.18

<!-- TOC -->
- [US 5.1.18](#us-5118)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
    - [System Sequence Diagram](#system-sequence-diagram)
    - [Domain Model](#domain-model)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
    - [4.2. Class Diagram](#42-class-diagram)
    - [4.3. Applied Patterns](#43-applied-patterns)
    - [4.4. Tests](#44-tests)
  - [5. Implementation](#5-implementation)
  - [6. Integration/Demonstration](#6-integrationdemonstration)
  - [7. Observations](#7-observations)
<!-- TOC -->


## 1. Context

This is the first time this User Story is being worked on. 
This User Story is related to the Admin.

## 2. Requirements

**US 5.1.18:** As a Doctor, I want to remove an operation requisition, so that the healthcare activities are provided as necessary. 

**Acceptance Criteria:**

- **5.1.18.1. -** Doctors can delete operation requests they created if the operation has not yet been scheduled. 

- **5.1.18.2. -** A confirmation prompt is displayed before deletion. 

- **5.1.18.3. -** Once deleted, the operation request is removed from the patient’s medical record and cannot be recovered. 

- **5.1.18.4. -** The system notifies the Planning Module and updates any schedules that were relying on this request. 

**Dependencies/References:**

- **US 5.1.1. -** US 5.1.18. is dependent on this User Story as it depends on an existing account of a staff in the system.

- **US 5.1.6. -** US 5.1.18. is dependent on this User Story as it depends on an existing account of a staff in the system with the right permissions.

- **US 5.1.12. -** US 5.1.18. is dependent on this User Story as it depends on an existing profile of a staff in the system.

- **US 5.1.16. -** US 5.1.18. is dependent on this User Story as it depends on an existing operation request in the system.

- **US 5.1.19. -** This User Story is dependent on US 5.1.18 as it needs it's functionality to work as intended.

- **US 5.1.20. -** US 5.1.18. is dependent on this User Story as it depends on an existing operation type in the system.

**Client Clarifications:**

> None yet.

## 3. Analysis

_// To do //_

### System Sequence Diagram

_// To do - if justified //_

### Domain Model

_// To do //_

## 4. Design

### 4.1. Realization

_// To do //_

### 4.2. Class Diagram

_// To do //_

### 4.3. Applied Patterns

_// To do //_

### 4.4. Tests

_// To do - layout still in development //_ 


## 5. Implementation

_// To do //_

## 6. Integration/Demonstration

_// To do //_

## 7. Observations

_// To do //_