# US 5.1.12

<!-- TOC -->
- [US 5.1.12](#us-5112)
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

This is the first time this user story is worked on.

## 2. Requirements

**US 5.1.12:**  As an Admin, I want to create a new staff profile, so that I can add them to the hospital’s roster.

**Acceptance Criteria:**

- 5.1.12.1: Admins can input staff details such as first name, last name, contact information, and specialization.

- 5.1.12.2: A unique staff ID (License Number) is generated upon profile creation.

- 5.1.12.3: The system ensures that the staff’s email and phone number are unique.

- 5.1.12.4: The profile is stored securely, and access is based on role-based permissions.


**Dependencies/References:**

This user story does not have dependencies.

**Client Clarifications:**

> **Question:** Can a user have both patient and healthcare staff profiles?
>
> **Answer:** No, a user cannot have both profiles. Staff and patients have separate identifications.

> **Question:** Are healthcare staff IDs unique across roles?
>
> **Answer:** Yes, staff IDs are unique and not role-specific (e.g., a doctor and nurse can share the same ID format).


> **Question:** Will there be a list of specializations in the system?
>
> **Answer:** Yes, a predefined list of specializations will be provided, but the system should allow for future additions.


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