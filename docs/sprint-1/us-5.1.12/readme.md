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

In this functionality, the admin can create a staff profile by inputting the following information:

- Name (first and last name)
- Contact information (email and phone number)
- Specialization

It’s important to note that specializations are selected from a predefined list, so the admin will need to choose one of the available options.

Upon creating the profile, a unique license number will be generated and assigned to the staff member, which will serve as the identifiable attribute for each profile. This license number is not role-specific, meaning all staff will share the same ID format.

Finally, as specified in the acceptance criteria, the contact information (email and phone number) must be unique. Consequently, the system must verify that no other profile exists with the same email or phone number.

### Domain Model

![Domain Model](diagrams/domain-model.svg)

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