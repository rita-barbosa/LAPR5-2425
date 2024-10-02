# US 5.1.4

<!-- TOC -->
- [US 5.1.4](#us-514)
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

This is the first time this user story worked on.

## 2. Requirements

**US 5.1.4:** As a Patient, I want to update my user profile, so that I can change my personal 
details and preferences.


**Acceptance Criteria:**

- **5.1.4.1:** Patients can log in and update their profile details (e.g., name, contact information, preferences).

- **5.1.4.2:** Changes to sensitive data, such as email, trigger an additional verification step (e.g., confirmation email).

- **5.1.4.3:** All profile updates are securely stored in the system.

- **5.1.4.4:** The system logs all changes made to the patient's profile for audit purposes.

**Dependencies/References:**

This user story is dependent on US-5.1.3, as a patient must have a user profile in order to update it.

**Client Clarifications:**

> **Question:** Can the same user have both a patient and a healthcare profile? 
>
> **Answer:** No. When it comes to patients and healthcare staff, they both have one of the rules that says that some of their, like, attributes need to be unique, and they both need their email to be unique.

> **Question:** Is the email the identifying attribute or is it the username? 
>
> **Answer:** I's the username. But typically, nowadays, most of the usernames that you have in all the systems are your email.

> **Question:** The first acceptance criterion mentions contact information. This attribute belongs to the patient profile, not the user profile. Therefore, my question is whether patients can also update their patient profile information in addition to their user profile information. And if so, is there any information of the patient profile that can't be updated?
>
> **Answer:** _Waiting for client clarification_

> **Question:** What does the term "preferences" refer to?
>
> **Answer:** _Waiting for client clarification_


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