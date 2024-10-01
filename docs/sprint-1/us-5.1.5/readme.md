# US 5.1.5

<!-- TOC -->
* [US 5.1.5](#us-515)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
    * [System Sequence Diagram](#system-sequence-diagram)
    * [Domain Model](#domain-model)
  * [4. Design](#4-design)
    * [4.1. Realization](#41-realization)
    * [4.2. Class Diagram](#42-class-diagram)
    * [4.3. Applied Patterns](#43-applied-patterns)
    * [4.4. Tests](#44-tests)
  * [5. Implementation](#5-implementation)
  * [6. Integration/Demonstration](#6-integrationdemonstration)
  * [7. Observations](#7-observations)
<!-- TOC -->


## 1. Context

This is the first time this user story is being requested, related to the GDPR (General Data Protection Regulation).

## 2. Requirements

**US 5.1.5:** As a Patient, I want to delete my account and all associated data, so that I can exercise my right to be forgotten as per GDPR.

**Acceptance Criteria:**

- 5.1.5.1. | Patients can request to delete their account through the profile settings.

- 5.1.5.2. | The system sends a confirmation email to the patient before proceeding with account deletion.

- 5.1.5.3 | Upon confirmation, all personal data is permanently deleted from the system within the legally required time frame (30 days).

- 5.1.5.4 | Patients are notified once the deletion is complete, and the system logs the action for GDPR compliance.

- 5.1.5.5 | Some anonymized data may be retained for legal or research purposes, but all identifiable information is erased.


**Dependencies/References:**

This feature is related to the creation on a patient's account (US 5.1.3) and its profile creation (US 5.1.8) too.

**Client Clarifications:**

> **Question:** How are duplicate patient profiles handled when registered by both the patient and admin?
>
> **Answer:** The system checks the email for uniqueness. The admin must first create the patient record, and then the patient can register using the same email.


> **Question:** What happens to patient data after the profile is deleted?
>
> **Answer:** Patient data must be retained for a legally mandated period before being anonymized or deleted.


## 3. Analysis


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