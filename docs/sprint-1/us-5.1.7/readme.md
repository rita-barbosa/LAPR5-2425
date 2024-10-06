# US 5.1.7

<!-- TOC -->
* [US 5.1.7](#us-517)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
    * [4.1. Realization](#41-realization)
    * [4.2. Class Diagram](#42-class-diagram)
    * [4.3. Applied Patterns](#43-applied-patterns)
    * [4.4. Tests](#44-tests)
  * [5. Implementation](#5-implementation)
  * [6. Integration/Demonstration](#6-integrationdemonstration)
  * [7. Observations](#7-observations)
<!-- TOC -->

* This is the first time this user story is presented.


* Identity and Access Management (IAM) is a system that manages and controls the access to a company's resources by entities,
that have their identities verified and their access permissions are clearly defined and enforced.


* Single Sign-on (SSO) is an authentication scheme that combines multiple applications log in screens and allows the user 
to log in a single time to access the resources and services of those applications.


## 2. Requirements

**US 5.1.7:**  As a Patient, I want to log in to the healthcare system using my external IAM credentials, so that I can
access my appointments, medical records, and other features securely.

**Acceptance Criteria:**

- 5.1.7.1. | Patients log in via an external Identity and Access Management (IAM) provider (e.g., Google, Facebook, or hospital SSO).

- 5.1.7.2. | After successful authentication via the IAM, patients are redirected to the healthcare system with a valid session.

- 5.1.7.3 | Patients have access to their appointment history, medical records, and other features relevant to their profile.

- 5.1.7.4 | Sessions expire after a defined period of inactivity, requiring re-authentication.


**Dependencies/References:**

This functionality is dependent on the creation of a patient profile by the Admin (5.1.8).

**Client Clarifications:**

> **Question:** How are duplicate patient profiles handled when registered by both the patient and admin?
>
> **Answer:** The system checks the email for uniqueness. The admin must first create the patient record, and then the patient can register using the same email.

> **Question:** In IAM external system, if a patient is signed in with a Google account and later uses other external system like Facebook, and both have different credentials, what happens?
>
> **Answer:** Assume the system only supports one IAM.

> **Question:** How much time does it take of inactivity to automatically log off of an account? What do you define as inactivity? Like no input from the mouse?
>
> **Answer:** Inactivity is defined as no interaction with the API. After 20 minutes of inactivity, the session should disconnect.

> **Question:** Can users hold multiple roles?
>
> **Answer:** No, each user can have only one role.

> **Question:** Can a user have both patient and healthcare staff profiles?
>
> **Answer:** No, a user cannot have both profiles. Staff and patients have separate identifications.


## 3. Analysis

In this feature, patients can log in to the healthcare system using the available external Identity and Access Management
(IAM) provider and its credentials.
The patient's identity is verified by the IAM service, and upon successful authentication, they are granted a 
valid session.

The patient has then access to their account and profile, along with all their data.

If the system does not detect any user interaction within 20 minutes, then the session is disconnected, requiring
the patient to authenticate again.

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