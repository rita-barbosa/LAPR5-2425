# US 5.1.2

<!-- TOC -->
* [US 5.1.2](#us-512)
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

This is the first time this user story is being requested.

## 2. Requirements

**US 5.1.2:** As a Backoffice User (Admin, Doctor, Nurse, Technician), I want to reset my password if I forget it, so
that I can regain access to the system securely.

**Acceptance Criteria:**

- **5.1.2.1.** Backoffice users can request a password reset by providing their email.

- **5.1.2.2.** The system sends a password rest link via email.

- **5.1.2.3** The reset link expires after a predefined period (e.g., 24 hours) for security.

- **5.1.2.4** Users must provide a new password that meets the system's password complexity rules.

**Dependencies/References:**

**5.1.1:** The Backoffice User needs to be created first, so they can change their password.

**Client Clarifications:**

> **Question:** What are the system's password requirements?
>
> **Answer:** 


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