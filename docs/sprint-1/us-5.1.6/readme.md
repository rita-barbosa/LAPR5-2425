# US 5.1.6

<!-- TOC -->
- [US 5.1.6](#us-516)
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

**US 5.1.6:** As a (non-authenticated) Backoffice User, I want to log in to the system using my credentials, so that I can access the backoffice features according to my assigned role.

**Acceptance Criteria:**

- 5.1.6.1: Backoffice users log in using their username and password.

- 5.1.6.2: Role-based access control ensures that users only have access to features appropriate to their role (e.g., doctors can manage appointments, admins can manage users and settings).

- 5.1.6.3:  After five failed login attempts, the user account is temporarily locked, and a notification is
sent to the admin.

- 5.1.6.4:  Login sessions expire after a period of inactivity to ensure security.

**Dependencies/References:**

This user story is related to US-5.1.1, as the backoffice user must be registered by the admin beforehand.

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
