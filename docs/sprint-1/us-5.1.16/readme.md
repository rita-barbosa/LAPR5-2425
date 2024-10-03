# US 5.1.16

<!-- TOC -->
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

**US 5.1.16:** As a Doctor, I want to request and operation, so that the Patient has access to the necessary healthcare.

**Acceptance Criteria:**

- **5.1.16.1.** Doctors can create and operation request by selecting the patient, operation type, priority, and
suggested deadline.

- **5.1.16.2.** The system validates that the operation type matches the doctor's specialization.

- **5.1.16.3** The operation request includes:

  - Patient ID
  - Doctor ID
  - Operation Type
  - Deadline
  - Priority

- **5.1.16.4** The system confirms successful submission of the operation request and logs the request in the patient's
medical history.

**Dependencies/References:**

This user story does not have dependencies.

**Client Clarifications:**

> **Question:** Is it mandatory for patients to have a user account to schedule a surgery?
>
> **Answer:** No, patients are not required to have a user account. The system administrator creates patient profiles.

> **Question:** What is the difference between appointment, surgery, and operation?
>
> **Answer:** Surgery is a medical procedure (e.g., hip surgery), while an operation request is when a doctor
> schedules that surgery for a patient. An appointment is the scheduled date for the operation, determined by the
> planning module

> **Question:** Can the same doctor who requests a surgery perform it?
>
> **Answer:**  Not necessarily. The planning module may assign different doctors based on availability and optimization.

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