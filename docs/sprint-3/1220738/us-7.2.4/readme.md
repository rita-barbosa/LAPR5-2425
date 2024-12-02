# US 7.2.4

<!-- TOC -->
- [US 7.2.4](#us-7.2.4)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
    - [3.1 System Sequence Diagram](#31-system-sequence-diagram)
    - [3.2 Domain Model](#32-domain-model)
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

This is the first time this US is being worked on.
It relates to the Admin.

## 2. Requirements

**US 7.2.4:** As an Admin, I want to add new Medical Condition, so that the Doctors can use it to update the Patient Medical Record.

**Acceptance Criteria:**

**US 7.2.4.1:** The functionality must have a Angular UI to comply with the specifications of the US 7.1.1.

**US 7.2.4.2:** The functionality must have an implementation on the .NET server in oder to function correctly.

**US 7.2.4.3:** The functionality must be related to the patient's medical record as it needs to eventually add the searched allergy to the referred document.

**US 7.2.4.4:** The functionality must add the medical condition to the patient's medical record and then inform the user of it's success/failure.

**US 7.2.4.5:** The functionality must not be allowed to add the already added medical conditions. 

**Dependencies/References:**

**US 7.1.#:** Is dependent on all these US as they pertain to integrating different modules of the system.

**US 7.2.1:** Is dependent on this US because it has the database that will hold all the medical conditions that will be used to search for the desired one.

**US 7.2.5:** Is dependent on this US because it will have the list of medical conditions that can be added. 

**Client Clarifications:**

>Question: None Yet.
>
>Answer: None Yet.

## 3. Analysis

This User Story asks to implement: 

- Firstly, the back-end implementation of a request that returns a list of medical conditions from the new MongoDB database that is implemented by the US 7.2.1. 

- Secondly, to implement the front end part of this US, in order to comply with the US's 7.2.4.1 acceptance criteria.  

This User Story will follow the same system as previous USs with the objective to add certain objects to the database.

## 4. Design

### 4.1. Realization

//TO BE DONE

#### Views

//TO BE DONE

### 4.2. Domain Model Excerpt

//TO BE DONE

### 4.3. Applied Patterns

//TO BE DONE

## 5. Implementation

//TO BE DONE

## 6. Testing

//TO BE DONE