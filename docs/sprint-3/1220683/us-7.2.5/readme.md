# US 7.2.5

<!-- TOC -->
* [US 7.2.5](#us-725)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
    * [Domain Model](#domain-model)
  * [4. Design](#4-design)
    * [4.1. Realization](#41-realization)
    * [Process View](#process-view)
      * [Level 1](#level-1)
      * [Level 2](#level-2)
      * [Level 3](#level-3)
      * [4.1.3 Development View](#413-development-view)
      * [4.1.4 Physical View](#414-physical-view)
    * [4.2. Applied Patterns](#42-applied-patterns)
    * [4.2. Tests](#42-tests)
  * [5. Implementation](#5-implementation)
  * [6. Integration/Demonstration](#6-integrationdemonstration)
  * [7. Observations](#7-observations)
<!-- TOC -->


## 1. Context

* This is the first time this user story is being requested.

## 2. Requirements

**US 7.2.5:** As a Doctor, I want to search for Medical Conditions, so that I can use it to update the Patient Medical Record.

**Acceptance Criteria:**

**US 7.2.5.1:** The functionality must have a way to cancel the search when the doctor changes their mind, effectively
dividing this US's functionality into 2, searching and then possibly adding.

**US 7.2.5.2:** The functionality must implement a rudimentary search system to show the medical conditions that can be added.

**US 7.2.5.3:** The functionality must not show the already added medical conditions OR inform the user they already have been added.

**Dependencies/References:**

This feature is related to the existence of a patient profile (US 5.1.8) and a patient medical record (US 7.2.14).

**Client Clarifications:**

> **Question:** What do you define as Medical Condition? Is it an allergy?
>
> **Answer:** They are two different things. A Medical Condition represents a diagnosed health issue or disease. Examples: Diabetes, Hypertension, Asthma, etc.


## 3. Analysis

When a doctor is looking through a patient's medical record, he has the possibility of updating it. One of the areas that
is subject to change is the one regarding medical conditions.

The search for medical conditions will be integrated in the patient medical record update. This search will be used to add
medical conditions to the medical record.

Regarding backend, a method will be needed to retrieve all the medical conditions that are persisted within the database.

### Domain Model

![domain-model.svg](domain-model.svg)

## 4. Design

### 4.1. Realization

The logical, physical, development and scenario views diagrams are generic for all the use cases of the backoffice component.
These diagrams can be found in the [generic views diagrams compilation file](../../team-decisions/views/general-views.md).

The process view levels are here presented as they represent a process specific to each user story.

### Process View

#### Level 1

> TBD

#### Level 2

As this level does not add any additional information regarding the user story's execution flow from level 1's diagram, 
it's diagram was deemed irrelevant.

#### Level 3

- _Visualization_<br>
> TBD


- _MDBackoffice_
> TBD

#### 4.1.3 Development View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#3-development-view).

#### 4.1.4 Physical View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#4-physical-view).
  
### 4.2. Applied Patterns

> TBD


### 4.2. Tests

_// To do //_


## 5. Implementation

> TBD

## 6. Integration/Demonstration

> TBD

## 7. Observations
