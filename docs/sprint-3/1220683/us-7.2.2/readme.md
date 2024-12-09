# US 7.2.2

<!-- TOC -->
* [US 7.2.2](#us-722)
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

**US 7.2.2:** As an Admin, I want to add new Allergy, so that the Doctors can use it to update the Patient Medical Record.

**Acceptance Criteria:**

- **7.2.2.1.** | The code identifying the Allergy must follow SNOMED CT or ICD-11 formats.

**Dependencies/References:**

This feature is related to the existence of a patient profile (US 5.1.8) and a patient medical record (US 7.2.14).

**Client Clarifications:**

> **Question:** What information is to be known in an Allergy? Like designation, and anything more?
>
> **Answer:** It consists of a code (for instance, SNOMED CT (Systematized Nomenclature of Medicine - Clinical Terms) or ICD-11 (International Classification of Diseases, 11th Revision)), a designation and an optional longer description.

## 3. Analysis

Allergies are conditions that a patient has, and those must be registered in the Patient Medical Record. The following
information must be given to register an allergy into the system:

* Code (SNOMED CT or ICD-11)
* Designation
* Description (optional)

### Domain Model

![domain-model.svg](domain-model.svg)

**Observation:** MRN refers to the medical record number.

## 4. Design

### 4.1. Realization

The logical, physical, development and scenario views diagrams are generic for all the use cases of the backoffice component.
These diagrams can be found in the [generic views diagrams compilation file](../../team-decisions/views/general-views.md).

The process view levels are here presented as they represent a process specific to each user story.

### Process View

#### Level 1

![us7.2.2-process-view-lvl1.svg](Process_View/Level-1/us7.2.2-process-view-lvl1.svg)

#### Level 2

As this level does not add any additional information regarding the user story's execution flow from level 1's diagram, 
it's diagram was deemed irrelevant.

#### Level 3

- _Visualization_<br>
![us7.2.2-process-view-visualization-lvl3.svg](Process_View/Level-3/us7.2.2-process-view-visualization-lvl3.svg)


- _MDBackoffice_
![us7.2.2-process-view-lvl3.svg](Process_View/Level-3/us7.2.2-process-view-lvl3.svg)

#### 4.1.3 Development View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#3-development-view).

#### 4.1.4 Physical View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#4-physical-view).
  
### 4.2. Applied Patterns

> #### **Repository Pattern**
>
>* **Components:** AllergyRepository, LogRepository
>
> The repositories are responsible for data access and retrieval, separating the logic for interacting with the database
> from the services and other layers. This pattern helps in abstracting the persistence logic.


> #### **DTO (Data Transfer Object) Pattern**
>
>* **Components:** MedicalConditionDto
>
> DTOs are used to transfer data between layers, especially from the controller layer to the service layer or vice versa.
> The purpose is to carry data in a structured and decoupled way without exposing internal entity representations directly.
> This pattern does not need to follow business rules.


> #### **Facade Pattern**
>
>* **Components:** LogService, AllergyService
>
> These services act as a Facade to simplify interaction with lower-level components like repositories. The Controller
> interacts with these service facades, keeping the complexity hidden from the higher layers.

### 4.2. Tests

_// To do //_


## 5. Implementation

> TBD

## 6. Integration/Demonstration

> TBD

## 7. Observations
