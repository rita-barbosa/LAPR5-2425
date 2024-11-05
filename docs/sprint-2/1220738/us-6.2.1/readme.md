# US 6.2.1

<!-- TOC -->
- [US 6.2.1](#us-621)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
      - [Logical View](#logical-view)
      - [Process View](#process-view)
        - [Level 1](#level-1)
        - [Level 2](#level-2)
        - [Level 3](#level-3)
      - [Development View](#development-view)
      - [Physical View](#physical-view)
<!-- TOC -->

## 1. Context

This is the first time this US is being worked on.
This US pertains to the Admin.

## 2. Requirements

**US 6.2.1:** As a Patient, I want to register for the healthcare application, so that I can create a user profile and book appointments online.

**Acceptance Criteria:**

- **6.2.1.1:** Patients can self-register using the external IAM system.

- **6.2.1.2:** During registration, patients provide personal details (e.g., name, email, phone) and create a profile.

- **6.2.1.3:** The system validates the email address by sending a verification email with a confirmation link.

- **6.2.1.4:** Patients cannot list their appointments without completing the registration process.

- **6.2.1.5:** The menu should be done in Angular.

**Dependencies/References:**

**5.1.8:** The Admin must create first the patient record, so that the patient can register using the same email.

**Client Clarifications:**

> **Question:** How are duplicate patient profiles handled when registered by both the patient and admin?
>
> **Answer:** The system checks the email for uniqueness. The admin must first create the patient record, and then the
> patient can register using the same email.

> **Question:** Can the same user have both a patient and a healthcare profile?
>
> **Answer:** No. When it comes to patients and healthcare staff, they both have one of the rules that says that some of
> their, like, attributes need to be unique, and they both need their email to be unique.

> **Question:** Is the email the identifying attribute or is it the username?
>
> **Answer:** It's the username. But typically, nowadays, most of the usernames that you have in all the systems are your
> email.

> **Question:** The user has contact information, email and phone, they are booth obligatory?
>
> **Answer:** Yes.

## 3. Analysis

This functionality has the objective of allowing the patient to register for the healthcare application, so he can create
their respective profile and book appointments.

The first process is for the patient to register itself using the external IAM, but also to provides some personal 
information, such as:
- name
- email
- phone number

After this registration, will occur the verification of the email address, by sending a verification email with a 
confirmation link.


### System Sequence Diagram

![us-6.2.1-ssd.svg](diagrams/ssd/us-6.2.1-ssd.puml.svg)

### Domain Model

![Domain Model Excerpt](diagrams/Domain-Model/domain-model-excerpt.svg)

## 4. Design

### 4.1. Realization

// TODO

#### Logical View

// TODO

#### Process View

##### Level 1

// TODO

##### Level 2

_[This diagram is not relevant.]_

##### Level 3

// TODO

#### Development View

// TODO

#### Physical View

// TODO


