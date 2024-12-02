# US 7.4.10

<!-- TOC -->
- [US 7.4.10](#us-7.4.10)
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
It relates to the System Administrator.

## 2. Requirements

**US 7.4.10:** As system administrator, I want the administrator to have SSH access to the virtual machine, by certificate only, without a password.

**Acceptance Criteria:**

**US 7.4.10.1:** SSH access to the virtual machine must require an administrator's private key.

**US 7.4.10.2:** Attempts to log in using a password must be rejected.

**US 7.4.10.3:** Invalid or untrusted keys must result in a denied connection.

**US 7.4.10.4:** A successful login must grant the administrator appropriate permissions to manage the VM.

**US 7.4.10.5:** All login attempts (both successful and failed) must be logged in the system's authentication logs (**/var/log/auth.log** or equivalent).

**Dependencies/References:**

**US 7.4.9 and 7.4.10:** They depend on this US as it creates the new type of access to the virtual machine.

**Client Clarifications:**

>**Question:** None yet.
>
>**Answer:** None yet.

## 3. Analysis

This User Story will use a public/private key pair for authentication. The administrator's public key will be stored in the VM’s **/.ssh/authorized_keys** file, and their private key will remain securely on their local machine.

The SSH server must be configured to:

1. Accept only public key authentication.
2. Deny password-based login attempts.
3. Log all access attempts (successful and failed).

All SSH login attempts will be logged in **/var/log/auth.log** or an equivalent location to provide an audit trail and facilitate monitoring of access to the VM.

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