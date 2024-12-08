# US 7.4.3

<!-- TOC -->
- [US 7.4.3](#us-7.4.3)
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

**US 7.4.3:** As a system administrator, I want to make a backup copy of the DB(s) to a Cloud environment using a script that renames it to the format yyyymmdd where is the name of the database, yyyy is the year the copy was made, mm is the month the copy was made and dd is the day the copy was made.

**Acceptance Criteria:**

**US 7.4.3.1:** The **yyyy** in the name format is for the year of the creation of the backup.

**US 7.4.3.2:** The **mm** in the name format is for the month of the creation of the backup.

**US 7.4.3.3:** The **dd** in the name format is for the day of the creation of the backup.

**US 7.4.3.4:** The backups are to be stored in a separate VM from the VM where the MongoDB DB is, the MDBackoffice DB is and the MDBackoffice as well.

**US 7.4.3.5:** In case there's an error while doing the backup, it should alert the admin and stop the action.

**US 7.4.3.6:** Also in case of an error it should log it in the appropriate folder, using the methods learned in class.

**Dependencies/References:**

**US 7.4.4-6 and 7.4.12:** They depend on this US as it creates the process to backup or even the backups themselves that will be later used or changed in later USs.

**Client Clarifications:**

>**Question:** None yet.
>
>**Answer:** None yet.

## 3. Analysis

This User Story will be a continuation of an earlier US, 6.4.6, since it implements a well established strategy.

This User Story will add the automatization of the backups depending on the database, it will change the location of the backups to a remote VM as to make sure they will not be lost in case of a malfunction of their current location.

This User Story will involve the system logs from Linux and possibly a FTS to send the files to other VMs.

## 4. Design

### 4.1. Realization

The design for this user story involves creating a script that automates the process of backing up databases to a Cloud environment with date-formatted naming. The script will identify the database(s) to back up, generate a backup file, and rename the file according to the format <DBName>_yyyyMMdd. The current date will be used for the yyyyMMdd part of the filename to ensure uniqueness and clarity.

The script will then transfer the backup to the specified Cloud environment using appropriate API commands or CLI tools for the chosen provider. It will rely on preconfigured authentication credentials and permissions for secure and successful uploads.

Error handling will be implemented to manage issues like connection failures, insufficient permissions, or backup failures, with logs generated for debugging and audit purposes. 

## 5. Implementation

//TO BE DONE

## 6. Testing

//TO BE DONE