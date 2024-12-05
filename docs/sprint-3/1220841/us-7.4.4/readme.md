# US 7.4.4

<!-- TOC -->
- [US 7.4.4](#us-744)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
  - [5. Implementation](#5-implementation)
<!-- TOC -->

## 1. Context

This user story is being implemented for the first time.

## 2. Requirements

**US 7.4.4:** As a system administrator, I want a script to be created using the backup prepared in previous US to manage the files resulting from this backup on the following schedule. 1 backup per month for the last year, 1 backup per week for the last month, 1 backup per day for the last week

**Acceptance Criteria:**

- 7.4.4.1: The script must ensure that only one backup per month is retained for the past year.
- 7.4.4.2: The script must ensure that only one backup per week is retained for the past month.
- 7.4.4.3: The script must ensure that only one backup per day is retained for the past week.
- 7.4.4.4: The script must run automatically to enforce the retention policy based on the backup schedule.

**Dependencies/References:**

This user story depends on [7.4.3](../../1220738/us-7.4.3/readme.md), as this is the functionality where the backup will be created.

## 3. Analysis

For this functionality a script will be developed in order to manage the backup files. Taking that into account, the script will need to:

- Retrieve all backup files data from the cloud storage, including creation dates.
- Filter Backups based on the defined retention schedule.
- Identify and remove files that do not fit the retention policy.
- Log actions performed by the script.

## 4. Design

### 4.1. Realization

//Todo

## 5. Implementation