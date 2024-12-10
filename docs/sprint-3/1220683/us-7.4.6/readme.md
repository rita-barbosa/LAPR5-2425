# US 7.4.6

<!-- TOC -->
* [US 7.4.6](#us-746)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
  * [5. Observations](#5-observations)
<!-- TOC -->

## 1. Context

* This is the first time this user story is being requested.

## 2. Requirements

**US 7.4.6:** As a system administrator, I want the US backup of the DB backup to have a lifetime of no more than 7 (seven) days, except for the US retention of the monthly and annual copies.

**Acceptance Criteria:**

- **7.4.6.1.** The system must automatically delete daily backups older than 7 days without impacting monthly and yearly backups.
- **7.4.6.2.** Monthly backups should be retained for at least 1 year.
- **7.4.6.3.** Yearly backups should be retained indefinitely, or until explicitly deleted by the administrator.

**Dependencies/References:**

* **US 6.4.6:** Database backup feature, which outlines how backups are created and stored.
* Backup retention policies align with regulatory and compliance requirements for data storage.

**Client Clarifications:**

> No clarifications.

## 3. Analysis

To implement this user story, the system requires functionality to:

1. Identify and categorize backups (daily, monthly, yearly) based on creation timestamps or naming conventions.
2. Automatically delete backups exceeding their retention period, ensuring no overlap with long-term backups.

## 4. Design

To implement this functionality, we use a script that will firstly categorize the backups by inserting them into folders.
The execution of this script can be scheduled every day as task in cron.

**Daily backups**, performed at 2 a.m. on weekdays, with more than 7 days will be erased.

The last full/integral backup,scheduled every Sunday at 2 a.m, will be moved to a "**Monthly**" folder. These backup files
have the validity of one year, after that they too will be erased. To better identify them, a nomenclature must be in place,
such as `backup-YYYY-MM.tar.gz`.

Lastly, the "**Yearly**" backups are essentially the last monthly backup, which should have the latest version of the data
in the system. These files should also have a nomenclature, such as `backup-YYYY.tar.gz`, and can only be deleted manually
by the administrator.

## 5. Observations

No observations.