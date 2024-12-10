# US 7.4.12

<!-- TOC -->
* [US 7.4.12](#us-7412)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
  * [5. Observations](#5-observations)
<!-- TOC -->

## 1. Context

* This is the first time this user story is being requested.

## 2. Requirements

**US 7.4.12:** As a system administrator, we need to ensure that backups have been carried out correctly if necessary.
To do this, we must automate their recovery, validating that the system is working at the end (e.g. database - execute a SQL query successfully after recovery).

**Acceptance Criteria:**

- **7.4.12.1.** The system must provide an automated mechanism to restore backups and validate the process with a predefined test (e.g., executing a SQL query).
- **7.4.12.2.** If the validation test fails, the system must notify the system administrator.
- **7.4.12.3.** Logs of recovery attempts and their outcomes must be maintained for auditing purposes, including timestamps and details of success/failure.
- **7.4.12.4.** The recovery process must be executable on-demand by the system administrator or automatically as part of scheduled tests.

**Dependencies/References:**

* **US 6.4.6:** Database backup feature, which outlines how backups are created and stored.
* **US 7.4.6:** Retention policies and lifecycle management of backups.

**Client Clarifications:**

> **Question:** Should the recovery validation process be scheduled, on-demand, or both?
>
> **Answer:** Recovery should only be performed when needed. As so, on-demand is likely to be the best approach.


## 3. Analysis

To implement this user story, the system requires functionality to:

1. Automate the restoration of backups, ensuring minimal manual intervention.
2. Execute predefined validation checks after the recovery process to confirm system integrity.
3. Integrate alert mechanisms for failed validation attempts.
4. Maintain logs of recovery and validation activities for transparency and troubleshooting.

Recovery operations must be isolated to prevent disruption in active systems.

## 4. Design

To validate the integrity of database backups, a script is used to automate the recovery validation process. This script
begins by restoring the latest database backup to a test or staging server, ensuring the backup is usable and effective 
for disaster recovery scenarios.

Once the restoration is complete, the script runs a series of SQL validation queries to check critical aspects of the data.
These include comparing record counts across important tables, verifying key configuration or metadata values, and ensuring
the most recent transaction or update timestamp aligns with expectations. These checks help confirm that no data is missing
and that the restored database is consistent with the original system.

Finally, the results of these validation queries are logged into a file. This log serves as an audit trail and provides 
clarity on whether each validation test passed or failed, offering actionable insights to system administrators. This 
process ensures that backups are not only stored but are also reliable and ready to support recovery when needed.

## 5. Observations

No observations.