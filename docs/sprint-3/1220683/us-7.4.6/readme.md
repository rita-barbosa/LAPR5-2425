# US 7.4.6

<!-- TOC -->
* [US 7.4.6](#us-746)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
  * [5. Implementation](#5-implementation)
    * [Daily erasure Script - _daily_management.sh_](#daily-erasure-script---_daily_managementsh_)
      * [Additional Steps](#additional-steps)
<!-- TOC -->

## 1. Context

* This is the first time this user story is being requested.

## 2. Requirements

**US 7.4.6:** As a system administrator, I want the US backup of the DB backup to have a lifetime of no more than 7 (seven) days, except for the US retention of the monthly and annual copies.

**Acceptance Criteria:**

- **7.4.6.1.** The system must automatically delete daily backups older than 7 days without impacting monthly and yearly backups.

**Dependencies/References:**

* **US 6.4.6:** Database backup feature, which outlines how backups are created and stored.
* Backup retention policies align with regulatory and compliance requirements for data storage.

**Client Clarifications:**

> No clarifications.

## 3. Analysis

To implement this user story, the system requires functionality to automatically delete backups exceeding their retention
period, ensuring no overlap with long-term backups.

## 4. Design

To implement this functionality, we use a script that will eliminate any file that is older than 7 days. The most recent
full backup will go be then retrieved and moved to the weekly backup directory, as stated in [US 7.4.4](../../1220841/us-7.4.4/readme.md).


## 5. Implementation

### Daily erasure Script - _daily_management.sh_

```console
#!/bin/bash


#!/bin/bash

# Directory where backups are stored
FULL_BACKUP_PATH="/path/to/full_backup" ## to be defined

# Find and delete files older than 7 days
find "$FULL_BACKUP_PATH" -type f -mtime +7 -exec rm -f {} \;

# Confirmation
echo "Old backups deleted successfully."
```

#### Additional Steps

1. Save the script as _daily_management.sh_ and make it executable:

```console
chmod +x daily_management.sh
```

2. Edit your crontab by adding the following line to schedule the script every day at 02:30:

```console
30 2 * * * /scripts/daily_management.sh
```
