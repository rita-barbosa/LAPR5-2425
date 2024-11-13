# US 6.4.6

<!-- TOC -->
- [US 6.4.6](#us-646)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
<!-- TOC -->

## 1. Context

This is the first time this user story is worked on.

## 2. Requirements

**US 6.4.6:** As system administrator, I want a backup strategy to be proposed, justified and implemented that minimizes RPO (Recovery Point Objective) and WRT (Work Recovery Time).

## 3. Analysis

The goal of this user story is to create and implement a backup plan that minimizes the Recovery Point Objective (RPO) - the maximum tolerable period of data loss - and the Work Recovery Time (WRT), the time required to restore and verify the data and application.

To achieve this, it will be essential to determine the most effective approach, whether it involves full, partial, or a combination of backup types. Additionally, an optimal backup schedule should be set, with RPO and WRT kept to a minimum at all times.

## 4. Design

### 4.1. Realization

- **Adopted Strategy:** To minimize both the Recovery Point Objective (RPO) and Work Recovery Time (WRT), we decided on a combination of full and differential backups. Full backups will be conducted on weekends, while differential backups will take place on weekdays. This approach strikes a balance between maintaining data integrity and ensuring efficient recovery.

  - **Full Backup (Integral Backup):** Scheduled every Sunday at 2 a.m., this process provides a comprehensive snapshot of all data, creating a solid baseline for recovery. The 2 a.m. timing coincides with minimal hospital activity, typically when only emergency services are active. Due to the length of time required for a full backup, scheduling it during low-activity periods on weekends is optimal.

  - **Differential Backup:** Performed at 2 a.m. on weekdays, this method captures only data that has changed since the last full backup, thereby reducing both backup time and storage requirements compared to daily full backups. The chosen time aligns with the hospital’s low-activity periods. This strategy ensures faster restoration since only one full backup and one differential backup are needed, as opposed to incremental backups, which may require restoring up to six separate backups in the worst case.

- **Analysis of RPO and WRT:**

  - **RPO (Recovery Point Objective):** This strategy may result in a maximum data loss of up to 24 hours on weekdays if an incident occurs shortly before the differential backup. In such cases, only data from that day would be lost.
  - **WRT (Work Recovery Time):** The recovery process involves restoring the last full backup followed by the most recent differential. This method generally ensures faster restoration compared to incremental backups.
