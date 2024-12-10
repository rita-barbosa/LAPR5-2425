# US 7.4.1

<!-- TOC -->
* [US 7.4.1](#us-741)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
    * [Plan Structure:](#plan-structure)
  * [4. Disaster Recovery Plan](#4-disaster-recovery-plan)
  * [5. Observations](#5-observations)
<!-- TOC -->

## 1. Context

This user story focuses on developing a disaster recovery (DR) plan that meets the **Minimum Business Continuity Objective (MBCO)** defined in Sprint B.
The MBCO specifies the minimum operability level to maintain during disruptions, guiding recovery priorities and actions.

## 2. Requirements

**US 7.4.1:** As the organization's administrator, I want a disaster recovery plan that meets the MBCO defined in Sprint B.

**Acceptance Criteria:**
- **7.4.1.1.** A disaster recovery plan document is created and available.
- **7.4.1.2.** The plan identifies critical services and their MBCO targets.
- **7.4.1.3.** Emergency response procedures are included.
- **7.4.1.4.** Backup and recovery actions are clearly defined and tested.
- **7.4.1.5.** An alternate hot site plan is outlined for quick recovery.
- **7.4.1.6.** Full system restoration steps are described in detail.

**Dependencies/References:**
- Related to MBCO proposal (**US 6.4.5**).

**Client Clarifications:**

> **Question:** Should the disaster recovery plan account for both natural disasters and cybersecurity incidents? What types of disruptions/topics should be focused on?
> 
> **Answer:** The DRP should cover all catastrophes that may occur, provided they are adequately included. Disruptions/topics are those that are thought likely to occur, jeopardising the company's normal behaviour and business.

## 3. Analysis

A DR (disaster recovery) plan ensures that key operations can continue or resume quickly after a disruption. The plan will:
- Prioritize **critical services** aligned with MBCO targets.
- Include detailed **response procedures** to minimize risks to lives and systems.
- Define **backup operations** for essential data processing.
- Specify **hot site requirements** to maintain continuity during major outages.
- Outline **system restoration steps** to return to pre-disruption status.

### Plan Structure:
1. **Goals**: Define the main objectives of the DR plan.
2. **Application Profile**: Identify critical services and their roles.
3. **Backup Procedures**: Describe methods to secure essential data.
4. **Emergency Response**: Document steps to protect lives and limit damages.
5. **Recovery Actions**: Detail processes for restoring systems.
6. **Hot Site Plan**: Outline alternative site activation.
7. **Full System Restoration**: Provide instructions for complete recovery.

## 4. Disaster Recovery Plan

> TBD

## 5. Observations

- The DR plan must be clear and concise to ensure usability during emergencies.
- Regular updates are necessary to reflect changing infrastructure and risks.
- References:
  - [Kyndryl Disaster Recovery Plan Guide](https://www.kyndryl.com/us/en/learn/disaster-recovery-plan) 
  - [TechTarget Disaster Recovery Plan Guide](https://www.techtarget.com/searchdisasterrecovery/definition/disaster-recovery-plan)
  - [IBM Disaster Recovery Plan Guide](https://www.ibm.com/topics/disaster-recovery-plan)
  - [IBM Disaster Recovery Plan Example](https://www.ibm.com/docs/en/i/7.3?topic=system-example-disaster-recovery-plan)
