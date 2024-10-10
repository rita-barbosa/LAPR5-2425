# Generic System View Diagrams

<!-- TOC -->
* [Generic System View Diagrams](#generic-system-view-diagrams)
  * [Introduction](#introduction)
  * [1. Logical View](#1-logical-view)
    * [1.1. Level 1](#11-level-1)
    * [1.2. Level 2](#12-level-2)
    * [1.3. Level 3](#13-level-3)
      * [1.3.1 Backoffice Component](#131-backoffice-component)
  * [2. Process View](#2-process-view)
  * [3. Development View](#3-development-view)
    * [3.1. Level 1](#31-level-1)
    * [3.2. Level 2](#32-level-2)
    * [3.3. Level 3](#33-level-3)
  * [4. Physical View](#4-physical-view)
    * [4.1. Level 1](#41-level-1)
    * [4.2. Level 2](#42-level-2)
    * [4.3. Level 3](#43-level-3)
  * [5. Scenario View](#5-scenario-view)
    * [5.1. Level 1](#51-level-1)
<!-- TOC -->


## Introduction

The diagrams of this project will follow a combination of two architecture representation models, C4 and 4+1.

The 4+1 View Model presents a way to describe a software system using multiple perspectives. The model is broken down into the following views:

> * **Logical view:** Focuses on the software's structure, addressing business needs and functionality.
> 
> 
> * **Process view:** Deals with the system's dynamic aspects, such as interactions and workflows.
> 
> 
> * **Development view:** Focuses on how the software is organized during development.
> 
> 
> * **Physical view:** Describes how the software is deployed and run on hardware infrastructure.
> 
> 
> * **Scenario view:** Shows how business processes interact with actors in the system through use cases and workflows. 


The C4 Model offers a hierarchical approach to visualizing software systems through four levels of detail. As you move 
through each level, the focus narrows, revealing more detail about smaller parts of the system. The levels in the C4 Model
are defined as follows:

> * **Level 1:** A high-level view of the entire system (framework).
>
> 
> * **Level 2:** A detailed description of the system's containers.
> 
> 
> * **Level 3:** A breakdown of the components within each container.
> 
> 
> * **Level 4:** A detailed look at the code or smaller parts of the components (this level will not be covered in this project).


The C4 Model organizes the system into varying levels of granularity, while the 4+1 View Model describes the system from
different perspectives. By integrating both models, the system can be represented from multiple viewpoints, each at different
levels of detail.

To visually model the system, the **Unified Modeling Language (UML)** is used.

---

## 1. Logical View

### 1.1. Level 1

![healthcare_system_vl1.drawio.svg](logical-view/healthcare_system_vl1.drawio.svg)

### 1.2. Level 2

![healthcare_system_vl2.drawio.svg](logical-view/healthcare_system_vl2.drawio.svg)

> explain the representation of two Backoffice API

### 1.3. Level 3

> explain why this view will have multiple diagrams

#### 1.3.1 Backoffice Component

![backoffice_vl3.drawio.svg](logical-view/backoffice_vl3.drawio.svg)

---

## 2. Process View

Process views will be done in each user story **readme** file.

---

## 3. Development View

### 3.1. Level 1

_//To do//_

### 3.2. Level 2

![vi2.drawio.svg](development-view\development-view-level-2.svg)

### 3.3. Level 3

_//To do//_

---

## 4. Physical View

### 4.1. Level 1

_//To do//_

### 4.2. Level 2

_//To do//_

### 4.3. Level 3

_//To do//_

---

## 5. Scenario View

### 5.1. Level 1

![use-case-diagram.svg](use-case-diagram.svg)

> Justify why having more levels of the scenario view isn't relevant to the project