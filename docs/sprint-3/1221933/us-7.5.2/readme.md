# US 7.5.2

<!-- TOC -->
* [US 7.5.2](#us-752)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
    * [4.1. Realization](#41-realization)
      * [Views](#views)
    * [4.2. Domain Model Excerpt](#42-domain-model-excerpt)
    * [4.3. Applied Patterns](#43-applied-patterns)
  * [5. Implementation](#5-implementation)
  * [6. Testing](#6-testing)
<!-- TOC -->


## 1. Context

This is the first time this US is being worked on.

## 2. Requirements

**US 7.5.2:** As a healthcare staff member, whenever I press the “i” key, I want to display/hide an overlay containing 
updated information about the selected room.

**Acceptance Criteria:**

**US 7.5.2.1:** The information should be always updated.

**Dependencies/References:**

**US 6.5.1:** The 3D model should be created.

**Client Clarifications:**

>**Question:**
>
>**Answer**:


## 3. Analysis

The main objective is to enable healthcare staff to quickly view or hide updated information about a selected room using
the "i" key. The overlay must display real-time data relevant to the selected room and remain in sync with the 3D model.

Main points to address are:

* The "i" key should toggle the overlay without conflicting with other shortcuts.
* The overlay must only appear when a room is selected, showing an appropriate message otherwise.
* Data displayed should update dynamically to ensure accuracy.

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