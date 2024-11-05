# US 6.1.1

<!-- TOC -->
* [US 6.1.1](#us-611)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
    * [4.1. Realization](#41-realization)
      * [Logical View](#logical-view)
      * [Process View](#process-view)
        * [Level 1](#level-1)
        * [Level 2](#level-2)
        * [Level 3](#level-3)
      * [Development View](#development-view)
      * [Physical View](#physical-view)
<!-- TOC -->


## 1. Context

This is the first time this user story is worked on.

## 2. Requirements

**US 6.1.1:** As user, I want to have an integrated UI for all modules of the system so that I don’t need to switch 
between application urls.

**Acceptance Criteria:**

- **6.1.1.1:** The UI should have every module in a unique page to avoid switching between URLs.

- **6.1.1.2:** The user permissions should control the visibility of each module in the integrated UI.

**Dependencies/References:**

**US 6.1.2 & 6.1.3 & 6.1.4 & 6.1.5:** The UI for all the modules should be created first so can be integrated on one UI.

This user story will depend on the Angular to develop the integrated UI.

## 3. Analysis

To implement this user story, a single-page application (SPA) will be developed using Angular. The integrated UI should
have the follow elements: 

  * module specific areas
  * shared layout

It's important to unsure that the modules access respect the user permissions, so it is only access by users with the
right permissions.

## 4. Design

### 4.1. Realization

The logical, physical, development and scenario views diagrams are generic for all the use cases of the backoffice component.

#### Logical View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#1-logical-view).

#### Process View

##### Level 1

![Process View - Level 1]()

##### Level 2

_[This diagram is not relevant.]_

##### Level 3

![Process View - Level 3]()


#### Development View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#3-development-view).

#### Physical View

The diagrams can be found in the [team decision views folder](../../team-decisions/views/general-views.md#4-physical-view).
