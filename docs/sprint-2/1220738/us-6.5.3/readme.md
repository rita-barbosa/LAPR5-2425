# US 6.5.3

<!-- TOC -->
- [US 6.5.3](#us-653)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
      - [Logical View](#logical-view)
      - [Process View](#process-view)
        - [Level 1](#level-1)
        - [Level 2](#level-2)
        - [Level 3](#level-3)
      - [Development View](#development-view)
      - [Physical View](#physical-view)
<!-- TOC -->

## 1. Context

This is the first time this US is being worked on.
This US pertains to the Admin.

## 2. Requirements

**US 6.5.3:** As a healthcare staff member, I want to see the hospital/clinic floor illuminated with ambient and directional light.

**Acceptance Criteria:**

**US 6.5.3.1 -** The solution should be done in three.js.

**US 6.5.3.2 -** The solution should have ambient light.

**US 6.5.3.3 -** The solution should have directional light.

**Dependencies/References:**

**US 6.5.1:** This US will be dependent on US 6.5.1 as it needs the model for the hospital.

**US 6.5.2:** This US will be dependent on US 6.5.2 as it needs the texture.

**US 6.5.4:** This US will be dependent on US 6.5.4 as it needs the ability to move around to see the effects of light on the model.

**Client Clarifications:**

> None yet.

## 3. Analysis

In User Story 6.5.3, the goal is to create a visual experience of a hospital or clinic floor plan in Three.js with both ambient and directional lighting. This visual feedback should simulate natural and artificial light sources to enhance clarity and depth perception for healthcare staff viewing the space.

### Environmental Setup

The hospital floor will be constructed in a Three.js scene thanks to US 6.5.1, representing the physical layout and essential structures of the space. This includes walls, floors, and possibly furniture or equipment as simplified 3D objects.
Ensuring the scene scale matches realistic dimensions will allow for accurate light distribution and realistic shadows.

### Lighting Types

>***Ambient Light:*** This light will provide a base level of illumination across the entire scene, reducing harsh contrasts. The ambient light intensity should be set to a low-medium level to prevent the environment from looking overly flat.

>***Directional Light:*** This type of light mimics sunlight or primary artificial light sources. Positioning and angling this light will cast realistic shadows, giving the floor and structures a sense of depth and realism.

## 4. Design

### 4.1. Realization

// TODO

#### Logical View

// TODO

#### Process View

##### Level 1

// TODO

##### Level 2

_[This diagram is not relevant.]_

##### Level 3

// TODO

#### Development View

// TODO

#### Physical View

// TODO


