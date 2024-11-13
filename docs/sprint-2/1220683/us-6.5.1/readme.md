# US 6.5.1

<!-- TOC -->
* [US 6.5.1](#us-651)
  * [1. Context](#1-context)
  * [2. Requirements](#2-requirements)
  * [3. Analysis](#3-analysis)
  * [4. Design](#4-design)
    * [4.1. Realization](#41-realization)
<!-- TOC -->


## 1. Context

This is the first time this user story is worked on.

## 2. Requirements

**US 6.5.1:** As a healthcare staff member, I want to see a 3D representation of the hospital/clinic floor.

**Acceptance Criteria:**

- 6.5.1.1: Its description should be imported from a JSON (JavaScript Object Notation) formatted file.

- 6.5.1.2: The floor must consist of several surgical rooms.

- 6.5.1.3: Each room must be enclosed by walls and include a door and a surgical table.

- 6.5.1.4: There should be no representation of the ceiling.

- 6.5.1.5: If a room is being used at any given time, a 3D model of a human body should be lying on the table.

- 6.5.1.6: Must be implemented in an higher-level API such as Three.js, WebGL or Babylon.js.

**Dependencies/References:**

This functionality has no dependencies, however it will be the base for other features, such has the implementation of 
textures (US 6.5.2), dynamic lighting (US 5.6.3) and camara movements via mouse (US 6.2.4).

**Client Clarifications:**

> **Question:** When should the human body be lying on the table? if at this precise moment there is a surgery going on? or if there are surgeries going to happen that day?
>
> **Answer:** If and only if there is a surgery going on on that precise room at that precise moment.

## 3. Analysis

This component can will be implemented using a higher-level API, three.js.

The hospital floor visualization comprises a set of rooms, each equipped with a door, walls, and a surgical table.
This representation must be described in a JSON file with the following information:

>- floor dimensions
>- An array of rooms, each with:
>   - room id
>   - room position on the floor map
>   - room dimensions (length, width)
> - wall inner texture
> - wall outer texture
> - surgical bed 3D model url
> - door model 3D url

The file must include any other items the team decides to introduce into the 3D representation.
No ceiling is used in the floor, ensure a clear view of all the components.

This file is to be loaded when a staff wants to check which rooms are available and their scheduled operations.

The following image represents the type of 3D visualization the team is aiming for:

![hospital_room.svg](hospital_room.svg)

## 4. Design

### 4.1. Realization

This functionality will use Three.js, a JavaScript library and API designed for creating and displaying animated 3D graphics
in a web browser using WebGL, compatible across different browsers.

This model will be composed of the following javascript classes:

* wall template - initializes a box geometry with its textures
* floor template - initializes a box geometry with its textures
* hospital floor template - compiles clones of many instances to generate the hospital floor
* surgical table template - loads a 3D model of a surgical table and its textures
* door template - loads a 3D model of a door and its textures
* patient template - loads a 3D model of a human and its textures
* hospital floor simulation template - initializes the 3D visualization and other components that will be incorporated later

Lights, textures and mouse based camara movements related classes will be added later by other user stories.

The following diagram goes in-depth about each instance attributes/transformations.

![3D_visualization_main_components.svg](3D_visualization_main_components.svg)




