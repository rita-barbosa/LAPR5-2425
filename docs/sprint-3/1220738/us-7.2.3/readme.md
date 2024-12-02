# US 7.2.3

<!-- TOC -->
- [US 7.2.3](#us-7.2.3)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
    - [3.1 System Sequence Diagram](#31-system-sequence-diagram)
    - [3.2 Domain Model](#32-domain-model)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
    - [4.2. Class Diagram](#42-class-diagram)
    - [4.3. Applied Patterns](#43-applied-patterns)
    - [4.4. Tests](#44-tests)
  - [5. Implementation](#5-implementation)
  - [6. Integration/Demonstration](#6-integrationdemonstration)
  - [7. Observations](#7-observations)
<!-- TOC -->


## 1. Context

This is the first time this US is being worked on.
It relates to the Doctor.

## 2. Requirements

**US 7.2.3:** As a Doctor, I want to search for Allergies, so that I can use it to update the Patient Medical Record.

**Acceptance Criteria:**

**US 7.2.3.1:** The functionality must have a Angular UI to comply with the specifications of the US 7.1.1.

**US 7.2.3.2:** The functionality must have an implementation on the .NET server in oder to function correctly.

**US 7.2.3.3:** The functionality must be related to the patient's medical record as it needs to eventually add the searched allergy to the referred document.

**US 7.2.3.4:** The functionality must have a way to cancel the search when the doctor changes their mind, effectively dividing this US's functionality into 2, searching and then possibly adding.

**US 7.2.3.5:** The functionality must implement a rudimentary search system to show the allergies that can be added.

**US 7.2.3.6:** The functionality must not show the already added allergies OR inform the user they already have been added.

**Dependencies/References:**

**US 7.1.#:** Is dependent on all these US as they pertain to integrating different modules of the system.

**US 7.2.1:** Is dependent on this US because it has the database that will hold all allergies that will be used to search for the desired one.

**Client Clarifications:**

>**Question:** Gostariamos de lhe perguntar se existe alguma lista de alergias que prefere que utilizemos no sistema por default,  se sim, quais? Também gostariamos de perguntar se quando diz "I want to search for Allergies, so that I can use it to update the Patient Medical Record" o que é que implica a ultima parte? Que a procura é feita para adicionar imediatamente ao perfil de paciente ou se é apenas uma procura feita para ir buscar a informação sobre uma alergia por exemplo?
>
>**Answer**: Considerem a seguinte lista exemplo de alergias:
>1. Peanut Allergy
>2. Shellfish Allergy (e.g., shrimp, lobster)
>3. Milk Allergy (Dairy products)
>4. Egg Allergy
>5. Tree Nut Allergy (e.g., almonds, walnuts)
>6. Wheat Allergy
>7. Penicillin Allergy
>8. Sulfa Drugs Allergy (e.g., sulfamethoxazole)
>9. Aspirin Allergy
>10. Local Anesthetics Allergy (e.g., Lidocaine)
>11. Pollen Allergy (e.g., grass, ragweed)
>12. Dust Mite Allergy
>13. Mold Allergy
>14. Cat Dander Allergy
>15. Dog Dander Allergy
>16. Latex Allergy
>17. Nickel Allergy (common in jewelry or metal objects)
>18. Bee Sting Allergy
>19. Fire Ant Sting Allergy
>20. Perfume Allergy (fragrance sensitivity)

>**Question:** Gostariamos de lhe perguntar se existe alguma lista de alergias que prefere que utilizemos no sistema por default,  se sim, quais? Também gostariamos de perguntar se quando diz "I want to search for Allergies, so that I can use it to update the Patient Medical Record" o que é que implica a ultima parte? Que a procura é feita para adicionar imediatamente ao perfil de paciente ou se é apenas uma procura feita para ir buscar a informação sobre uma alergia por exemplo?
>
>**Answer**: Em relação à segunda parte da pergunta, o que se pretende é que o médico possa indicar quais as alergias que o paciente tem seleccionando-as da lista de alergias existentes no sistema. notem que um paciente pode ter mais que uma alergia.

## 3. Analysis

This User Story asks to implement: 

- Firstly, the back-end implementation of a request that returns a list of allergies from the new MongoDB database that is implemented by the US 7.2.1. 

- Secondly, to implement the front end part of this US, in order to comply with the US's 7.2.3.1 acceptance criteria.  

- Thirdly, to add the possibility to add new allergies and also add them to a patient's medical records.

This User Story will follow the same system as previous USs with the objective to list certain objects.

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