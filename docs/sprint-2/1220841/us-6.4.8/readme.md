# US 6.4.8

<!-- TOC -->
- [US 6.4.8](#us-648)
  - [1. Context](#1-context)
  - [2. Requirements](#2-requirements)
  - [3. Analysis](#3-analysis)
  - [4. Design](#4-design)
    - [4.1. Realization](#41-realization)
<!-- TOC -->


## 1. Context

This is the first time this user story is worked on.

## 2. Requirements

**US 6.4.8:** As system administrator, I want to get users with more than 3 incorrect accesses attempts.

## 3. Analysis

For this user story, a script needs to be developed to count users' incorrect access attempts by analyzing the `/etc/log/auth.log` file. The script should then record information for users with more than three failed attempts in a specific results file. If this file does not exist when the script runs, it should be created automatically.

References: [monitoring-failed-login-attempts-on-linux](https://www.networkworld.com/article/969378/monitoring-failed-login-attempts-on-linux.html)

## 4. Design

### 4.1. Realization

//TODO