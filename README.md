# Hotel Management System

## Overview

This is a functional programming project designed to mimic a hotel management system.

## Main BNF structure

```markdown

<add> ::= "ADD. " <hotelsID> <hotel> 
<remove> ::= "REMOVE. " <hotelsID> 

<make_reservation> ::= "MAKE RESERVATION. " <guest> <hotelsID> <check_in> <check_out> <price>
<cancel_reservation> ::= "CANCEL RESERVATION. " <reservationID>
<add_additional_guest> ::= "ADD ADDITIONAL GUEST. " <guest> <reservationID>

<check_in> ::= "CHECK IN: " <date> " " <time> ". "
<check_out> ::= "CHECK OUT: " <date> " " <time> ". "
<guest> ::= "GUEST: " <name> " " <surname> ". "

<hotel> ::= "HOTEL: " <text> ". " |  <hotel> "CHAIN OF " <hotel> | <hotel> <floors>
<floors> ::= <floor> | <floor> <floors>
<floor> ::= "FLOOR: " <number> ". " <rooms>
<rooms> :: <room> | <room> <rooms>
<room> ::= "ROOM: " <number> ". " | <room> "ROOM SECTION " <room> | <room> <amenities> ". "

<price> ::= "PRICE: " <number> ". "
<hotelsID> ::= <number>
<reservationID> :: <number>

<amenities> ::= "AMENITIES: " <amenity> | <amenities> ", " <amenity>
<amenity> ::= "TV" | "WI-FI" | "MINI-BAR" | "BALCONY" | "AC"

<number> ::= [0-9]+
<text> ::= ( [a-z] | [A-Z])+
<digit> ::= [0-9]
<char> ::= ( [a-z] | [A-Z])


<name> ::= <text>
<surname> ::= <text>
<date> ::= <digit> <digit> <digit> <digit> "-" <digit> <digit> "-" <digit> <digit>
<time> ::= <digit> <digit> ":" <digit> <digit>
```

### Changes regarding BNF structure and Lib2.hs

Previously, when adding hotel entities, the id of the hotel would be automatically assigned based on when the command was executed, however, since the implementation of concurrent stateTransitioning, this proved troublesome. ID's started to become non-deterministic (after saving and loading, different reservations could be made with the same input).
So, to improve reliability, changes to MakeReservation and Add commands were required. User must now provide an ID which they want to assign a hotel to.
Now, if a command to add a hotel has the same id as a hotel previously added, the command will fail and spew out an invalid command error.

Additional changes were required to Lib2.hs show instance in query, date, time, price datatypes.

### Commands

* `add` - adds a specific hotel entity. The entity can include a hotel, a specific room within a hotel, special properties.

    Example:
    ```
    ADD. 1. HOTEL: Medis. CHAIN OF. HOTEL: Medis. FLOOR: 1. ROOM: 105. AMENITIES: Balcony, TV. 
    ```
* `remove` - removes a hotel entity from the available hotel list.

    Example:
    ```
    REMOVE. 1. 
    ```
* `make_reservation` - makes a reservation of a corresponding hotel room. The hotel room must exist in the available hotel list and it is called by inputting the same hotel. Future configurations could be made for the reservation to call on the ID. 
    
    Example:
    ```
    MAKE RESERVATION. GUEST: ELVINAS SVILPA. 1. CHECK IN: 2024-09-16 20:20. CHECK OUT: 2024-09-20 12:00. PRICE: 510.

    ```
* `cancel_reservation` - cancels a reservation based on its reservation ID.

    Example:
    ```
    CANCEL RESERVATION. 1. 
    ```
* `add_additional_guest` - adds an additional guest to a room based on its reservation ID.

    Example:
    ```
    ADD ADDITIONAL GUEST. GUEST: Antrasis Elvinas. 1. 
    ```


### Batch queries and Lib3.hs commands

* `SAVE` - saves the current state in an efficient way (using marshall state). Information is saved in `info.txt`.

    Example:
    ```
    >>> SAVE
    ```

* `LOAD` - loads the state from `info.txt`

    Example:
    ```
    >>> LOAD
    ```

* `LIST` - lists the formatted state

    Example:
    ```
    >>> LIST
    ```

* Batch queries
    
    Type `:paste` to enable batch-mode. Then, all queries must start with `BEGIN` and end with `END`.

    ```markdown
    >>> :paste
    | BEGIN
    | <...>
    | END
    ```

    (`Ctrl + D` or `Control^ + D` on Mac) to end Batch query mode.



