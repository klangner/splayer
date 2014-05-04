# Specification version 0.1

## Model

* Player contains model which defines:
** Team name
** Is goalkeeper
** Uniform number
** Functions which process:
*** *init* message
*** *see* message
*** *sense_body* message


## Player and world state

* Player state consists of:
** Player position
** Player direction

* World state consist of
** Position of all players
** ball position
** Play mode


## Starting and exiting player

* After starting the player will send *init* message and start accepting messages from server
* After receiving *fullstate* message with playMode == game_over send *bye* message and disconnect.
