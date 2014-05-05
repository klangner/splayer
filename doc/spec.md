# Specification

## Data structures

* **PlayerModel** consists of:
  * team_name
  * player_play_style
  * uniform_number

* **PlayerState** consists of:
  * player_position
  * player_direction

* **WorldState** consist of:
  * players_position (Contains position of all players)
  * ball_position
  * play_mode


## Behaviour

### Starting and exiting player

* After starting, the player will send **init** message and start accepting messages from server.
* When playMode == game_over send **bye** message and disconnect.


### Processing server messages

* (init Side Unum PlayMode)
  * update uniform_number in **PlayerModel**
  * update play_mode in **WorldState**
  * Send *move* command if play_mode == before_kick_off
