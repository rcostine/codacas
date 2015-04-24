codacas
=======
Re-implementation of the codacas space war game, illustrating use of Scala with Java NIO, and actors.

Background
----------
This was a popular multi-user game played on the Control Data hardware at Temple University in the early 80s.

Environment
-----------
This rendition is setup as a Play 2.3 (typesafe-activator) project. Currently, there is no web-based interaction with the
Game and Player actors (but this is planned). The intention is to expand the game to use scala.js on the front-end, and have
it connect to the backend using websockets. This will allow streaming of game related events back to the browser in realtime.

Server setup
------------
Currently, the way I've been starting the game server is to run the org.costine.codacas.system.Codacas application 
with two parameters.

The first parameter is the listen port.

The second parameter is the number of milliseconds that the universe will be updated.

For example: org.costine.codacas.system.Codacas 2001 100

That starts the game, and listens on TCP port 2001, and updates the game state every tenth of a second.

I've been loading the application in intellij, and running the class there. Eventually, once this becomes a real Play app,
the game engine will be started as a background job.

"Client" setup
--------------
To connect to the game, I currently use netcat (nc) on my MAC (OSX) as follows in a bash script called "codacas":

```bash
#!/bin/bash
clear
save_state=$(stty -g)
stty raw -echo
nc localhost 2001
rc=$?
stty "$save_state"
if [ $rc -ne 0 ]
then
        echo
        echo "Game down for maintenance, try again later."
        sleep 5
fi
```