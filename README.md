# Flappy Bird NeuroEvolution learning with Erlang
**by Omri Gil and Tomer Shaked**

This project is the final assignment in the "Functional programming for parallel and distributed systems". we implemented a full flappy bird clone which is autonomously controlled by a neural network.

the network learns through a genetic process. In each iteration we spawn up to a few thousand networks and compare their result solving the same pipes. we then kill the worst 75% of networks and use the 25% remaining networks to repopulate. each repopulated network passes through a random mutation.

the whole system is fault tolerant. if a neural network falls mid run it would get restarted without any notice in the system. if a whole computer/node gets disconnected all the responsibilities of that computer will be passed to a different live node without stopping the learning process or the graphics.

## Installation
we are using erlang version 22 on ubuntu. this will probably also work on windows but it was not tested.
Use the package manager to install erlang:
```bash
sodu apt-get install erlang
```

and your ready to go!
## Usage
there are multiple ways you can run the program:

### single console
to run the code on a single console change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE, 'pc1@<enter pc1 hostname>').
-define(PC1, 'pc1@<enter pc1 hostname>').
-define(PC2, 'pc1@<enter pc1 hostname>').
-define(PC3, 'pc1@<enter pc1 hostname>').
-define(PC4, 'pc1@<enter pc1 hostname>').
```
then from a bash terminal run the following command:
```bash
erl -sname 'pc1' -setcookie yummy
```
next move to the Run instructions.

### four consoles on one computer
to run the code on four consoles change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE, 'pc1@<enter pc1 hostname>').
-define(PC1, 'pc1@<enter pc1 hostname>').
-define(PC2, 'pc2@<enter pc1 hostname>').
-define(PC3, 'pc3@<enter pc1 hostname>').
-define(PC4, 'pc4@<enter pc1 hostname>').
```
then in each bash terminal run the following command:
```bash
erl -sname 'pc<x>' -setcookie yummy
```
where <x> is indexed between 1-4.

next move to the Run instructions.

### four consoles on four different computers
to run the code on a four computers change the configuration in src/Constants.hrl to:

```erlang
-define(GRAPHICS_NODE, 'pc1@<enter pc1 ip>').
-define(PC1, 'pc1@<enter pc1 ip>').
-define(PC2, 'pc2@<enter pc2 ip>').
-define(PC3, 'pc3@<enter pc3 ip>').
-define(PC4, 'pc4@<enter pc4 ip>').
```

then in each bash terminal run the following command:
```bash
erl -name 'pc<x>@<pc ip address>' -setcookie yummy
```
where <x> is indexed between 1-4 and <pc ip address> is the ip of the computer which runs the code.

next move to the Run instructions.

## Run
to run the code in each of your open consoles run the following command:
```erlang
cover:compile_directory().
```
to compile the code.
then only on the console which is pc1 run the following command:
```erlang
graphics:start(<number_of_networks>).
```
where <number of networks> is how many birds you want to run in parallel on all computers. number of networks can run between 100 to 2000(not limited) depending on the strength of the computer/s you are using. The only restriction is that the number of networks will divide by 4. suggested default is 1000.

to start running the program press the start button. you can use the toggle graphics button to disable the graphics and get up to a x10 times increase in speed.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

