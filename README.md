# CHIP-8 INTERPRETER

![space invaders game over](https://raw.githubusercontent.com/ghikio/chip-8/master/.extras/readme-main-image.png)

This project aims to fully implement a Chip-8 interpreter in Clojure using a fully (or almost) functional approach (as far as I understand the paradigm, let me know if I missed something :)). Is all done from scratch and it's only dependency is [quil](https://github.com/quil/quil), used to draw the screen. 

Also it's aimed to improve my clojure understanding through a kind of real project, so if you find something that could have been done in a more idiomatic way, let me know!

##### v1.0.0 todo:

* [ ] Implement sound
* [x] Fix collision bug
* [x] Drastically improve execution speed, it's unplayable in this state :(
* [ ] Okay now it goes too fast, find a way to limit the FPS... someday
* [x] Remove the hardcoded ROM path and write it as a argument specified from terminal

##### main references:

- [Cowgod's Chip-8 Technical Reference v1.0](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#2.1)
- [chip8-clj](https://github.com/cesarolea/chip8-clj)
