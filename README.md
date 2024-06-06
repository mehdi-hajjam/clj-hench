<h1 align="center">clj-hench</h1>
<p align="center">
<img src="https://img.shields.io/badge/License-EPL_1.0-red.svg">
</p>

_This repository contains the code I have used to play in four [Battlesnake](https://play.battlesnake.com/) leagues as clj-hench._

---

<p align="center">
<a href="https://ibb.co/q70XYsK"><img src="https://static.battlesnake.com/play/releases/2.1.1/ui/img/404.png" alt="Capture-d-e-cran-2023-08-07-a-18-05-33" border="0"></a>

### BUILD AND DEPLOYMENT

```bash
# Create the uberjar
$ clojure -X:uberjar :jar hench.jar
# Send it to the server      
$ rsync -avz hench.jar root@IP:/root 
```

#### HOSTING

During the different leagues, I have used Digital Ocean droplets. 
The minimal configuration was enough to go through the League and final tournaments.

### RESULTS

I have played in 2021 Fall League (48th, [3rd ex aequo](https://x.com/i/status/1462131662603489288) of the Fall League Gold Tier Championship), 2022 Spring League, 2022 Summer League and 2022 Fall League (31th, quarter finals of the Platinum tournament).

The version I used for the different leagues is archived in a branch adequately named.

I encourage you to [register](https://play.battlesnake.com/) and play the game, it's a lot of fun! 

### TRIVIA

- `clj-hench` was the only Clojure-based snake at the time I played.

- I came up with their current tag line "A competitive game where your code is the controller." during an interview with [Taraneh](https://play.battlesnake.com/profile/taraneh).

- `hench` means snake in Moroccan Arabic.

### LICENSE

_Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version._